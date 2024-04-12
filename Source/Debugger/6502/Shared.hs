unit Shared
{
    uses "ZeroPage"
    
    uses "/Source/Compiler/Types"
    uses "/Source/Runtime/6502/ZeroPage.asm"
    
    delegate byte GetRAMByteDelegate(uint address);
    GetRAMByteDelegate getRAMByte;
    uint getRAMWord(uint address) { return getRAMByte(address) + (getRAMByte(address+1) << 8); }
    
    char StringGetChar(uint sPtr, uint index)
    {
        byte bvalue = getRAMByte(sPtr+4+index);
        return char(bvalue);
    }
    uint ArrayGetItem(uint aPtr, uint index)
    {
        uint item;
        type avtype = type(getRAMByte(aPtr+4));
        switch (avtype)
        {
            case bool:
            {
                byte slot = byte(index & 0x07);
                index = index >> 3;
                item = getRAMByte(aPtr+5+index);
                switch (slot)
                {
                    case 0:
                    {
                        item = item & 0x01;
                    }
                    case 1:
                    {
                        item = item & 0x02;
                    }
                    case 2:
                    {
                        item = item & 0x04;
                    }
                    case 3:
                    {
                        item = item & 0x08;
                    }
                    case 4:
                    {
                        item = item & 0x10;
                    }
                    case 5:
                    {
                        item = item & 0x20;
                    }
                    case 6:
                    {
                        item = item & 0x40;
                    }
                    case 7:
                    {
                        item = item & 0x80;
                    }
                }
                if (item != 0)
                {
                    item = 1;
                }
                else
                {
                    item = 0;
                }
            }
            case byte:
            case char:
            {
                item = getRAMByte(aPtr+5+index);
            }
            default:
            {
                item = getRAMWord(aPtr+5+index*2);           
            }
        }
        return item;
    }
    
    bool IsMachineReferenceType(uint typeValue)
    {
        return typeValue >= 0x0D;
    }
    bool IsMachineReferenceType(string typeName)
    {
        return !Types.IsValueType(typeName);
    }
        
    
    string TypeToString(uint value, string vtype, bool isReference, uint limit)
    {
        string content;
        if (isReference)
        {
            uint refValue = getRAMByte(value + 0x0600) + getRAMByte(value + 0x0700) << 8;
            content = TypeToString(refValue, vtype, false, limit);
        }
        else
        {
            string vType;
            string kType;
            string tname = vtype;
            
            switch (vtype)
            {
                case "char":
                {
                    content = "'" + char(value) + "'";
                }
                case "byte":
                {
                    if (IsHexDisplayMode)
                    {
                        content = "0x" + value.ToHexString(2);
                    }
                    else
                    {
                        content = value.ToString();
                    }
                }
                case "uint":
                {
                    if (IsHexDisplayMode)
                    {
                        content = "0x" + value.ToHexString(4);
                    }
                    else
                    {
                        content = value.ToString();
                    }
                }
                case "bool":
                {
                    if (value == 0)
                    {
                        content = "false";
                    }
                    else
                    {
                        content = "true";
                    }
                }
                case "string":
                {
                    uint length = getRAMWord(value+2);
                    string str;
                    for (uint i = 0; i < length; i++)
                    {
                        str = str + StringGetChar(value, i);
                    }
                    if (IsHexDisplayMode)
                    {
                        
                        uint i = 0;
                        loop
                        {
                            if (i == str.Length) { break; }
                            content += (byte(str[i])).ToHexString(2) + " ";
                            if (content.Length >= limit) { break; }
                            i++;
                        }
                    }
                    else
                    {
                        if ((str.Length > limit) && (limit > 4))
                        {
                            str = str.Substring(0, limit-4);
                            str = str + "..";
                        }
                        content = '"' + str + '"';
                    }
                }
                case "array":
                {
                    content = "[";
                    type avtype = type(getRAMByte(value+4));
                    string avtypes = avtype.ToString();
                    uint asize = getRAMWord(value+2);
                    bool first = true;
                    for (uint i=0; i < asize; i++)
                    {
                        if (!first)
                        {
                            content += ", ";
                        }
                        if (content.Length >= limit)
                        {
                            content += "..";
                            break;
                        }
                        uint item = ArrayGetItem(value, i);
                        content += TypeToString(item, avtypes, false, limit);
                        uint cl = content.Length;
                        first = false;
                    }
                    content += "]";
                }
                case "int":
                {
                    if (IsHexDisplayMode)
                    {
                        content = "0x" + value.ToHexString(4); // easy since 'value' is a uint
                    }
                    else
                    {
                        int ivalue = Int.FromBytes(byte(value & 0xFF), byte(value >> 8));
                        content = ivalue.ToString();
                    }
                }
                default:
                {
                    content = value.ToHexString(4) + " [" + vtype + "]";
                }
            }
        }
        return content;
    }
 
    
     
    ShowHopperHeap(GetRAMByteDelegate currentGetRAMByte)
    {
        getRAMByte = currentGetRAMByte;
        
        uint heapStart = getRAMByte(ZHEAPSTART) << 8;
        uint heapSize  = getRAMByte(ZHEAPSIZE)  << 8;
        uint freeList  = getRAMByte(ZFREELISTL) + getRAMByte(ZFREELISTH) << 8;
        
        // walk the free list
        <uint> freeBlocks;
        uint current = freeList;
        uint total = 0;
        PrintLn();
        PrintLn("Free List: " + freeList.ToHexString(4));
        loop
        {
            uint blockSize = getRAMWord(current);
            uint nextBlock = getRAMWord(current+2);
            uint prevBlock = getRAMWord(current+4);
            
            freeBlocks.Append(current);
            
            PrintLn("  0x" + current.ToHexString(4) + " 0x" + blockSize.ToHexString(4)+ " 0x" + nextBlock.ToHexString(4)+ " 0x" + prevBlock.ToHexString(4), Colour.LightestGray, Colour.Black); 
            if (nextBlock == 0) { break; }
            current = nextBlock;
        }
        PrintLn("Heap: " + heapStart.ToHexString(4));
        current = heapStart;
        loop
        {
            uint blockSize = getRAMWord(current);
            if (!freeBlocks.Contains(current))
            {
                byte objectType     = getRAMByte(current+2);
                byte referenceCount = getRAMByte(current+3);
                
                Print("  0x" + current.ToHexString(4) + " 0x" + blockSize.ToHexString(4) + " [0x" + objectType.ToHexString(2) + "-" + referenceCount.ToHexString(2) + "]", Colour.LightestGray, Colour.Black);
                type et = type(objectType);
                switch (et)
                {
                    case array:
                    {
                        uint count = getRAMByte(current+4) + getRAMByte(current+5) << 8;
                        PrintLn(" (array: [" + count.ToString() + "])");
                        string content = TypeToString(current+2, "array", false, 100);
                        if (content.Length > 0)
                        {
                            PrintLn("      " + content);
                        }
                    }
                    case string:
                    {
                        uint length = getRAMByte(current+4) + getRAMByte(current+5) << 8;
                        PrintLn(" (string: [" + length.ToString() + "])");
                        string content = TypeToString(current+2, "string", false, 100);
                        if (content.Length > 0)
                        {
                            PrintLn("      " + content);
                        }
                    }
                    default:
                    {
                        PrintLn();
                    }
                }
            }
            if (blockSize == 0)
            {
                break; // uninitialized heap?
            }
            current += blockSize;
            if (current >= heapStart + heapSize) { break; }
        }
    }
    ShowHopperValueStack(GetRAMByteDelegate currentGetRAMByte)
    {
        getRAMByte = currentGetRAMByte;           
        byte sp        = getRAMByte(ZSP);
        byte bp        = getRAMByte(ZBP);
        
        PrintLn();
        byte spi = sp;
        
        Print("SP -> ", Colour.MatrixRed, Colour.Black);
        PrintLn("0x" + sp.ToHexString(2), Colour.LightestGray, Colour.Black);
        uint entries = 0;
        loop
        {
            if (spi == 0) { break; }
            spi--;
            uint value = getRAMByte(0x0600 + spi) + (getRAMByte(0x0700 + spi) << 8);
            byte vtype = byte(getRAMByte(0x0500 + spi));
            string leftText = "      ";
            if (spi == bp)
            {
                leftText = "BP -> ";
            }
            
            string tstring = Type.ToString(type(vtype));
            
            bool isReference = tstring == "ref";
            string content = TypeToString(value, tstring, isReference, 30);
            
            string referenceCount = "      ";
            if (IsMachineReferenceType(vtype))
            {
                byte count = getRAMByte(value + 1);
                referenceCount = "[0x" + count.ToHexString(2) + "]";
            }
            
            Print(leftText, Colour.MatrixRed, Colour.Black);
            if (spi == bp)
            {
                Print("0x" + spi.ToHexString(2), Colour.LightestGray, Colour.Black);
            }
            else
            {
                Print("0x" + spi.ToHexString(2));
            }
            Print(" 0x" + value.ToHexString(4) + ":0x" + vtype.ToHexString(2) + referenceCount, Colour.LightestGray, Colour.Black); 
            Print(" (" + tstring + ")" + " " + content);
            PrintLn();
            entries++;
            if (entries == 12) 
            { 
               PrintLn("      ...");
               break; 
            }
        }
    }
    ShowVariableW(string name, byte zpIndex)
    {
        Print("  " + (name + ":").Pad(' ', 20));
        uint ui = getRAMByte(zpIndex) + getRAMByte(zpIndex+1) << 8;
        PrintLn(" 0x" + ui.ToHexString(4) + " (" + ui.ToString() + ")", Colour.LightestGray, Colour.Black); 
    }
    ShowVariableB(string name, byte zpIndex)
    {
        Print("  " + (name + ":").Pad(' ', 20));
        uint b = getRAMByte(zpIndex);
        PrintLn(" 0x" + b.ToHexString(2), Colour.LightestGray, Colour.Black); 
    }
    ShowHopperStringVariables(GetRAMByteDelegate currentGetRAMByte)
    {
        getRAMByte = currentGetRAMByte;
        PrintLn();
        ShowVariableW("FLENGTH", ZP.LLENGTH);
        ShowVariableW("LCOUNT",  ZP.LCOUNT);
        ShowVariableW("FSIZE",   ZP.FSIZE);
        ShowVariableB("FTYPE",   ZP.FTYPE);
        ShowVariableW("FSOURCEADDRESS",      ZP.FSOURCEADDRESS);
        ShowVariableW("FDESTINATIONADDRESS", ZP.FDESTINATIONADDRESS);
    }
}
