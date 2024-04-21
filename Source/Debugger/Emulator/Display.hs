unit Display
{
    <uint,uint> methodSizes;     // <index, length>
    <uint,uint> methodAddresses; // <address,index>
    
    <uint,string> sourceMap;     // Z80: <address,sourceLine>
    
    string pathLoaded;
    <string> sourceLines;

#ifdef Z80        
    
    FindMethods(<byte> code)
    {
        <uint,uint> methodLastAddresses;  // <index,address>
        _ = GetMethodAddresses(code, ref methodAddresses, ref methodLastAddresses);
        foreach (var sf in methodAddresses)
        {
            uint index = sf.value;
            uint methodAddress = sf.key;
            uint methodLastAddress = methodLastAddresses[index];
            uint methodSize = methodLastAddress - methodAddress + 1;
            methodSizes[index] = methodSize;
        }
    }
    uint GetZ80MethodIndex(uint address)
    {
        foreach (var sf in methodAddresses)
        {
            uint methodAddress = sf.key;
            uint index = sf.value;
            if (address >= methodAddress)
            {
                uint lastAddress = methodAddress + methodSizes[index];
                if (address < methodAddress + methodSizes[index])
                {
                    return index;
                }
            }         
        }
        return 0xFFFF;
    }
    string GetZ80ArgumentInfo(uint methodIndex, int offset, ref string typeName, ref string refName)
    {
        string name;
        typeName = "";
        refName = "";
        <string, <string> > argumentInfo= Code.GetArguments(methodIndex);
        string soffset = offset.ToString();
        if (argumentInfo.Contains(soffset))
        {
            <string> info = argumentInfo[soffset]; // <ref, type, name>
            name = info[2];
            typeName = info[1];
            if (info[0] == "")
            {
                refName = "ref ";
            }
        }
        return name;
    }
    string GetZ80LocalName(uint address, uint methodIndex, int offset, ref string typeName)
    {
        string name;
        typeName = "";
        //uint methodStartAddress;
        bool found;
        foreach (var sf in methodAddresses)
        {
            uint methodAddress = sf.key;
            uint index = sf.value;
            if (index == methodIndex)
            {
                //methodStartAddress = methodAddress;
                //Print(" " + methodStartAddress.ToHexString(4));
                found = true;
                break;
            }
        }
        if (found)
        {
            string soffset = offset.ToString();
            <string, <string> > locals = Code.GetLocals(methodIndex);
            foreach (var kv in locals)
            {
                //string range = kv.key;
                <string> info = kv.value; // <name, type, offset>
                if (info[2] == soffset)
                {
                    /*
                    <string> parts = range.Split('-');
                    uint startRange;
                    uint endRange;
                    _ = UInt.TryParse(parts[0], ref startRange);
                    _ = UInt.TryParse(parts[1], ref endRange);
                    startRange += methodStartAddress;
                    endRange += methodStartAddress;
                    
                    if ((address >= startRange) && (address <= endRange))
                    {
                        Print(" " + startRange.ToHexString(4) + "-" + endRange.ToHexString(4));
                    */
                        typeName = info[1];
                        name     = info[0];
                        break;
                    //}
                }
            }
        }
        return name;
    }
#endif
  
    Initialize()
    {
        //Screen.Clear();
        
#ifdef Z80        
        sourceMap = MapZSource();
#else        
        // find the method address ranges for disassembly
        methodSizes = Code.GetMethodSizes();
        uint indexMax = 0;
        foreach (var sz in methodSizes)
        {
            if (sz.key > indexMax)
            {
                indexMax = sz.key;
            }
        }
        uint methodAddress = 0;
        for (uint index = 0; index <= indexMax; index++)
        {
            if (!methodSizes.Contains(index)) { continue; }   
            uint methodSize = methodSizes[index];
            methodAddresses[methodAddress] = index;
            methodAddress += methodSize;
        }
#endif        
    } 
    
    string getSourceLine(string path, string lnum)
    {
        if (pathLoaded != path)
        {
            file sourceFile = File.Open(path);
            if (sourceFile.IsValid())
            {
                sourceLines.Clear();
                pathLoaded = path;
                loop
                {
                    string ln = sourceFile.ReadLine();
                    if (ln.Length == 0)
                    {
                        if (!sourceFile.IsValid())
                        {
                            break;
                        }
                    }
                    sourceLines.Append(ln);
                }
            }
        }
        string sourceLine;
        uint iline;
        if (UInt.TryParse(lnum, ref iline))
        {
            if (iline > 0)
            {
                iline--;
            }
            if (sourceLines.Count > iline)
            {
                sourceLine = sourceLines[iline];
            }
        }
        return sourceLine;
    }
    
    MemoryDump(string hexpage)
    {
        uint page;
        _ = UInt.TryParse("0x" + hexpage, ref page);
        
        page = page << 8;
        
        PrintLn();
        Print("      ");
        for (byte i = 0; i < 16; i++)
        {
           Print("x" + i.ToHex() + " ");  
           if (i == 7)
           {
               Print(' ');
           }  
        }
        string plainText;
        for (uint i = 0; i < 256; i++)
        {
            uint address = page + i;
            if (i % 16 == 0)
            {
                PrintLn();
                Print((address).ToHexString(4));
            }
            if (i % 8 == 0)
            {
                Print(" ");
            }
            byte b = GetMemory(address);
            Print(" " + (b).ToHexString(2), Colour.LightestGray, Colour.Black);
            char ch = '.';
            if ((b >= 32) && (b < 127))
            {
                ch = char(b);
            }
            plainText += ch;
            if (i % 16 == 15)
            {
                Print("  " + plainText);
                plainText = "";
            }
        }
        if (plainText != "")
        {
            Print("  " + plainText);
        }
        PrintLn();
    }
    
    ShowDisassembly(uint address, uint instructions)
    {
        string commentPrefix = "// ";
        if (OGMode)
        {
            commentPrefix = "; ";
        }
        if (address != InvalidAddress)
        {
            PrintLn();
        }
        
        string src;
        string srcName;
        
//#ifndef Z80        
        //<uint,uint> methodSizes;     // <index, length>
        //<uint,uint> methodAddresses; // <address,index>
        uint methodIndex;
        uint methodStart;
        uint methodLength;
        bool found;
        foreach (var kv in methodAddresses)
        {
            methodStart = kv.key + CPU.Entry;
            if (address >= methodStart)
            {
                methodIndex = kv.value;
                methodLength = methodSizes[methodIndex];
                if (address < methodStart + methodLength)
                {
                    found = true;
                    break;
                }
            }
        }
        <string,variant> methodSymbols;
        <string,string> debugInfo;
        if (found)
        {
            methodSymbols = Code.GetMethodSymbols(methodIndex);
            if (methodSymbols.Count != 0)
            {
                src = methodSymbols["source"];
                srcName = Path.GetFileName(src);
                string ln = methodSymbols["line"];
                string nm = methodSymbols["name"];
                debugInfo = methodSymbols["debug"];
            }
        }
//#endif        
        
        bool firstLine = true;
        loop
        {
            if (instructions == 0) { break; }
            if (address == InvalidAddress) { break; }
//#ifndef Z80
            if (address > methodStart + methodLength-1) { break; }
//#endif            
            string operandString;
            uint operand;
            string comment;
            string debugLine;
            
#ifdef Z80
            OpCode instruction = GetInstruction(address);
            byte opCodeLength  = GetOpCodeLength(instruction);
            byte length = GetInstructionLength(instruction);
            byte operandLength = length - opCodeLength;
            if (operandLength == 1)
            {
                operand = GetMemory(address+opCodeLength);
            }
            else if (operandLength == 2)
            {
                operand = GetMemory(address+opCodeLength) + (GetMemory(address+opCodeLength+1) << 8); 
            }
            
            //<uint,string> sourceMap;     // Z80: <address,sourceLine>
            if (sourceMap.Contains(address))
            {
                string sourceLine = sourceMap[address];
                <string> parts = sourceLine.Split(':');
                src = parts[0];
                srcName = Path.GetFileName(src);
                debugLine = parts[1];
            }
#else

            OpCode instruction = OpCode(GetMemory(address));
            byte length        = GetInstructionLength(instruction);
            if (length == 2)
            {
                operand = GetMemory(address+1);
            }
            else if (length == 3)
            {
                operand = GetMemory(address+1) + (GetMemory(address+2) << 8); 
            }
            string debugAddress = (address - methodStart).ToString();
            if (debugInfo.Contains(debugAddress))
            {
                debugLine = debugInfo[debugAddress];
            }
#endif
            if (debugLine.Length != 0)
            {
                string sourceLine = getSourceLine(src, debugLine);
                if (sourceLine.Length != 0)
                {
                    comment = sourceLine.Trim();
                    if (comment.Length < 34)
                    {
                        comment = comment.Pad(' ', 34);
                        comment = comment + srcName + ":" + debugLine;
                    }
                }
                else
                {
                    comment = src + ":" + debugLine;  
                }
            }
            uint colour = Colour.MatrixBlue;
            if (firstLine)
            {
                colour = Colour.Ocean;
            }
            string disassembly = Disassemble(address, instruction, operand);
            if (address == CPU.PC)
            {
                string location = disassembly.Substring(0,6);
                disassembly = disassembly.Substring(6);
                Print("PC -> ", Colour.MatrixRed, Colour.Black);
                Print(location, Colour.LightestGray, Colour.Black);
                Print(disassembly.Pad(' ', 42), colour, Colour.Black); // disassembly
            }
            else
            {
                Print("      " + disassembly.Pad(' ', 48), colour, Colour.Black); // disassembly
            }
            if (comment.Length != 0)
            {
                comment = comment.Replace("//", "  ");
                Print(commentPrefix + comment, Colour.MatrixGreen, Colour.Black); // comment
            }
            PrintLn();
            
            address += length;
            instructions--;
            firstLine = false;
        }
    }
    ShowCurrentInstruction(uint instructions)
    {
        ShowDisassembly(CPU.PC, instructions);
    }
    ShowZeroPage()
    {
        MemoryDump("00");
    }
    ShowCallStack()
    {
        PrintLn();
        CPU.ShowStack();
    }
    ShowRegisters(bool showStringVariables, bool showWozVariables)
    {
        PrintLn();
        string names = GetRegisterNames();
        string registers = GetRegisters();
        PrintLn(names);
        if (CPU.PC == 0xFFFF)
        {
            registers = registers.Substring(4);
            Print("FFFF",    Colour.MatrixRed, Colour.Black);
            Print(registers, Colour.LightestGray, Colour.Black);
        }
        else
        {
            Print(registers, Colour.LightestGray, Colour.Black);
        }
        if (showStringVariables)
        {
            GetRAMByteDelegate getRAMByte = GetMemory;
            ShowHopperStringVariables(getRAMByte);
        }
        if (showWozVariables)
        {
            GetRAMByteDelegate getRAMByte = GetMemory;
            ShowWozMonVariables(getRAMByte);
        }
        PrintLn();
    }
}
