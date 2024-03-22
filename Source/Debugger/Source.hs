unit Source
{
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/JSON/Code"
    
    <byte> code;
    bool symbolsLoaded;
    bool definitionSymbolsLoaded;
    bool SymbolsLoaded { get { return symbolsLoaded;  } }
    bool DefinitionSymbolsLoaded { get { return definitionSymbolsLoaded; } set {  definitionSymbolsLoaded = value; } }
    
    ClearSymbols()
    {
        Code.Clear();
        symbolsLoaded = false;
        // before calling Symbols.Import
        SysCalls.New();
        LibCalls.New();
        Symbols.New();
    }
    
    bool LoadDefinitions(string symbolsPath)
    {
        if (!definitionSymbolsLoaded)
        {
            // before calling Symbols.Import
            SysCalls.New();
            LibCalls.New();
            Symbols.New();
            definitionSymbolsLoaded = false;
            
            Editor.SetStatusBarText("Loading definitions '" + symbolsPath + "' ..");
            if (Symbols.Import(symbolsPath))
            {
                uint symbolsLoaded = Symbols.GetSymbolsCount();
                Editor.SetStatusBarText("Definitions loaded for " + symbolsLoaded.ToString() + " symbols.");
                definitionSymbolsLoaded = true;
            }
        }
        return definitionSymbolsLoaded;
    }
    
    LoadSymbols(bool onlyNamedTypes)
    {
        string ihexPath = Monitor.CurrentHexPath;
        if (!symbolsLoaded && (ihexPath.Length != 0))
        {
            Source.ClearSymbols();
                
            string extension = Path.GetExtension(ihexPath);
            string codePath = ihexPath.Replace(extension, ".hexe");
            
            // LoadRawCode because it fills methodStart[..]
            code = LoadRawCode(codePath);
            
            string codegenPath  = ihexPath.Replace(extension, ".code");
            codegenPath = Path.GetFileName(codegenPath);
            codegenPath = Path.Combine("/Debug/Obj", codegenPath);
            if (File.Exists(codegenPath))
            {
                if (IsDebugger)
                {
                    Editor.SetStatusBarText("Loading symbols '" + codegenPath + "' ..");
                }
                else
                {
                    Output.Print(char(0x0D));
                }
                if (Code.ParseCode(codegenPath, false, true))
                {
                    uint methods = Code.GetMethodSymbolsCount();
                    if (IsDebugger)
                    {
                        Editor.SetStatusBarText("Symbols loaded for " + methods.ToString() + " methods.");
                    }
                    else
                    {
                        Output.Print(char(0x0D) + "Symbols loaded for " + methods.ToString() + " methods.");
                    }
                    symbolsLoaded = true;
                }
            }
            if (IsDebugger)
            {
                string symbolsPath  = ihexPath.Replace(extension, ".json");
                symbolsPath = Path.GetFileName(symbolsPath);
                symbolsPath = Path.Combine("/Debug/Obj", symbolsPath);
                if (File.Exists(symbolsPath))
                {
                    _ = LoadDefinitions(symbolsPath);
                }
            }
        }
    }
    <string, <string> > GetLocals(uint methodIndex, ref uint startAddress)
    {
        <string, <string> > localCandidates = Code.GetLocals(code, methodIndex, ref startAddress);
        return localCandidates;
    }
    <uint, <string> > GetGlobals(uint methodIndex, uint pc)
    {
        <uint, <string> > usedGlobals;
        uint distance = 8;
        loop
        {
            usedGlobals = Code.GetGlobals(code, methodIndex, pc, distance);
            if (pc == 0)
            {
                break;
            }
            // try to stop when we have 5 or more globals close to the current PC
            // Examples:
            //   0F91: 0F5F->0F95
            //   0F91: 0F5F->0F95
            //   16A4: 164E->17B7
            if (usedGlobals.Count >= 8)
            {
                break;
            }
            distance = distance + 8;
            if (distance > 32)
            {
                break;
            }
        }
        return usedGlobals;
    }
    uint GetCodeLength()
    {
        return code.Count;
    }
    byte GetCode(uint address)
    {
        return code[address];
    }
    string Disassemble(ref uint address, int entryPointOffset)
    {
       <uint> currentTargets;
       <uint> currentJixLabels;
       return Instructions.Disassemble(code, ref address, long(entryPointOffset), ref currentTargets, ref currentJixLabels, false);
    }
    
    uint CallAddressFromReturnToAddress(uint methodIndex, uint returnToAddress)
    {
        uint callFrom = returnToAddress - 1; 
        // -1 works for both CALLW (-3) and CALLREL (-1) based on
        // how string GetSourceIndex(uint address, uint methodIndex) works
        return callFrom;
    }
    
    bool IsListType(string typeString, ref string memberType)
    {
        memberType = "";
        if (typeString == "list")
        {
            return true;
        }
        else if (Types.IsList(typeString))
        {
            memberType = Types.GetValueFromCollection(typeString);
            return true;
        }
        return false;
    }
    bool IsDictionaryType(string typeString, ref string keyType, ref string valueType)
    {
        keyType = "";
        valueType = "";
        if (typeString == "dictionary")
        {
            return true;
        }
        else if (Types.IsDictionaryOrPair(typeString))
        {
            keyType   = Types.GetKeyFromCollection(typeString);
            valueType = Types.GetValueFromCollection(typeString);
            return true;
        }
        return false;
    }
    bool IsArrayType(string typeString, ref string memberType)
    {
        memberType = "";
        if (typeString == "array") 
        {
            return true;
        }
        else if (Types.IsArray(typeString))
        {
            memberType = Types.GetValueFromCollection(typeString);
            return true;
        }
        return false;
    }
    
    char StringGetChar(uint sPtr, uint index)
    {
        byte bvalue = Pages.GetPageByte(sPtr+4+index);
        return char(bvalue);
    }
    
    uint ListGetNextItem(ref uint pCurrent)
    {
        uint pItem = Pages.GetPageWord(pCurrent + 0);
        pCurrent   = Pages.GetPageWord(pCurrent + 2);
        return pItem;   
    }
    
    uint ListGetItem(uint lPtr, uint index)
    {
        uint item;
        type   lvtype  = type(Pages.GetPageByte(lPtr+4));
        string lvtypes = lvtype.ToString();
        uint pCurrent = Pages.GetPageWord(lPtr+5);
        bool first = true;
        bool iValueType = Types.IsValueType(lvtypes);
        while (pCurrent != 0)
        {
            item = ListGetNextItem(ref pCurrent);
            if (index == 0)
            {
                //if (iValueType)
                //{
                //    item = Pages.GetPageWord(item + 3); // variant box for value type
                //}
                break;
            }
            index--;
        }
        return item;
    }
    
    bool DictionaryNextItem(uint dPtr, ref uint iterator, ref uint kValue, ref uint vValue)
    {
        // Dictionary memory map:
        //   0000 heap allocator size
        //   13   type = tDictionary
        //   00   GC reference count
        //   xx   kType: key type tString or tUint
        //   xx   vType: value type
        //   xxxx count of entries
        //   xxxx capacity (number of slots in block of entries)
        //   xxxx pEntries (memory block of HashStringEntry or HashUIntEntry entries)

        type dktype   = type(Pages.GetPageByte(dPtr+2));
        type dvtype   = type(Pages.GetPageByte(dPtr+3));
        uint count    = Pages.GetPageWord(dPtr+4);
        uint capacity = Pages.GetPageWord(dPtr+6);
        uint pEntries = Pages.GetPageWord(dPtr+8);
        
        if ((count == 0) || (capacity == 0))
        {
            return false;
        }
        if (iterator == 0xFFFF)
        {
            return false;
        }
        bool success = false;
        
        loop
        {
            uint pEntry = pEntries + (iterator << 3);
            iterator = (iterator + 1) % capacity;
            
            // 16 bit stack slots:
            //
            // xxxx key
            // XXxxxxxx isOccupied / Hash
            // xxxx pValue variant
            
            // 32 bit stack slots:
            //
            // xxxx key
            // XXxx isOccupied / Hash
            // xxxxxxxx pValue variant
            
            
            bool notOccupied;
            if (dktype == string)
            {
                notOccupied = ((Pages.GetPageWord(pEntry+2) == 0) && (Pages.GetPageWord(pEntry+4) == 0));
            }
            else
            {
                notOccupied = (Pages.GetPageByte(pEntry+2) == 0);
            }
            if (notOccupied)
            {
                // empty entry or tombstone
            }
            else
            {
                kValue  = Pages.GetPageWord(pEntry);
                vValue = Pages.GetPageWord(pEntry+6);
                if (iterator == 0)
                {
                    iterator = 0xFFFF; // end indicator for foreach loop
                }
                success = true;
                break;
            }
            if (iterator == 0)
            {
                success = false;
                break;
            }
        }
        return success;
    }
    
    uint ArrayGetItem(uint aPtr, uint index)
    {
        uint item;
        type avtype = type(Pages.GetPageByte(aPtr+4));
        switch (avtype)
        {
            case bool:
            {
                byte slot = byte(index & 0x07);
                index = index >> 3;
                item = Pages.GetPageByte(aPtr+5+index);
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
                item = Pages.GetPageByte(aPtr+5+index);
            }
            default:
            {
                item = Pages.GetPageWord(aPtr+5+index*2);
            }
        }
        return item;
    }
    string TypeToString(uint value, string vtype, bool isReference, uint limit)
    {
        return TypeToString(value, vtype, isReference, limit, "");
    }
    string TypeToString(uint value, string vtype, bool isReference, uint limit, string recordMemberType)
    {
        string content;
        if (isReference)
        {
            uint refValue = Pages.GetPageByte(value + 0x0600) + Pages.GetPageByte(value + 0x0700) << 8;
            content = TypeToString(refValue, vtype, false, limit, recordMemberType);
        }
        else
        {
            string vType;
            string kType;
            string tname = vtype;
            
            if (Types.IsRecord(vtype))
            {
                vtype = "record";
            }
            else if (IsListType(vtype, ref vType))
            {
                vtype = "list";
            }
            else if (IsDictionaryType(vtype, ref kType, ref vType))
            {
                vtype = "dictionary";
            }
            else if (IsArrayType(vtype, ref vType))
            {
                vtype = "array";
            }
            else if (Types.IsEnum(vtype))
            {
                vtype = "enum";
            }
            else if (Types.IsFlags(vtype))
            {
                vtype = "flags";
            }
            else if (Types.IsDelegate(vtype))
            {
                vtype = "delegate";
            }
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
                    uint length = Pages.GetPageWord(value+2);
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
                case "dictionary":
                {
                    type dktype = type(Pages.GetPageByte(value+2));
                    type dvtype = type(Pages.GetPageByte(value+3));
                    
                    string dktypes = dktype.ToString();
                    string dvtypes = dvtype.ToString();
                    content = "<";
                    bool first = true;
                    uint iterator = 0;
                    uint kValue;
                    uint vValue;
                    
                    if (dktype == Types.ToType(kType))
                    {
                        dktypes = kType;
                    }
                    if (dvtype == Types.ToType(vType))
                    {
                        dvtypes = vType;
                    }
                    while (DictionaryNextItem(value, ref iterator, ref kValue, ref vValue))
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
                        content += "<";
                        content += TypeToString(kValue, dktypes, false, limit);
                        content += ", ";
                        content += TypeToString(vValue, dvtypes, false, limit);
                        content += ">";
                        first = false;
                    }
                    content += ">";
                    
                }
                case "record": // RECORD
                {
                    tname = Types.QualifyRecord(tname);
                    < <string> > members = FindRecord(tname);
                    
                    content = "{";
                    uint iMember;
                    type   lvtype  = type(Pages.GetPageByte(value+4));
                    string lvtypes = lvtype.ToString();
                    uint pCurrent = Pages.GetPageWord(value+5);
                    if (lvtypes != "variant")
                    {
                        content += "??? " + lvtypes + " ???";
                        // record members are always variant for now
#ifdef DEBUGGER
                        OutputDebug("TypeToString: " + tname + " " + lvtypes + " " + 
                                    Editor.CurrentPath + ":" + Editor.GetCurrentLineNumber().ToString());
#endif                
                    }
                    else
                    {
                        while (pCurrent != 0)
                        {
                            if (iMember != 0)
                            {
                                content += ", ";
                            }
                            if (content.Length >= limit)
                            {
                                content += "..";
                                break;
                            }
                            string mtypes     = lvtypes;
                            
                            bool mReferenceType = false;
                            if (members.Count > iMember)
                            {
                                <string> member = members[iMember];
                                content += member[0] + ": ";
                                mtypes = member[1];
                                mReferenceType = IsMachineReferenceType(mtypes);
                            }
                            uint pItem = ListGetNextItem(ref pCurrent);
                            if (mReferenceType)
                            {
                                content += TypeToString(pItem, mtypes, false, limit);    
                            }
                            else
                            {
                                content += TypeToString(pItem, lvtypes, false, limit, mtypes); // variant    
                            }
                            iMember++;
                        }
                    }
                    // RECORD if empty, placeholder content (and don't recurse)
                    if (iMember == 0)
                    {
                        bool first = true;
                        foreach (var member in members)
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
                            content += member[0] + ": ";
                            first = false;
                        }
                    }
                    content += "}";
                }
                case "list":
                {
                    content = "<";
                    type   lvtype  = type(Pages.GetPageByte(value+4));
                    string lvtypes = lvtype.ToString();
                    uint pCurrent = Pages.GetPageWord(value+5);
                    bool first = true;
                    bool iValueType = !IsMachineReferenceType(lvtypes);
                    
                    if (lvtype == Types.ToType(vType))
                    {
                        lvtypes = vType;
                    }
                    
                    while (pCurrent != 0)
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
                        uint pItem = ListGetNextItem(ref pCurrent);
                        if (iValueType)
                        {
                            content += TypeToString(pItem, lvtypes, false, limit);
                        }
                        else
                        {
                            content += TypeToString(pItem, lvtypes, false, limit);    
                        }
                        first = false;
                    }
                    content += ">";
                }
                case "variant":
                {
                    type mtype = type(Pages.GetPageByte(value));
                    string mtypes = mtype.ToString();
                    if (mtype == variant)
                    {
                        // unbox the value type
                        mtype   = type(Pages.GetPageByte(value+2));
                        mtypes = mtype.ToString();
                        bool mValueType = !IsMachineReferenceType(mtypes);
                        if (mValueType)
                        {
                            uint pMember = Pages.GetPageWord(value+3);
                            if (recordMemberType.Length != 0)
                            {
                                mtypes = recordMemberType;
                            }
                            content += TypeToString(pMember, mtypes, false, limit);   
                        }
                        else
                        {
                            content = value.ToHexString(4) + " v[" + mtypes + "]"; // what's this?
                        }
                    }
                    else
                    {
                        bool mValueType = !IsMachineReferenceType(mtypes);
                        if (!mValueType)
                        {
                            // reference type other than variant
                            content += TypeToString(value, mtypes, false, limit); 
                        }
                        else
                        {
                            content = value.ToHexString(4) + " v[" + mtypes + "]"; // what's this?
                        }
                    }
                }
                case "array":
                {
                    content = "[";
                    type avtype = type(Pages.GetPageByte(value+4));
                    string avtypes = avtype.ToString();
                    uint asize = Pages.GetPageWord(value+2);
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
                case "long":
                {
                    byte b0 = Pages.GetPageByte(value+2);
                    byte b1 = Pages.GetPageByte(value+3);
                    byte b2 = Pages.GetPageByte(value+4);
                    byte b3 = Pages.GetPageByte(value+5);
                    
                    if (IsHexDisplayMode)
                    {
                        content = "0x" + b3.ToHexString(2) + b2.ToHexString(2) + b1.ToHexString(2) + b0.ToHexString(2);
                    }
                    else
                    {
                        long lvalue = Long.FromBytes(b0, b1, b2, b3);
                        content = lvalue.ToString();
                    }
                }
                case "float":
                {
                    byte b0 = Pages.GetPageByte(value+2);
                    byte b1 = Pages.GetPageByte(value+3);
                    byte b2 = Pages.GetPageByte(value+4);
                    byte b3 = Pages.GetPageByte(value+5);

                    if (IsHexDisplayMode)
                    {
                        content = "0x" + b3.ToHexString(2) + b2.ToHexString(2) + b1.ToHexString(2) + b0.ToHexString(2);
                    }
                    else
                    {
                        float fvalue = Float.FromBytes(b0, b1, b2, b3);
                        content = fvalue.ToString();
                    }
                }
                
                case "enum":
                {
                    tname = Types.QualifyEnum(tname);
                    content = Symbols.DecodeEnum(tname, value);
                    uint iFirst; uint iLast;
                    if (content.IndexOf('.', ref iFirst) && content.LastIndexOf('.', ref iLast) && (iFirst != iLast))
                    {
                        // "Keyboard.Key.Enter" -> "Key.Enter"
                        content = content.Substring(iFirst+1);
                    }
                }
                case "flags":
                {
                    tname = Types.QualifyFlags(tname);
                    content = Symbols.DecodeFlags(tname, value);
                    uint iFirst; uint iLast;
                    if (!content.Contains('|') && content.IndexOf('.', ref iFirst) && content.LastIndexOf('.', ref iLast) && (iFirst != iLast))
                    {
                        // "Keyboard.Key.Enter" -> "Key.Enter"
                        content = content.Substring(iFirst+1);
                    }
                }
                case "delegate":
                {
                    tname = Types.QualifyDelegateName(tname);
                    if (value == 0)
                    {
                        content = "null"; // illegal delegate value
                    }
                    else 
                    {
                        uint codeStart = Pages.GetZeroPage("CODESTART");
                        if (!Code.MethodExists(value) && (value >= codeStart))
                        {
                            // probably a method address, not an index
                            uint address = value - codeStart;
                            value = Code.LocationToIndex(address);
                        }
                        string methodName;
                        if (Code.MethodExists(value))
                        {
                            methodName = Code.GetMethodName(value);
                        }
                        else
                        {
                            methodName = "0x" + value.ToHexString(4);
                        }
                        content = methodName + "(..)";
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
    
}
