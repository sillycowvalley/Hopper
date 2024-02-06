unit Symbols
{

    uses "/Source/System/System"
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/Tokens/LibCalls"
      
    // globals
    <string> gNames;
    <string,uint> gIndex;
    <uint, string> gTypes;
    // code location of initialization code:
    <uint, long> gStartPos;
    <uint, uint> gStartLine;
    <uint, string> gSourcePath;

    // functions, arguments, return types
    <string> fNames;
    <string,uint> fIndex;
    <uint, <uint> > fOverloads;
    // for each overload uint:
    <uint, string> fReturnTypes;
    <uint, < < string > > > fArgumentNamesAndTypes;
    <uint, < < string > > > fLocalNamesAndTypes; // and offsets
    // code location of function bodies
    <uint, long> fStartPos;
    <uint, uint> fStartLine;
    <uint, string> fSourcePath;
    
    <uint, byte> fSysCall;
    <uint, byte> fSysCallOverload;
    <string, byte> fSysCallCount;
    
    <uint, byte> fLibCall;
    <uint, byte> fLibCallOverload;
    <string, byte> fLibCallCount;
    
    <uint,<byte> > fCodeStream;
    <uint, <string,string> > fDebugInfo;
    <uint,uint> fCalls;

    uint iNextOverload; // not needed after first pass
    
    // delegates, arguments, return types
    <string> fdNames;
    <string,uint> fdIndex;
    <uint, <uint> > fdOverloads;
    // for each overload uint:
    <uint, string > fdReturnTypes;
    <uint, < < string > > > fdArgumentNamesAndTypes;
    
    uint idNextOverload; // not needed after first pass
        
    // records      
    <string, uint> rIndex;
    <uint, < <string> > > rMembers;
          
    // enums
    <string, <string, uint> > eValues;
    <string, uint> eIndex;
    <uint, <string,uint> > eMembers;
        
    // flags
    <string, <string, uint> > flValues;
    <string, uint> flIndex;
    <uint, <string,uint> > flMembers;
    <uint, <uint,string> > flMembersReversed;
    <uint, <uint> > flMembersSorted;
    
    // preprocessor symbols
    <string, string> pdValues;
    
    // constants
    <string, string> cDefinitions;
    <string, string> cValues;
    <string, string> cTypes;
    
    // unit names
    <string> nameSpaces;
    <string,string> sourceNameSpaces;
    
    // locations for context help
    <string,string> locations;
    
    < <string, string> > deferredTypes;
         
    uint iLastOverload;
    <uint, bool> overloadsCompiled;
    
    <uint, <uint> > fTouches;
         
    New()
    {
        // clear any globals
        fNames.Clear();
        fIndex.Clear();
        fOverloads.Clear();
        fReturnTypes.Clear();
        fArgumentNamesAndTypes.Clear();
        fLocalNamesAndTypes.Clear();
        fStartPos.Clear();
        fStartLine.Clear();             
        fSourcePath.Clear();  
        fCodeStream.Clear();
        fDebugInfo.Clear();
        fCalls.Clear();
        fTouches.Clear();
        
        fSysCall.Clear();
        fSysCallOverload.Clear();
        fSysCallCount.Clear();
        
        fLibCall.Clear();
        fLibCallOverload.Clear();
        fLibCallCount.Clear();
                
        iNextOverload = 1; 
        
        pdValues.Clear();
        
        cDefinitions.Clear();
        cValues.Clear();
        cTypes.Clear();
        
        gNames.Clear();
        gIndex.Clear();
        gTypes.Clear();
        gStartPos.Clear();
        gStartLine.Clear();
        gSourcePath.Clear();
                                                                                         
        nameSpaces.Clear();
        sourceNameSpaces.Clear();
        
        locations.Clear();
        
        fdNames.Clear();
        fdIndex.Clear();
        fdOverloads.Clear();
        fdReturnTypes.Clear();
        fdArgumentNamesAndTypes.Clear();
        idNextOverload = 0; 
        
        rIndex.Clear();
        rMembers.Clear();
        
        eValues.Clear();
        eIndex.Clear();
        eMembers.Clear();
        
        flValues.Clear();
        flIndex.Clear();
        flMembers.Clear();
        flMembersReversed.Clear();
        flMembersSorted.Clear();
        
        overloadsCompiled.Clear();
        
    }
    
    uint GetNamedTypesCount()
    {
        uint count;
        count += fdNames.Count;
        count += rIndex.Count;
        count += eIndex.Count;
        count += flIndex.Count;
        return count;
    }
    uint GetSymbolsCount()
    {
        uint count;
        count += fNames.Count;
        count += fSysCall.Count;
        count += fLibCall.Count;
        count += pdValues.Count;
        count += gNames.Count;
        count += fdNames.Count;
        count += rIndex.Count;
        count += eIndex.Count;
        count += flIndex.Count;
        count += cDefinitions.Count;
        count += nameSpaces.Count;
        return count;
    }
    
    string GetNamespace(string sourcePath)
    {
        string namespace;
        sourcePath = sourcePath.ToLower();
        if (sourceNameSpaces.Contains(sourcePath))
        {
            namespace = sourceNameSpaces[sourcePath];
        }
        return namespace;
    }
    string GetNamespaceLocation(string namespace)
    {
        foreach (var kv in sourceNameSpaces)
        {
            if (kv.value == namespace)
            {
                return kv.key;
            }
        }
        return "";
    }
    
    string GetLocation(string symbolName)
    {
        string location;
        if (locations.Contains(symbolName))
        {
            location = locations[symbolName];
        }
        return location;
    }
    AddLocation(string symbolName, string location)
    {
        if (locations.Contains(symbolName))
        {
            Parser.ErrorAtCurrent("duplicate symbol location for '" + symbolName + "'");
        }
        else
        {
            uint iFirst; uint iLast;
            if (symbolName.IndexOf('.', ref iFirst) && symbolName.LastIndexOf('.', ref iLast) && (iFirst != iLast))
            {
                symbolName = symbolName.Substring(iFirst+1); // "Keyboard.Key.Backspace" -> "Key.Backspace":
            }
            locations[symbolName] = location;
        }
    }
    
    string GetNamespace(uint oi)
    {
        
        //<string,uint> fIndex;
        //<uint, <uint> > fOverloads;
        string namespace;
        foreach (var f in fIndex)
        {
            uint index = f.value;
            string functionName = f.key;
            <uint> overloads = fOverloads[index];
            foreach (var overload in overloads)
            {
                if (overload == oi)
                {
                    namespace = functionName;
                    uint iDot;
                    if (namespace.IndexOf('.', ref iDot))
                    {
                        namespace = namespace.Substring(0, iDot);
                    }
                    break;
                }
            }
            if (namespace.Length != 0)
            {
                break;
            }
        }
        return namespace;
    }
    
    string GetFunctionName(uint oi)
    {
        
        //<string,uint> fIndex;
        //<uint, <uint> > fOverloads;
        string name;
        foreach (var f in fIndex)
        {
            uint index = f.value;
            string functionName = f.key;
            <uint> overloads = fOverloads[index];
            foreach (var overload in overloads)
            {
                if (overload == oi)
                {
                    name = functionName;
                    break;
                }
            }
            if (name.Length != 0)
            {
                break;
            }
        }
        return name;
    }
    
    
    string DecodeEnum(string enumName, uint value)
    {
        string result = enumName + "(" + value.ToString() + ")"; // fallback
        if (eIndex.Contains(enumName))
        {
            uint index = eIndex[enumName];
            <string,uint> members = eMembers[index];
            foreach (var kv in members)
            {
                if (kv.value == value)
                {
                    result = kv.key;
                    break;
                }
            }
        }
        return result;
    }
    uint GetEnumUpperBound(string enumName, string currentNamespace)
    {
        uint upperBound = 0;
        enumName = Symbols.QualifyEnum(enumName, currentNamespace);
        if (eIndex.Contains(enumName))
        {
            uint index = eIndex[enumName];
            <string,uint> members = eMembers[index];
            foreach (var kv in members)
            {
                if (kv.value > upperBound)
                {
                    upperBound = kv.value;
                }
            }
        }
        return upperBound;
    }
    uint GetFlagsUpperBound(string flagsName, string currentNamespace)
    {
        uint upperBound = 0;
        flagsName = Symbols.QualifyFlags(flagsName, currentNamespace);
        if (flIndex.Contains(flagsName))
        {
            uint index = flIndex[flagsName];
            <string,uint> members = flMembers[index];
            foreach (var kv in members)
            {
                upperBound = upperBound | kv.value;
            }
        }
        return upperBound;
    }
    bool FindEnum(string enumName, string memberName, ref uint value)
    {
        bool found = false;
        if (eIndex.Contains(enumName))
        {
            uint index = eIndex[enumName];
            <string,uint> members = eMembers[index];
            if (members.Contains(memberName))
            {
                value = members[memberName];
                found = true;
            }
        }            
        return found;
    }
    
    bool FindRecord(string recordName, ref < <string> > members)
    {
        bool found = false;
        if (rIndex.Contains(recordName))
        {            
            uint index = rIndex[recordName];
            members = rMembers[index];
            found = true;
        }            
        return found;
    }
    
    string DecodeFlags(string flagsName, uint value)
    {
        string fallback = flagsName + "(0x" + value.ToHexString(4) + ")"; // fallback
        string result;
        if (flIndex.Contains(flagsName))
        {
            uint index = flIndex[flagsName];
            <uint> membersSorted = flMembersSorted[index];
            <uint, string> membersReversed = flMembersReversed[index];
            bool useZero = (value == 0);
            string zeroValue;
            foreach (var key in membersSorted)
            {
                uint flagValue = key;
                if (flagValue == 0)
                {
                    zeroValue = membersReversed[key];
                }
                if (flagValue == (value & flagValue))
                {
                    value = value - flagValue;
                    if (result.Length != 0)
                    {
                        result = result + " | ";
                    }
                    result = result + membersReversed[key];
                    if (value == 0)
                    {
                        break;
                    }
                }
            }
            if (useZero && (zeroValue.Length != 0))
            {
                result = zeroValue;
            }
            else if (value != 0)
            {
                result = fallback;
            }
        }
        if (result.Length == 0)
        {
            result = fallback;
        }
        return result;
    }
    
    bool FindFlags(string flagsName, string memberName, ref uint value)
    {
        bool found = false;
        if (flIndex.Contains(flagsName))
        {
            uint index = flIndex[flagsName];
            <string,uint> members = flMembers[index];
            if (members.Contains(memberName))
            {
                value = members[memberName];
                found = true;
            }
        }            
        return found;
    }
    
    AddNameSpace(string name, string sourcePath)
    {
        if (!nameSpaces.Contains(name))
        {
            nameSpaces.Append(name);
            sourcePath = sourcePath.ToLower();
            sourceNameSpaces[sourcePath] = name;
        }
    }
    
    bool DefineExists(string name)
    {
        bool exists = false;
        if (pdValues.Contains(name))
        {
            exists = true;
        }
        return exists;
    }
    string GetDefineValue(string name)
    {
        string value;
        if (DefineExists(name))
        {
            value = pdValues[name];
        }
        return value;
    }
    AddDefine(string name, string value)
    {
        pdValues[name] = value;
    }
    
    bool ConstantExists(string name)
    {
        bool exists = false;
        if (cValues.Contains(name))
        {
            exists = true;
        }
        return exists;
    }
    string GetConstantValue(string name)
    {
        string value;
        if (ConstantExists(name))
        {
            value = cValues[name];
        }
        return value;
    }
    string GetConstantType(string name)
    {
        string value;
        if (ConstantExists(name))
        {
            value = cTypes[name];
        }
        return value;
    }
    AddConstant(string constantName, string actualType, string constantValue)
    {
        string key = actualType + " " + constantName;
        cDefinitions[key] = constantValue;
        cValues[constantName] = constantValue;
        cTypes[constantName] = actualType;
    }
    
    bool GlobalExists(string name)
    {
        bool exists = true;
        loop
        {
            if (fNames.Contains(name))
            {
                break;       
            }
            if (gNames.Contains(name))
            {
                break;       
            }
            if (rIndex.Contains(name))
            {
                break;
            }
            if (eIndex.Contains(name))
            {
                break;
            }
            if (flIndex.Contains(name))
            {
                break;
            }
            if (cValues.Contains(name))
            {
                break;
            }
            exists = false;
            break;
        }
        return exists;
    }     
    bool GlobalMemberExists(string name)
    {
        return gNames.Contains(name);
    }
    uint GetGlobalMemberIndex(string name)
    {
        return gIndex[name];
    }
    uint GetGlobalAddress(string name)
    {
        uint address = gIndex[name];
        address = address*2;
        return address;
    }
    uint GetGlobalCount()
    {
        return gNames.Count;
    }
    string GetGlobalName(uint index)
    {
        return gNames[index];
    }
    string GetGlobalType(uint index)
    {
        return gTypes[index];
    }
    <string> GetNameSpaces()
    {
        return nameSpaces;
    }
    
    <string, string> GetGlobalStart(uint iGlobal)
    {
        <string, string> startToken;
        if (gStartPos.Contains(iGlobal))
        {
            if (gStartPos[iGlobal]  != 0)
            {
                // -1 so that the next call to Advance() returns the opening '{'
                long isp = gStartPos[iGlobal] - 1;
                startToken["pos"]    = isp.ToString();
                uint isl = gStartLine[iGlobal];
                startToken["line"]   = isl.ToString();
                startToken["source"] = gSourcePath[iGlobal];     
            }
        }
        return startToken;
    }
    string GetGlobalLocation(uint iGlobal)
    {
        string location;
        if (gStartPos.Contains(iGlobal))
        {
            uint isl = gStartLine[iGlobal];
            location = gSourcePath[iGlobal] + ":" + isl.ToString();
        }
        return location;
    }
    
    bool IsRecordType(string name, string currentNamespace)
    {
        bool isRecord = false;
        uint winner = 0;
        loop
        {
            if (!name.Contains('.') && (name.Length != 0))
            {
                string candidate = currentNamespace + "." + name;
                if (eIndex.Contains(candidate))
                {
                    winner++;
                }
                if (rIndex.Contains(candidate))
                {
                    winner++;
                    isRecord = true;
                }
                if (flIndex.Contains(candidate))
                {
                    winner++;
                }
                if (fdNames.Contains(candidate))
                {
                    winner++;
                }
                if (isRecord && (winner == 1))
                {
                    break; // don't look at other namespaces if current wins
                }
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (currentNamespace == nameSpace)
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (eIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (rIndex.Contains(candidate))
                        {
                            winner++;
                            isRecord = true;
                        }
                        if (flIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (fdNames.Contains(candidate))
                        {
                            winner++;
                        }
                    }
                }
                break;
            }
            if (eIndex.Contains(name))
            {
                winner++;
            }
            if (rIndex.Contains(name))
            {
                winner++;
                isRecord = true;
            }
            if (flIndex.Contains(name))
            {
                winner++;
            }
            if (fdNames.Contains(name))
            {
                winner++;
            }
            break;
        }
        if (winner > 1)
        {
            Parser.ErrorAtCurrent("ambiguous undecorated named type name");
            isRecord = false;
        }
        return isRecord;
    }
    
    bool IsEnumType(string name, string currentNamespace)
    {
        bool isEnum = false;
        uint winner = 0;
        loop
        {
            if (!name.Contains('.') && (name.Length != 0))
            {
                string candidate = currentNamespace + "." + name;
                if (eIndex.Contains(candidate))
                {
                    winner++;
                    isEnum = true;
                }
                if (rIndex.Contains(candidate))
                {
                    winner++;
                }
                if (flIndex.Contains(candidate))
                {
                    winner++;
                }
                if (fdNames.Contains(candidate))
                {
                    winner++;
                }
                if (isEnum && (winner == 1))
                {
                    break; // don't look at other namespaces if current wins
                }
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (currentNamespace == nameSpace)
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (eIndex.Contains(candidate))
                        {
                            winner++;
                            isEnum = true;
                        }
                        if (rIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (flIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (fdNames.Contains(candidate))
                        {
                            winner++;
                        }
                    }
                }
                break;
            }
            if (eIndex.Contains(name))
            {
                winner++;
                isEnum = true;
            }
            if (rIndex.Contains(name))
            {
                winner++;
            }
            if (flIndex.Contains(name))
            {
                winner++;
            }
            if (fdNames.Contains(name))
            {
                winner++;
            }
            break;
        }
        if (winner > 1)
        {
            Parser.ErrorAtCurrent("ambiguous undecorated named type name");
            isEnum = false;
        }
        return isEnum;
    }
    bool IsFlagsType(string name, string currentNamespace)
    {
        bool isFlags = false;
        uint winner = 0;
        loop
        {
            if (!name.Contains('.') && (name.Length != 0))
            {
                string candidate = currentNamespace + "." + name;
                if (rIndex.Contains(candidate))
                {
                    winner++;
                }
                if (eIndex.Contains(candidate))
                {
                    winner++;
                }
                if (flIndex.Contains(candidate))
                {
                    winner++;
                    isFlags = true;
                }
                if (fdNames.Contains(candidate))
                {
                    winner++;
                }
                if (isFlags && (winner == 1))
                {
                    break; // don't look at other namespaces if current wins
                }
                
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (currentNamespace == nameSpace)
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (rIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (eIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (flIndex.Contains(candidate))
                        {
                            winner++;
                            isFlags = true;
                        }
                        if (fdNames.Contains(candidate))
                        {
                            winner++;
                        }
                    }
                }
                break;
            }
            if (rIndex.Contains(name))
            {
                winner++;
            }
            if (eIndex.Contains(name))
            {
                winner++;
            }
            if (flIndex.Contains(name))
            {
                winner++;
                isFlags = true;
            }
            if (fdNames.Contains(name))
            {
                winner++;
            }
            break;
        }
        if (winner > 1)
        {
            Parser.ErrorAtCurrent("ambiguous undecorated named type name");
            isFlags = false;
        }
        return isFlags;
    }
    
    string QualifyConstantIdentifier(string name, string currentNamespace)
    {
        if (!name.Contains('.'))
        {
            loop
            {
                string candidate = currentNamespace + "." + name;
                if (cValues.Contains(candidate))
                {
                    name = candidate;
                    break;
                }
                char f = name[0];
                if (f.IsUpper()) // public constant names only
                {
                    uint winner = 0;
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == currentNamespace)             
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (cValues.Contains(candidate))
                        {
                            name = candidate;
                            winner++;
                        }
                    }
                    if (winner > 1)
                    {
                        Parser.ErrorAtCurrent("ambiguous undecorated constant identifier");
                    }
                }
                break;
            }
        }
        return name;
    }
    
    string QualifyMethodName(string name, string currentNamespace)
    {
        if (!name.Contains('.'))
        {
            loop
            {
                string candidate = currentNamespace + "." + name;
                if (fNames.Contains(candidate))
                {
                    name = candidate;
                    break;
                }
                char f = name[0];
                if (f.IsUpper()) // public method names only
                {
                    uint winner = 0;
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == currentNamespace)             
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (fNames.Contains(candidate))
                        {
                            name = candidate;
                            winner++;
                        }
                    }
                    if (winner > 1)
                    {
                        Parser.ErrorAtCurrent("ambiguous undecorated named function or method name");
                    }
                }
                break;
            }
        }
        return name;    
    }
    
    string QualifyDelegate(string name, string currentNamespace)
    {
        if (!name.Contains('.'))
        {
            loop
            {
                string candidate = currentNamespace + "." + name;
                if (fdNames.Contains(candidate))
                {
                    name = candidate;
                    break;
                }
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == currentNamespace)             
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (fdNames.Contains(candidate))
                        {
                            name = candidate;
                            break;
                        }
                    }
                }
                break;
            }
        }
        return name;
    }
    
    string QualifyRecord(string name, string currentNamespace)
    {
        if (!name.Contains('.') && (name != ""))
        {
            loop
            {
                string candidate = currentNamespace + "." + name;
                if (rIndex.Contains(candidate))
                {
                    name = candidate;
                    break;
                }
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == currentNamespace)             
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (rIndex.Contains(candidate))
                        {
                            name = candidate;
                            break;
                        }
                    }
                }
                break;
            }
        }
        return name;
    }
    string QualifyEnum(string name, string currentNamespace)
    {
        if (!name.Contains('.'))
        {
            loop
            {
                string candidate = currentNamespace + "." + name;
                if (eIndex.Contains(candidate))
                {
                    name = candidate;
                    break;
                }
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == currentNamespace)             
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (eIndex.Contains(candidate))
                        {
                            name = candidate;
                            break;
                        }
                    }
                }
                break;
            }
        }
        return name;
    }
    
    string QualifyFlags(string name, string currentNamespace)
    {
        if (!name.Contains('.'))
        {
            loop
            {
                string candidate = currentNamespace + "." + name;
                if (flIndex.Contains(candidate))
                {
                    name = candidate;
                    break;
                }
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == currentNamespace)             
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (flIndex.Contains(candidate))
                        {
                            name = candidate;
                            break;
                        }
                    }
                }
                break;
            }
        }
        return name;
    }
    string QualifiedNamedType(string name, string currentNamespace)
    {
        if (IsRecordType(name, currentNamespace))
        {
            name = QualifyRecord(name, currentNamespace);
        }
        else if (IsEnumType(name, currentNamespace))
        {
            name = QualifyEnum(name, currentNamespace);
        }
        else if (IsFlagsType(name, currentNamespace))
        {
            name = QualifyFlags(name, currentNamespace);
        }
        else if (IsDelegateType(name, currentNamespace))
        {
            name = QualifyDelegate(name, currentNamespace);
        }
        return name;
    }
    
    bool IsDelegateType(string name, string currentNamespace)
    {
        bool isDelegate = false;
        uint winner = 0;
        loop
        {
            if (!name.Contains('.') && (name.Length != 0))
            {
                string candidate = currentNamespace + "." + name;
                if (eIndex.Contains(candidate))
                {
                    winner++;
                }
                if (flIndex.Contains(candidate))
                {
                    winner++;
                }
                if (fdNames.Contains(candidate))
                {
                    winner++;
                    isDelegate = true;
                }
                if (isDelegate && (winner == 1))
                {
                    break; // don't look at other namespaces if current wins
                }
                
                char f = name[0];
                if (f.IsUpper())
                {
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (currentNamespace == nameSpace)
                        {
                            continue;
                        }
                        candidate = nameSpace + "." + name;
                        if (eIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (flIndex.Contains(candidate))
                        {
                            winner++;
                        }
                        if (fdNames.Contains(candidate))
                        {
                            winner++;
                            isDelegate = true;
                        }
                    }
                }
                break;
            }
            if (eIndex.Contains(name))
            {
                winner++;
            }
            if (flIndex.Contains(name))
            {
                winner++;
            }
            if (fdNames.Contains(name))
            {
                winner++;
                isDelegate = true;
            }
            break;
        }
        if (winner > 1)
        {
            Parser.ErrorAtCurrent("ambiguous undecorated named type name");
            isDelegate = false;
        }
        return isDelegate;
    }
        
    bool IsNamedType(<string,string> token)
    {
        string name = token["lexeme"];
        
        uint winner = 0;
        loop
        {
            if (!name.Contains('.'))
            {
                foreach (var nameSpace in nameSpaces)
                {
                    string candidate = nameSpace + "." + name;
                    if (eIndex.Contains(candidate))
                    {
                        winner++;
                    }
                    if (rIndex.Contains(candidate))
                    {
                        winner++;
                    }
                    if (flIndex.Contains(candidate))
                    {
                        winner++;
                    }
                    if (fdNames.Contains(candidate))
                    {
                        winner++;
                    }
                }
                break;
            }
            if (eIndex.Contains(name))
            {
                winner++;
            }
            if (rIndex.Contains(name))
            {
                winner++;
            }
            if (flIndex.Contains(name))
            {
                winner++;
            }
            if (fdNames.Contains(name))
            {
                winner++;
            }
            break;
        }
        if (winner > 1)
        {
            Parser.ErrorAt(token, "ambiguous named type '" + name + "'");
        }
        if (winner == 0)
        {
            Parser.ErrorAt(token, "unrecognized type name");    
        }
        return (winner == 1);
    }
    
    DeferValidation(<string, string> token)
    {
        deferredTypes.Append(token);
    }
    
    bool DeferredValidation()
    {
        bool success = true;
        foreach (var token in deferredTypes)
        {
            if (!IsNamedType(token))
            {
                success = false;
                break;
            }     
        }
        return success;
    }
    
    bool TryFindMethod(string currentLocation, ref uint iBest)
    {
        bool found;
        uint closest = 0xFFFF;
        
        <string> parts = currentLocation.Split(':');
        string sourcePath = (parts[0]).ToLower();
        uint currentLine;
        _ = UInt.TryParse(parts[1], ref currentLine);
        
        foreach (var sourcekv in fSourcePath)
        {
            if ((sourcekv.value).ToLower() == sourcePath)
            {
                uint iOverload = sourcekv.key;
                uint startLine = fStartLine[iOverload];
                if (startLine <= currentLine)
                {
                    uint delta = currentLine - startLine;
                    if (delta < closest)
                    {
                        closest = delta;
                        found = true;
                        iBest = iOverload;
                    }
                }
            }
        }
        return found;
    }
    
    bool TryFindMethod(string currentNamespace, ref string name, ref bool setter, ref bool getter)
    {
        setter = false;
        getter = false;
        string methodNameCandidate = Symbols.QualifyMethodName(name, currentNamespace);
        bool found = fNames.Contains(methodNameCandidate);
        if (found)
        {
            name = methodNameCandidate;
        }
        else
        {
            methodNameCandidate = Symbols.QualifyMethodName(name + "_Get", currentNamespace);
            if (fNames.Contains(methodNameCandidate))
            {
                getter = true; found = true;
                name = methodNameCandidate.Replace("_Get", "");
            }
            methodNameCandidate = Symbols.QualifyMethodName(name + "_Set", currentNamespace);
            if (fNames.Contains(methodNameCandidate))
            {
                setter = true; found = true;
                name = methodNameCandidate.Replace("_Set", "");
            }
        }
        return found;
    }
    
    string GetMethodLocation(uint iOverload)
    {
        string location = fSourcePath[iOverload] + ":" + (fStartLine[iOverload]).ToString();
        return location;   
    }
    
    
    bool GetFunctionIndex(string name, ref uint index)
    {
        // returnType == void
        bool found = false;
        if (fNames.Contains(name))
        {
            uint length = fNames.Count;
            for (uint i=0; i < length; i++)
            {
                if (name == fNames[i])
                {
                    index = i;
                    found = true;
                    break;
                }
            }
        }     
        if (!found && !name.Contains('.'))
        {
            foreach (var nameSpace in nameSpaces)
            {
                string candidate = nameSpace + "." + name;
                if (GetFunctionIndex(candidate, ref index))
                {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }
    <uint> GetFunctionOverloads(uint fIndex)
    {
      <uint> overloads = fOverloads[fIndex];
      return overloads;
    }
    
    SetCodeStream(uint iOverload, <byte> codeStream, <string,string> debugInfo)
    {
        fCodeStream[iOverload] = codeStream;
        fDebugInfo[iOverload] = debugInfo;
    }
    AddFunctionCall(uint iOverload)
    {
        if (!fCalls.Contains(iOverload))
        {
            fCalls[iOverload] = 0;
        }
        fCalls[iOverload] = fCalls[iOverload] + 1;
    }
    
    bool IsSysCall(uint iOverload)
    {
        return fSysCall.Contains(iOverload);
    }
    byte GetSysCallIndex(uint iOverload)
    {
        return fSysCall[iOverload];
    }
    byte GetSysCallOverload(uint iOverload)
    {
        return fSysCallOverload[iOverload];
    }
    SetSysCall(uint iOverload, byte iSysCall, byte iSysCallOverload)
    {
        fSysCall[iOverload] = iSysCall;    
        fSysCallOverload[iOverload] = iSysCallOverload;
    }
    
    
    bool IsLibCall(uint iOverload)
    {
        return fLibCall.Contains(iOverload);
    }
    byte GetLibCallIndex(uint iOverload)
    {
        return fLibCall[iOverload];
    }
    byte GetLibCallOverload(uint iOverload)
    {
        return fLibCallOverload[iOverload];
    }
    SetLibCall(uint iOverload, byte iLibCall, byte iLibCallOverload)
    {
        fLibCall[iOverload] = iLibCall;    
        fLibCallOverload[iOverload] = iLibCallOverload;
    }
    
    
    <string, string> GetOverloadStart(uint iOverload)
    {
        <string, string> startToken;
        // -1 so that the next call to Advance() returns the opening '{'
        long pos = fStartPos[iOverload];
        pos = pos - 1;
        startToken["pos"]    = pos.ToString();
        uint ln = fStartLine[iOverload];
        startToken["line"]   = ln.ToString();
        startToken["source"] = fSourcePath[iOverload];     
        return startToken;
    }
    
    string GetOverloadReturnType(uint iOverload)
    {
        string returnType = fReturnTypes[iOverload];
        return returnType;
    }
      
    < < string > > GetOverloadArguments(uint iOverload)
    {
        < < string > > arguments = fArgumentNamesAndTypes[iOverload];
        return arguments;
    }
    
    string GetDelegateReturnType(uint iOverload)
    {
        string returnType = fdReturnTypes[iOverload];
        return returnType;
    }
      
    < < string > > GetDelegateArguments(uint iOverload) 
    {
        < < string > > arguments = fdArgumentNamesAndTypes[iOverload];
        return arguments;
    }
    
    bool GetFunctionDelegateIndex(string name, ref uint index)
    {
        // returnType == void
        bool found = false;
        if (fdNames.Contains(name))
        {
            uint length = fdNames.Count;
            for (uint i=0; i < length; i++)
            {
                if (name == fdNames[i])
                {
                    index = i;
                    found = true;
                    break;
                }
            }
        }                
        if (!found && !name.Contains('.'))
        {
            foreach (var nameSpace in nameSpaces)
            {
                string candidate = nameSpace + "." + name;
                if (GetFunctionDelegateIndex(candidate, ref index))
                {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }
    <uint> GetFunctionDelegateOverloads(uint fdIndex)
    {
      <uint> overloads = fdOverloads[fdIndex];
      return overloads;
    }
    
    AddGlobalMember(string name, string typeString, <string> blockPos)
    {
        loop
        {
            // globals
            //  <string> gNames;
            //  <string,uint> gIndex;
            //  <uint,string> gTypes;
            // code location of initialization code:
            //  <uint, long> gStartPos;
            //  <uint, uint> gStartLine;
            //  <uint, string> gSourcePath;
            if (gNames.Contains(name))
            {
                Parser.Error("duplicate global member definition");
                break;
            }     
            gNames.Append(name);
            uint iGlobal = gNames.Count-1;
            gIndex[name] = iGlobal;
            gTypes[iGlobal] = typeString;
            if (blockPos.Count != 0) // has initialization code?
            {
                long startPos;
                uint startLine;
                if (Long.TryParse(blockPos[0], ref startPos))
                {
                    gStartPos[iGlobal] = startPos; // location of method block '{', zero if no initialization
                }
                if (UInt.TryParse(blockPos[1], ref startLine))
                {
                    gStartLine[iGlobal] = startLine; // location of method block '{'
                }
                gSourcePath[iGlobal] = blockPos[2];
            }
            break;
        }
    }
                            
    AddFunction(string name, < <string > > arguments, string returnType, <string> blockPos)
    {
        // functions, arguments, return types
        //   <string> fNames;
        //   <string,uint> fIndex;
        //   <uint, <uint>> fOverloads;
    
        // for each overload uint:
        //   <uint, string > fReturnTypes;
        //   <uint, < < string > > > fArgumentNamesAndTypes;
        //
        //   <uint, long> fStartPos;
        //   <uint, uint> fStartLine;
        //   <uint, string> fSourcePath;
        //     OR
        //   <uint,uint> fSysCall;   
        
        uint index = 0;
        
        if (!GetFunctionIndex(name, ref index))
        {
            fNames.Append(name);
            index = fNames.Count-1;
            fIndex[name] = index;
        }
        bool isMain = name.EndsWith(".main");
    
        <uint> overloads;
        
        if (fOverloads.Contains(index))
        {
            overloads = fOverloads[index];
        }
    
        loop
        {
            foreach (var overload in overloads)
            {
                if (Types.ArgumentsEqual(fArgumentNamesAndTypes[overload], arguments, false, false))
                {
                    if (fReturnTypes[overload] == returnType)
                    {
                        Parser.ErrorAtCurrent("duplicate function definition");
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("function definitions differ only by return type");
                    }
                    break;
                }
            }    
            uint iCurrentOverload = 0;
            if (!isMain)
            {
                if (iNextOverload == 0)
                {
                    iNextOverload = 1;
                }
                iCurrentOverload = iNextOverload;
                iNextOverload++;
            }
            
            overloads.Append(iCurrentOverload);
            fArgumentNamesAndTypes[iCurrentOverload] = arguments;
            fReturnTypes[iCurrentOverload] = returnType;
            
            long startPos;
            if (blockPos.Count != 0)
            {
                uint startLine;
                if (Long.TryParse(blockPos[0], ref startPos))
                {
                    fStartPos[iCurrentOverload] = startPos; // location of method block '{'
                }
                if (UInt.TryParse(blockPos[1], ref startLine))
                {
                    fStartLine[iCurrentOverload] = startLine; // location of method block '{'
                }
                fSourcePath[iCurrentOverload] = blockPos[2];
            }
            
            if (startPos == 0) // system or library
            {
                byte iSysCall;
                
                if (SysCalls.TryParseSysCall(name, ref iSysCall))
                {
                    if (!fSysCallCount.Contains(name))
                    {
                        fSysCallCount[name] = 0;    
                    }
                    byte iSysCallOverload = fSysCallCount[name];
                    fSysCallCount[name] = iSysCallOverload+1;
                    fSysCall[iCurrentOverload] = iSysCall;
                    fSysCallOverload[iCurrentOverload] = iSysCallOverload;
                }
                else if (LibCalls.TryParseLibCall(name, ref iSysCall))
                {
                    if (!fLibCallCount.Contains(name))
                    {
                        fLibCallCount[name] = 0;    
                    }
                    byte iLibCallOverload = fLibCallCount[name];
                    fLibCallCount[name] = iLibCallOverload+1;
                    fLibCall[iCurrentOverload] = iSysCall;
                    fLibCallOverload[iCurrentOverload] = iLibCallOverload;
                }
                else
                {
                    Parser.Error("undefined 'system' or 'library' method '" + name + "'");
                    break; 
                }
            }
            fOverloads[index] = overloads;
            
            break;
        }
    }
    
    AddMethod(string methodName, < <string > > arguments, <string> blockPos)
    {
        AddFunction(methodName, arguments, "void", blockPos); // 'void' is only used internally
    }
    
    AddRecord(string identifier, < <string> > members)
    {
        // <string, uint> rIndex;
        // <string, < <string> > > rMembers;
        uint iNext = rIndex.Count;
        rIndex[identifier] = iNext;
        rMembers[iNext] = members;
    }
 
    AddEnum(string identifier, <string, uint> members)
    {
        // <string, <string, uint> > eValues;
        <string, uint> values;
        eValues[identifier] = values;
        uint iNext = eIndex.Count;
        eIndex[identifier] = iNext;
        eMembers[iNext] = members;
    }   
    <string, <string, uint> > GetEnums()
    {
        return eValues;
    }
    <string, uint> GetEnumMembers(string name)
    {
        uint index = eIndex[name];
        return eMembers[index];
    }
    
    <string, <string, uint> > GetFlags()
    {
        return flValues;
    }
    <string, uint> GetFlagMembers(string name)
    {
        uint index = flIndex[name];
        return flMembers[index];
    }
        
    AddFlags(string identifier, <string, uint> members)
    {
        // <string, <string, uint> > flValues;
        <string, uint> values;
        flValues[identifier] = values;
        uint iNext = flIndex.Count;
        flIndex[identifier] = iNext;
        flMembers[iNext] = members;
        <uint, string> membersReversed;
        <uint> membersSorted;
        foreach (var kv in members)
        {
            uint   key = kv.value;
            string value = kv.key;
            membersReversed[key] = value;
            uint slength = membersSorted.Count;
            
            bool inserted = false;
            for (uint i=0; i < slength; i++)
            {
                if (membersSorted[i] < key)
                {
                    membersSorted.Insert(i, key);
                    inserted = true;
                    break;
                }
            }
            if (!inserted)
            {
                membersSorted.Append(key);
            }
        }
        
        flMembersReversed[iNext] = membersReversed;
        flMembersSorted[iNext] = membersSorted;
     }     
    
    
    AddFunctionDelegate(string name, < <string > > arguments, string returnType)
    {
        // functions, arguments, return types
        //   <string> fdNames;
        //   <string,uint> fdIndex;
        //   <uint, <uint>> fdOverloads;
    
        // for each overload uint:
        //   <uint, string > fdReturnTypes;
        //   <uint, < < string > > > fdArgumentNamesAndTypes;
            
        uint index = 0;
        if (!GetFunctionDelegateIndex(name, ref index))
        {
            fdNames.Append(name);
            index = fdNames.Count-1;
            fdIndex[name] = index;
        }
        <uint> overloads;
        
        if (fdOverloads.Contains(index))
        {
            overloads = fdOverloads[index];
        }
    
        loop
        {
            foreach (var overload in overloads)
            {
                if (Types.ArgumentsEqual(fdArgumentNamesAndTypes[overload], arguments, false, false))
                {
                    if (fdReturnTypes[overload] == returnType)
                    {
                        Parser.Error("duplicate delegate definition");
                    }
                    else
                    {
                        Parser.Error("delegate definitions differ only by return type");
                    }
                    break;
                }
            }    
            overloads.Append(idNextOverload);
            fdArgumentNamesAndTypes[idNextOverload] = arguments;
            fdReturnTypes[idNextOverload] = returnType;
            
            fdOverloads[index] = overloads;
            idNextOverload++;
            break;
        }
    }
             
    
    AddMethodDelegate(string methodName, < <string > > arguments)
    {
        AddFunctionDelegate(methodName, arguments, "void"); // 'void' is only used internally
    }
    
    AppendLocalNamesAndTypes(uint iOverload, < <string> > localNamesAndTypes)
    {
        < <string> >  appendList;
        if (fLocalNamesAndTypes.Contains(iOverload))
        {
            appendList = fLocalNamesAndTypes[iOverload];
        }
        
        foreach (var localNameAndType in localNamesAndTypes)
        {
            appendList.Append(localNameAndType);
        }
        fLocalNamesAndTypes[iOverload] = appendList;
    }
    
    bool ExportCode(string codePath)
    {
        // <uint,<byte> > fCodeStream;
        // <uint, <string,string> > fDebugInfo;
        // <uint,uint> fCalls;    
        <uint> usedOverloads;
        bool success = true;
        loop
        {
            foreach (var kv in fCodeStream)
            {
                if (!usedOverloads.Contains(kv.key))
                {
                    usedOverloads.Append(kv.key);
                }
            }
            foreach (var kv in fCalls)
            {
                if (!usedOverloads.Contains(kv.key))
                {
                    usedOverloads.Append(kv.key);
                }
            }
            <string, variant> dict;
            
            <byte> constantStream = CodeStream.GetConstantStream();
            if (constantStream.Count != 0)
            {
                <string,variant> cdict;
                cdict["data"] = constantStream;    
                dict["const"] = cdict;
            }
            
            // <string> gNames;
            // <uint,string> gTypes;

            <string, <string,string> > globals;
            uint gNamesCount = gNames.Count;
            for (uint i = 0; i < gNamesCount; i++)
            {
                uint offset = i * 2;
                <string,string> gDict;
                string gName = gNames[i];
                gDict["name"] = gName;
                
                string gTypeName = gTypes[i];
                byte typeByte = Types.ToByte(gTypeName);
                type typeType = type(typeByte);
                
                gDict["type"] = typeType.ToString();
                
                uint iDot;
                if (gName.IndexOf('.', ref iDot))
                {
                    gName = gName.Substring(0, iDot);
                    gTypeName = QualifiedNamedType(gTypeName, gName);
                }
                gDict["definition"] = gTypeName;
                
                globals[offset.ToString()] = gDict;
            }
            if (globals.Count != 0)
            {
                dict["globals"] = globals;
            }
        
            
            foreach (var iUsedOverload in usedOverloads)
            {
                <string,variant> mdict;
                
                if (IsSysCall(iUsedOverload))
                {
                    continue;
                }
                if (IsLibCall(iUsedOverload))
                {
                    continue;
                }
                
                if (fCalls.Contains(iUsedOverload))
                {
                    mdict["hits"] = fCalls[iUsedOverload];
                }
                if (fCodeStream.Contains(iUsedOverload))
                {
                    < < string > > argsAndTypes = fArgumentNamesAndTypes[iUsedOverload];
                    uint argCount = argsAndTypes.Count;
                    int acount;
                    
                    <string, <string, string> > argdicts;
                    foreach (var argument in argsAndTypes)
                    {
                        <string, string> argdict;
                        string name = argument[2];
                        string aref = argument[0];
                        if (aref == "ref")
                        {
                            aref = "true";
                        }
                        else
                        {
                           aref = "false";
                        }
                        byte typeByte = Types.ToByte(argument[1]);
                        type typeType = type(typeByte);
                        
                        argdict["name"] = name;
                        argdict["type"] = typeType.ToString();
                        argdict["ref"] = aref;
                        
                        int offset = 0 - (int(argCount)-acount)*2;
                        argdicts[offset.ToString()] = argdict;
                        acount++;
                    }
                    if (argdicts.Count != 0)
                    {
                        mdict["arguments"]   = argdicts;
                    }
                    if (fLocalNamesAndTypes.Contains(iUsedOverload))
                    {
                        < < string > > localsAndTypes = fLocalNamesAndTypes[iUsedOverload];
                        <string, <string, string> > localdicts;
                        foreach (var local in localsAndTypes)
                        {
                            <string, string> localdict;
                            string lrange  = local[0];
                            string lname   = local[1];
                            string ltype   = local[2];
                            string loffset = local[3];
                            localdict["name"] = lname;
                            localdict["type"] = ltype;
                            localdict["offset"] = loffset;
                            localdicts[lrange] = localdict;
                        }
                        if (localdicts.Count != 0)
                        {
                            mdict["locals"]   = localdicts;
                        }
                    }
                    mdict["code"]   = fCodeStream[iUsedOverload];
                    mdict["debug"]   = fDebugInfo[iUsedOverload];
                    mdict["line"]   = fStartLine[iUsedOverload];
                    mdict["source"] = fSourcePath[iUsedOverload];
                    mdict["name"] = GetFunctionName(iUsedOverload);
                }
                dict[iUsedOverload.ToString()] = mdict;
            }
            
            if (!JSON.Write(codePath, dict))
            {
                success = false;
            }
            break;
        } // loop
        return success;
    }
        
    bool Export(string jsonPath)
    {
        bool success = true;
        loop
        {
            <string, variant> dict;
            
            <string,string> ndict;
            foreach (var nkv in sourceNameSpaces)
            {
                ndict[nkv.value] = nkv.key;
            }
            dict["units"]   = ndict;
            
            dict["symbols"]   = pdValues;
            dict["constants"] = cDefinitions;
            
            dict["locations"]   = locations;
            
            <string, <string, variant> > rdict;
            foreach (var rc in rIndex)
            {
                <string, variant> rentry;
                uint index = rc.value;
                uint mIndex = 0;
                foreach (var mkv in rMembers[index])
                {
                    <string> recordParts = mkv;
                    <string,string> recordmember;
                    recordmember[recordParts[0]] = recordParts[1];
                    rentry[mIndex.ToString()] = recordmember;
                    mIndex++;
                }
                rdict[rc.key] = rentry;
            }
            if (rdict.Count != 0)
            {
                dict["records"] = rdict;
            }
            
            <string, <string, variant> > edict;
            foreach (var en in eIndex)
            {
                <string, variant> eentry;
                uint index = en.value;
                eentry["members"] = eMembers[index];                
                edict[en.key] = eentry;
            }
            if (edict.Count != 0)
            {
                dict["enums"] = edict;
            }
            
            <string, <string, variant> > fldict;
            foreach (var fl in flIndex)
            {
                <string, variant> flentry;
                uint index = fl.value;
                flentry["members"] = flMembers[index];
                fldict[fl.key] = flentry;
            }
            if (fldict.Count != 0)
            {
                dict["flags"] = fldict;
            }
            
            <string, <string, variant> > gdict;
            foreach (var g in gIndex)
            {
                <string, variant> gentry;
                uint index = g.value;
                gentry["type"] = gTypes[index];
                if (gStartPos.Contains(index))
                {
                    gentry["pos"]    = gStartPos[index];                
                    gentry["line"]   = gStartLine[index];
                    gentry["source"] = gSourcePath[index];
                }
                gdict[g.key] = gentry;
            }
            if (gdict.Count != 0)
            {
                dict["globals"] = gdict;           
            }

            <string, <string, variant> > fddict;            
            foreach (var fd in fdIndex)
            {
                <string, variant> fdentry;
                uint index = fd.value;
                
                <uint> overloads = fdOverloads[index];
                foreach (var overload in overloads)
                {
                    <string, variant> odict;
                    
                    if (fdReturnTypes.Contains(overload))
                    {
                        string rt = fdReturnTypes[overload];
                        if (rt != "void")
                        {
                            odict["returntype"] = rt;
                        }
                    }
                    if (fdArgumentNamesAndTypes.Contains(overload))
                    {
                        <string, string> args;
                        uint ai = 0;
                        foreach (var arg in fdArgumentNamesAndTypes[overload])
                        {
                            //<ref, type, name>
                            <string> ls = arg;
                            string rf = ls[0];
                            string tp = ls[1];
                            string nm = ls[2];
                            if (rf.Length != 0)
                            {
                                tp = rf + " " + tp;
                            }
                            args[ai.ToString() + ":" + nm] = tp;        
                            ai++;
                        }
                        if (args.Count != 0)
                        {
                            odict["arguments"] = args;
                        }
                    }
                    fdentry[overload.ToString()] = odict;
                }
                fddict[fd.key] = fdentry;
            }
            if (fddict.Count != 0)
            {
                dict["delegates"] = fddict;
            }
            
            <string, <string, variant> > fdict;            
            foreach (var f in fIndex)
            {
                <string, variant> fentry;
                uint index = f.value;
                
                <uint> overloads = fOverloads[index];
                foreach (var overload in overloads)
                {
                    <string, variant> odict;
                    
                    if (fReturnTypes.Contains(overload))
                    {
                        string rt = fReturnTypes[overload];
                        if (rt != "void")
                        {
                            odict["returntype"] = rt;
                        }
                    }
                    if (fArgumentNamesAndTypes.Contains(overload))
                    {
                        <string, string> args;
                        uint ai = 0;
                        foreach (var arg in fArgumentNamesAndTypes[overload])
                        {
                            //<ref, type, name>
                            <string> ls = arg;
                            string rf = ls[0];
                            string tp = ls[1];
                            string nm = ls[2];
                            if (rf.Length != 0)
                            {
                                tp = rf + " " + tp;
                            }
                            args[ai.ToString() + ":" + nm] = tp;
                            ai++;
                        }
                        if (args.Count != 0)
                        {
                            odict["arguments"] = args;
                        }
                    }
                    if (fStartPos.Contains(overload))
                    {
                        odict["pos"]    = fStartPos[overload];
                        odict["line"]   = fStartLine[overload];
                        odict["source"] = fSourcePath[overload];
                    }
                    if (fSysCall.Contains(overload))
                    {
                        odict["syscall"]         = fSysCall[overload];
                        odict["overload"] = fSysCallOverload[overload];
                    }
                    if (fLibCall.Contains(overload))
                    {
                        odict["libcall"]  = fLibCall[overload];
                        odict["overload"] = fLibCallOverload[overload];
                    }
                    fentry[overload.ToString()] = odict;
                }
                fdict[f.key] = fentry;
            }
            if (fdict.Count != 0)
            {
                dict["functions"] = fdict;
            }
            if (!JSON.Write(jsonPath, dict))
            {
                success = false;
            }
            break;
        } // loop
        return success;
    }
    
    bool Import(string jsonPath)
    {
        return Import(jsonPath, false);
    }
    bool Import(string jsonPath, bool onlyNamedTypes)
    {
        bool success = true;
        loop
        {
            if (!File.Exists(jsonPath))
            {
                success = false;
                break;
            }
            <string, variant> dict;
            if (!JSON.Read(jsonPath, ref dict))
            {
                success = false;
                break;
            }
            foreach (var kv in dict)
            {
                switch (kv.key)
                {
                    case "constants":
                    {
                        if (!onlyNamedTypes)
                        {
                            <string, string> constantDict = kv.value;
                            foreach (var constant in constantDict)
                            {
                                string constantKey = constant.key;
                                if (constantKey.Contains(' '))
                                {
                                    <string> parts = constantKey.Split(' ');
                                    cValues[parts[1]] = constant.value;
                                    cTypes[parts[1]] = parts[0];
                                }
                                else // TODO REMOVE
                                {
                                    cValues[constantKey] = constant.value;
                                    cTypes[constantKey] = "";
                                }
                            }
                        }
                    }
                    case "symbols": // #define symbols
                    {
                        if (!onlyNamedTypes)
                        {
                            pdValues = kv.value;
                        }
                    }
                    case "records":
                    {
                        <string, variant> rdict = kv.value;
                        foreach (var kv2 in rdict)
                        {
                            string name = kv2.key;
                            <string, variant> values = kv2.value;
                            
                            < <string> > members;
                            foreach (var kv3 in values)
                            {
                                <string,string> member = kv3.value;
                                
                                foreach (var kv4 in member)
                                {
                                    <string> memberList;
                                    memberList.Append(kv4.key);
                                    memberList.Append(kv4.value);
                                    members.Append(memberList);
                                }
                            } 
                            AddRecord(name, members);      
                        }
                    }
                    case "enums":
                    {
                        <string, variant> edict = kv.value;
                        foreach (var kv2 in edict)
                        {
                            string name = kv2.key;
                            <string, variant> values = kv2.value;
                            <string,string> lmembers = values["members"];
                            <string,uint> members;
                            foreach (var kv6 in lmembers)
                            {
                                string l = kv6.value;
                                uint ui;
                                if (UInt.TryParse(l, ref ui))
                                {
                                    members[kv6.key] = ui;
                                }
                            }
                            AddEnum(name, members);
                        }
                    }
                    case "flags":
                    {
                        <string, variant> fdict = kv.value;
                        foreach (var kv2 in fdict)
                        {
                            string name = kv2.key;
                            <string, variant> values = kv2.value;
                            <string,string> lmembers = values["members"];
                            <string,uint> members;
                            foreach (var kv6 in lmembers)
                            {
                                string l = kv6.value;
                                uint ui;
                                if (UInt.TryParse(l, ref ui))
                                {
                                    members[kv6.key] = ui;
                                }
                            }
                            AddFlags(name, members);
                        }
                    }
                    case "globals":
                    {
                        if (!onlyNamedTypes)
                        {
                            // globals
                            //   <string> gNames;
                            //   <string,uint> gIndex;
                            //   <uint,string> gTypes;
                            // code location of initialization code:
                            //   <uint, long> gStartPos;
                            //   <uint, uint> gStartLine;
                            //   <uint, string> gSourcePath;
                            <string, variant> gdict = kv.value;
                            foreach (var kv2 in gdict)
                            {
                                string name = kv2.key;
                                <string, string> values = kv2.value;
                                string typeString = values["type"];
                                <string> blockPos;
                                if (values.Contains("line"))
                                {
                                    blockPos.Append(values["pos"]);
                                    blockPos.Append(values["line"]);
                                    blockPos.Append(values["source"]);
                                }
                                Symbols.AddGlobalMember(name, typeString, blockPos);    
                            }
                        } // !onlyNamedTypes
                    }
                    case "delegates":
                    {
                        // delegates, arguments, return types
                        //   <string> fdNames;
                        //   <string,uint> fdIndex;
                        //   <uint, <uint> > fdOverloads;
                        // for each overload uint:
                        //   <uint, string > fdReturnTypes;
                        //   <uint, < < string > > > fdArgumentNamesAndTypes;
                        
                        <string, variant> fdict = kv.value;
                        foreach (var kv2 in fdict)
                        {
                            string name = kv2.key;
                            <string, variant> overloads = kv2.value;
                            foreach (var kv3 in overloads)
                            {
                                if (UInt.TryParse(kv3.key, ref idNextOverload))
                                {
                                }
                                <string, variant> odict = kv3.value;
                                string returnType = "void";
                                if (odict.Contains("returntype"))
                                {
                                    returnType = odict["returntype"];
                                }
                                < <string> > arguments;
                                if (odict.Contains("arguments"))
                                {
                                    <string,string> adict = odict["arguments"];
                                    uint argCount = adict.Count;
                                    for (uint ai = 0; ai < argCount; ai++)
                                    {
                                        foreach (var kv4 in adict)
                                        {
                                            //<ref, type, name>
                                            string argName = kv4.key;
                                            string argOrder = ai.ToString() + ":";
                                            if (argName.StartsWith(argOrder))
                                            {
                                                argName = argName.Substring(argOrder.Length);
                                                <string> alist;
                                                string rf;
                                                string tn = kv4.value;
                                                if (tn.StartsWith("ref "))
                                                {
                                                    rf = "ref";
                                                    tn = tn.Substring(4);
                                                }
                                                alist.Append(rf);
                                                alist.Append(tn);
                                                alist.Append(argName);       
                                                arguments.Append(alist);
                                            }
                                        } // kv4
                                    }
                                }
                                AddFunctionDelegate(name, arguments, returnType);
                            } // kv3
                        } // kv2
                    }
                    case "functions":
                    {
                        if (!onlyNamedTypes)
                        {
                            // functions, arguments, return types
                            //  <string> fNames;
                            //  <string,uint> fIndex;
                            //  <uint, <uint> > fOverloads;
                            // for each overload uint:
                            //  <uint, string> fReturnTypes;
                            //  <uint, < < string > > > fArgumentNamesAndTypes;
                            // code location of function bodies
                            //  <uint, long> fStartPos;
                            //  <uint, uint> fStartLine;
                            //  <uint, string> fSourcePath;
                            <string, variant> fdict = kv.value;
                            foreach (var kv2 in fdict)
                            {
                                string name = kv2.key;
                                <string, variant> overloads = kv2.value;
                                foreach (var kv3 in overloads)
                                {
                                    if (!UInt.TryParse(kv3.key, ref iNextOverload))
                                    {
                                        PrintLn("bad iNextOverload");
                                    }
                                    
                                    <string, variant> odict = kv3.value;
                                    string returnType = "void";
                                    if (odict.Contains("returntype"))
                                    {
                                        returnType = odict["returntype"];
                                    }
                                    <string> blockPos;
                                    
                                    if (odict.Contains("line"))
                                    {
                                        string ln  = odict["line"];
                                        string pos = odict["pos"];
                                        string src = odict["source"];
                                        blockPos.Append(pos);
                                        blockPos.Append(ln);
                                        blockPos.Append(src);
                                    }
                                    byte iSysCall;
                                    byte iSysCallOverload;
                                    bool isSysCall;
                                    if (odict.Contains("syscall"))
                                    {
                                        string sc = odict["syscall"];
                                        string ov = odict["overload"];
                                        uint scui;
                                        if (UInt.TryParse(sc, ref scui))
                                        {
                                            iSysCall = byte(scui);
                                        }
                                        uint ovui;
                                        if (UInt.TryParse(ov, ref ovui))
                                        {
                                            iSysCallOverload = byte(ovui);
                                        }
                                        isSysCall = true;
                                    }
                                    byte iLibCall;
                                    byte iLibCallOverload;
                                    bool isLibCall;
                                    if (odict.Contains("libcall"))
                                    {
                                        string sc = odict["libcall"];
                                        string ov = odict["overload"];
                                        uint scui;
                                        if (UInt.TryParse(sc, ref scui))
                                        {
                                            iLibCall = byte(scui);
                                        }
                                        uint ovui;
                                        if (UInt.TryParse(ov, ref ovui))
                                        {
                                            iLibCallOverload = byte(ovui);
                                        }
                                        isLibCall = true;
                                    }
                                    < <string> > arguments;
                                    if (odict.Contains("arguments"))
                                    {
                                        <string,string> adict = odict["arguments"];
                                        uint argCount = adict.Count;
                                        for (uint ai = 0; ai < argCount; ai++)
                                        {
                                            foreach (var kv4 in adict)
                                            {
                                                //<ref, type, name>
                                                string argName = kv4.key;
                                                string argOrder = ai.ToString() + ":";
                                                if (argName.StartsWith(argOrder))
                                                {
                                                    argName = argName.Substring(argOrder.Length);
                                                    <string> alist;
                                                    string rf;
                                                    string tn = kv4.value;
                                                    if (tn.StartsWith("ref "))
                                                    {
                                                        rf = "ref";
                                                        tn = tn.Substring(4);
                                                    }
                                                    alist.Append(rf);
                                                    alist.Append(tn);
                                                    alist.Append(argName);       
                                                    arguments.Append(alist);
                                                }
                                            } // kv4
                                        }
                                    }
                                    uint iOverload = iNextOverload;
                                    AddFunction(name, arguments, returnType, blockPos);
                                    if (isSysCall)
                                    {
                                        SetSysCall(iOverload, iSysCall, iSysCallOverload);
                                    }
                                    if (isLibCall)
                                    {
                                        SetLibCall(iOverload, iLibCall, iLibCallOverload);
                                    }
                                } // kv3
                            } // kv2  
                        } // !onlyNamedTypes
                    }
                    case "units":
                    {
                        if (typeof(kv.value) == list)
                        {
                            <string> ulist = kv.value;
                            foreach (var name in ulist)
                            {
                                AddNameSpace(name, "");
                            }
                        }
                        else if (typeof(kv.value) == dictionary)
                        {
                            <string,string> udict = kv.value;
                            foreach (var namekv in udict)
                            {
                                AddNameSpace(namekv.key, namekv.value);
                            }
                        }
                    }
                    case "locations":
                    {
                        <string,string> ldict = kv.value;
                        foreach (var locationkv in ldict)
                        {
                            AddLocation(locationkv.key, locationkv.value);
                        }
                    }
                    
                    default:
                    {
#ifndef JSON_EXPRESS
                        PrintLn("case '" + kv.key + "' not supported in Symbols.Import");
#endif
                    }
                }
            }
            break;
        }
        return success;
    }
 
    OverloadToCompile(uint iOverload)
    {
        if (!overloadsCompiled.Contains(iOverload))
        {
            if (IsSysCall(iOverload))
            {
                overloadsCompiled[iOverload] = true; // syscall : pretend it is already compiled
            }
            else if (IsLibCall(iOverload))
            {
                overloadsCompiled[iOverload] = true; // libcall : pretend it is already compiled
            }
            else
            {
                overloadsCompiled[iOverload] = false;
                
                <string,variant> methodBlock = Block.GetMethodBlock();
                uint iCaller;
                if (methodBlock.Contains("iOverload"))
                {
                    string sCaller = methodBlock["iOverload"];
                    if (UInt.TryParse(sCaller, ref iCaller))
                    {
                        if (!fTouches.Contains(iCaller))
                        {
                            <uint> empty;
                            fTouches[iCaller] = empty;
                        }
                        <uint> touchesList = fTouches[iCaller];
                        touchesList.Append(iOverload);
                        fTouches[iCaller] = touchesList;
                    }
                }
            }
        }
    }
    
          
    OverloadWasCompiled(uint iOverload)
    {
        overloadsCompiled[iOverload] = true;
    }
    bool OverloadNextToCompile(ref uint iOverload)
    {
        string lastSource;
        <string, string> lastToken = Symbols.GetOverloadStart(iLastOverload);
        if ((lastToken.Count != 0) && (lastToken["pos"] != "0"))
        {
            lastSource = lastToken["source"];
            lastSource = lastSource.ToLower();
        }
        
        bool oneMore = false;
        for (int pass = 0; pass < 2; pass++)
        {
            foreach (var kv in overloadsCompiled)
            {
                if (!kv.value)
                {
                    iOverload = kv.key;
                    <string, string> nextToken = Symbols.GetOverloadStart(iOverload);
                    string nextSource = nextToken["source"];
                    nextSource = nextSource.ToLower();
                    if ((pass == 1) || (nextSource == lastSource))
                    {
                        oneMore = true;
                        iLastOverload = iOverload;
                        break;
                    }
                }
            }
            if (oneMore)
            {
                break;
            }
        }
        return oneMore;
    }
    
    string GetMethodNameFromOverload(uint iOverload)
    {
        string name;
        foreach (var f in fIndex)
        {
            <string, variant> fentry;
            uint index = f.value;
            <uint> overloads = fOverloads[index];
            foreach (var overload in overloads)
            {
                if (iOverload == overload)
                {
                    name = fNames[index];
                    break;
                }
            }
            if (name.Length != 0)
            {
                break;
            }
        }
        return name;
    }
    uint ExportTouchTree(ref <string> contentLines, uint iFrom, uint indent, ref <uint> touchesDone)
    {
        string pad;
        pad = pad.Pad(' ', indent);
        string fromName = GetMethodNameFromOverload(iFrom);
        
        uint cSize = 0;
        touchesDone.Append(iFrom);
        bool isLeaf = true;
        if (fTouches.Contains(iFrom))
        {
            <uint> calls = fTouches[iFrom];
            foreach (var t in calls)
            {
                uint iTo = t;
                if (!touchesDone.Contains(iTo))
                {
                    cSize = cSize + ExportTouchTree(ref contentLines, iTo, indent+2, ref touchesDone);
                    isLeaf = false;
                }
            }
        }
        uint cSizeSelf;
        if (fCodeStream.Contains(iFrom))
        {
            <byte> codeStream = fCodeStream[iFrom];
            cSizeSelf = codeStream.Count;
        }
        cSize = cSize + cSizeSelf;
        string content = pad + fromName;
        content = content.Pad(' ', 60);
        string sSizeSelf = cSizeSelf.ToString();
        sSizeSelf = sSizeSelf.LeftPad(' ', 6);
        string sSize = cSize.ToString();
        sSize = sSize.LeftPad(' ', 6);
        content = content + sSizeSelf + sSize;
        if (isLeaf)
        {
            content = content + " <-";
        }
        contentLines.Insert(0, content);
        return cSize;
    }
    ExportTouchTree(string path)
    {
        <string> contentLines;
        <uint> touchesDone;
        uint cSize = 0;
        foreach (var kv in fTouches)
        {
            uint iFrom = kv.key;
            if (!touchesDone.Contains(iFrom))
            {
                cSize = cSize + ExportTouchTree(ref contentLines, iFrom, 0, ref touchesDone);
            }
        }
        if (File.Exists(path))
        {
            File.Delete(path);
        }
        file touchesFile = File.Create(path);
        foreach (var line in contentLines)
        {
            touchesFile.Append(char(0x0A) + line);
        }
        touchesFile.Flush();
    }
}
