unit Symbols
{

    uses "/Source/System/System"
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Tokens/SysCalls"
      
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
    // code location of function bodies
    <uint, long> fStartPos;
    <uint, uint> fStartLine;
    <uint, string> fSourcePath;
    
    <uint, byte> fSysCall;
    <uint, byte> fSysCallOverload;
    <string, byte> fSysCallCount;
    
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
                
    // enums
    <string, <string, uint> > eValues;
    <string, uint> eIndex;
    <uint, <string,uint> > eMembers;
        
    // flags
    <string, <string, uint> > flValues;
    <string, uint> flIndex;
    <uint, <string,uint> > flMembers;
    
    // preprocessor symbols
    <string, string> pdValues;
    
    // constants
    <string, string> cValues;
    
    // unit names
    <string> nameSpaces;
    
    < <string, string> > deferredTypes;
         
    uint iLastOverload;
    <uint, bool> overloadsCompiled;
         
    New()
    {
        // clear any globals
        fNames.Clear();
        fIndex.Clear();
        fOverloads.Clear();
        fReturnTypes.Clear();
        fArgumentNamesAndTypes.Clear();
        fStartPos.Clear();
        fStartLine.Clear();             
        fSourcePath.Clear();  
        fCodeStream.Clear();
        fDebugInfo.Clear();
        fCalls.Clear();
        
        fSysCall.Clear();
        fSysCallOverload.Clear();
        fSysCallCount.Clear();
            
        iNextOverload = 0; 
        
        pdValues.Clear();
        cValues.Clear();
        
        gNames.Clear();
        gIndex.Clear();
        gTypes.Clear();
        gStartPos.Clear();
        gStartLine.Clear();
        gSourcePath.Clear();
                                                                                         
        nameSpaces.Clear();
        
        fdNames.Clear();
        fdIndex.Clear();
        fdOverloads.Clear();
        fdReturnTypes.Clear();
        fdArgumentNamesAndTypes.Clear();
        idNextOverload = 0; 
        
        eValues.Clear();
        eIndex.Clear();
        eMembers.Clear();
        
        flValues.Clear();
        flIndex.Clear();
        flMembers.Clear();
        
        overloadsCompiled.Clear();
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
            if (namespace.Length > 0)
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
            if (name.Length > 0)
            {
                break;
            }
        }
        return name;
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
    
    AddNameSpace(string name)
    {
        if (!nameSpaces.Contains(name))
        {
            nameSpaces.Append(name);
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
    AddConstant(string name, string value)
    {
        cValues[name] = value;
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
        return gNames.Length;
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
            // -1 so that the next call to Advance() returns the opening '{'
            long isp = gStartPos[iGlobal] - 1;
            startToken["pos"]    = isp.ToString();
            uint isl = gStartLine[iGlobal];
            startToken["line"]   = isl.ToString();
            startToken["source"] = gSourcePath[iGlobal];     
        }
        return startToken;
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
                if (f.IsUpper())
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
    
    string QualifyEnum(string name, string currentNamespace)
    {
        if (!name.Contains('.'))
        {
            loop
            {
                // flags
                <string, <string, uint> > flValues;
                <string, uint> flIndex;
                <uint, <string,uint> > flMembers;
                
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
    
    bool GetFunctionIndex(string name, ref uint index)
    {
        // returnType == void
        bool found = false;
        if (fNames.Contains(name))
        {
            uint length = fNames.Length;
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
            uint length = fdNames.Length;
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
            uint iGlobal = gNames.Length-1;
            gIndex[name] = iGlobal;
            gTypes[iGlobal] = typeString;
            if (blockPos.Length > 0) // has initialization code?
            {
                long startPos;
                uint startLine;
                if (Token.TryParseLong(blockPos[0], ref startPos))
                {
                    gStartPos[iGlobal] = startPos; // location of method block '{'
                }
                if (Token.TryParseUInt(blockPos[1], ref startLine))
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
            index = fNames.Length-1;
            fIndex[name] = index;
        }
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
                        Parser.Error("duplicate function definition");
                    }
                    else
                    {
                        Parser.Error("function definitions differ only by return type");
                    }
                    break;
                }
            }    
            overloads.Append(iNextOverload);
            fArgumentNamesAndTypes[iNextOverload] = arguments;
            fReturnTypes[iNextOverload] = returnType;
            
            if (blockPos.Length > 0) // not system
            {
                long startPos;
                uint startLine;
                if (Token.TryParseLong(blockPos[0], ref startPos))
                {
                    fStartPos[iNextOverload] = startPos; // location of method block '{'
                }
                if (Token.TryParseUInt(blockPos[1], ref startLine))
                {
                    fStartLine[iNextOverload] = startLine; // location of method block '{'
                }
                fSourcePath[iNextOverload] = blockPos[2];
            }
            else
            {
                byte iSysCall;
                if (!SysCalls.TryParse(name, ref iSysCall))
                {
                    Parser.Error("undefined 'system' method '" + name + "'");
                    break;    
                }
                
                if (!fSysCallCount.Contains(name))
                {
                    fSysCallCount[name] = 0;    
                }
                byte iSysCallOverload = fSysCallCount[name];
                fSysCallCount[name] = iSysCallOverload+1;
                fSysCall[iNextOverload] = iSysCall;
                fSysCallOverload[iNextOverload] = iSysCallOverload;
            }
            fOverloads[index] = overloads;
            iNextOverload++;
            break;
        }
    }
    
    AddMethod(string methodName, < <string > > arguments, <string> blockPos)
    {
        AddFunction(methodName, arguments, "void", blockPos); // 'void' is only used internally
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
    
    AddFlags(string identifier, <string, uint> members)
    {
        // <string, <string, uint> > flValues;
        <string, uint> values;
        flValues[identifier] = values;
        uint iNext = flIndex.Count;
        flIndex[identifier] = iNext;
        flMembers[iNext] = members;
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
            index = fdNames.Length-1;
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
            if (constantStream.Length > 0)
            {
                <string,variant> cdict;
                cdict["data"] = constantStream;    
                dict["const"] = cdict;
            }
            
            //<string> gNames;
            uint count;
            <string,string> globals;
            foreach (var glb in gNames)
            {
                globals[count.ToString()] = glb;
                count = count + 2;
            }
            if (globals.Count > 0)
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
                
                if (fCalls.Contains(iUsedOverload))
                {
                    mdict["hits"] = fCalls[iUsedOverload];
                }
                if (fCodeStream.Contains(iUsedOverload))
                {
                    < < string > > argsAndTypes = fArgumentNamesAndTypes[iUsedOverload];
                    uint argCount = argsAndTypes.Length;
                    uint acount;
                    <string,string> locals;
                    foreach (var argument in argsAndTypes)
                    {
                        string name = argument[2];
                        acount++;
                        uint offset = (argCount-acount)*2;
                        locals[offset.ToString()] = name;
                    }
                    if (locals.Count > 0)
                    {
                        mdict["locals"]   = locals;
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
            
            dict["units"]   = nameSpaces;
            dict["symbols"]   = pdValues;
            dict["constants"] = cValues;
            
            <string, <string, variant> > edict;
            foreach (var en in eIndex)
            {
                <string, variant> eentry;
                uint index = en.value;
                eentry["members"] = eMembers[index];                
                edict[en.key] = eentry;
            }
            if (edict.Count > 0)
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
            if (fldict.Count > 0)
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
            if (gdict.Count > 0)
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
                            if (rf.Length > 0)
                            {
                                tp = rf + " " + tp;
                            }
                            args[ai.ToString() + ":" + nm] = tp;        
                            ai++;
                        }
                        if (args.Count > 0)
                        {
                            odict["arguments"] = args;
                        }
                    }
                    fdentry[overload.ToString()] = odict;
                }
                fddict[fd.key] = fdentry;
            }
            if (fddict.Count > 0)
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
                            if (rf.Length > 0)
                            {
                                tp = rf + " " + tp;
                            }
                            args[ai.ToString() + ":" + nm] = tp;
                            ai++;
                        }
                        if (args.Count > 0)
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
                    fentry[overload.ToString()] = odict;
                }
                fdict[f.key] = fentry;
            }
            if (fdict.Count > 0)
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
                        cValues = kv.value;
                    }
                    case "symbols":
                    {
                        pdValues = kv.value;
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
                                if (Token.TryParseUInt(l, ref ui))
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
                                if (Token.TryParseUInt(l, ref ui))
                                {
                                    members[kv6.key] = ui;
                                }
                            }
                            AddFlags(name, members);
                        }
                    }
                    case "globals":
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
                            AddGlobalMember(name, typeString, blockPos);    
                        }
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
                                if (Token.TryParseUInt(kv3.key, ref idNextOverload))
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
                                if (!Token.TryParseUInt(kv3.key, ref iNextOverload))
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
                                    if (Token.TryParseUInt(sc, ref scui))
                                    {
                                        iSysCall = byte(scui);
                                    }
                                    uint ovui;
                                    if (Token.TryParseUInt(ov, ref ovui))
                                    {
                                        iSysCallOverload = byte(ovui);
                                    }
                                    isSysCall = true;
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
                            } // kv3
                        } // kv2  
                    }
                    case "units":
                    {
                        <string> ulist = kv.value;
                        foreach (var name in ulist)
                        {
                            AddNameSpace(name);
                        }
                    }
                    default:
                    {
                        PrintLn("case '" + kv.key + "' not supported in Symbols.Import");
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
            else
            {
                overloadsCompiled[iOverload] = false;
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
        if (lastToken.Count > 0)
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
}
