unit Symbols
{
    uses "/Source/System/System"
  
    // constants : order matters for constant indices so only lists work
    <string> iNames;
    <string,uint> iIndex;
    <int> iValues;
    
    <string> bNames;
    <string,uint> bIndex;
    <byte> bValues;
    
    <string> uiNames;
    <string,uint> uiIndex;
    <uint> uiValues;
    
    <string> bbNames;
    <string,uint> bbIndex;
    <bool> bbValues;
    <string> cNames;
    <string,uint> cIndex;
    <char> cValues;
    <string> sNames;
    <string,uint> sIndex;
    <string> sValues;
    
    // methods and arguments
    <string> mNames;
    <string,uint> mIndex;
    <string, < <string> > > mArgumentNamesAndTypes; // order matters
    // source location of body
    <string,long> mStartPos;
    <string,uint> mStartLine;
    <string,string> mSourcePath;
    
    // functions, arguments and returntype
    <string> fNames;
    <string,uint> fIndex;
    <string, < <string> > > fArgumentNamesAndTypes; // order matters
    <string,string> fReturnTypes;
    // source location of body
    <string,long> fStartPos;
    <string,uint> fStartLine;
    <string,string> fSourcePath;
    
    // globals
    <string> globalNames;
    <string> globalValues;
    <string> globalTypes;
    
    // current locals
    <string> localNames;
    <string> localTypes;
    
    <uint>    localCountOnLoopEntry;
    
    // current arguments and returntype
    <string> argumentNames;
    <string> argumentTypes;
    string returnType; 
    
    New()
    {
        bNames.Clear();
        uiNames.Clear();
        iNames.Clear();
        sNames.Clear();
        bValues.Clear();
        ui.Values.Clear();
        iValues.Clear();
        sValues.Clear();
        
        mNames.Clear();
        mArgumentNamesAndTypes.Clear();
        mStartPos.Clear();
        mSourcePath.Clear();
        
        fNames.Clear();
        fArgumentNamesAndTypes.Clear();
        fReturnTypes.Clear();
        fStartPos.Clear();
        fSourcePath.Clear();
        
        globalNames.Clear();
        globalValues.Clear();
        globalTypes.Clear();
        
        localNames.clear();
        localTypes.clear();
        
        argumentNames.clear();
        argumentTypes.clear();
    }
    
    SetLocalCountOnLoopEntry(uint localsOnLoopEntry)
    {
        localCountOnLoopEntry.Append(localsOnLoopEntry);
    }
    PopLocalCountOnLoopEntry()
    {
        uint lastIndex = localCountOnLoopEntry.Length - 1;
        localCountOnLoopEntry.Remove(lastIndex);
    }
    uint GetLocalCountOnLoopEntry()
    {
        return localCountOnLoopEntry[localCountOnLoopEntry.Length - 1];
    }
    uint WriteConstant(string name, uint value)
    {
        uiNames.Append(name);
        uint index = uiNames.Length-1;
        uiIndex[name] = index;
        uiValues.Append(value);
        return index;
    }
    uint WriteConstant(string name, byte value)
    {
        bNames.Append(name);
        uint index = bNames.Length-1;
        bIndex[name] = index;
        bValues.Append(value);
        return index;
    }
    uint WriteConstant(string name, char value)
    {
        cNames.Append(name);
        uint index = cNames.Length-1;
        cIndex[name] = index;
        cValues.Append(value);
        return index;
    }
    uint WriteConstant(string name, bool value)
    {
        bbNames.Append(name);
        uint index = bbNames.Length-1;
        bbIndex[name] = index;
        bbValues.Append(value);
        return index;
    }
    uint WriteConstant(string name, int value)
    {
        iNames.Append(name);
        uint index = iNames.Length-1;
        iIndex[name] = index;
        iValues.Append(value);
        return index;
    }
    uint WriteConstant(string name, string value)
    {
        sNames.Append(name);
        uint index = sNames.Length-1;
        sIndex[name] = index;
        sValues.Append(value);
        return index;
    }
    bool ResolveConstant(string name, ref bool value)
    {
        bool success = false;
        if (bbNames.Contains(name))
        {
            uint index = bbIndex[name];
            value = bbValues[index];
            success = true;
        }
        return success;
    }
    bool ResolveConstant(string name, ref byte value)
    {
        bool success = false;
        if (bNames.Contains(name))
        {
            uint index = bIndex[name];
            value = bValues[index];
            success = true;
        }
        return success;
    }
    bool ResolveConstant(string name, ref int value)
    {
        bool success = false;
        if (iNames.Contains(name))
        {
            uint index = iIndex[name];
            value = iValues[index];
            success = true;
        }
        return success;
    }
    bool ResolveConstant(string name, ref uint value)
    {
        bool success = false;
        if (uiNames.Contains(name))
        {
            uint index = uiIndex[name];
            value = uiValues[index];
            success = true;
        }
        return success;
    }
    AddGlobal(string name, string gtype, string value)
    {
        globalNames.Append(name);
        globalTypes.Append(gtype);
        globalValues.Append(value);
    }
    string GetGlobalType(string name)
    {
        return globalTypes[name];
    }
    string GetGlobalValue(string name)// initial value
    {
        return globalValue[name];
    }
    uint AddLocal(string name, string ltype)
    {
        localNames.Append(name);
        localTypes.Append(ltype);
        uint index = localNames.Length - 1;
        return index;
    }
    AddArguments(< <string> > arguments)
    {
        foreach (var argument in arguments)
        {
            argumentTypes.Append(argument[0]); // type
            argumentNames.Append(argument[1]); // identifier
        }
    }
    
    AddMethodArguments(string name)
    {
        AddArguments(mArgumentNamesAndTypes[name]);
    }
    AddFunctionArguments(string name)
    {
        AddArguments(fArgumentNamesAndTypes[name]);
    }
    SetCurrentReturnType(string name)
    {
        returnType = fReturnTypes[name];
    }
    string GetCurrentReturnType()
    {
        return returnType;
    }
    ResetLocals()
    {
        localNames.Clear();
        localTypes.Clear();
        argumentNames.Clear();
        argumentTypes.Clear();
    }
    uint GetArgumentsToPopCount()
    {
        return argumentNames.Length;
    }
    bool LocalSymbolExists(string name)
    {
        bool symbolExists;
        loop
        {
            if (localNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (argumentNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            break;
        }
        return symbolExists;
    }
    uint AddMethod(string name, < <string> > arguments, <string> blockPos)
    {
        mNames.Append(name);
        uint index = mNames.Length - 1;
        mIndex[name] = index;
        mArgumentNamesAndTypes[name] = arguments;
        long startPos;
        uint startLine;
        if (Token.TryParseLong(blockPos[0], ref startPos))
        {
            mStartPos[name] = startPos; // location of main block '{'
        }
        if (Token.TryParseUInt(blockPos[1], ref startLine))
        {
            mStartLine[name] = startLine; // location of main block '{'
        }
        mSourcePath[name] = blockPos[2];
        return index;
    }
    uint AddFunction(string name, < <string> > arguments, string returnType, <string> blockPos)
    {
        fNames.Append(name);
        uint index = fNames.Length - 1;
        fIndex[name] = index;
        fArgumentNamesAndTypes[name] = arguments;
        long startPos;
        uint startLine;
        if (Token.TryParseLong(blockPos[0], ref startPos))
        {
            fStartPos[name] = startPos; // location of main block '{'
        }
        if (Token.TryParseUInt(blockPos[1], ref startLine))
        {
            fStartLine[name] = startLine; // location of main block '{'
        }
        fSourcePath[name] = blockPos[2];
        fReturnTypes[name] = returnType;
        return index;
    }
    bool SymbolExists(string name)
    {
        bool symbolExists;
        loop
        {
            if (fNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (mNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (globalNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (iNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (sNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (cNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (bNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (uiNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (bbNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            if (localNames.Contains(name))
            {
                symbolExists = true;
                break;
            }
            break;
        }
        return symbolExists;
    }
    
    bool indexOf(<string> lst, string name, ref uint index)
    {
        bool found = false;
        if (lst.Contains(name))
        {
            uint length = lst.Length;
            for (uint i =0; i < length; i++)
            {
                if (name == lst[i])
                {
                    index = i;
                    found = true;
                    break;
                }
            }
        }
        return found;
    }
    long calculateGlobalOffset(<string> types, uint index)
    {
        long offset = 0xFFFF; // actually should be 0x0000 so that first -2 -> 0xFFFE
        uint i = 0;
        while (i < index)
        {
            string tp = types[i];
            offset = offset - 2;
            i++;
        }
        return offset - 1; // make up for not 0xFFFF, not 0x0000 above
    }
    uint calculateLocalOffset(<string> types, uint index) // IX + n + 1 for locals
    {
        uint offset = 0;
        uint i = 0;
        loop
        {
            string tp = types[i];
            offset = offset + 2;
            if (i == index)
            {
                break;
            }
            i++;
        }
        return offset;
    }
    uint calculateArgumentOffset(<string> types, uint index)
    {
        uint offset = 0;
        uint i = types.Length-1;
        loop
        {
            string tp = types[i];
            if (i == index)
            {
                break;
            }
            offset = offset + 2;
            i--;
        }
        return offset;
    }
    <string,string> GetVariable(string name)
    {
        <string,string> variable;
        uint index;
        if (localNames.Contains(name))
        {
            if (indexOf(localNames, name, ref index))
            {
                variable["name"] = name;
                variable["kind"] = "local";
                variable["type"] = localTypes[index];
                uint offset = calculateLocalOffset(localTypes, index);
                variable["offset"] = offset.ToString();
            }
        }
        else if (argumentNames.Contains(name))
        {
            if (indexOf(argumentNames, name, ref index))
            {
                variable["name"] = name;
                variable["kind"] = "argument";
                variable["type"] = argumentTypes[index];
                uint offset = calculateArgumentOffset(argumentTypes, index);
                variable["offset"] = offset.ToString();
            }
        }
        
        else if (globalNames.Contains(name))
        {
            if (indexOf(globalNames, name, ref index))
            {
                variable["name"] = name;
                variable["kind"] = "global";
                variable["type"] = globalTypes[index];
                long loffset = calculateGlobalOffset(globalTypes, index);
                variable["offset"] = loffset.ToString();
            }
        }
        return variable;
    }
    <string,string> GetFunction(string name)
    {
        <string,string> function;
        if (fNames.Contains(name))
        {
            function["name"] = name;
            function["kind"] = "function";
            function["returntype"] = fReturnTypes[name];
        }
        return function;
    }
    < <string> > GetFunctionArguments(string name)
    {
         < <string> > arguments = fArgumentNamesAndTypes[name];
        return arguments;
    }
    <string,string> GetMethod(string name)
    {
        <string,string> method;
        if (mNames.Contains(name))
        {
            method["name"] = name;
            method["kind"] = "function";
        }
        return method;
    }
    < <string> > GetMethodArguments(string name)
    {
         < <string> > arguments = mArgumentNamesAndTypes[name];
        return arguments;
    }
    
    uint GetGlobalsCount()
    {
        return globalNames.Length;
    }
    string GetGlobalType(uint gIndex)
    {
        return globalTypes[gIndex];
    }
    string GetGlobalName(uint gIndex)
    {
        return globalNames[gIndex];
    }
    string GetGlobalValue(uint gIndex)
    {
        return globalValues[gIndex];
    }
    
    uint GetLocalsCount()
    {
        return localNames.Length;
    }
    string GetLocalType(uint lIndex)
    {
        return localTypes[lIndex];
    }
    string GetLocalName(uint lIndex)
    {
        return localNames[lIndex];
    }
    PopLastLocal()
    {
        uint lastIndex = localNames.Length - 1;
        localNames.Remove(lastIndex);
        localTypes.Remove(lastIndex);
    }
    
    bool GetMethodIndex(string name, ref uint index)
    {
        bool found = false;
        if (mNames.Contains(name))
        {
            uint length = mNames.Length;
            for (uint i =0; i < length; i++)
            {
                if (name == mNames[i])
                {
                    index = i;
                    found = true;
                    break;
                }
            }
        }
        return found;
    }
    bool GetFunctionIndex(string name, ref uint index)
    {
        bool found = false;
        if (fNames.Contains(name))
        {
            uint length = fNames.Length;
            for (uint i =0; i < length; i++)
            {
                if (name == fNames[i])
                {
                    index = i;
                    found = true;
                    break;
                }
            }
        }
        return found;
    }
    string GetMethodName(uint mIndex)
    {
        return mNames[mIndex];
    }
    string GetFunctionName(uint fIndex)
    {
        return fNames[fIndex];
    }
    long GetFunctionPos(string name)
    {
        return fStartPos[name];
    }
    uint GetFunctionLine(string name)
    {
        return fStartLine[name];
    }
    string GetFunctionSourcePath(string name)
    {
        return fSourcePath[name];
    }
    long GetMethodPos(string name)
    {
        return mStartPos[name];
    }
    uint GetMethodLine(string name)
    {
        return mStartLine[name];
    }
    string GetMethodSourcePath(string name)
    {
        return mSourcePath[name];
    }
}
