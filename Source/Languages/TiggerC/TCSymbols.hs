unit TCSymbols
{
    friend TCStatement, TCExpression, TCCompile;
    
    uses "TCToken"
    uses "TCType"
    uses "TCCode"
    
    
    record Variable
    {
        string Type;
        int    Offset;
        bool   IsGlobal;
    }
    record Function
    {
        string ReturnType;
        bool   Prototype;
        <string, Variable> Arguments;
        <string> ArgumentNames;
    }
    
    uint blockLevel;
    < <string, Variable> > variables;
    <string, Function> functions;
    <string,bool> symbols;
    
    string currentFunction;
    bool isNaked;
    string CurrentFunction { get { return currentFunction; } }
    bool   CurrentIsNaked  { get { return isNaked; } set { isNaked = value; } }
    
    uint BlockLevel { get { return blockLevel; } set { blockLevel = value; } }
    
    string VariableComment()
    {
        return "(level=" + (GetCurrentVariableLevel()).ToString() + ", levelBytes=" + (GetLevelBytes(GetCurrentVariableLevel())).ToString() + ")";
    }
    uint GetCurrentVariableLevel()
    {
        if (variables.Count == 0)
        {
            Die(0x0B);
        }
        return variables.Count-1;
    }
    
    byte GetLevelBytes(uint level)
    {
        byte bytes;
        <string, Variable> scopeVariables = variables[level];
        foreach (var kv in scopeVariables)
        {
            Variable variable = kv.value;
            if (variable.Offset >= 0) // not arguments)
            {
                bytes += IsByteType(variable.Type) ? 1 : 2;
            }
        }
        return bytes;
    }
    
    EnterBlock(bool generate, string comment)
    {
        if (!generate)
        {
            TCCode.Defer("{ // " + comment);
        }
        else
        {
            TCCode.PadOut("{ // " + comment, 0);
        }
        <string, Variable> level;
        variables.Append(level);
        blockLevel++;
    }
    FreeAutomaticAllocations(uint variableLevel)
    {
        bool commented;
        <string, Variable> level = variables[variableLevel];
        foreach (var kv in level)
        {
            Variable v = kv.value;
            string memberType;
            if (IsArrayType(v.Type, ref memberType))
            {
                string arrayType = v.Type;
                if (!arrayType.Contains("[]"))
                {
                    if (!commented)
                    {
                        TCCode.PadOut("", 0);
                        TCCode.PadOut("// free automatic allocations " + VariableComment(), 0);
                        commented = true;
                    }
        
                    TCCode.PushVariable(kv.key, v.Offset, false, v.IsGlobal);
                    TCCode.PadOut("TCSys.Free();", 0);
                    TCCode.PadOut("PLY", 0);
                    TCCode.PadOut("PLY", 0);
                }
            }
        }
    }
    LeaveBlock(string name, bool generate)
    {
        if ((name == "if") || (name == "else") || (name == "for") || (name == "while") || (name == "case"))
        {
            FreeAutomaticAllocations(GetCurrentVariableLevel());
            TCCode.PopBytes(name + " locals");
            LocalOffset -= GetLevelBytes(GetCurrentVariableLevel());
        }
        variables.Remove(variables.Count-1);
        if (name == "main")
        {
            FreeAutomaticAllocations(GetCurrentVariableLevel());
            TCCode.PopBytes(byte(GlobalOffset), "global variable " + VariableComment());
        }
        
        blockLevel--;
        if (generate)
        {
            if (name.Length != 0)
            {
                TCCode.PadOut("} // " + name, 0);
            }
            else
            {   
                TCCode.PadOut("}", 0);
            }
        }
    }
    
    DefineSymbol(string name)
    {
        symbols[name] = true;
        TCCode.PadOut("#define " + name, 0);
    }
    bool IsDefined(string name)
    {
        return symbols.Contains(name);
    }
    
    SetFunctionPrototype(string functionName)
    {
        Function function = functions[functionName];
        function.Prototype = true;
        functions[functionName] = function;
    }
    bool DefineFunction(string returnType, string functionName)
    {
        bool success;
        loop
        {
            if (functions.Contains(functionName))
            {
                Token token = TCScanner.Current();
                Function proto = functions[functionName];
                if (!proto.Prototype)
                {
                    // second definition can only follow prototype
                    Error(token.SourcePath, token.Line, "function with name '" + functionName + "' already exists");
                    break;
                }
                // 2nd definition following a prototype
                if (returnType != proto.ReturnType)
                {
                    Error(token.SourcePath, token.Line, "return type does not match protype definition for '" + functionName + "'");
                    break;
                }
                // fall through and redefine
                // TODO : validate arguments against prototype
                // TODO : don't allow multiple prototypes before function definition (only one)
            }
            
            Function function;
            function.ReturnType = returnType;
            functions[functionName] = function;
            currentFunction = functionName;
            success =  true;
        
            break;
        } // loop
        return success;
    }
    UpdateArgumentOffsets(string functionName)
    {
        <uint, string> argumentIndex;
        <uint, byte>   argumentSize;
        
        Function function = functions[functionName];
        <string, Variable> arguments = function.Arguments;
        if (arguments.Count != 0)
        {
            foreach (var argument in arguments)
            {
                Variable variable = argument.value;
                argumentIndex[variable.Offset] = argument.key;
                argumentSize [variable.Offset] = (IsByteType(variable.Type) ? 1 : 2);
            }            
            uint i = argumentIndex.Count - 1;
            int  offset = 0;
            loop
            {
                string name = argumentIndex[i];
                byte size   = argumentSize[i];
                
                string variableType;
                int    oldOffset;
                bool   isGlobal;
                _ = GetVariable(name, ref variableType, ref oldOffset, ref isGlobal);
                offset -= size;
                SetVariableOffset(name, offset);
                if (i == 0) { break; }
                i--; 
            }
        }
    }
    
    
    bool DefineArgument(string functionName, string argumentType, string argumentName, int offset)
    {
        Function function = functions[functionName];
        
        <string> argumentNames = function.ArgumentNames;
        <string, Variable> arguments = function.Arguments;
        if (arguments.Contains(argumentName))
        {
            // exists already
            Token token = TCScanner.Current();
            Error(token.SourcePath, token.Line, "argument with name '" + argumentName + "' already exists");
            return false;
        }
        Variable argument;
        argument.Type = argumentType;
        argument.Offset = offset;
        argument.IsGlobal = false;
        arguments[argumentName] = argument;
        argumentNames.Append(argumentName);
        function.Arguments = arguments;
        function.ArgumentNames = argumentNames;
        functions[functionName] = function;
        /*
        if (arguments.Count == 1)
        {
            Print(" : ");
        }
        else
        {
            Print(", ");
        }
        Print(argumentType + " " + argumentName);
        */
        return true;
    }
    
    bool GetFunction(string functionName, ref string returnType)
    {
        if (functions.Contains(functionName))
        {
            Function function = functions[functionName];
            returnType  = function.ReturnType;
            return true;
        }
        return false; // not found
    }
    <string,Variable> GetArguments(string functionName, ref <string> argumentNames)
    {
        argumentNames = (functions[functionName]).ArgumentNames;
        return (functions[functionName]).Arguments;
    }
    
    bool DefineVariable(string variableType, string variableName, int offset, bool isGlobal)
    {
        uint level = variables.Count - 1;
        <string, Variable> scopeVariables = variables[level];
        if (scopeVariables.Contains(variableName))
        {
            // exists already
            Token token = TCScanner.Current();
            Error(token.SourcePath, token.Line, "variable with name '" + variableName + "' already exists");
            return false;
        }
        Variable newVariable;
        newVariable.Type  = variableType;
        newVariable.Offset = offset;
        newVariable.IsGlobal = isGlobal;
        scopeVariables[variableName] = newVariable;
        
        variables[level] = scopeVariables;   
        //PrintLn();
        //Print(level.ToString() + ": " +variableType + " " + variableName);
        return true;
    }
    
        
    bool GetVariable(string variableName, ref string variableType, ref int offset, ref bool isGlobal)
    {
        uint level = variables.Count - 1;
        loop
        {
            <string, Variable> scopeVariables = variables[level];
            if (scopeVariables.Contains(variableName))
            {
                Variable variable = scopeVariables[variableName];
                variableType  = variable.Type;
                isGlobal = variable.IsGlobal;
                offset = variable.Offset;
                return true;
            }
            if (level == 0) { break; }
            level--;
        }
        return false; // not found
    }
    SetVariableOffset(string variableName, int offset)
    {
        uint level = variables.Count - 1;
        loop
        {
            <string, Variable> scopeVariables = variables[level];
            if (scopeVariables.Contains(variableName))
            {
                Variable variable = scopeVariables[variableName];
                variable.Offset = offset;
                scopeVariables[variableName] =  variable;
                variables[level] = scopeVariables;
                break;
            }
            if (level == 0) { break; }
            level--;
        }
    }
}
