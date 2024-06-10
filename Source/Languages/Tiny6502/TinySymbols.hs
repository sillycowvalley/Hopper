unit TinySymbols
{
    friend TinyStatement, TinyExpression, TinyCompile;
    
    uses "TinyToken"
    uses "TinyType"
    
    record Variable
    {
        string Type;
        int    Offset;
        bool   IsGlobal;
    }
    record Function
    {
        string ReturnType;
        <string, Variable> Arguments;
    }
    
    uint blockLevel;
    < <string, Variable> > variables;
    <string, Function> functions;
    <string,bool> symbols;
    
    uint BlockLevel { get { return blockLevel; } }
    
    byte GetCurrentLevelBytes()
    {
        byte bytes;
        if (variables.Count > 0)
        {
            <string, Variable> scopeVariables = variables[variables.Count - 1];
            foreach (var kv in scopeVariables)
            {
                Variable variable = kv.value;
                if (variable.Offset >= 0) // not arguments)
                {
                    bytes += IsByteType(variable.Type) ? 1 : 2;
                }
            }
        }
        return bytes;
    }
    
    EnterBlock(bool generate)
    {
        if (!generate)
        {
            TinyCode.Defer("{");
        }
        else
        {
            TinyCode.Append("{");
        }
        <string, Variable> level;
        variables.Append(level);
        blockLevel++;
    }
    LeaveBlock(string name, bool generate)
    {
        variables.Remove(variables.Count-1);
        
        if (name == "main")
        {
            TinyCode.PopBytes(byte(GlobalOffset), "global variable");
        }
        
        blockLevel--;
        if (generate)
        {
            if (name.Length != 0)
            {
                TinyCode.Append("} // " + name);
            }
            else
            {   
                TinyCode.Append("}");
            }
        }
    }
    
    DefineSymbol(string name)
    {
        symbols[name] = true;
        TinyCode.Append("#define " + name);
    }
    bool IsDefined(string name)
    {
        return symbols.Contains(name);
    }
    
    bool DefineFunction(string returnType, string functionName)
    {
        if (functions.Contains(functionName))
        {
            // exists already
            Token token = TinyScanner.Current();
            Error(token.SourcePath, token.Line, "function with name '" + functionName + "' already exists");
            return false;
        }
        Function function;
        function.ReturnType = returnType;
        functions[functionName] = function;
        //PrintLn();
        //Print("func: " + returnType + " " + functionName);
        return true;
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
        
        <string, Variable> arguments = function.Arguments;
        if (arguments.Contains(argumentName))
        {
            // exists already
            Token token = TinyScanner.Current();
            Error(token.SourcePath, token.Line, "argument with name '" + argumentName + "' already exists");
            return false;
        }
        Variable argument;
        argument.Type = argumentType;
        argument.Offset = offset;
        argument.IsGlobal = false;
        arguments[argumentName] = argument;
        function.Arguments = arguments;
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
    
    bool DefineVariable(string variableType, string variableName, int offset, bool isGlobal)
    {
        uint level = variables.Count - 1;
        <string, Variable> scopeVariables = variables[level];
        if (scopeVariables.Contains(variableName))
        {
            // exists already
            Token token = TinyScanner.Current();
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
