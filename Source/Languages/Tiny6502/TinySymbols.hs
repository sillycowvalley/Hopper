unit TinySymbols
{
    friend TinyStatement, TinyExpression, TinyCompile;
    
    uses "TinyToken"
    uses "TinyType"
    
    record Variable
    {
        string Type;
    }
    record Function
    {
        string ReturnType;
        <string, Variable> Arguments;
    }
    
    uint blockLevel;
    < <string, Variable> > variables;
    <string, Function> functions;
    
    EnterBlock()
    {
        <string, Variable> level;
        variables.Append(level);
        blockLevel++;
    }
    LeaveBlock()
    {
        variables.Remove(variables.Count-1);
        blockLevel--;
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
    
    bool DefineArgument(string functionName, string argumentType, string argumentName)
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
    
    bool DefineVariable(string variableType, string variableName)
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
        
        scopeVariables[variableName] = newVariable;
        
        variables[level] = scopeVariables;   
        //PrintLn();
        //Print(level.ToString() + ": " +variableType + " " + variableName);
        return true;
    }
    
    bool GetVariable(string variableName, ref string variableType)
    {
        uint level = variables.Count - 1;
        loop
        {
            <string, Variable> scopeVariables = variables[level];
            if (scopeVariables.Contains(variableName))
            {
                Variable variable = scopeVariables[variableName];
                variableType  = variable.Type;
                return true;
            }
            if (level == 0) { break; }
            level--;
        }
        return false; // not found
    }
}
