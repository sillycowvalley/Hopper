unit TinySymbols
{
    friend TinyStatement, TinyExpression, TinyCompile;
    
    uses "TinyToken"
    uses "TinyType"
    
    record Variable
    {
        string Type;
    }
    
    uint blockLevel;
    < <string, Variable> > variables;
    
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
    
    bool DefineVariable(string variableType, string variableName)
    {
        uint level = variables.Count -1;
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
        PrintLn(level.ToString() + ": " +variableType + " " + variableName);
        return true;
    }
    
    bool GetVariable(string variableName, ref string variableType)
    {
        uint level = variables.Count -1;
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
