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
    
    <string, <string,bool> > functionCalls;
    <string, bool>           functionsToCompile;
    <string>                 systemCalls;
    <string,bool>            staticCandidates; // functions that are candidates for static local variables
    <string,bool>            staticArguments;
    <string,bool>            requiresFrame;
    <string,uint>            staticArgumentsStartOffset;
    uint                     staticBudget;     // bytes still available for static locals after all global allocations
    
    string currentFunction;
    bool isNaked;
    string CurrentFunction { get { return currentFunction; } set { currentFunction = value; } }
    bool   CurrentIsNaked  { get { return isNaked; } set { isNaked = value; } }
    
    bool heapRequired;
    bool HeapRequired      { get { return heapRequired; } }
    
    uint BlockLevel { get { return blockLevel; } set { blockLevel = value; } }
    
    bool MustCompile(string name)
    {
        return functionsToCompile.Contains(name);
    }
    
    string VariableComment()
    {
        return "(level=" + (GetCurrentVariableLevel()).ToString() + ", levelBytes=" + (GetLevelBytes(GetCurrentVariableLevel())).ToString() + ")";
    }
    Reset()
    {
        functions.Clear();
        symbols.Clear();
        staticBudget = GlobalLimit - GlobalOffset;
        staticBudget = staticBudget - ArgumentReserve;
        //PrintLn();
        //PrintLn("GlobalLimit=" + (GlobalLimit).ToString() + ", GlobalOffset=" + (GlobalOffset).ToString() + ", ArgumentReserve=" + (ArgumentReserve).ToString() + ", staticBudget=" + staticBudget.ToString());
    }
    uint shortFall;
    bool RequestStaticLocal(string functionName, string variable, uint bytes)
    {
        bool success;
        if (staticCandidates.Contains(functionName))
        {
            if (staticCandidates[functionName])
            {
                if (staticBudget > bytes)
                {
                    staticBudget -= bytes;
                    success = true;                
                }
                else
                {
                    shortFall += bytes;
                    requiresFrame[functionName] = true;
                }
            }
        }
        /*
        if (!FirstPass && (staticBudget <= 16) && (shortFall < 8))
        {
            PrintLn();
            Print("functionName=" + functionName + ", variable=" + variable + ", staticBudget=" + staticBudget.ToString() + (success ? " +" : " -"));
            if (success)
            {
                Print(", Address=0x" + (GlobalOffset + GlobalStart).ToHexString(2));
            }
            else
            {
                Print(", shortFall=" + shortFall.ToString());
            }
        }
        */
        return success;
    }
    bool RequestStaticArgument(string functionName, ref uint offset)
    {
        bool success;
        if (staticArguments.Contains(functionName))
        {
            if (staticArguments[functionName])
            {
                offset = staticArgumentsStartOffset[functionName];
                return true;
            }
        }
        return success;
    }
    
    string GetArgumentString(string functionName)
    {
        string argumentString = "(";
        <string> argumentNames;
        <string,Variable> arguments;
        if (functions.Contains(functionName))
        {
            arguments = GetArguments(functionName, ref argumentNames);
            string comma;
            foreach (var argumentName in argumentNames)
            {
                Variable argument = arguments[argumentName];
                argumentString = argumentString + comma + argument.Type;
                comma = ", ";
            }
        }
        argumentString = argumentString + ")";
        return argumentString;
    }
        
    ExportFunctionCalls(<string, bool> calls, int currentDepth, int maxDepth)
    {
        uint count = 0;
        uint total = calls.Count;
        foreach (var kv in calls)
        {
            string name = kv.key;
            count++;
            
            bool recursion = !staticCandidates[name];
            
            string callCount;
            callCount = callCount + GetArgumentString(name);
            
            /*
            if (functionCalls.Contains(name))
            {
                uint itCalls = (functionCalls[name]).Count;
                if (itCalls > 0)
                {
                    callCount = " (" + itCalls.ToString() + ")";
                }
            }
            */
            if (currentDepth <= maxDepth)
            {
                if (count == total)
                {
                    PadOut("'-- " + name + (recursion ? " (recursion)" : "") + callCount, currentDepth);
                }
                else
                {
                    PadOut("|-- " + name + (recursion ? " (recursion)" : "") + callCount, currentDepth);
                }
            }
            if (recursion)
            {
                continue;
            }
            
            // Recursively export nested function calls if within the depth limit
            if (functionCalls.Contains(name))
            {
                ExportFunctionCalls(functionCalls[name], currentDepth + 1, maxDepth);
            }
       }
    }
    WalkFunctionCalls(<string, bool> calls, int currentDepth, <string> callStack)
    {
        foreach (var kv in calls)
        {
            string name = kv.key;
            
            if (!functionsToCompile.Contains(name))
            {
                functionsToCompile[name] = false;
            }
            
            // Check for recursion
            bool recursion = callStack.Contains(name);
            if (recursion)
            {
                staticCandidates[name] = false;
                continue;
            }
            callStack.Append(name); // Add the function to the call stack
            
            // Recursively export nested function calls if within the depth limit
            if (functionCalls.Contains(name))
            {
                WalkFunctionCalls(functionCalls[name], currentDepth + 1, callStack);
            }
            callStack.Remove(callStack.Count-1);
        }
    }
    
    < <string> > FindIndependentLists(<string, <string> > functionCalls)
    {
        < <string> > independentLists;
       <string> processedFunctions;
        loop
        {
            <string> currentList;
    
            // Find all functions that do not call any other functions
            foreach (var func in functionCalls)
            {
                if (processedFunctions.Contains(func.key)) { continue; }
                if ((func.value).Count == 0)
                {
                    currentList.Append(func.key);
                }
            }
    
            // If no more independent functions, break
            if (currentList.Count == 0)
            {
                break;
            }
    
            // Add the current list to the independent lists
            independentLists.Append(currentList);
    
            // Remove these functions from all call lists
            foreach (var func in functionCalls)
            {
                if (processedFunctions.Contains(func.key)) { continue; }
                <string> calls;
                foreach (var call in func.value)
                {
                    if (!currentList.Contains(call))
                   {
                        calls.Append(call);
                    }
                }
                functionCalls[func.key] = calls;
            }
            // Remove processed functions from the main list
            foreach (var independentFunc in currentList)
            {
                processedFunctions.Append(independentFunc);
            }
        }
    
        return independentLists;
    }
    
    
    AllocateStaticArguments()
    {
        uint argumentsUsed = 0;
        
        <string, <string> > functionCallsCopy;
        foreach (var kv in functionCalls)
        {
            if (functionsToCompile.Contains(kv.key))
            {
                <string> calls;
                foreach (var kv2 in kv.value)
                {
                    if (!systemCalls.Contains(kv2.key))
                    {
                        calls.Append(kv2.key);
                    }
                }
                functionCallsCopy[kv.key] = calls; 
            }
        }
        /*
        foreach (var kv in functionCallsCopy)
        {
            PrintLn();
            Print(kv.key + ":");
            foreach (var kv2 in kv.value)
            {
                Print(" " + kv2.key);
            }
        }
        PrintLn();
        */
        
        < <string> > independentLists = FindIndependentLists(functionCallsCopy);
        
        uint maxCandidates  = 0;
        foreach (var independents in independentLists)
        {
            if (independents.Count > maxCandidates)
            {
                maxCandidates = independents.Count;
            }
        }
        uint count = maxCandidates;
        loop
        {
            foreach (var independents in independentLists)
            {
                if (independents.Count == count)
                {
                    // staticCandidates
                    
                    <string> argumentNames;
                    <string,Variable> arguments;
                    uint maxArgumentBytes;
                    foreach(var functionName in independents) 
                    {
                        uint argumentBytes;
                        if (functions.Contains(functionName))
                        {
                            arguments = GetArguments(functionName, ref argumentNames);
                            foreach (var argument in arguments)
                            {
                                Variable variable = argument.value;
                                argumentBytes += (IsByteType(variable.Type) ? 1 : 2);
                            }
                        }
                        if (argumentBytes > maxArgumentBytes)
                        {
                            maxArgumentBytes = argumentBytes;
                        }
                    }
                
                    //PrintLn();
                    //Print(maxArgumentBytes.ToString() + ":");
                    foreach(var name in independents) 
                    { 
                        string args = GetArgumentString(name);
                        if (args != "()")
                        {
                            //Print(name, Colour.Ocean, Colour.Black);
                            //Print(args + " "); 
                            staticArguments[name]      = true;
                            staticArgumentsStartOffset[name] = argumentsUsed;
                        }
                    }
                    argumentsUsed += maxArgumentBytes;
                }
            }
            count--;
            if (count == 0) { break; }
        }
        ArgumentReserve = argumentsUsed;
        /*
        PrintLn();
        Print("GlobalLimit=" + (GlobalLimit).ToString());
        PrintLn();
        Print("GlobalStart=" + (GlobalStart).ToHexString(2));
        PrintLn();
        Print("ArgumentReserve=" + (ArgumentReserve).ToHexString(2));
        */
    }
    
    ExportFunctionTable()
    {
        int maxDepth = 2;
        functionsToCompile["main"] = false;
        string name;
        loop
        {
            name = "";
            foreach (var kv in functionsToCompile)
            {
                if (kv.value == false)
                {
                    name = kv.key;
                    break;
                }
            }
            if (name.Length == 0) { break; }
            
            functionsToCompile[name] = true;
            staticCandidates[name] = true; // candidate for static locals
            staticArgumentsStartOffset[name] = 0;
            staticArguments[name] = false;
            
            // Initialize the call stack with the current function
            <string> callStack;
            callStack.Append(name);
            if (functionCalls.Contains(name)) 
            {
                WalkFunctionCalls(functionCalls[name], 1, callStack);
            }
            else
            {
                // system call prototypes not included in functionCalls
                systemCalls.Append(name);
            }
        }
        AllocateStaticArguments();
        
        PadOut("/*", 0);
        PadOut("  Functions called:", 0);
        PadOut("", 0);
        foreach (var kv in functionsToCompile)
        {
            name = kv.key;
            if (!systemCalls.Contains(name))
            {
                string statics;
                if (staticArguments[name])
                {
                    statics = " 0x" + (staticArgumentsStartOffset[name]).ToHexString(2);
                }   
                PadOut(name +  GetArgumentString(name) + statics, 1);
                if (functionCalls.Contains(name))
                {
                    ExportFunctionCalls(functionCalls[name], 2, maxDepth);
                }
            }
        }
        
        PadOut("", 0);
        if (systemCalls.Count != 0)
        {
            PadOut(" System calls used:", 0);
            PadOut("", 0);
            foreach (var syscall in systemCalls)
            {
                switch (syscall)
                {
                    case "malloc":
                    case "free":
                    {
                        heapRequired = true;
                    }
                }
                PadOut(syscall, 1);
            }
            PadOut("", 0);
        }
        PadOut("*/", 0);
        PadOut("", 0);
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
            if (!variable.IsGlobal && (variable.Offset >= 0)) // not arguments)
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
                    TCCode.Call("free");
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
            if (FirstPass)
            {
                requiresFrame[functionName] = false;
            }
            currentFunction = functionName;
            success =  true;
        
            break;
        } // loop
        return success;
    }
    InitializeFunctionCalls(string name)
    {
        if (!functionCalls.Contains(name))
        {
            <string, bool> calls;
            functionCalls[name] = calls;
        }
    }
    AddFunctionCall(string name, string callName)
    {
        if (!functionCalls.Contains(name))
        {
            InitializeFunctionCalls(name); // typically 'main' for global allocations
        }
        <string, bool> calls = functionCalls[name];
        if (!calls.Contains(callName))
        {
            calls[callName] = true;
            functionCalls[name] = calls;
        }
    }
    UpdateArgumentOffsets(string functionName)
    {
        <uint, string> argumentIndex;
        <uint, byte>   argumentSize;
        
        uint staticOffset;
        bool staticArguments = RequestStaticArgument(functionName, ref staticOffset);
        
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
            if (staticArguments)
            {
                uint i = 0;
                loop
                {
                    string name = argumentIndex[i];
                    byte size   = argumentSize[i];
                    
                    SetStaticArgument(name, int(staticOffset));
                    staticOffset += size;
                    i++;
                    if (i == argumentIndex.Count) { break; }
                }
            }
            else
            {
                uint i = argumentIndex.Count - 1;
                int  offset = 0;
                loop
                {
                    string name = argumentIndex[i];
                    byte size   = argumentSize[i];
                    
                    offset -= size;
                    SetVariableOffset(name, offset);
                    if (!FirstPass)
                    {
                        requiresFrame[functionName] = true;
                    }
                    if (i == 0) { break; }
                    i--; 
                }
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
        string functionName = CurrentFunction;  
        if (FirstPass && (functionName != "<none>") && !isGlobal /*&& !requiresFrame[functionName]*/)
        {
            bool stackVariable;
            string memberType;
            if (level >= 2)
            {
                stackVariable = true;
            }
            else if (IsArrayType(variableType, ref memberType) && (variableType != memberType + "[]") && (variableType != "const " + memberType + "[]"))
            {
                stackVariable = true;
            }
            if (stackVariable)
            {
                requiresFrame[functionName] = true;
            }
            /*
            PrintLn();
            Print(functionName + " " + level.ToString() + ": " +variableType + " " + variableName);
            Print(stackVariable ? " STACK" : "", Colour.Red, Colour.Black);
            */
        }
        return true;
    }
    bool GetRequiresFrame(string functionName)
    {
        if (requiresFrame.Contains(functionName))
        {
            return requiresFrame[functionName];
        }
        return false;
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
    SetStaticArgument(string variableName, int offset)
    {
        uint level = variables.Count - 1;
        loop
        {
            <string, Variable> scopeVariables = variables[level];
            if (scopeVariables.Contains(variableName))
            {
                Variable variable = scopeVariables[variableName];
                variable.Offset = -(offset+1); // to differentiate from regular globals : flip sign and subtract 1 to revert
                variable.IsGlobal = true;
                scopeVariables[variableName] =  variable;
                variables[level] = scopeVariables;
                break;
            }
            if (level == 0) { break; }
            level--;
        }
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
                
                //Print(" S:" + variableName + " " + offset.ToString());
                break;
            }
            if (level == 0) { break; }
            level--;
        }
    }
}
