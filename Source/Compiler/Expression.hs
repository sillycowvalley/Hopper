unit Expression
{
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    uses "/Source/Compiler/Tokens/SysCalls"
    
    uses "/Source/Compiler/CodeGen/Block"
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/CodeGen/CodeStream"
    
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Constant"
    
    PushTypeFromString(string typeName)
    {
        byte b = ToByte(typeName);
        CodeStream.AddInstructionPUSHI(b);
    }
    
    InitializeVariable(string variableType)
    {
        string name;
        // initialize an empty global or local and leave new stack slot at [top]
        if (Types.IsValueType(variableType) || Types.IsDelegate(variableType))
        {
            CodeStream.AddInstruction(Instruction.PUSHI0);
        }
        else if (Types.IsArray(variableType))
        {
            // push size
            uint sz = Types.GetArraySizeFromCollection(variableType);
            CodeStream.AddInstructionPUSHI(sz);               
            
            // push value type
            string valueType = Types.GetValueFromCollection(variableType);
            PushTypeFromString(valueType);

            name = "Array";            
        }
        else if (Types.IsList(variableType))
        {
            // push value type
            string valueType = Types.GetValueFromCollection(variableType);
            PushTypeFromString(valueType);
            name = "List";            
        }
        else if (Types.IsDictionary(variableType))
        {
            // push key type
            string keyType = Types.GetKeyFromCollection(variableType);
            PushTypeFromString(keyType);
            
            // push value type
            string valueType = Types.GetValueFromCollection(variableType);
            PushTypeFromString(valueType);
            name = "Dictionary";            
        }
        else if (  (variableType == "file")
                || (variableType == "directory")
                || (variableType == "float")
                || (variableType == "long")
                || (variableType == "string")
               )
        {
            name = "" + variableType[0];
            name = name.ToUpper();
            name = name + variableType.Substring(1);        
        }
        else if (variableType == "variant")
        {
            CodeStream.AddInstruction(Instruction.PUSHI0); // null placeholder
        }
        else
        {
            PrintLn(variableType);
            Parser.ErrorAtCurrent("not implemented");
            Die(0x0A);
        }
        if (name.Length > 0)
        {
            CodeStream.AddInstructionSysCall0(name, "New");
        }
    }
    CompileDynamicCast(string sourceType, string castToType)
    {
        switch (sourceType)
        {
            case "type":
            {
                switch (castToType)
                {
                    case "byte":
                    case "uint":
                    {
                        // ok
                    }
                    default:
                    {
                        Print("'" + sourceType + "' -> '" + castToType + "'");
                        Die(0x0A);
                    }
                }
            }
            case "byte":
            {
                switch (castToType)
                {
                    case "char":
                    case "uint":
                    case "int":
                    {
                        // ok
                    }
                    case "type":
                    {
                        Types.DynamicCastToType();
                    }
                    default:
                    {
                        Print("'" + sourceType + "' -> '" + castToType + "'");
                        Die(0x0A);
                    }
                }
            }
            case "char":
            {
                switch (castToType)
                {
                    case "byte":
                    case "uint":
                    case "int":
                    {
                        // ok
                    }
                    default:
                    {
                        Print("'" + sourceType + "' -> '" + castToType + "'");
                        Die(0x0A);
                    }
                }
            }
            case "uint":
            {
                switch (castToType)
                {
                    case "char":
                    case "byte":
                    {
                         Types.DynamicCastToByte();
                    }
                    case "int":
                    {
                         CodeStream.AddInstructionSysCall0("UInt", "ToInt");
                    }
                    case "long":
                    {
                         CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                    }
                    default:
                    {
                        //if (((castToType == "string") || (castToType == "array")) && DefineExists("H6502"))
                        //{
                        //    // simple 16 bit pointers
                        //}
                        //else
                        //{
                            // CODEGEN : runtime cast from sourceType to castToType
                            Print("'" + sourceType + "' -> '" + castToType + "'");
                            Die(0x0A);
                        //}
                    }
                }
            }
            case "int":
            {
                switch (castToType)
                {
                    case "char":
                    case "byte":
                    {
                         Types.DynamicCastToByte();
                    }
                    case "uint":
                    {
                         Types.DynamicCastIntToUInt();
                    }
                    case "float":
                    {
                         CodeStream.AddInstructionSysCall0("Int", "ToFloat");
                    }
                    case "long":
                    {
                         CodeStream.AddInstructionSysCall0("Int", "ToLong");
                    }
                    default:
                    {
                        // CODEGEN : runtime cast from sourceType to castToType
                        Print("'" + sourceType + "' -> '" + castToType + "'");
                        Die(0x0A);
                    }
                }
            }
            case "long":
            {
                switch (castToType)
                {
                    case "uint":
                    {
                        CodeStream.AddInstructionSysCall0("Long", "ToUInt");
                    }
                    case "int":
                    {
                        CodeStream.AddInstructionSysCall0("Long", "ToInt");
                    }
                    default:
                    {
                        // CODEGEN : runtime cast from sourceType to castToType
                        Print("'" + sourceType + "' -> '" + castToType + "'");
                        Die(0x0A);
                    }
                }
            }
            case "variant":
            {
                if (Types.IsSimpleType(castToType))
                {
                    RuntimeValueCast(castToType);
                }
                else
                {
                    // CODEGEN : runtime cast from sourceType to castToType
                    Print("'" + sourceType + "' -> '" + castToType + "'");
                    Die(0x0A);
                }    
            }
            default:
            {
                if (Types.IsFlags(sourceType) 
                 || Types.IsEnum(sourceType)
                 || (sourceType == "enum")
                 || (sourceType == "flags")
                   )
                {
                    switch (castToType)
                    {
                        case "uint":
                        {
                            // ok
                        }
                        case "char":
                        case "byte":
                        {
                            Types.DynamicCastToByte();
                        }
                        case "int":
                        {
                            // verify that the top/sign bit is not set
                            Types.DynamicCastIntToUInt();
                        }
                        case "long":
                        {
                            CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                        }
                        default:
                        {
                            Print("'" + sourceType + "' -> '" + castToType + "'");
                            Die(0x0A);
                        }
                    }
                }
                //else if (((sourceType == "string") || (sourceType == "array")|| (sourceType == "V[]")) 
                //         && (castToType == "uint")
                //         && DefineExists("H6502")
                //        )
                //{
                //    // simple 16 bit pointers
                //}
                else
                {
                    // CODEGEN : runtime cast from sourceType to castToType
                    Print("'" + sourceType + "' -> '" + castToType + "'");
                    Parser.ErrorAtCurrent("not implemented");
                    Die(0x0A);
                }
            }
        }
    }                
    string CompileReferenceArgument()
    {
        string argumentType = "";
        loop
        {
            if (!Parser.Check(HopperToken.Identifier) && !Parser.Check(HopperToken.DottedIdentifier))
            {
                Parser.ErrorAtCurrent("variable identifier expected");
                break;
            }
            <string,string> currentToken = Parser.CurrentToken;
            string variableName = currentToken["lexeme"];
            Parser.Advance(); // identifier
            string fullName;
            argumentType = Types.GetTypeString(variableName, true, ref fullName);
            if (Parser.HadError)
            {
                break;
            }
            if (Symbols.GlobalMemberExists(fullName))
            {
                uint globalAddress = Symbols.GetGlobalAddress(fullName);
                CodeStream.AddInstructionPUSHI(globalAddress);
                // push and add GP, the global "floor" for child processes
                CodeStream.AddInstruction(Instruction.PUSHGP);
                CodeStream.AddInstruction(Instruction.ADD);
            }   
            else
            {
                bool isRef;
                int offset = Block.GetOffset(variableName, ref isRef);
                if (isRef)
                {
                    // push the content of the variable (already an address) to the stack
                    if ((offset > -129) && (offset < 128))
                    {
                        byte operand =  CodeStream.IntToByte(offset);
                        CodeStream.AddInstruction(Instruction.PUSHLOCALB, operand);
                    }
                    else
                    {
                        uint operand =  CodeStream.IntToUInt(offset);  
                        CodeStream.AddInstruction(Instruction.PUSHLOCALW, operand);
                    }
                }
                else
                {
                    // push the stack address of the variable to the stack
                    if ((offset > -129) && (offset < 128))
                    {
                        byte operand =  CodeStream.IntToByte(offset);
                        CodeStream.AddInstruction(Instruction.PUSHSTACKADDRB, operand);
                    }
                    else
                    {
                        uint operand =  CodeStream.IntToUInt(offset);  
                        CodeStream.AddInstruction(Instruction.PUSHSTACKADDRW, operand);
                    }
                }
            }
            
            break;
        }
        return argumentType;
    }
    
    string CompileMethodCall(string methodName, string thisVariable)
    {
        string returnType;
        < <string > > arguments;
        bool isDelegateType = false;
        bool isLocal = false;
        loop
        {
            // consider delegates (variables, not methods)
            string qualifiedName;
            string variableType = Types.GetTypeString(methodName, false, ref qualifiedName);
            if (variableType.Length > 0)
            {
                if (Types.IsDelegate(variableType))
                {
                    isDelegateType = true;
                    string delegateType = Block.GetType(methodName, ref isLocal);
                }
            }
            if (!methodName.Contains('.'))
            {
                if (!isDelegateType)
                {
                    methodName = Types.QualifyMethodName(methodName);
                }
                else
                {
                    methodName = Types.QualifyDelegateName(methodName);
                }
            }
            
            if (methodName.EndsWith("_Get"))
            {
                if (thisVariable.Length > 0)
                {
                    
                    string qualifiedThis;
                    string thisType = Types.GetTypeString(thisVariable, true, ref qualifiedThis);
                    CodeStream.AddInstructionPushVariable(qualifiedThis); // push this
                    
                    uint fIndex;
                    if (!Symbols.GetFunctionIndex(methodName, ref fIndex))
                    {
                        Parser.ErrorAtCurrent("getter method not found");
                        break;
                    }
                    // get the "this" argument
                    <uint> overloads = GetFunctionOverloads(fIndex);
                    arguments = Symbols.GetOverloadArguments(overloads[0]);
                }
                break; // no other arguments
            }
            else if (methodName.EndsWith("_Set"))
            {
                // assume our "value" argument is on the stack already since it is an assignment
                uint fIndex;
                if (!Symbols.GetFunctionIndex(methodName, ref fIndex))
                {
                    Parser.ErrorAtCurrent("setter method not found");
                    break;
                }
                <uint> overloads = GetFunctionOverloads(fIndex);
                arguments = Symbols.GetOverloadArguments(overloads[0]);
                break;
            }
            uint fIndex;
            if (!isDelegateType && !GetFunctionIndex(methodName, ref fIndex) && (methodName.Length > 0))
            {
                char fChar = methodName[0];
                if (fChar.IsLower() && methodName.Contains('.') && (thisVariable.Length == 0))
                {
                    // this?
                    <string> parts = methodName.Split('.');
                    thisVariable = parts[0];
                    string qualifiedThis;
                    string typeString = Types.GetTypeString(thisVariable, false, ref qualifiedThis);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (Types.IsDictionary(typeString))
                    {
                        methodName = "Dictionary." + parts[1];
                    }
                    else if (Types.IsList(typeString))
                    {
                        methodName = "List." + parts[1];
                    }
                    else if (Types.IsArray(typeString))
                    {
                        methodName = "Array." + parts[1];    
                    }
                    else if (Types.IsSimpleType(typeString))
                    {
                        // System units that generally allow this.Xxxx(..)
                        char f = typeString[0];
                        f = f.ToUpper();
                        methodName = f + typeString.Substring(1) + "." + parts[1];
                    }
                }
            }
            
            Parser.Consume(HopperToken.LParen, '(');
            if (Parser.HadError)
            {
                break;
            }
        
            uint n = 0;
            if (thisVariable.Length > 0)
            {
                string qualifiedThis;
                string thisType = Types.GetTypeString(thisVariable, true, ref qualifiedThis);
                CodeStream.AddInstructionPushVariable(qualifiedThis); // push this
                <string> argument;
                argument.Append(""); // no "ref"
                argument.Append(thisType);
                argument.Append("a" + n.ToString());
                arguments.Append(argument);
                n++;
            }
            loop
            {
                if (Parser.HadError)
                {
                    break;
                }
                if (Parser.Check(HopperToken.RParen))
                {
                    Parser.Advance();
                    break;
                }
                <string> argument;
                bool isRef = false;
                if (Parser.Check(HopperToken.Keyword, "ref"))
                {
                    Parser.Advance();
                    argument.Append("ref");
                    isRef = true;
                }
                else
                {
                    argument.Append(""); // no "ref"
                }
                string argumentType;
                if (isRef)
                {
                    argumentType = CompileReferenceArgument();
                }
                else  
                {  
                    argumentType = CompileExpression("");
                }
                if (Parser.HadError)
                {
                    break;
                }
                argument.Append(argumentType);
                argument.Append("a" + n.ToString());
                arguments.Append(argument);
                n++;
                if (!Parser.Check(HopperToken.RParen))
                {
                    Parser.Consume(HopperToken.Comma, ',');
                }
                continue; // next argument
            }
            break;
        } // argumentLoop
        loop
        {
            if (isDelegateType)
            {
                uint iOverload;
                if (isLocal) // Tricksy! Method call with lowercase name.
                {
                    iOverload = Types.FindOverload(methodName, arguments, ref returnType);
                }
                else
                {
                    iOverload = Types.FindVisibleOverload(methodName, arguments, ref returnType);
                }
                if (Parser.HadError)
                {
                    break;
                }
                CodeStream.AddInstructionPushVariable(methodName);
                CodeStream.AddInstruction(Instruction.CALLREL);
                break;
            }
            if (Parser.HadError)
            {
                break;
            }
            uint iOverload = Types.FindVisibleOverload(methodName, arguments, ref returnType);
            if (Parser.HadError)
            {
                break;
            }
            // !isDelegate: already added where the delegate method was assigned to a variable
            Symbols.OverloadToCompile(iOverload); // CompileMethodCall(methodName): Setters, function calls, actual method calls
            Symbols.AddFunctionCall(iOverload);   // CompileMethodCall(methodName)
            if (Symbols.IsSysCall(iOverload))
            {
                byte iSysCall = Symbols.GetSysCallIndex(iOverload);
                byte iSysOverload = Symbols.GetSysCallOverload(iOverload);
                switch (iSysOverload)
                {
                    case 0:
                    {
                        if (!TryUserSysCall(methodName))
                        {
                            CodeStream.AddInstruction(Instruction.SYSCALL0, iSysCall);
                        }
                    }
                    case 1:
                    {
                        CodeStream.AddInstruction(Instruction.SYSCALL1, iSysCall);
                    }
                    default:
                    {
                        CodeStream.AddInstructionPUSHI(iSysOverload);
                        CodeStream.AddInstruction(Instruction.SYSCALL, iSysCall);
                    }
                }
            }
            else
            {
                if (DefineExists("H6502"))
                {
                    if (iOverload <= 0x3FFF)
                    {
                        uint beOverload = 0xC000 | iOverload;
                        CodeStream.AddInstruction(Instruction.CALLW, beOverload);
                    }
                    else
                    {
                        Parser.Error("H6502 has a limit of 16383 for function indices, (was '" + iOverload.ToString() + "')");
                    }
                }
                else if (iOverload < 256)
                {
                    CodeStream.AddInstruction(Instruction.CALLB, byte(iOverload));
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.CALLW, iOverload);
                }
            }
            break;
        } // loop
        return returnType;
    }
    
    string compileFunctionCall(string functionName, string expectedType, string thisVariable)
    {
        string actualType;
        loop
        {
            actualType = CompileMethodCall(functionName, thisVariable);
            break;
        } // loop
        return actualType;            
    }
    
    string compileConstant(string expectedType, string constantName)
    {
        string actualType;
        loop
        {
            string value = Symbols.GetConstantValue(constantName);
            long l;
            float f;
            if (Long.TryParse(value, ref l))
            {
                if (l < -32768)
                {
                    uint constantAddress = CodeStream.CreateLongConstant(l);
                    CodeStream.AddInstructionPUSHI(constantAddress);
                    CodeStream.AddInstructionSysCall0("Long", "NewFromConstant");
                    actualType = "long";
                }
                else if (l < 0)
                {
                    Die(0x0A); // CODEGEN : push value
                    actualType = "-int";
                }
                else if (l < 256)
                {
                    uint ui = uint(l);
                    CodeStream.AddInstructionPUSHI(ui);       
                    actualType = "byte";
                }
                else if (l < 32768)
                {
                    uint ui = uint(l);
                    CodeStream.AddInstructionPUSHI(ui);       
                    actualType = "+int";
                }
                else if (l <= 65535)
                {
                    uint ui = uint(l);
                    CodeStream.AddInstructionPUSHI(ui);       
                    actualType = "uint";
                }
                else
                {
                    uint constantAddress = CodeStream.CreateLongConstant(l);
                    CodeStream.AddInstructionPUSHI(constantAddress);
                    CodeStream.AddInstructionSysCall0("Long", "NewFromConstant");
                    actualType = "long";
                }            
            }
            else if (value == "true")
            {
                CodeStream.AddInstruction(Instruction.PUSHI1);
                actualType = "bool";
            }
            else if (value == "false")
            {
                CodeStream.AddInstruction(Instruction.PUSHI0);
                actualType = "bool";
            }
            else if (Float.TryParse(value, ref f))
            {
                uint constantAddress = CodeStream.CreateFloatConstant(f);
                CodeStream.AddInstructionPUSHI(constantAddress);
                CodeStream.AddInstructionSysCall0("Float", "NewFromConstant");
                actualType = "float";
            }
            else
            {
                AddString(value);
                actualType = "string";
            }
            break;
        }
        
        return actualType;
    }
    
    string compilePrimary(string expectedType)
    {
        string actualType;
        loop
        {
            <string,string> currentToken = Parser.CurrentToken;
            HopperToken tokenType = Token.GetType(currentToken);
            bool isDotted = false;
            if (tokenType == HopperToken.DottedIdentifier)
            {
                tokenType = HopperToken.Identifier;
                isDotted = true;
            }
            switch (tokenType)
            {
                case HopperToken.Bool:
                {
                    if (currentToken["lexeme"] == "true")
                    {
                        CodeStream.AddInstruction(Instruction.PUSHI1);
                    }
                    else if (currentToken["lexeme"] == "false")
                    {
                        CodeStream.AddInstruction(Instruction.PUSHI0);
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("invalid boolean token " + currentToken["type"]);
                    }
                    Parser.Advance();
                    actualType = "bool";
                }
                case HopperToken.Char:
                {
                    Parser.Advance();
                    char c = String.GetChar(currentToken["lexeme"], 0);
                    byte b = byte(c);
                    CodeStream.AddInstructionPUSHI(b);
                    actualType = "char";
                }
                case HopperToken.StringConstant:
                {
                    Parser.Advance();
                    actualType = "string";
                    AddString(currentToken["lexeme"]);
                }
                case HopperToken.Float:
                {
                    Parser.Advance();
                    float f;
                    if (!Float.TryParse(currentToken["lexeme"], ref f))
                    {
                        Parser.ErrorAtCurrent("invalid float token " + currentToken["type"]);
                    }
                    actualType = "float";
                    uint constantAddress = CodeStream.CreateFloatConstant(f);
                    CodeStream.AddInstructionPUSHI(constantAddress);
                    CodeStream.AddInstructionSysCall0("Float", "NewFromConstant");
                }
                case HopperToken.Integer:
                {
                    Parser.Advance();
                    long l;
                    if (!Long.TryParse(currentToken["lexeme"], ref l))
                    {
                        Parser.ErrorAtCurrent("invalid integer token " + currentToken["type"]);
                    }
                    if (l < -32768)
                    {
                        actualType = "long";
                        uint constantAddress = CodeStream.CreateLongConstant(l);
                        CodeStream.AddInstructionPUSHI(constantAddress);
                        CodeStream.AddInstructionSysCall0("Long", "NewFromConstant");
                    }
                    else if (l < 0)
                    {
                        actualType = "-int";
                        // CODEGEN TODO
                        Die(0x0A);
                    }
                    else if (l < 256)
                    {
                        actualType = "byte";
                        uint ui = uint(l);
                        CodeStream.AddInstructionPUSHI(ui);
                    }
                    else if (l < 32768)
                    {
                        actualType = "+int";
                        uint ui = uint(l);
                        CodeStream.AddInstruction(Instruction.PUSHIW, ui);
                    }
                    else if (l <= 65535)
                    {
                        actualType = "uint";
                        uint ui = uint(l);
                        CodeStream.AddInstruction(Instruction.PUSHIW, ui);
                    }
                    else
                    {
                        actualType = "long";
                        uint constantAddress = CodeStream.CreateLongConstant(l);
                        CodeStream.AddInstructionPUSHI(constantAddress);
                        CodeStream.AddInstructionSysCall0("Long", "NewFromConstant");
                    }
                }
                case HopperToken.Identifier:
                {
                    Parser.Advance();
                    string identifier = currentToken["lexeme"];
                    string qualifiedName;
                    string typeString = Types.GetTypeString(identifier, false, ref qualifiedName);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    
                    if (typeString.Length == 0)
                    {
                        if (Types.IsEnum(identifier) || Types.IsFlags(identifier))
                        {
                            if (Parser.Check(HopperToken.LParen))
                            {
                                Parser.Advance(); // (
                                string expressionType = CompileExpression("uint");
                                if (Parser.HadError)
                                {
                                    break;
                                }        
                                if (expressionType != "uint")
                                {
                                    if (!Types.AutomaticUpCastTop(expressionType, "uint"))
                                    {
                                        Parser.ErrorAtCurrent("cannot cast '" 
                                        + expressionType + "' to '" 
                                        + identifier + "'");
                                    }
                                }
                                // CODEGEN: confirm that uint is a valid member of the enum or flags
                                // "Instruction(byte)"
                                //PrintLn();
                                //Print("TODO: runtime check that this is a valid member: " + identifier + "(" + expressionType + ")");
                                
                                Parser.Consume(HopperToken.RParen, ')');
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                actualType = identifier;
                                break;
                            }
                        }        
                    }
                    
                    if (typeString.Length != 0)
                    {
                        if (Parser.Check(HopperToken.LBracket))
                        {
                            // array/list/dictionary GetItem
                            Parser.Advance(); // [
                            if (Types.IsArray(typeString)) 
                            {
                                uint bIndex;
                                if (typeString.IndexOf('[', ref bIndex))
                                {
                                }
                                // array GetItem
                                typeString = typeString.Substring(0, bIndex);
                                // push first argument: the array
                                CodeStream.AddInstructionPushVariable(qualifiedName);
                            
                                // second argument: the index
                                string indexType = CompileExpression("uint");
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                if (indexType != "uint")
                                {
                                    if (Types.AutomaticUpCastTop(indexType, "uint"))
                                    {
                                        indexType = "uint";
                                    }
                                }
                                if (indexType != "uint")
                                {
                                    Parser.ErrorAtCurrent("array index type invalid");
                                    break;
                                }
                                Parser.Consume(HopperToken.RBracket, ']');
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                CodeStream.AddInstructionSysCall0("Array", "GetItem");
                                actualType = typeString;
                            }
                            else if (typeString == "string")
                            {
                                // push first argument: the string
                                CodeStream.AddInstructionPushVariable(qualifiedName);
                            
                                // second argument: the index
                                string indexType = CompileExpression("uint");
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                if (indexType != "uint")
                                {
                                    if (Types.AutomaticUpCastTop(indexType, "uint"))
                                    {
                                        indexType = "uint";
                                    }
                                }
                                if (indexType != "uint")
                                {
                                    Parser.ErrorAtCurrent("string index type invalid");
                                    break;
                                }
                                Parser.Consume(HopperToken.RBracket, ']');
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                CodeStream.AddInstructionSysCall0("String", "GetChar");
                                actualType = "char";    
                            }
                            else if (Types.IsDictionary(typeString)) 
                            {
                                // dictionary GetItem
                                
                                // push first argument: the dictionary
                                CodeStream.AddInstructionPushVariable(qualifiedName);
                                
                                string keyType = Types.GetKeyFromCollection(typeString);
                                string valueType = Types.GetValueFromCollection(typeString);
                                
                                // second argument, they key
                                string actualKeyType = CompileExpression(keyType);
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                if (keyType != actualKeyType)
                                {
                                    if (keyType == "string")
                                    {
                                        Parser.ErrorAtCurrent("dictionary key type should be 'string', (not '" + actualKeyType + "')");    
                                        break;
                                    }
                                    if (Types.AutomaticUpCastTop(actualKeyType, keyType))
                                    {
                                        actualKeyType = "uint";
                                    }
                                    if (actualKeyType != "uint")
                                    {
                                        Parser.ErrorAtCurrent("dictionary key type should be '"+keyType+"', (not '" + actualKeyType + "')");
                                        break;
                                    }
                                }
                                Parser.Consume(HopperToken.RBracket, ']');
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                CodeStream.AddInstructionSysCall0("Dictionary", "Get");
                                actualType = valueType;
                            }
                            else if (Types.IsList(typeString)) 
                            {
                                // list GetItem
                                
                                // push first argument: the list
                                CodeStream.AddInstructionPushVariable(qualifiedName);
                                                               
                                string valueType = Types.GetValueFromCollection(typeString);
                                
                                // second argument: the index
                                string indexType = CompileExpression("uint");
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                if (indexType != "uint")
                                {
                                    if (Types.AutomaticUpCastTop(indexType, "uint"))
                                    {
                                        indexType = "uint";
                                    }
                                }
                                if (indexType != "uint")
                                {
                                    Parser.ErrorAtCurrent("list index type invalid");
                                    break;
                                }
                                Parser.Consume(HopperToken.RBracket, ']');
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                CodeStream.AddInstructionSysCall0("List", "GetItem");
                                actualType = valueType;
                            }
                            else
                            {
                                Parser.ErrorAtCurrent("identifier not array, list or dictionary type");
                            }
                            break;
                        } // array GetItem
                        
                        else
                        {
                            uint fdIndex;
                            if (Symbols.GetFunctionDelegateIndex(typeString, ref fdIndex))
                            {
                                if (Parser.Check(HopperToken.LParen))
                                {
                                    // delegate method call
                                    actualType = compileFunctionCall(identifier, expectedType, "");
                                    break;
                                }
                            }
                            // regular variable identifier
                            CodeStream.AddInstructionPushVariable(qualifiedName);
                            actualType = typeString;
                            break;
                        }                     
                        
                    } // (typeString.Length != 0)
                    
                    if (Symbols.ConstantExists(identifier))
                    {
                        actualType = compileConstant(expectedType, identifier);
                        break;
                    }
                    
                    // Resolve delegate methods: "ValidateFilePathExists"
                    // - if 'expectedType' is a delegate type, match the method 'identifier' arguments
                    //   and returntype
                    // - then push address of method on to stack
                    uint fdIndex;
                    if (Symbols.GetFunctionDelegateIndex(expectedType, ref fdIndex))
                    {
                        <uint> doverloads = Symbols.GetFunctionDelegateOverloads(fdIndex);
                        uint winner = 0;
                        uint wiOverload;
                        string returnType;
                        foreach (var doverload in doverloads)
                        {
                            < <string> > darguments = Symbols.GetDelegateArguments(doverload);
                            string dReturnType = Symbols.GetDelegateReturnType(doverload);
                            uint iOverload = Types.FindOverload(identifier, darguments, ref returnType);
                            if (Parser.HadError)
                            {
                                break;
                            }
                            if (returnType == dReturnType)
                            {
                                wiOverload = iOverload;
                                winner++;
                            }
                        }
                        if (Parser.HadError)
                        {
                            break;
                        }
                        if (winner > 1)
                        {
                            Parser.ErrorAtCurrent("ambiguous function matches for delegate type '" + expectedType + "'");
                            break;        
                        }
                        if (winner == 0)
                        {
                            Parser.ErrorAtCurrent("no function matches for delegate type '" + expectedType + "'");
                        }
                        Symbols.OverloadToCompile(wiOverload); // delegate pushed onto stack
                        // push function index to stack
                        CodeStream.AddInstructionPUSHI(wiOverload);
                        actualType = expectedType;
                        break;
                    }
                    
                    <string> nameSpaces = Symbols.GetNameSpaces();
                    if (isDotted)
                    {
                        // this?
                        <string> parts = identifier.Split('.');
                        string thisVariable = parts[0];
                        string qualifiedThis;
                        typeString = Types.GetTypeString(thisVariable, false, ref qualifiedThis);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        string functionName = parts[1];
                        uint cIndex;
                        if (((functionName == "key") || (functionName == "value")) &&
                            (thisVariable.Length > 0) && typeString.IndexOf(',', ref cIndex))
                        {
                            CodeStream.AddInstructionPushVariable(qualifiedThis);
                            if (functionName == "key")
                            {
                                CodeStream.AddInstructionSysCall0("Pair", "Key");
                                actualType = Types.GetKeyFromCollection(typeString);
                            }
                            else if (functionName == "value")
                            {
                                CodeStream.AddInstructionSysCall0("Pair", "Value");
                                actualType = Types.GetValueFromCollection(typeString);
                            }
                            break;
                        }
                        
                        
                        if (!Parser.Check(HopperToken.LParen))
                        {
                            // no '(' : getter?
                            functionName = functionName + "_Get";
                        }
                        
                        if (typeString.Length > 0)
                        {
                            // "this" case
                            if (Types.IsDictionary(typeString))
                            {
                                typeString = "Dictionary";
                            }
                            else if (Types.IsList(typeString))
                            {
                                typeString = "List";
                            }
                            else if (Types.IsArray(typeString))
                            {
                                typeString = "Array";
                            }
                            else if (Types.IsEnum(typeString) || Types.IsFlags(typeString))
                            {
                                // like key.ToChar()
                                uint iDot;
                                if (identifier.IndexOf('.', ref iDot))
                                {
                                    thisVariable = identifier.Substring(0, iDot);
                                    functionName = identifier.Substring(iDot+1);
                                    qualifiedName = Types.QualifyMethodName(functionName);
                                }
                            }                           
                            if (qualifiedName.Length == 0)
                            {
                                foreach (var nameSpace in nameSpaces)
                                {
                                    if (nameSpace == typeString)
                                    {
                                        qualifiedName = nameSpace + "." + functionName;
                                        break;
                                    }
                                    string lowerNameSpace = nameSpace.ToLower();
                                    if (lowerNameSpace == typeString)
                                    {
                                        qualifiedName = nameSpace + "." + functionName;
                                        break;
                                    }
                                }
                            }
                            if (qualifiedName.Length > 0)
                            {
                                actualType = compileFunctionCall(qualifiedName, expectedType, thisVariable);
                                break;
                            }
                        } // this case
                        
                        else
                        {
                            // enums and flags
                            uint fDot;
                            uint lDot;
                            bool oneDot = false;
                            if (identifier.IndexOf('.', ref fDot))
                            {
                                oneDot = true;
                                if (identifier.LastIndexOf('.', ref lDot))
                                {
                                    if (fDot != lDot) // 2 dots
                                    {
                                        oneDot = false;
                                        // 2 dot enums like "Keyboard.Key.Delete"
                                        string typeName = identifier.Substring(0, lDot);
                                        uint value;
                                        if (Symbols.FindEnum(typeName, identifier, ref value))
                                        {
                                            CodeStream.AddInstructionPUSHI(value);
                                            actualType = "enum";    
                                            break;
                                        }
                                        if (Symbols.FindFlags(typeName, identifier, ref value))
                                        {
                                            CodeStream.AddInstructionPUSHI(value);
                                            actualType = "flags";    
                                            break;
                                        }             
                                    }
                                }
                                if (oneDot)
                                {
                                    actualType = "";
                                    foreach (var nameSpace in nameSpaces)
                                    {
                                        string typeName = nameSpace + "." + identifier.Substring(0, fDot);
                                        string memberName = nameSpace + "." + identifier;
                                        uint value;
                                        if (Symbols.FindEnum(typeName, memberName, ref value))
                                        {
                                            CodeStream.AddInstructionPUSHI(value);
                                            actualType = "enum";    
                                            break;
                                        }
                                        if (Symbols.FindFlags(typeName, memberName, ref value))
                                        {
                                            CodeStream.AddInstructionPUSHI(value);
                                            actualType = "flags";    
                                            break;
                                        }
                                    }    
                                    if (actualType.Length > 0)
                                    {
                                        break;
                                    }
                                }
                            }
                            typeString = parts[0];
                            qualifiedName = "";
                            foreach (var nameSpace in nameSpaces)
                            {
                                if (nameSpace == typeString)
                                {
                                    qualifiedName = nameSpace + "." + functionName;
                                    break;
                                }
                            }
                            if (qualifiedName.Length > 0)
                            {
                                actualType = compileFunctionCall(qualifiedName, expectedType, "");
                                break;
                            }     
                        }
                    } // isDotted
                    else
                    {
                        // not isDotted
                        if (!Parser.Check(HopperToken.LParen))
                        {
                            // no '(' : getter?
                            identifier = identifier + "_Get";
                        }
                        string qualifiedIdentifier = Types.QualifyMethodName(identifier);
                        bool success = false;
                        uint fIndex;
                        if (GetFunctionIndex(qualifiedIdentifier, ref fIndex))
                        {
                            actualType = compileFunctionCall(qualifiedIdentifier, expectedType, "");
                            success = !Parser.HadError;
                            break;
                        }
                    }
                    Parser.ErrorAtCurrent("'" + currentToken["lexeme"] + "' is not defined");
                }
                case HopperToken.Keyword:
                {
                    string typeName = currentToken["lexeme"];
                    bool doTypeOf;
                    bool typeId = Token.IsTypeKeyword(typeName);
                    Parser.Advance();
                    if (!IsSimpleType(typeName))
                    {
                        if (Parser.Check(HopperToken.LParen))
                        {
                           if (typeName == "typeof")
                            {
                                // built-in
                                doTypeOf = true;
                            }
                            else
                            {
                                Parser.ErrorAt(currentToken, "compilePrimary not implemented");
                            }            
                        }
                        else
                        {
                            if (typeId)
                            {
                                // stand alone type keyword
                            }
                            else
                            {
                                Parser.ErrorAt(currentToken, "compilePrimary not implemented");
                            }
                        }
                    }
                    
                    string sourceType;
                    if (Parser.Check(HopperToken.LParen, "'(' expected"))
                    {
                        if (!IsValueType(typeName))
                        {
                            if (!doTypeOf)
                            {
                                if ((typeName == "long") || (typeName == "float"))
                                {
                                    // CompileDynamicCast(..) deals with these
                                }
                                //else if (((typeName == "string") || (typeName == "array")) && DefineExists("H6502"))
                                //{
                                //    // simple 16 bit pointers
                                //}
                                else
                                {
                                    Parser.ErrorAtCurrent("'" + typeName + "' is an invalid type for simple cast");
                                }
                            }
                        }
                        Parser.Advance(); // (
                        sourceType = CompileExpression(expectedType);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        if (!Parser.Check(HopperToken.RParen, "')' expected"))
                        {
                            break;
                        }
                        Parser.Advance(); // )
                        typeId = false;
                    }
                    else if (typeId)
                    {
                        PushTypeFromString(typeName);
                        actualType = "type";
                    }
                    if (typeId)
                    {
                        // done already
                    }
                    else if (doTypeOf)
                    {
                        // push the type of what's on the stack
                        CodeStream.AddInstructionSysCall0("Types", "BoxTypeOf");
                        actualType = "type";
                    }
                    else if (typeName != sourceType)
                    {
                        if (Types.AutomaticUpCastTop(sourceType, typeName))
                        {
                            // trivial   
                        }
                        else
                        {
                            // runtime cast from sourceType to castToType
                            CompileDynamicCast(sourceType, typeName);
                        }
                        actualType = typeName; // simple cast
                    }
                    else
                    {
                        actualType = typeName; // nop
                    }
                    if (Parser.HadError)
                    {
                        break;
                    }
                }
                case HopperToken.LParen:
                {
                    Parser.Advance(); // (
                    actualType = CompileExpression(expectedType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    Parser.Consume(HopperToken.RParen, ')');
                }
                default:
                {
                    Parser.ErrorAtCurrent("compilePrimary not implemented for " + currentToken["type"]);
                }
            }
            break;
        } // loop
        return actualType;            
    }
    
    string compileUnary(string expectedType)
    {
        string actualType;
        loop
        {
            <string,string> operationToken = Parser.CurrentToken;
            HopperToken operation = HopperToken.Undefined;
            if (Parser.Check(HopperToken.Subtract))
            {
                Parser.Advance();
                operation = HopperToken.Subtract;
            }
            else if (Parser.Check(HopperToken.Add))
            {
                Parser.Advance();
                operation = HopperToken.Add;
            }
            else if (Parser.Check(HopperToken.BooleanNot))
            {
                Parser.Advance();
                operation = HopperToken.BooleanNot;
            }
            else if (Parser.Check(HopperToken.BitNot))
            {
                Parser.Advance();
                operation = HopperToken.BitNot;
            }
            actualType = compilePrimary(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            if ((operation == HopperToken.Subtract) || (operation == HopperToken.Add))
            {
                if (!Types.IsNumericType(actualType))
                {
                    Parser.ErrorAtCurrent("unary + and - only legal for numeric types");
                    break;
                }
                if (operation == HopperToken.Subtract) 
                {
                    switch (actualType)
                    {
                        case "long":
                        {
                            CodeStream.AddInstructionSysCall0("Long", "Negate");
                        }
                        case "float":
                        {
                            CodeStream.AddInstruction(Instruction.PUSHI0);
                            CodeStream.AddInstructionSysCall0("UInt", "ToFloat");
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstructionSysCall0("Float", "Sub");
                        }
                        case "byte":
                        {
                            CodeStream.AddInstruction(Instruction.PUSHI0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "-int";
                        }
                        case "int":
                        {
                            CodeStream.AddInstruction(Instruction.PUSHI0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "int";
                        }
                        case "-int":
                        {
                            CodeStream.AddInstruction(Instruction.PUSHI0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "+int";
                        }
                        case "+int":
                        {
                            CodeStream.AddInstruction(Instruction.PUSHI0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "int";
                        }
                        case "uint":
                        {
                            CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                            CodeStream.AddInstructionSysCall0("Long", "Negate");
                            actualType = "long";
                        }
                        default:
                        {
                            Parser.ErrorAtCurrent("unary + and - only legal for numeric types, (not '" + actualType + "')");
                        }
                    }
                }
            }
            else if (operation == HopperToken.BooleanNot)
            {
                if (actualType != "bool")
                {
                    Parser.ErrorAt(operationToken, "type mismatch, 'bool' expected");
                    break;
                }
                CodeStream.AddInstruction(Instruction.BOOLNOT);
            }
            else if (operation == HopperToken.BitNot)
            {
                if (Types.IsFlags(actualType))
                {
                    // ok
                }
                else if ((actualType != "byte") 
                      && (actualType != "uint") 
                      && (actualType != "+int")
                      && (actualType != "flags")
                        )
                {
                    Parser.ErrorAtCurrent("bitwise operations only legal for 'uint', 'flags' and 'byte', (not '" + actualType + "')");     
                    break;
                }
                CodeStream.AddInstruction(Instruction.BITNOT);
            }
            break;
        } // loop
        return actualType;            
    }
    
    string compileFactor(string expectedType)
    {
        string actualType;
        loop
        {
            actualType = compileUnary(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.Multiply) || Parser.Check(HopperToken.Divide) || Parser.Check(HopperToken.Modulus))
                {
                    <string,string> operationToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(operationToken);
                    
                    if (!Types.IsNumericType(actualType))
                    {
                        Parser.ErrorAtCurrent("multiply, divide and modulus operations only legal for numeric types");
                        break;
                    }
                    Advance(); // *, /, %
                    string rightType = compileUnary(expectedType);
                    if (actualType != rightType)
                    {
                        if (Types.AutomaticUpCastTop(rightType, actualType))
                        {
                            rightType = actualType;    
                        }
                        else if (Types.AutomaticUpCastNext(actualType, rightType))
                        {
                            actualType = rightType;    
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "')");
                        }
                    }
                    if (Parser.HadError)
                    {
                        break;
                    }
                    switch (operation)
                    {
                        case HopperToken.Multiply:
                        {
                            switch (actualType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "Mul");
                                }
                                case "float":
                                {
                                    CodeStream.AddInstructionSysCall0("Float", "Mul");
                                }
                                default:
                                {
                                    if (Types.IsSignedIntType(actualType))
                                    {
                                        CodeStream.AddInstruction(Instruction.MULI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.MUL);
                                    }
                                }
                            }
                        }
                        case HopperToken.Divide:
                        {
                            switch (actualType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "Div");
                                }
                                case "float":
                                {
                                    CodeStream.AddInstructionSysCall0("Float", "Div");
                                }
                                default:
                                {
                                    if (Types.IsSignedIntType(actualType))
                                    {
                                        CodeStream.AddInstruction(Instruction.DIVI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.DIV);
                                    }
                                }
                            }
                        }
                        case HopperToken.Modulus:
                        {
                            switch (actualType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "Mod");
                                }
                                case "float":
                                {
                                    Parser.ErrorAtCurrent("modulus operation not legal for 'float'");
                                    break;
                                }
                                default:
                                {
                                    if (Types.IsSignedIntType(actualType))
                                    {
                                        CodeStream.AddInstruction(Instruction.MODI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.MOD);
                                    }
                                }
                            }
                        }
                    }                   
                    continue;    
                }
                break;
            } // loop         
            break;
        } // loop
        return actualType;                        
    }
    
    string compileShift(string expectedType)
    {
        string actualType;
        loop
        {
            actualType = compileFactor(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                // TODO: ditch ShiftRight and ShiftLeft in favour of GT GT and LT LT
                if (Parser.Check(HopperToken.ShiftRight) || Parser.Check(HopperToken.ShiftLeft))
                {
                    if ((actualType != "byte") 
                     && (actualType != "uint")  
                     && (actualType != "int")
                     && (actualType != "+int")
                     && (actualType != "-int")
                     )
                    {
                        Parser.ErrorAtCurrent("shift operations only legal for integral types");
                        break;
                    }
                    
                    <string,string> operationToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(operationToken);
                    
                    Advance(); // <<, >>
                    // only constants supported for now?!
                    string shiftValue = ParseConstantExpression("byte"); // constant expression
                    if (Parser.HadError)
                    {
                        break;
                    }
                    uint iShift;
                    if (UInt.TryParse(shiftValue, ref iShift))
                    {
                    }
                    if ((iShift < 0) || (iShift > 16))
                    {
                        Parser.ErrorAtCurrent("shift operand must be between 0 and 16");
                    }
                    CodeStream.AddInstructionPUSHI(byte(iShift));
                    if (operation == HopperToken.ShiftRight)
                    {
                        CodeStream.AddInstruction(Instruction.BITSHR);
                    }
                    else
                    {
                        // HopperToken.ShiftLeft
                        CodeStream.AddInstruction(Instruction.BITSHL);
                    } 
                }
                break;
            }
            break;
        } // loop
        return actualType;            
    }
    
    string compileTerm(string expectedType)
    {
        string actualType;
        loop
        {
            actualType = compileShift(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.Add) || Parser.Check(HopperToken.Subtract))
                {
                    <string,string> operationToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(operationToken);
                    
                    if (!Types.IsNumericType(actualType))
                    {
                        if ((operation == HopperToken.Add) && ((actualType == "string") || (actualType == "char")))
                        {
                            // ok
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("add and subtract operations only legal for numeric types");
                            break;
                        }
                    }
                    Advance(); // +, -
                    string rightType = compileShift(expectedType);
                    if (actualType != rightType)
                    {
                        if ((operation == HopperToken.Add) && ((rightType == "string") || (rightType == "char")))
                        {
                            // ok
                        }
                        else if (Types.AutomaticUpCastTop(rightType, actualType))
                        {
                            rightType = actualType;    
                        }
                        else if (Types.AutomaticUpCastNext(actualType, rightType))
                        {
                            actualType = rightType;    
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "')");
                        }
                    }
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (operation == HopperToken.Add)
                    {
                        switch (actualType)
                        {
                            case "string":
                            {
                                byte iSysCall;
                                if (!TryParseSysCall("String.Append", ref iSysCall))
                                {
                                    Die(3); // key not found
                                }
                                if (rightType == "char")
                                {
                                    // string Append(string,char)
                                    //PrintLn("B:String.Append " + iSysCall.ToString());
                                    CodeStream.AddInstruction(Instruction.SYSCALL1, iSysCall);
                                    
                                }
                                else
                                {
                                    // string Append(string,string)
                                    if (!TryUserSysCall("String.Append"))
                                    {
                                        CodeStream.AddInstruction(Instruction.SYSCALL0, iSysCall);
                                    }
                                }
                            }
                            case "char":
                            {
                                if (rightType == "string")
                                {
                                    CodeStream.AddInstruction(Instruction.SWAP);
                                    CodeStream.AddInstruction(Instruction.PUSHI0);
                                    CodeStream.AddInstruction(Instruction.SWAP);
                                    //PrintLn("D:String.InsertChar");
                                    CodeStream.AddInstructionSysCall0("String", "InsertChar");
                                }
                                else
                                {
                                    Die(0x0A);
                                }
                                actualType = "string";
                            }
                            case "long":
                            {
                                CodeStream.AddInstructionSysCall0("Long", "Add");
                            }
                            case "float":
                            {
                                CodeStream.AddInstructionSysCall0("Float", "Add");
                            }
                            default:
                            {
                                if (Types.IsSignedIntType(actualType))
                                {
                                    CodeStream.AddInstruction(Instruction.ADDI);
                                }
                                else
                                {
                                    CodeStream.AddInstruction(Instruction.ADD);
                                }
                            }
                        }
                    }
                    else
                    {
                        // HopperToken.Subtract:
                        switch (actualType)
                        {
                            case "long":
                            {
                                CodeStream.AddInstructionSysCall0("Long", "Sub");
                            }
                            case "float":
                            {
                                CodeStream.AddInstructionSysCall0("Float", "Sub");
                            }
                            default:
                            {
                                if (Types.IsSignedIntType(actualType))
                                {
                                    CodeStream.AddInstruction(Instruction.SUBI);
                                }
                                else
                                {
                                    CodeStream.AddInstruction(Instruction.SUB);
                                }
                            }
                        }
                    }                   
                    continue;        
                }
                break;
            } // loop     
            break;
        } // loop
        return actualType;            
    }
    string compileComparison(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileTerm(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            if (Parser.Check(HopperToken.LT) || Parser.Check(HopperToken.LE) || Parser.Check(HopperToken.GT) || Parser.Check(HopperToken.GE))
            {
                actualType = "bool";
                if (!Types.IsOrdinalType(leftType))
                {
                    Parser.ErrorAtCurrent("comparison operations only legal for ordinal types");
                    break;
                }
                <string,string> comparisonToken = Parser.CurrentToken;
                HopperToken operation = Token.GetType(comparisonToken);
                Advance(); // <, <=, >, >=
                string rightType = compileTerm(expectedType);
                if (leftType != rightType)
                {
                    if (Types.AutomaticUpCastTop(rightType, leftType))
                    {
                        rightType = leftType;
                    }
                    else if (Types.AutomaticUpCastNext(leftType, rightType))
                    {
                        leftType = rightType;
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("type mismatch, '" + leftType + "' expected (was '" + rightType + "')");
                    }
                }
                if (Parser.HadError)
                {
                    break;
                }      
                if (leftType == "string")
                {
                    if (rightType != "string")
                    {
                       Parser.ErrorAtCurrent("type mismatch, '" + leftType + "' expected (was '" + rightType + "')");
                    }
                    
                    CodeStream.AddInstructionSysCall0("String", "Compare"); //-1|0|1 -> [top]
                    //  1 : >  || >= -> true
                    // -1 : <  || <= -> true
                    //  0 : >= || <= -> true
                    
                    // HopperToken.GT
                    // 1 -> true
                    // else false       
                    Instruction pushInstruction = Instruction.PUSHI1;
                    Instruction compareInstruction = Instruction.EQ;
                    if (operation == HopperToken.LT)
                    {
                        // -1 -> true
                        // else false
                        pushInstruction = Instruction.PUSHIM1;
                    }
                    else if (operation == HopperToken.GE)
                    {
                        //  0 -> true
                        //  1 -> true
                        // -1 -> false
                        pushInstruction    = Instruction.PUSHIM1;
                        compareInstruction = Instruction.NE;
                    }
                    else if (operation == HopperToken.LE)
                    {
                        //  0 -> true
                        // -1 -> true
                        //  1 -> false
                        compareInstruction = Instruction.NE;
                    }
                    CodeStream.AddInstruction(pushInstruction);
                    CodeStream.AddInstruction(compareInstruction);
                } // string
                else
                {
                    bool signedInt = Types.IsSignedIntType(leftType);
                    switch (operation)
                    {
                        case HopperToken.LT:
                        {
                            switch (leftType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "LT");
                                }
                                case "float":
                                {
                                    CodeStream.AddInstructionSysCall0("Float", "LT");
                                }
                                default:
                                {
                                    if (signedInt)
                                    {
                                        CodeStream.AddInstruction(Instruction.LTI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.LT);
                                    }
                                }
                            }
                        }
                        case HopperToken.LE:
                        {
                            switch (leftType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "LE");
                                }
                                case "float":
                                {
                                    CodeStream.AddInstructionSysCall0("Float", "LE");
                                }
                                default:
                                {
                                    if (signedInt)
                                    {
                                        CodeStream.AddInstruction(Instruction.LEI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.LE);
                                    }
                                }
                            }
                        }
                        case HopperToken.GT:
                        {
                            switch (leftType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "GT");
                                }
                                case "float":
                                {
                                    CodeStream.AddInstructionSysCall0("Float", "GT");
                                }
                                default:
                                {
                                    if (signedInt)
                                    {
                                        CodeStream.AddInstruction(Instruction.GTI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.GT);
                                    }
                                }
                            }
                        }
                        case HopperToken.GE:
                        {
                            switch (leftType)
                            {
                                case "long":
                                {
                                    CodeStream.AddInstructionSysCall0("Long", "GE");
                                }
                                case "float":
                                {
                                    CodeStream.AddInstructionSysCall0("Float", "GE");
                                }
                                default:
                                {
                                    if (signedInt)
                                    {
                                        CodeStream.AddInstruction(Instruction.GEI);
                                    }
                                    else
                                    {
                                        CodeStream.AddInstruction(Instruction.GE);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            break;
        } // loop
        return actualType;
    }
    
    string compileBitAnd(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileComparison(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            loop
            {
                if (Parser.Check(HopperToken.BitAnd))           
                {
                    if (!IsBitwiseType(leftType))
                    {
                        Parser.ErrorAtCurrent("bitwise operations only legal for 'uint', 'int', 'flags' and 'byte', (not '" + leftType + "')");
                        break;
                    }
                    Parser.Advance(); // &
                    string rightType = compileComparison(expectedType);
                    if (actualType != rightType)
                    {
                        if (Types.AutomaticUpCastTop(rightType, actualType))
                        {
                            rightType = actualType;    
                        }
                        else if (Types.AutomaticUpCastNext(actualType, rightType))
                        {
                            actualType = rightType;    
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "')");
                        }
                    }            
                    if (Parser.HadError)
                    {
                        break;
                    }       
                    CodeStream.AddInstruction(Instruction.BITAND);
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return actualType;
    }
    
#ifdef UNUSED
    string compileBitXor(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileBitAnd(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            
            if (Parser.Check(HopperToken.BitXor))           
            {
                Parser.ErrorAtCurrent("compileBitXor not implemented");
                Die(0x0A);
            }
            break;
        }
        return actualType;
    }
#endif
    string compileBitOr(string expectedType)
    {
        string actualType;
        loop
        {
            //string leftType = compileBitXor(expectedType);
            string leftType = compileBitAnd(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            loop
            {
                if (Parser.Check(HopperToken.BitOr))           
                {
                    if (!IsBitwiseType(leftType))
                    {
                        Parser.ErrorAtCurrent("bitwise operations only legal for 'uint', 'int', 'flags' and 'byte', (not '" + leftType + "')");
                        break;
                    }
                    Parser.Advance(); // |
                    //string rightType = compileBitXor(expectedType);
                    string rightType = compileBitAnd(expectedType);
                    if (actualType != rightType)
                    {
                        if (Types.AutomaticUpCastTop(rightType, actualType))
                        {
                            rightType = actualType;    
                        }
                        else if (Types.AutomaticUpCastNext(actualType, rightType))
                        {
                            actualType = rightType;    
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "')");
                        }
                    }            
                    if (Parser.HadError)
                    {
                        break;
                    }       
                    CodeStream.AddInstruction(Instruction.BITOR);
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return actualType;
    }
    
    string compileBooleanAnd(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileBitOr(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            <uint> jumpShortCircuits;
            loop
            {
                if (Parser.Check(HopperToken.BooleanAnd))
                {
                    if (leftType != "bool")
                    {
                        Parser.ErrorAtCurrent("boolean expression expected");
                        break;
                    }
                    
                    // short circuit check:                 
                    CodeStream.AddInstruction(Instruction.DUP, byte(0));
                    jumpShortCircuits.Append(CodeStream.NextAddress);
                    CodeStream.AddInstructionJump(Instruction.JZW);
                    
                    Parser.Advance(); // &&
                    string rightType = compileBitOr(expectedType);
                    if (rightType != "bool")
                    {
                        Parser.ErrorAtCurrent("boolean expression expected");
                    }     
                    CodeStream.AddInstruction(Instruction.BOOLAND);

                    continue;                        
                }
// shortCircuit:
                uint pastAddress = CodeStream.NextAddress;
                foreach (var jumpShortCircuit in jumpShortCircuits)               
                {
                    CodeStream.PatchJump(jumpShortCircuit, pastAddress);
                }
                break;
            } // loop
            break;
        } // loop
        return actualType;    
    }
    
    string compileBooleanOr(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileBooleanAnd(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            <uint> jumpShortCircuits;
            loop
            {
                if (Parser.Check(HopperToken.BooleanOr))
                {
                    if (leftType != "bool")
                    {
                        Parser.ErrorAtCurrent("boolean expression expected");
                        break;
                    }
                    
                    // short circuit check:                 
                    CodeStream.AddInstruction(Instruction.DUP, byte(0));
                    jumpShortCircuits.Append(CodeStream.NextAddress);
                    CodeStream.AddInstructionJump(Instruction.JNZW);
                                            
                    Parser.Advance(); // ||
                    string rightType = compileBooleanAnd(expectedType);
                    if (rightType != "bool")
                    {
                        Parser.ErrorAtCurrent("boolean expression expected");
                    }
                    CodeStream.AddInstruction(Instruction.BOOLOR);
                    continue;    
                }
// shortCircuit:                
                uint pastAddress = CodeStream.NextAddress;
                foreach (var jumpShortCircuit in jumpShortCircuits)               
                {
                    CodeStream.PatchJump(jumpShortCircuit, pastAddress);
                }
                break;
            }
            break;
        } // loop
        return actualType;
    }
    
    string compileEquality(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileBooleanOr(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            actualType = leftType;
            loop
            {
                if (Parser.Check(HopperToken.EQ) || Parser.Check(HopperToken.NE))
                {
                    if (IsEqualityType(leftType))
                    {
                        // ok
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("equality operation not only legal for '" + leftType + "'");
                        break;
                    }
                    <string,string> equalityToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(equalityToken);
                    actualType = "bool"; // result
                    Parser.Advance(); // != or ==
                    string rightType = compileBooleanOr(expectedType);
                    if (leftType != rightType)
                    {
                        if (Types.AutomaticUpCastTop(rightType, leftType))
                        {
                            rightType = leftType;    
                        }
                        else if (Types.AutomaticUpCastNext(leftType, rightType))
                        {
                            leftType = rightType;    
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("type mismatch, '" + leftType + "' expected (was '" + rightType + "')");
                        }
                    }
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (operation == HopperToken.EQ)
                    {
                        switch (leftType)
                        {
                            case "long":
                            {
                                CodeStream.AddInstructionSysCall0("Long", "EQ");
                            }
                            case "float":
                            {
                                CodeStream.AddInstructionSysCall0("Float", "EQ");
                            }
                            case "string":
                            {
                                CodeStream.AddInstructionSysCall0("String", "Compare");
                                CodeStream.AddInstruction(Instruction.PUSHI0);
                                CodeStream.AddInstruction(Instruction.EQ);
                            }
                            default:
                            {
                                // bool, byte, char, enum, flags, int, uint, type
                                CodeStream.AddInstruction(Instruction.EQ);
                            }
                        }
                    }
                    else
                    {
                        // HopperToken.NE
                        switch (leftType)
                        {
                            case "long":
                            {
                                CodeStream.AddInstructionSysCall0("Long", "EQ");
                                CodeStream.AddInstruction(Instruction.BOOLNOT);
                            }
                            case "float":
                            {
                                CodeStream.AddInstructionSysCall0("Float", "EQ");
                                CodeStream.AddInstruction(Instruction.BOOLNOT);
                            }
                            case "string":
                            {
                                CodeStream.AddInstructionSysCall0("String", "Compare");
                                CodeStream.AddInstruction(Instruction.PUSHI0);
                                CodeStream.AddInstruction(Instruction.NE);
                            }
                            default:
                            {
                                // bool, byte, char, enum, flags, int, uint, type
                                CodeStream.AddInstruction(Instruction.NE);
                            }
                        }
                    }
                }
                break;
            }
            break;
        } // loop
        return actualType; 
    }
    
    string CompileExpression(string expectedType)
    {
        string actualType;
        
        actualType = compileEquality(expectedType);
        
        return actualType;
    }
}
