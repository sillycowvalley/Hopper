unit Expression
{
    uses "Tokens/Token"
    uses "Tokens/Scanner"
    uses "Tokens/Parser"
    uses "Tokens/SysCalls"
    uses "Tokens/LibCalls"
    
    uses "CodeGen/Block"
    uses "CodeGen/Instructions"
    uses "CodeGen/CodeStream"
    
    uses "Symbols"
    uses "Types"
    uses "Constant"
    uses "Collection"
    uses "Record"
    
    PushTypeFromString(string typeName)
    {
        byte b;
        b = ToByte(typeName);
        CodeStream.AddInstructionPUSHI(b);
    }
    
    InitializeVariable(string variableType, bool lazy)
    {
        uint sz;
        string name;
        string recordName;
        // initialize an empty global or local and leave new stack slot at [top]
        
        if (Types.IsValueType(variableType) || Types.IsDelegate(variableType))
        {
            CodeStream.AddInstructionPUSHI(0);
        }
        else if (Types.IsArray(variableType))
        {
            // push size
            if (!variableType.Contains("[]"))
            {
                sz = Types.GetArraySizeFromCollection(variableType); // expect strict size, don't infer
            }
            CodeStream.AddInstructionPUSHI(sz);               
            
            // push value type
            string valueType = (variableType == "V[]") ? "uint" // arbitrary choice since it is about to be assigned to in Slice
                                                       : Types.GetValueFromCollection(variableType);
            PushTypeFromString(valueType);
            name = "Array";            
        }
        else if (Types.IsRecord(variableType))
        {
            recordName = variableType;
            PushTypeFromString("variant");
            name = "List";
        }
        else if (Types.IsList(variableType))
        {
            // push value type
            string valueType = Types.GetValueFromCollection(variableType);
            PushTypeFromString(valueType);
            name = "List";
        }
        else if (Types.IsDictionaryOrPair(variableType))
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
            CodeStream.AddInstructionPUSHI(0); // null placeholder
        }
        else
        {
            Parser.ErrorAtCurrent("not implemented for '" + variableType + "'");
            Die(0x0A);
        }
        if (name.Length != 0)
        {
            CodeStream.AddInstructionSysCall0(name, "New");
            if (!lazy && (recordName.Length != 0))
            {
                Record.InitializeMembers(recordName);
            }
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
                        Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
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
                        Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                    }
                }
            }
            case "bool":
            {
                switch (castToType)
                {
                    case "char":
                    case "uint":
                    case "int":
                    case "byte":
                    {
                        // ok
                    }
                    case "type":
                    {
                        Types.DynamicCastToType();
                    }
                    default:
                    {
                        Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
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
                        Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
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
                        if (Types.IsDelegate(castToType))
                        {
                            // ok - 16 bits wide for pointer type
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                        }
                    }
                }
            }
            case "+int":
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
                         // ok
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
                        if (Types.IsDelegate(castToType))
                        {
                            // ok - 16 bits wide for pointer type
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                        }
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
                        if (Types.IsDelegate(castToType))
                        {
                            // ok - 16 bits wide for pointer type
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                        }
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
                        Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                    }
                }
            }
            case "float":
            {
                switch (castToType)
                {
                    case "uint":
                    {
                        CodeStream.AddInstructionSysCall0("Float", "ToUInt");
                    }
                    case "long":
                    {
                        CodeStream.AddInstructionSysCall0("Float", "ToLong");
                    }
                    default:
                    {
                        Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                    }
                }
            }
            case "variant":
            {
                if (Types.IsSimpleType(castToType))
                {
                    // "|bool|byte|char|uint|int|long|float|string|type|file|directory|delegate|"
                    RuntimeValueCast(castToType);
                }
                else
                {
                    Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
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
                            Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                        }
                    }
                }
                else if (Types.IsDelegate(sourceType))
                {
                    switch (castToType)
                    {
                        case "uint":
                        case "int":
                        {
                            // ok - 16 bits wide for pointer type
                        }
                        default:
                        {
                            Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
                        }
                    }
                }
                else
                {
                    // CODEGEN : runtime cast from sourceType to castToType
                    Parser.ErrorAtCurrent("'" + sourceType + "' -> '" + castToType + "' not implemented");
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
                    if ((offset > -129) && (offset < 128) && !NoPackedInstructions)
                    {
                        byte operand =  CodeStream.IntToByte(offset);
                        CodeStream.AddInstruction(Instruction.PUSHLOCALB, operand);
                    }
                    else
                    {
                        uint operand =  Types.IntToUInt(offset);  
                        CodeStream.AddInstruction(Instruction.PUSHLOCAL, operand);
                    }
                }
                else
                {
                    // push the stack address of the variable to the stack
                    if ((offset > -129) && (offset < 128) && !NoPackedInstructions)
                    {
                        byte operand =  CodeStream.IntToByte(offset);
                        CodeStream.AddInstruction(Instruction.PUSHSTACKADDRB, operand);
                    }
                    else
                    {
                        uint operand =  Types.IntToUInt(offset);  
                        CodeStream.AddInstruction(Instruction.PUSHSTACKADDR, operand);
                    }
                }
            }
            
            break;
        }
        return argumentType;
    }
    
    string CompileMethodCall(string methodName, string thisVariable, string dotThisType)
    {
        string returnType;
        < <string > > arguments;
        bool isDelegateType = false;
        string collectionValueType;
        bool isLocal = false;
        loop
        {
            // consider delegates (variables, not methods)
            string qualifiedName;
            string variableType = Types.GetTypeString(methodName, false, ref qualifiedName);
            if (variableType.Length != 0)
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
                if ((thisVariable.Length != 0) || (dotThisType.Length != 0))
                {
                    
                    if (thisVariable.Length != 0) 
                    {
                        string qualifiedThis;
                        string thisType = Types.GetTypeString(thisVariable, true, ref qualifiedThis);
                        CodeStream.AddInstructionPushVariable(qualifiedThis); // push this
                    }
                    
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
            if (!isDelegateType && !GetFunctionIndex(methodName, ref fIndex) && (methodName.Length != 0))
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
                    if (Types.IsDictionaryOrPair(typeString))
                    {
                        methodName = "Dictionary." + parts[1];
                        collectionValueType = Types.GetValueFromCollection(typeString);
                    }
                    else if (Types.IsList(typeString))
                    {
                        methodName = "List." + parts[1];
                        collectionValueType = Types.GetValueFromCollection(typeString);
                    }
                    else if (Types.IsArray(typeString))
                    {
                        methodName = "Array." + parts[1];    
                        collectionValueType = Types.GetValueFromCollection(typeString);
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
            
            Parser.Consume(HopperToken.LParen);
            if (Parser.HadError)
            {
                break;
            }
        
            uint n = 0;
            if ( (thisVariable.Length != 0) || (dotThisType.Length != 0))
            {
                string thisType = dotThisType;
                if (thisVariable.Length != 0)
                {
                    string qualifiedThis;
                    thisType = Types.GetTypeString(thisVariable, true, ref qualifiedThis);
                    CodeStream.AddInstructionPushVariable(qualifiedThis); // push this
                }
                <string> argument;
                argument.Append(""); // no "ref"
                argument.Append(thisType);
                argument.Append("a" + n.ToString());
                arguments.Append(argument);
                n++;
            }
            <string> expectedArgumentTypes;
            <uint> iOverloads = Symbols.GetFunctionOverloads(fIndex);
            bool first = true;
            foreach (var iOverload in iOverloads)
            {
                < < string > > methodArguments = Symbols.GetOverloadArguments(iOverloads[0]);
                uint nth;
                foreach (var methodArgument in methodArguments)
                {
                    if (nth == expectedArgumentTypes.Count)
                    {
                        // first time we have seen an nth argument, just add it to the end of the list of arguments
                        expectedArgumentTypes.Append(methodArgument[1]); // <ref, type, name>
                    }
                    else
                    {
                        if (methodArgument[1] != expectedArgumentTypes[nth])
                        {
                            expectedArgumentTypes.SetItem(nth, ""); // ambiguous argument type
                        }
                    }
                    nth++;
                }
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
                if (Parser.CheckKeyword("ref"))
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
                else if ((expectedArgumentTypes.Count != 0) && (n < expectedArgumentTypes.Count))
                {
                    argumentType = CompileExpression(expectedArgumentTypes[n]);
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
                    Parser.Consume(HopperToken.Comma);
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
                if (IsCDecl)
                {
                    CDeclPostCALLDelegate(iOverload); // delegate
                }
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
                if (collectionValueType.Length != 0)
                {
                    < < string > > overloadArguments = Symbols.GetOverloadArguments(iOverload);
                    uint argCount = overloadArguments.Count;
                    if (argCount > 1)
                    {
                        <string> templateArgument = overloadArguments[argCount-1];
                        <string> actualArgument   = arguments[argCount-1];
                        if ((templateArgument[0] == "") && (templateArgument[1] == "V") && (collectionValueType != actualArgument[1]))
                        {
                            // For a single trailing 'V' template argument, we can attempt to promote types
                            // like List.Append(xx) for example.
                            string expressionType = actualArgument[1];
                            Instruction before = CodeStream.GetLastInstruction();
                            if (collectionValueType == "variant")
                            {
                                // a variant value container can contain anything
                                if (Types.IsValueType(expressionType))
                                {
                                    // box value types before adding to collection as variant                                            
                                    byte vt = Types.ToByte(expressionType);
                                    CodeStream.AddInstructionPUSHI(vt);
                                    CodeStream.AddInstructionSysCall0("Variant", "Box");
                                }
                            }
                            else if (!Types.AutomaticUpCastTop(expressionType, collectionValueType))
                            {
                                Parser.ErrorAtCurrent("type mismatch in argument, expect '" + collectionValueType + "', was '" + expressionType + "'");       
                                break;
                            }
                            /*
                            if (before != CodeStream.GetLastInstruction())
                            {
                                OutputDebug(methodName + ":" + collectionValueType);
                                OutputDebug(templateArgument);
                                OutputDebug(actualArgument);
                            }
                            */
                        }
                    }
                }
                byte iSysCall = Symbols.GetSysCallIndex(iOverload);
                byte iSysOverload = Symbols.GetSysCallOverload(iOverload);
                if (TryUserSysCall(methodName, iSysOverload))
                {
                    // done
                }
                else if (!NoPackedInstructions && (iSysOverload == 0))
                {
                    CodeStream.AddInstruction(Instruction.SYSCALL0, iSysCall);
                }
                else if (!NoPackedInstructions && (iSysOverload == 1))
                {
                    CodeStream.AddInstruction(Instruction.SYSCALL1, iSysCall);
                }
                else if (!NoPackedInstructions && (iSysOverload == 2))
                {
                    CodeStream.AddInstruction(Instruction.SYSCALL2, iSysCall);
                }
                else
                {
                    CodeStream.AddInstructionPUSHI(iSysOverload);
                    CodeStream.AddInstruction(Instruction.SYSCALL, iSysCall);
                }
            }
            else if (Symbols.IsLibCall(iOverload))
            {
                byte iLibCall = Symbols.GetLibCallIndex(iOverload);
                byte iLibOverload = Symbols.GetLibCallOverload(iOverload);
                if (!NoPackedInstructions && (iLibOverload == 0))
                {
                    CodeStream.AddInstruction(Instruction.LIBCALL0, iLibCall);
                }
                else if (!NoPackedInstructions && (iLibOverload == 1))
                {
                    CodeStream.AddInstruction(Instruction.LIBCALL1, iLibCall);
                }
                else
                {
                    CodeStream.AddInstructionPUSHI(iLibOverload);
                    CodeStream.AddInstruction(Instruction.LIBCALL, iLibCall);
                }
            }
            else
            {
                if (CodeStream.IsShortCalls && (iOverload < 256) && !NoPackedInstructions)
                {
                    CodeStream.AddInstruction(Instruction.CALLB, byte(iOverload));
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.CALL, iOverload);
                }
            }
            if (IsCDecl)
            {
                CDeclPostCALL(iOverload); // CompileMethodCall
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
            actualType = CompileMethodCall(functionName, thisVariable, "");
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
            string constantType = Symbols.GetConstantType(constantName);
            if (constantType.Length != 0)
            {
                switch (constantType)
                {
                    case "bool":
                    {
                        if (value == "true")
                        {
                            CodeStream.AddInstructionPUSHI(1);
                            actualType = "bool";
                        }
                        else if (value == "false")
                        {
                            CodeStream.AddInstructionPUSHI(0);
                            actualType = "bool";
                        }
                    }
                    case "char":
                    {
                        uint ui;
                        if (UInt.TryParse(value, ref ui) && (ui >= 0) && (ui <= 255))
                        {
                            CodeStream.AddInstructionPUSHI(ui);       
                            actualType = "char";
                        }
                        else if (value.Length == 1)
                        {
                            ui = uint(value[0]);
                            if (ui < 32)
                            {
                                CodeStream.AddInstructionPUSHI(ui);       
                                actualType = "char";
                            }
                        }
                    }
                    case "string":
                    {
                        CodeStream.AddString(constantType, value);
                        actualType = constantType;
                    }
                    default:
                    {
                        long l;
                        float f;
                        
                        if (Types.IsByteArray(constantType))
                        {
                            CodeStream.AddString(constantType, value);
                            actualType = constantType;
                        }
                        else if (Long.TryParse(value, ref l))
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
                        else
                        {
                            if (Float.TryParse(value, ref f))
                            {
                                uint constantAddress = CodeStream.CreateFloatConstant(f);
                                CodeStream.AddInstructionPUSHI(constantAddress);
                                CodeStream.AddInstructionSysCall0("Float", "NewFromConstant");
                                actualType = "float";
                            }
                        }
                    } // default
                    
                }
            }
            else
            {
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
                    CodeStream.AddInstructionPUSHI(1);
                    actualType = "bool";
                }
                else if (value == "false")
                {
                    CodeStream.AddInstructionPUSHI(0);
                    actualType = "bool";
                }
                else 
                {
                    if (Float.TryParse(value, ref f))
                    {
                        uint constantAddress = CodeStream.CreateFloatConstant(f);
                        CodeStream.AddInstructionPUSHI(constantAddress);
                        CodeStream.AddInstructionSysCall0("Float", "NewFromConstant");
                        actualType = "float";
                    }
                    else
                    {
                        CodeStream.AddString("string", value);
                        actualType = "string";
                    }
                }
            }
            break;
        }
        return actualType;
    }
    
    string compileKeywordPrimary(string expectedType)
    {
        string actualType;
        loop
        {
            <string,string> currentToken = Parser.CurrentToken;
            string typeName = currentToken["lexeme"];
            bool doTypeOf;
            bool typeId = Token.IsTypeKeyword(typeName); // array|bool|byte|char|delegate|dictionary|directory|enum|file|flags|float|int|uint|list|long|pair|string|type|variant|const|ref|var
            Parser.Advance();
            if (!IsSimpleType(typeName)) // bool|byte|char|uint|int|long|float|string|type|file|directory|delegate
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
                        // stand-alone type keyword that is not a simple type: array|dictionary|enum|flags|list|pair|variant|const|ref|var
                    }
                    else
                    {
                        Parser.ErrorAt(currentToken, "compilePrimary not implemented");
                    }
                }
            }
            
            // either:
            //    typeof(..) 
            // or stand-alone type keyword that is one of these:
            //    array|bool|byte|char|const|directory|delegate|dictionary|enum|file|flags|float|int|list|long|pair|ref|string|type|uint|variant|var
            
            string sourceType;
            if (Parser.Check(HopperToken.LParen))
            {
                if (!IsValueType(typeName)) // bool|byte|char|uint|int|type|delegate|<enum-type-name>|<flags-type-name>|<delegate-type-name>
                {
                    // array|const|directory|dictionary|enum|file|flags|float|list|long|pair|ref|string|type|variant|var
                    
                    if (!doTypeOf)
                    {
                        if ((typeName == "long") || (typeName == "float") || (typeName == "string"))
                        {
                            // CompileDynamicCast(..) deals with these
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("'" + typeName + "' is an invalid type for simple cast");
                        }
                    }
                }
                Parser.Advance(); // (
                
                // typeof(<expression>)  or bool|byte|char|delegate|uint|int|type (<expression>)
                sourceType = CompileExpression(expectedType);
                if (Parser.HadError)
                {
                    break;
                }
                Parser.Consume(HopperToken.RParen);
                if (Parser.HadError)
                {
                    break;
                }
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
            break;
        } // loop
        return actualType;
    }
    
    string compileNamedTypeCast(string identifier)
    {
        string actualType;
        loop
        {
            Parser.Advance(); // (
            string expressionType = CompileExpression("uint");
            if (Parser.HadError)
            {
                break;
            }
            if (expressionType == "int")
            {
                CompileDynamicCast(expressionType, "uint");
                expressionType = "uint";
            }
            if (expressionType != "uint")
            {
                if (Types.IsDelegate(identifier) || !Types.AutomaticUpCastTop(expressionType, "uint"))
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
            
            Parser.Consume(HopperToken.RParen);
            if (Parser.HadError)
            {
                break;
            }
            actualType = identifier;
            break;
        } // loop
        return actualType;
    }
    
    string compileCollectionAccessor(string identifier, string typeString, string qualifiedName, string expectedType)
    {
        string actualType;
        loop
        {
            // array/string/list/dictionary GetItem
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
                if (indexType == "int")
                {
                    CompileDynamicCast(indexType, "uint");
                    indexType = "uint";
                }
                if (indexType != "uint")
                {
                    Parser.ErrorAtCurrent("array index type invalid");
                    break;
                }
                Parser.Consume(HopperToken.RBracket);
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
                Parser.Consume(HopperToken.RBracket);
                if (Parser.HadError)
                {
                    break;
                }
                CodeStream.AddInstructionSysCall0("String", "GetChar");
                actualType = "char";    
            }
            else if (Types.IsDictionaryOrPair(typeString)) 
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
                    if ((actualKeyType == "int") && (keyType == "uint"))
                    {
                        CompileDynamicCast(actualKeyType, keyType);
                        actualKeyType = keyType;
                    }
                    if (actualKeyType != "uint")
                    {
                        Parser.ErrorAtCurrent("dictionary key type should be '"+keyType+"', (not '" + actualKeyType + "')");
                        break;
                    }
                }
                Parser.Consume(HopperToken.RBracket);
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
                Parser.Consume(HopperToken.RBracket);
                if (Parser.HadError)
                {
                    break;
                }
                CodeStream.AddInstructionSysCall0("List", "GetItem"); // compileCollectionAccessor: .. = list[i]
                actualType = valueType;
            }
            else
            {
                Parser.ErrorAtCurrent("identifier not string, array, list or dictionary type");
            }
            
            if (Parser.HadError)
            {
                break;
            }
            
            if (Parser.Check(HopperToken.Dot))
            {
                actualType = compileDotFunctionCall(actualType, expectedType);
            }
            
            break;
        } // loop
        return actualType;
    }
    string compileConstantCollectionAccessor(string identifier, string qualifiedName, string expectedType)
    {
        string actualType;
        loop
        {
            // array or string constant item access:
            Parser.Advance(); // [
            string constantType = Symbols.GetConstantType(qualifiedName);
            if ((constantType == "string") || constantType.StartsWith("byte["))
            {
                // push first argument: the string from the constant
                string value = Symbols.GetConstantValue(qualifiedName);
                
                CodeStream.AddString(constantType, value);
                
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
                Parser.Consume(HopperToken.RBracket);
                if (Parser.HadError)
                {
                    break;
                }
                if (constantType == "string")
                {
                    CodeStream.AddInstructionSysCall0("String", "GetChar");
                    actualType = "char";  
                }
                else if (constantType.StartsWith("byte["))
                {
                    CodeStream.AddInstructionSysCall0("Array", "GetItem");
                    actualType = "byte";  
                }
                else
                {
                    Parser.ErrorAtCurrent("invalid expected type (" + constantType + ") for constant collection accessor");
                }
            }
            else
            {
                Parser.ErrorAtCurrent("identifier not array or string type (" + expectedType + ")");
            }
            
            if (Parser.HadError)
            {
                break;
            }
            
            if (Parser.Check(HopperToken.Dot))
            {
                actualType = compileDotFunctionCall(actualType, expectedType);
            }
            
            break;
        } // loop
        
        return actualType;
    }
    
    string compileVariableIdentifier(string identifier, string typeString, string qualifiedName, string expectedType)
    {
        string actualType;
        loop
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
            // regular variable identifier (including delegate variable with no (..))
            CodeStream.AddInstructionPushVariable(qualifiedName);
            actualType = typeString;

            break;
        } // loop
        return actualType;
    }
    
    string compileDelegateExpression(string identifier, uint fdIndex, string expectedType)
    {
        string actualType;
        loop
        {
            // - if 'expectedType' is a delegate type, match the method 'identifier' arguments
            //   and returntype
            // - then push address of method on to stack
            
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
            if (Symbols.IsSysCall(wiOverload))
            {
                Parser.ErrorAtCurrent("system calls cannot be delegates");
            }
            if (Symbols.IsLibCall(wiOverload))
            {
                Parser.ErrorAtCurrent("library calls cannot be delegates");
            }
            
            // delegate was pushed onto stack so we need to compile it
            Symbols.OverloadToCompile(wiOverload); 
            
            // push function index to stack
            if (CodeStream.IsShortCalls && (wiOverload < 256) && !NoPackedInstructions)
            {
                CodeStream.AddInstruction(Instruction.PUSHDB, byte(wiOverload));
            }
            else
            {
                CodeStream.AddInstruction(Instruction.PUSHD, wiOverload);
            }
            actualType = expectedType;
            break;
        } // loop
        return actualType;
    }
    
    string compileDotFunctionCall(string thisForDotCallType, string expectedType)
    {
        string actualType;
        loop
        {
            Parser.Advance(); // consume '.'
            <string,string> currentToken = Parser.CurrentToken;
            string identifier = currentToken["lexeme"];
            
            HopperToken tokenType = Token.GetType(currentToken);
            if (tokenType != HopperToken.Identifier)
            {
                Parser.ErrorAtCurrent("function identifier expected");
                break;
            }
            Parser.Advance();
            
            string thisTypeString;
            if (Types.IsDictionaryOrPair(thisForDotCallType))
            {
                thisTypeString = "Dictionary";
            }
            else if (Types.IsList(thisForDotCallType))
            {
                thisTypeString = "List";
            }
            else if (Types.IsArray(thisForDotCallType))
            {
                thisTypeString = "Array";
            }
            else if (Types.IsRecord(thisForDotCallType))
            {
                byte iMember;
                // FindMember(string thisTypeString, string memberName, ref byte iMember, ref string actualType)
                if (!Record.FindMember(thisForDotCallType, identifier, ref iMember, ref actualType))
                {
                    Parser.ErrorAtCurrent("record member identifier expected");
                    break;
                }
                CodeStream.AddInstructionPUSHI(iMember);
                CodeStream.AddInstructionSysCall0("List", "GetItem");
                if (Types.IsRecord(actualType))
                {
                    // if the member is also a record type, make sure its members exist
                    Record.LazyInitializeMembers(actualType);
                }
                break;
            }
            else
            {
                <string> nameSpaces = Symbols.GetNameSpaces();
                foreach (var nameSpace in nameSpaces)
                {
                    if (thisForDotCallType == nameSpace.ToLower())
                    {
                        thisTypeString = nameSpace;
                        break;
                    }
                }
            }
            string qualifiedFunctionName = thisTypeString + "." + identifier;
            
            //PrintLn(" " + qualifiedFunctionName + " " + thisForDotCallType + " " + actualType);
            uint fIndex;
            if (!Symbols.GetFunctionIndex(qualifiedFunctionName, ref fIndex))
            {
                if (Symbols.GetFunctionIndex(qualifiedFunctionName + "_Get", ref fIndex))
                {
                    qualifiedFunctionName += "_Get";       
                }
            }
            
            actualType = CompileMethodCall(qualifiedFunctionName, "", thisForDotCallType);
            
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.Dot))
            {
                actualType = compileDotFunctionCall(actualType, expectedType);
            }
            
            break;
        } // loop
        return actualType;
    }
    
    string compileIdentifierPrimary(string expectedType, bool isDotted)
    {
        uint fdIndex;
        byte iMember;
        uint iDot;
        uint fIndex;
        bool oneDot;
        string namespaceAsTypeString;
        string qualifiedIdentifier;
        string actualType;
        string qualifiedName;
        string identifier;
        string identifierTypeString;
        string constantIdentifier;
        <string,string> currentToken;
        <string> nameSpaces;
        loop
        {
            currentToken = Parser.CurrentToken;
    
            Parser.Advance();
            identifier = currentToken["lexeme"];
            
            
            identifierTypeString = Types.GetTypeString(identifier, false, ref qualifiedName);
            if (Parser.HadError)
            {
                break;
            }
            if (identifierTypeString.Length != 0)
            {
                if (Parser.Check(HopperToken.LBracket))
                {
                    // <identifier> [ <expression> ]
                    actualType = compileCollectionAccessor(identifier, identifierTypeString, qualifiedName, expectedType);
                    break;
                }
                else
                {
                    // <identifier>  or <delegate-identifier> (...)
                    actualType = compileVariableIdentifier(identifier, identifierTypeString, qualifiedName, expectedType);
                    break;
                }
            } // (identifierTypeString.Length != 0)
            
            else // (identifierTypeString.Length == 0)
            {
                // named type casts
                if (Types.IsEnum(identifier) || Types.IsFlags(identifier) || Types.IsDelegate(identifier))
                {
                    if (Parser.Check(HopperToken.LParen))
                    {
                        actualType = compileNamedTypeCast(identifier);
                        break;
                    }        
                }
            }
            
            constantIdentifier = Types.QualifyConstantIdentifier(identifier);
            if (Symbols.ConstantExists(constantIdentifier))
            {
                if (!IsVisibleConstant(constantIdentifier))
                {
                    Parser.ErrorAtCurrent("'" + constantIdentifier + "' is private");
                    break;
                }
                if (Parser.Check(HopperToken.LBracket))
                {
                    // <constant identifier> [ <expression> ]
                    actualType = compileConstantCollectionAccessor(identifier, constantIdentifier, expectedType);
                }
                else
                {
                    // <constant identifier>
                    actualType = compileConstant(expectedType, constantIdentifier);
                }
                break;
            }
            
            if (Symbols.GetFunctionDelegateIndex(expectedType, ref fdIndex))
            {
                actualType = compileDelegateExpression(identifier, fdIndex, expectedType);
                break;
            }
            
            nameSpaces = Symbols.GetNameSpaces();
            if (isDotted)
            {
                // this?
                <string> parts = identifier.Split('.');
                string thisVariable = parts[0];
                string qualifiedThis;
                string thisTypeString = Types.GetTypeString(thisVariable, false, ref qualifiedThis);
                if (Parser.HadError)
                {
                    break;
                }
                if (parts.Count < 2)
                {
                    Parser.Error("invalid identifier");
                    break;
                }
                string functionName = parts[1];
                if ( Types.IsDictionaryOrPair(thisTypeString) && ((functionName == "key") || (functionName == "value")) )
                {
                    // honorary getters for dictionary iterator pairs:
                    CodeStream.AddInstructionPushVariable(qualifiedThis);
                    if (functionName == "key")
                    {
                        CodeStream.AddInstructionSysCall0("Pair", "Key");
                        actualType = Types.GetKeyFromCollection(thisTypeString);
                    }
                    else if (functionName == "value")
                    {
                        CodeStream.AddInstructionSysCall0("Pair", "Value");
                        actualType = Types.GetValueFromCollection(thisTypeString);
                    }
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (Parser.Check(HopperToken.Dot))
                    {
                        actualType = compileDotFunctionCall(actualType, expectedType);
                    }
                    break;
                }
                if (Record.FindMember(thisTypeString, functionName, ref iMember, ref actualType))
                {
                    CodeStream.AddInstructionPushVariable(qualifiedThis);
                    CodeStream.AddInstructionPUSHI(iMember);
                    CodeStream.AddInstructionSysCall0("List", "GetItem");
                    if (Types.IsRecord(actualType))
                    {
                        // if the member is also a record type, make sure its members exist
                        Record.LazyInitializeMembers(actualType);
                    }
                    break;
                }
                if (Parser.HadError)
                {
                    break;
                }
                
                if (!Parser.Check(HopperToken.LParen))
                {
                    // no '(' : getter?
                    functionName = functionName + "_Get";
                }
                if (thisTypeString.Length != 0)
                {
                    // "this" case
                    if (Types.IsDictionaryOrPair(thisTypeString))
                    {
                        thisTypeString = "Dictionary";
                    }
                    else if (Types.IsList(thisTypeString))
                    {
                        thisTypeString = "List";
                    }
                    else if (Types.IsArray(thisTypeString))
                    {
                        thisTypeString = "Array";
                    }
                    else if (Types.IsEnum(thisTypeString) || Types.IsFlags(thisTypeString))
                    {
                        // like key.ToChar()
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
                            if (nameSpace == thisTypeString)
                            {
                                qualifiedName = nameSpace + "." + functionName;
                                break;
                            }
                            string lowerNameSpace = nameSpace.ToLower();
                            if (lowerNameSpace == thisTypeString)
                            {
                                qualifiedName = nameSpace + "." + functionName;
                                break;
                            }
                        }
                    }

                    if (qualifiedName.Length != 0)
                    {
                        // "this": method qualified by the type/namespace of the variable
                        actualType = compileFunctionCall(qualifiedName, expectedType, thisVariable);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        if (Parser.Check(HopperToken.Dot))
                        {
                            actualType = compileDotFunctionCall(actualType, expectedType);
                        }
                        break;
                    }
                    // fall through to undefined identifier error
                } // "this" case
                
                else
                {
                    // getter?
                    
                    namespaceAsTypeString = parts[0];
                    qualifiedName = "";
                    foreach (var nameSpace in nameSpaces)
                    {
                        if (nameSpace == namespaceAsTypeString)
                        {
                            qualifiedName = nameSpace + "." + functionName;
                            break;
                        }
                    }
                    if (qualifiedName.Length != 0)
                    {
                        if (GetFunctionIndex(qualifiedName, ref fIndex))
                        {
                            // qualified by namespace name rather than by variable name:
                            actualType = compileFunctionCall(qualifiedName, expectedType, "");
                            if (Parser.HadError)
                            {
                                break;
                            }
                            if (Parser.Check(HopperToken.Dot))
                            {
                                actualType = compileDotFunctionCall(actualType, expectedType);
                            }
                            break;
                        }
                    }
                    
                    // enums and flags
                    oneDot = true;
                    if (Parser.Check(HopperToken.Dot)) // next token is another '.'
                    {
                        // 2 dot enums like "Keyboard.Key.Delete"
                        oneDot = false;
                        Parser.Advance(); // consume '.'
                        
                        <string,string> typeToken = Parser.CurrentToken;
                        identifier = typeToken["lexeme"];
                        
                        HopperToken tokenType = Token.GetType(typeToken);
                        if (tokenType != HopperToken.Identifier)
                        {
                            Parser.ErrorAtCurrent("member name identifier expected");
                            break;
                        }
                        Parser.Advance();
                        
                        string typeName = parts[0] + "." + parts[1];
                        identifier = typeName + "." + identifier;
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
                    if (oneDot)
                    {
                        actualType = "";
                        
                        foreach (var nameSpace in nameSpaces)
                        {
                            string typeName = nameSpace + "." + parts[0];
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
                        if (actualType.Length != 0)
                        {
                            break;
                        }
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
                qualifiedIdentifier = Types.QualifyMethodName(identifier);
                if (GetFunctionIndex(qualifiedIdentifier, ref fIndex))
                {
                    // unqualified method call: we qualify using namespace scope rules
                    actualType = compileFunctionCall(qualifiedIdentifier, expectedType, "");
                    break;
                }
            }
            Parser.ErrorAtCurrent("'" + currentToken["lexeme"] + "' is not defined");
            break;
        } // loop
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
                        CodeStream.AddInstructionPUSHI(1);
                    }
                    else if (currentToken["lexeme"] == "false")
                    {
                        CodeStream.AddInstructionPUSHI(0);
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
                    
                    CodeStream.AddString(actualType, currentToken["lexeme"]);
                }
                case HopperToken.LBrace:
                {
                    string value = Collection.ParseConstant(expectedType, ref actualType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    CodeStream.AddString(actualType, value);
                }
                
                case HopperToken.Float:
                {
                    Parser.Advance();
                    float f;
                    if (!Float.TryParse(currentToken["lexeme"], ref f)) // converting the constant lexeme back to a float
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
                        CodeStream.AddInstruction(Instruction.PUSHI, ui);
                    }
                    else if (l <= 65535)
                    {
                        actualType = "uint";
                        uint ui = uint(l);
                        CodeStream.AddInstruction(Instruction.PUSHI, ui);
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
                    actualType = compileIdentifierPrimary(expectedType, isDotted);
                }
                case HopperToken.Keyword:
                {
                    actualType = compileKeywordPrimary(expectedType);
                }
                case HopperToken.LParen:
                {
                    Parser.Advance(); // (
                    actualType = CompileExpression(expectedType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    Parser.Consume(HopperToken.RParen);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (Parser.Check(HopperToken.Dot))
                    {
                        actualType = compileDotFunctionCall(actualType, expectedType);
                    }
                }
                default:
                {
                    if (Token.IsDelimiter(tokenType))
                    {
                        Parser.ErrorAtCurrent("unexpected '" + Token.ToString(tokenType) + "'");
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("compilePrimary not implemented for " + currentToken["type"]);
                    }
                }
            } // switch
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
            else if (Parser.Check(HopperToken.BitAnd))
            {
                Parser.Advance();
                <string,string> currentToken = Parser.CurrentToken;
                HopperToken tokenType = Token.GetType(currentToken);
                if ((tokenType != HopperToken.Identifier) && (tokenType != HopperToken.DottedIdentifier))
                {
                    Parser.ErrorAtCurrent("variable or method identifier expected");
                    break;
                }
                
                // borrowed from AddInstructionPushVariable(..) :
                string variableName = currentToken["lexeme"];
                string fullName;
                string variableType = Types.GetTypeString(variableName, false, ref fullName);
                if (Parser.HadError)
                {
                    break;
                }
                if (false && (variableType.Length == 0)) // don't allow & with method for now (delegates suffice)
                {
                    string methodName = Types.QualifyMethodName(variableName);
                    uint fIndex;
                    if (GetFunctionIndex(methodName, ref fIndex))
                    {
                        Parser.Advance();
                        <uint> overloads = GetFunctionOverloads(fIndex);
                        if (overloads.Count != 1)
                        {
                            Parser.ErrorAtCurrent("'&' not implemented for overloaded methods");
                            break;
                        }
                        uint methodIndex = overloads[0];
                        CodeStream.AddInstructionPUSHI(methodIndex);
                        actualType = "uint";
                        break;
                    }
                    if (Parser.HadError)
                    {
                        break;
                    }
                    Parser.ErrorAtCurrent("variable or method identifier expected");
                    break;
                }
                
                // we found a variable, global or local:
                Parser.Advance();
                
                if (Symbols.GlobalMemberExists(fullName))
                {
                    uint globalAddress = Symbols.GetGlobalAddress(fullName);
                    CodeStream.AddInstructionPUSHI(globalAddress);
                    actualType = "uint";
                }
                
                else
                {
                    bool isRef;
                    int offset = Block.GetOffset(variableName, ref isRef);
                    if (isRef)
                    {
                        Parser.ErrorAtCurrent("'&' not implemented for 'ref' arguments");
                        break;
                    }
                    uint localOffset;
                    if ((offset > -129) && (offset < 128))
                    {
                        localOffset =  CodeStream.IntToByte(offset);
                    }
                    else
                    {
                        localOffset =  Types.IntToUInt(offset);
                    }
                    CodeStream.AddInstructionPUSHI(localOffset);
                    actualType = "uint";
                }
                break;
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
                            CodeStream.AddInstructionPUSHI(0);
                            CodeStream.AddInstructionSysCall0("UInt", "ToFloat");
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstructionSysCall0("Float", "Sub");
                        }
                        case "byte":
                        {
                            CodeStream.AddInstructionPUSHI(0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "-int";
                        }
                        case "int":
                        {
                            CodeStream.AddInstructionPUSHI(0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "int";
                        }
                        case "-int":
                        {
                            CodeStream.AddInstructionPUSHI(0);
                            CodeStream.AddInstruction(Instruction.SWAP);
                            CodeStream.AddInstruction(Instruction.SUBI);   
                            actualType = "+int";
                        }
                        case "+int":
                        {
                            CodeStream.AddInstructionPUSHI(0);
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
                    
                    string shiftType = compileFactor(expectedType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if ((actualType != "byte") 
                     && (actualType != "uint")  
                     && (actualType != "int")
                     && (actualType != "+int")
                     )
                    {
                        Parser.ErrorAtCurrent("shift operand must be positive integral type");
                        break;
                    }
                                        // TODO : runtime check to verify that 15 >= [top] >= 0
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
                                if (rightType == "char")
                                {
                                    // string Append(string,char)
                                    CodeStream.AddInstructionSysCall("String", "Append", 1);
                                }
                                else
                                {
                                    // string Append(string,string)
                                    CodeStream.AddInstructionSysCall("String", "Append", 0);
                                }
                            }
                            case "char":
                            {
                                if (rightType == "string")
                                {
                                    CodeStream.AddInstruction(Instruction.SWAP);
                                    CodeStream.AddInstructionPUSHI(0);
                                    CodeStream.AddInstruction(Instruction.SWAP);
                                    CodeStream.AddInstructionSysCall("String", "InsertChar", 0);
                                }
                                else if (rightType == "char")
                                {
                                    // str = ch + ch
                                    CodeStream.AddInstructionSysCall("Char", "ToString", 0);
                                    CodeStream.AddInstruction(Instruction.SWAP);
                                    CodeStream.AddInstructionPUSHI(0);
                                    CodeStream.AddInstruction(Instruction.SWAP);
                                    CodeStream.AddInstructionSysCall("String", "InsertChar", 0);
                                }
                                else
                                {
                                    Die(0x0A);
                                }
                                actualType = "string";
                            }
                            case "long":
                            {
                                CodeStream.AddInstructionSysCall("Long", "Add", 0);
                            }
                            case "float":
                            {
                                CodeStream.AddInstructionSysCall("Float", "Add", 0);
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
                                CodeStream.AddInstructionSysCall("Long", "Sub", 0);
                            }
                            case "float":
                            {
                                CodeStream.AddInstructionSysCall("Float", "Sub", 0);
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
                if (Parser.HadError)
                {
                    break;
                } 
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
                    
                    switch (operation)
                    {
                        case HopperToken.LT:
                        {
                            //  1 -> false
                            //  0 -> false
                            // -1 -> true
                            CodeStream.AddInstructionPUSHI(0);
                            CodeStream.AddInstructionPUSHI(1);
                            CodeStream.AddInstruction(Instruction.SUBI); // 0-1 -> -1
                            CodeStream.AddInstruction(Instruction.EQ);
                        }
                        case HopperToken.GE:
                        {
                            //  1 -> true
                            //  0 -> true
                            // -1 -> false
                            CodeStream.AddInstructionPUSHI(0);
                            CodeStream.AddInstructionPUSHI(1);
                            CodeStream.AddInstruction(Instruction.SUBI); // 0-1 -> -1
                            CodeStream.AddInstruction(Instruction.NE);
                        }
                        case HopperToken.LE:
                        {
                            //  1 -> false
                            //  0 -> true
                            // -1 -> true
                            CodeStream.AddInstructionPUSHI(1); 
                            CodeStream.AddInstruction(Instruction.NE);
                        }
                        case HopperToken.GT:
                        {
                            //  1 -> true
                            //  0 -> false       
                            // -1 -> false       
                            CodeStream.AddInstructionPUSHI(1); 
                            CodeStream.AddInstruction(Instruction.EQ);
                        }    
                    }
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
    

    string compileBitOr(string expectedType)
    {
        string actualType;
        loop
        {
            string leftType = compileBitXor(expectedType);
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
                    string rightType = compileBitXor(expectedType);
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
            loop
            {
                if (Parser.Check(HopperToken.BitXor))           
                {
                    if (!IsBitwiseType(leftType))
                    {
                        Parser.ErrorAtCurrent("bitwise operations only legal for 'uint', 'int', 'flags' and 'byte', (not '" + leftType + "')");
                        break;
                    }
                    Parser.Advance(); // ^
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
                    CodeStream.AddInstruction(Instruction.BITXOR);
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
                    CodeStream.AddInstruction(Instruction.DUP0);
                    jumpShortCircuits.Append(CodeStream.NextAddress);
                    CodeStream.AddInstructionJump(Instruction.JZ);
                    
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
                    CodeStream.AddInstruction(Instruction.DUP0);
                    jumpShortCircuits.Append(CodeStream.NextAddress);
                    CodeStream.AddInstructionJump(Instruction.JNZ);
                                            
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
                                CodeStream.AddInstructionPUSHI(0);
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
                                CodeStream.AddInstructionPUSHI(0);
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
    
    string compileTernary(string expectedType)
    {
        string actualType;
        loop
        {
            actualType = compileEquality(expectedType);
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.Question)) // ?
            {
                if (IsZ80) // TODO: ternary
                {
                    Parser.ErrorAtCurrent("ternary expression not supported by Z80 compiler");
                    break;
                }
                if (actualType != "bool")
                {
                    Parser.ErrorAtCurrent("ternary test expression must be boolean");
                    break;
                }
                
                // if false jump past
                uint jumpPast = CodeStream.NextAddress;
                CodeStream.AddInstructionJump(Instruction.JZ);
                
                <byte> mainStream = CodeStream.CurrentStream;
                CodeStream.New();
            
                Parser.Advance(); // ?
                string trueExpressionType = CompileExpression(expectedType);
                Parser.Consume(HopperToken.Colon);
                if (Parser.HadError)
                {
                    break;
                }
                
                <byte> trueStream = CodeStream.CurrentStream;
                CodeStream.New();
                
                
                
                string falseExpressionType = CompileExpression(expectedType);
                if (Parser.HadError)
                {
                    break;
                }
                
                <byte> falseStream = CodeStream.CurrentStream;
                
                CodeStream.New(mainStream);
                CodeStream.AppendCode(trueStream);
                
                actualType = trueExpressionType;
                if (trueExpressionType != falseExpressionType)
                {
                    // AutomaticUpCastTop(string actualType, string desiredType)
                    if (Types.AutomaticUpCastTop(trueExpressionType, falseExpressionType))
                    {
                        actualType = falseExpressionType;
                    }
                }
                
                // jump end (past a potential "else" block)
                uint jumpEnd = CodeStream.NextAddress;
                CodeStream.AddInstructionJump(Instruction.J);
// past:
                uint pastAddress = CodeStream.NextAddress;
                CodeStream.PatchJump(jumpPast, pastAddress); 
                
                CodeStream.AppendCode(falseStream);
                
                if (actualType != falseExpressionType)
                {
                    // AutomaticUpCastTop(string actualType, string desiredType)
                    if (Types.AutomaticUpCastTop(falseExpressionType, actualType))
                    {
                        actualType = trueExpressionType;  
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("'true' and 'false' expressions must be of the same type");
                    }
                }
                if (Parser.HadError)
                {
                    break;
                }
// end:
                uint endAddress = CodeStream.NextAddress;
                CodeStream.PatchJump(jumpEnd, endAddress);
                
                
            } // ?
            break;
        }
        return actualType;
    }
    
    string CompileExpression(string expectedType)
    {
        string actualType;
        
        actualType = compileTernary(expectedType);
        
        return actualType;
    }
}
