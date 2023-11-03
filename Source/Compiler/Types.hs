unit Types
{
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    
    uses "/Source/Compiler/CodeGen/Block"
    uses "/Source/Compiler/CodeGen/CodeStream"
    uses "/Source/Compiler/CodeGen/Instructions"
    
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/Constant"
    
    uint iCurrentOverload;
    string currentNamespace;
    
    SetCurrentMethod(uint iOverload)
    {
        iCurrentOverload = iOverload;
        currentNamespace = Symbols.GetNamespace(iOverload);
    }
    uint GetCurrentMethod()
    {
        return iCurrentOverload;
    }
    
    bool EnumValue(string candidate, ref string typeName, ref string valueName, ref uint value)
    {
        bool success = false;
        uint fDot;
        uint sDot;
        if (candidate.IndexOf('.', ref fDot))
        {
            if (candidate.LastIndexOf('.', ref sDot))
            {
                if (fDot != sDot)
                {
                    // 2 dots
                    typeName = candidate.Substring(0, sDot);
                    valueName = candidate.Substring(sDot+1);
                    success = IsEnum(typeName);
                }
                else
                {
                    // 1 dot
                    typeName = candidate.Substring(0, fDot);
                    valueName = candidate.Substring(fDot+1);
                    success = IsEnum(typeName);
                }
            }
        }
        else
        {
            // no dots so cannot possibly be a 'value'
        }
        if (success)
        {
            typeName = Symbols.QualifyEnum(typeName, currentNamespace);
            valueName = typeName + "." + valueName;
            success = Symbols.FindEnum(typeName, valueName, ref value);
        }
        return success;
    }
    
    bool FlagsValue(string candidate, ref string typeName, ref string valueName, ref uint value)
    {
        bool success = false;
        uint fDot;
        uint sDot;
        if (candidate.IndexOf('.', ref fDot))
        {
            if (candidate.LastIndexOf('.', ref sDot))
            {
                if (fDot != sDot)
                {
                    // 2 dots
                    typeName = candidate.Substring(0, sDot);
                    valueName = candidate.Substring(sDot+1);
                    success = IsFlags(typeName);
                }
                else
                {
                    // 1 dot
                    typeName = candidate.Substring(0, fDot);
                    valueName = candidate.Substring(fDot+1);
                    success = IsFlags(typeName);
                }
            }
        }
        else
        {
            // no dots so cannot possibly be a 'value'
        }
        if (success)
        {
            typeName = Symbols.QualifyFlags(typeName, currentNamespace);
            valueName = typeName + "." + valueName;
            success = Symbols.FindFlags(typeName, valueName, ref value);
        }
        return success;
    }               
    
    bool AutomaticUpCast(string actualType, string desiredType, bool codeGenTop, bool codeGenNext)
    {
        // codeGenTop:  if the cast is not a NOP, CODEGen for [top]
        // codeGenNext: if the cast is not a NOP, CODEGen for [next]
        bool ok = false;
        bool nop = false;
        loop
        {
            if (actualType == "byte")
            {
                ok = ((desiredType == "+int") 
                   || (desiredType == "int") 
                   || (desiredType == "uint")
                   || (desiredType == "long")
                   || (desiredType == "float")
                   );
                nop = (desiredType == "uint");
            }
            else if (actualType == "+int")
            {
                ok = ((desiredType == "int") 
                   || (desiredType == "uint")
                   || (desiredType == "long")
                   || (desiredType == "float")
                     );
                nop = (desiredType == "uint");
            }
            else if (actualType == "int")
            {
                ok = ((desiredType == "long")
                   || (desiredType == "float")
                     );
            }
            else if (actualType == "uint")
            {
                ok = ((desiredType == "long")
                   || (desiredType == "float")
                     );
            }
            else if (actualType == "-int")
            {
                ok = ((desiredType == "int")
                   || (desiredType == "long")
                   || (desiredType == "float")
                     );
            }
            else if (actualType == "long")
            {
                ok = ((desiredType == "float")
                   || (desiredType == "uint")
                     );
            }
            break;
        }
        if (!nop && ok && ((desiredType == "long") || (desiredType == "float") || (desiredType == "uint"))) // cast to new reference types?
        {
            if (codeGenNext)
            {
                CodeStream.AddInstruction(Instruction.SWAP); // [next] <-> [top]
            }
            switch (actualType)
            {
                case "long":
                {
                    if (desiredType == "float")
                    {
                        CodeStream.AddInstructionSysCall0("Long", "ToFloat");
                    }
                    else if (desiredType == "uint")
                    {
                        CodeStream.AddInstructionSysCall0("Long", "ToUInt");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                case "float":
                {
                    if (desiredType == "long")
                    {
                        CodeStream.AddInstructionSysCall0("Float", "ToLong");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                case "byte":
                {
                    if (desiredType == "float")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToFloat");
                    }
                    else if (desiredType == "long")
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                case "uint":
                {
                    if (desiredType == "float")
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                        CodeStream.AddInstructionSysCall0("Long", "ToFloat");
                    }
                    else if (desiredType == "long")
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                case "int":
                {
                    if (desiredType == "float")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToFloat");
                    }
                    else if (desiredType == "long")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToLong");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                case "+int":
                {
                    if (desiredType == "float")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToFloat");
                    }
                    else if (desiredType == "long")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToLong");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                case "-int":
                {
                    if (desiredType == "float")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToFloat");
                    }
                    else if (desiredType == "long")
                    {
                        CodeStream.AddInstructionSysCall0("Int", "ToLong");
                    }
                    else
                    {
                        PrintLn(actualType + " -> " + desiredType);
                        Die(0x0A);
                    }
                }
                default:
                {
                    // CODEGEN for [top] : actualType->desiredType
                    PrintLn(actualType + " -> " + desiredType);
                    Die(0x0A);
                }
            }
            if (codeGenNext)
            {
                CodeStream.AddInstruction(Instruction.SWAP); // [next] <-> [top]
            }
        }
        if (!ok && (actualType == "enum"))
        {
            ok = Types.IsEnum(desiredType);
        }
        if (!ok && (actualType == "flags"))
        {
            ok = Types.IsFlags(desiredType);
        }
        if (!ok && (desiredType == "enum"))
        {
            ok = Types.IsEnum(actualType);
        }
        if (!ok && (desiredType == "flags"))
        {
            ok = Types.IsFlags(actualType);
        }
        return ok;
    }
    
    bool AutomaticUpCastTop(string actualType, string desiredType)
    {
        return AutomaticUpCast(actualType, desiredType, true, false);
    }
    bool AutomaticUpCastNext(string actualType, string desiredType)
    {
        return AutomaticUpCast(actualType, desiredType, false, true);
    }
    
    bool IsNumericType(string typeString)
    {
        string valueTypes = "|byte|uint|int|+int|-int|long|float|";
        return valueTypes.Contains("|" + typeString + "|");
    }
    
    bool IsSignedIntType(string typeString)
    {
        string valueTypes = "|int|+int|-int|";
        return valueTypes.Contains("|" + typeString + "|");
    }
    bool IsOrdinalType(string typeString)
    {
        string valueTypes = "|byte|char|uint|int|+int|-int|string|long|float|";
        return valueTypes.Contains("|" + typeString + "|");
    }
    bool IsBitwiseType(string typeString) 
    {
        // used by '&' (bitwise and), '|' (bitwise or), '==' and '!=' (below)
        bool isBitwise;
        string valueTypes = "|byte|uint|int|+int|-int|flags|";
        isBitwise = valueTypes.Contains("|" + typeString + "|");
        if (!isBitwise)
        {
            isBitwise = IsFlags(typeString);
        }
        return isBitwise;
    }
    bool IsEqualityType(string typeString)
    {
        bool isEquality;
        if (IsOrdinalType(typeString))
        {
            isEquality = true;
        }
        else if (IsBitwiseType(typeString))
        {
            isEquality = true;
        }
        else if ((typeString == "bool") || (typeString == "enum") || (typeString == "type"))
        {
            isEquality = true;
        }
        else if (IsEnum(typeString))
        {
            isEquality = true;
        }
        else if (IsDelegate(typeString))
        {
            isEquality = true;
        }
        return isEquality;
    }
    bool IsByteRange(string typeString, ref byte upperBound)
    {
        upperBound = 0;
        if (typeString == "byte")
        {
            upperBound = 255;
        }  
        else if (typeString == "char")
        {
            upperBound = 255;
        }  
        else if (typeString == "type")
        {
            upperBound = 25; // nasty limit but probably won't change much ("list" type)
        }
        else if (typeString == "bool")
        {
            upperBound = 1;
        }
        else
        {
            if (Types.IsEnum(typeString))
            {
                uint limit = Symbols.GetEnumUpperBound(typeString, currentNamespace);
                if (limit < 256)
                {
                    upperBound = byte(limit);
                }
            }
            else if (Types.IsFlags(typeString))
            {
                uint limit = Symbols.GetFlagsUpperBound(typeString, currentNamespace);
                if (limit < 256)
                {
                    upperBound = byte(limit);
                }
            }
        }
        return upperBound != 0;
    }
    
    bool IsValueType(string typeString)
    {
        bool isValue;
        string valueTypes = "|bool|byte|char|uint|int|type|delegate|";
        string keyString = "|" + typeString + "|";
        isValue = valueTypes.Contains(keyString);
        if (!isValue)
        {
            isValue = Types.IsEnum(typeString) 
                   || Types.IsFlags(typeString)
                   || Types.IsDelegate(typeString);
        }   
        return isValue;
    }
    bool IsWordValueType(string typeString)
    {
        bool isValue;
        string valueTypes = "|uint|int|delegate|";
        string keyString = "|" + typeString + "|";
        isValue = valueTypes.Contains(keyString);
        if (!isValue)
        {
            isValue = Types.IsEnum(typeString) 
                   || Types.IsFlags(typeString)
                   || Types.IsDelegate(typeString);
        }   
        return isValue;
    }
    bool IsSimpleType(string typeString) // not compound like list, array, dictionary, pair or potentially variant
    {
        bool isValue;
        string simpleTypes = "|bool|byte|char|uint|int|long|float|string|type|file|directory|delegate|";
        string keyString = "|" + typeString + "|";
        isValue = simpleTypes.Contains(keyString);
        return isValue;
    }
    byte ToByte(string typeName)
    {
        byte b;
        switch (typeName)
        {
            case "byte":
            {
                b = byte(byte);
            }
            case "char":
            {
                b = byte(char);   
            }
            case "bool":
            {
                b = byte(bool);
            }
            case "int":
            {
                b = byte(int);
            }
            case "uint":
            {
                b = byte(uint);
            }
            case "long":
            {
                b = byte(long);
            }
            case "float":
            {
                b = byte(float);
            }
            case "file":
            {
                b = byte(file);
            }
            case "directory":
            {
                b = byte(directory);
            }
            case "string":
            {
                b = byte(string);
            }
            case "variant":
            {
                b = byte(variant);
            }
            case "type":
            {
                b = byte(type);
            }
            case "list":
            {
                b = byte(list);
            }
            case "array":
            {
                b = byte(array);
            }
            case "dictionary":
            {
                b = byte(dictionary);
            }
            case "enum":
            {
                b = byte(enum);
            }
            case "flags":
            {
                b = byte(flags);
            }
            case "pair":
            {
                b = byte(pair);
            }
            case "delegate":
            {
                b = byte(delegate);
            }
            default:
            {
                // compound types
                if (Types.IsList(typeName))
                {
                    b = byte(list);
                }
                else if (Types.IsArray(typeName))
                {
                    b = byte(array);
                }
                else if (Types.IsDictionaryOrPair(typeName))
                {
                    b = byte(dictionary);
                }
                
                // named types
                else if (Types.IsEnum(typeName))
                {
                    b = byte(enum);
                }
                else if (Types.IsFlags(typeName))
                {
                    b = byte(flags);
                }
                else if (Types.IsDelegate(typeName))
                {
                    b = byte(delegate);
                }
                else
                {
                    PrintLn(typeName);
                    Die(0x0A);
                }
            }
        }
        if (b == 26)
        {
            b = 19; // tHashDictionary -> tDictionary
        }
        if (b == 27)
        {
            b = 16; // tHashPair -> tPair
        }
        return b;
    }
    bool tryParseTypeString(ref string typeString)
    {
        bool success = false;
        loop
        {
            bool isSimple = false;
            <string,string> typeToken = Parser.CurrentToken; // Keyword, Identifier (named type) or '<'
            if (Parser.Check(HopperToken.Keyword, "|bool|byte|char|delegate|uint|int|long|float|string|type|variant|file|directory|"))
            {
                isSimple = true;    
            }
            else if (Parser.Check(HopperToken.Identifier) || Parser.Check(HopperToken.DottedIdentifier))
            {
                // enum or flags?
                isSimple = Types.IsEnum(typeToken["lexeme"]) 
                        || Types.IsFlags(typeToken["lexeme"])
                        || Types.IsDelegate(typeToken["lexeme"]);
            }
            if (isSimple)
            {
                success = true;
                typeString = typeString + typeToken["lexeme"];
                Parser.Advance();
                if (Parser.Check(HopperToken.LBracket))
                {
                    // like bool[8000]
                    if (!IsValueType(typeString))
                    {
                        Parser.ErrorAtCurrent("arrays can only contain value types");
                        success = false;
                        break;
                    }
                    Parser.Advance(); // [
                    typeString = typeString + "[";
                    string size = ParseConstantExpression("uint");
                    if (Parser.HadError)
                    {
                        success = false;
                        break;
                    }
                    typeString = typeString + size;
                    if (!Parser.Check(HopperToken.RBracket))
                    {
                        Parser.ErrorAtCurrent(']');    
                        success = false;
                        break;
                    }
                    Parser.Advance(); // ]
                    typeString = typeString + "]";
                }
            }
            else if (Parser.Check(HopperToken.LT)) // <
            {
                success = true;
                Parser.Advance(); // <
                // <byte> and <string, byte>
                typeString = typeString + "<";   
                
                if (!tryParseTypeString(ref typeString))
                {
                    success = false;
                    break;
                }
                if (Parser.Check(HopperToken.Comma)) // ,
                {
                    // TODO : make <byte,byte,byte> illegal
                    Parser.Advance(); // , 
                    typeString = typeString + ",";   
                    if (!tryParseTypeString(ref typeString))
                    {
                        success = false;
                        break;
                    }
                }
                if (!Parser.Check(HopperToken.GT))
                {
                    Parser.ErrorAtCurrent('>');    
                    success = false;
                    break;
                }
                Parser.Advance(); // >
                typeString = typeString + ">";
            }           
            break;
        }
        return success;
    }    
    string ParseType()
    {
        string typeString;
        if (tryParseTypeString(ref typeString))
        {
        }
        return typeString;        
    }    
    
    uint GetArraySizeFromCollection(string collectionType)
    {
        uint fb;
        uint lb;
        uint sz;
        if (!collectionType.IndexOf('[', ref fb))
        {
            Parser.ErrorAtCurrent("invalid array type '" + collectionType + "'");
        }
        string szs = collectionType.Substring(fb+1);
        if (!szs.IndexOf(']', ref lb))
        {
            Parser.ErrorAtCurrent("invalid array type '" + collectionType + "'");
        }
        szs = szs.Substring(0, lb);
        if (!UInt.TryParse(szs, ref sz))
        {
            Parser.ErrorAtCurrent("invalid array type '" + collectionType + "'");
        }
        return sz;
    }
    
    string GetKeyFromCollection(string collectionType)
    {
        // collectionType could be dictionary or pair (they look the same
        string keyType;
        uint cIndex;    
        if (IsDictionaryOrPair(collectionType) && collectionType.IndexOf(',', ref cIndex))
        {
            keyType = collectionType.Substring(1, cIndex-1);        
        }
        else
        {
            Parser.ErrorAtCurrent("invalid collection type for key '" + collectionType + "'");
        }
        return keyType;
    }
    
    string GetValueFromCollection(string collectionType)
    {
        // collectionType could be dictionary or pair (they look the same)
        string valueType;
        uint cIndex;    
        if (IsDictionaryOrPair(collectionType) && collectionType.IndexOf(',', ref cIndex))
        {
            valueType = collectionType.Substring(cIndex+1);
            valueType = valueType.Substring(0, valueType.Length-1);     
        }
        else if (IsList(collectionType))
        {
            valueType = collectionType.Substring(1);
            valueType = valueType.Substring(0, valueType.Length-1);         
        }
        else if (IsArray(collectionType))
        {
            uint fb;
            if (!collectionType.IndexOf('[', ref fb))
            {
                Parser.ErrorAtCurrent("invalid collection type for value '" + collectionType + "'");
            }
            else
            {
                valueType = collectionType.Substring(0, fb);
            }
        }
        else
        {
            Parser.ErrorAtCurrent("invalid collection type for value '" + collectionType + "'");
        }
        return valueType;
    }
    
    string GetIteratorFromCollection(string collectionType)
    {
        string iteratorType;
        uint bIndex;
        uint cIndex;
        if (collectionType == "string")
        {
            iteratorType = "char";
        }
        else if (collectionType.IndexOf('[', ref bIndex))
        {
            // 'byte[uint]' -> 'byte'
            iteratorType = collectionType.Substring(0, bIndex);    
        }
        else if (IsDictionaryOrPair(collectionType) && collectionType.IndexOf(',', ref bIndex))
        {
            // '<string,string>' -> '<string,string>' (pair type looks the same as dictionary type)
            iteratorType = collectionType;        
        }
        else if (IsList(collectionType) && collectionType.StartsWith('<'))
        {
            // '<string>' -> 'string'
            iteratorType = collectionType.Substring(1, collectionType.Length-2);
        }
        else
        {
            Parser.ErrorAtCurrent("invalid collection type for iterator '" + collectionType + "'");
        }   
        return iteratorType;
    }
    
    bool IsDictionaryOrPair(string typeString)
    {
        bool isDictionary = false;
        // <<string,int>> vs <string,int>
        uint length = typeString.Length;
        uint i = 0;
        uint nesting = 0;
        loop
        {
            if (i == length)
            {
                break;
            }   
            char c = typeString[i];
            if (c == '<')
            {
                nesting++;
            }
            else if (c == '>')
            {
                nesting--;
            }
            else if (c == ',')
            {
                if (nesting == 1)
                {
                    isDictionary = true;
                }
            }
            i++;
        }
        if (isDictionary)
        {
            isDictionary = (typeString[0] == '<') && (typeString[length-1] == '>') && (nesting == 0);
        }
        return isDictionary;
    }
    
    bool IsList(string typeString)
    {
        // <<string,int>> vs <string,int>
        uint length = typeString.Length;
        bool isList = (length > 2);
        uint i = 0;
        uint nesting = 0;
        loop
        {
            if (i == length)
            {
                break;
            }   
            char c = typeString[i];
            if (c == '<')
            {
                nesting++;
            }
            else if (c == '>')
            {
                nesting--;
            }
            else if (c == ',')
            {
                if (nesting == 1)
                {
                    isList = false;
                }
            }
            i++;
        }      
        if (isList)
        {
            isList = (typeString[0] == '<') && (typeString[length-1] == '>') && (nesting == 0);
        }
        return isList;
    }
    bool IsArray(string typeString)
    {
        bool isArray= false;
        if (typeString.Contains('['))
        {
            isArray = true;
        }   
        return isArray;
    }
    
    string QualifyFlags(string identifier)
    {
        return Symbols.QualifyFlags(identifier, currentNamespace);
    }
    string QualifyEnum(string identifier)
    {
        return Symbols.QualifyEnum(identifier, currentNamespace);
    }
    
    string QualifyMethodName(string identifier)
    {
        return Symbols.QualifyMethodName(identifier, currentNamespace);
    }
    string QualifyDelegateName(string identifier)
    {
        return Symbols.QualifyDelegate(identifier, currentNamespace);
    }
    
    bool IsEnum(string identifier)
    {
        return Symbols.IsEnumType(identifier, currentNamespace);
    }
    
    bool IsFlags(string identifier)
    {
        return Symbols.IsFlagsType(identifier, currentNamespace);
    }
    bool IsDelegate(string identifier)
    {
        return Symbols.IsDelegateType(identifier, currentNamespace);
    }
    
          
    bool ArgumentsEqual(< <string> > actual, < <string> > target)
    {
        return ArgumentsEqual(actual, target, true, true);
    }
    bool ArgumentsEqual(< <string> > actual, < <string> > target, bool upCastAllowed, bool nameCastAllowed)
    {
        bool equal = true;
        loop
        {
            uint lActual = actual.Length;
            uint lTarget = target.Length;
            if (lActual != lTarget)
            {
                equal = false;
                break;
            }
            for (uint i=0; i < lActual; i++)
            {
                <string> actualList = actual[i];
                <string> targetList = target[i];
                if (actualList.Length != 3)
                {
                    equal = false;
                    break;
                }
                if (targetList.Length != 3)
                {
                    equal = false;
                    break;
                }
                if (actualList[0] != targetList[0]) // ref
                {
                    equal = false;
                    break;
                }

                bool localUpCastAllowed = upCastAllowed;
                if (!localUpCastAllowed && nameCastAllowed)
                {
                    localUpCastAllowed = (actualList[1] == "enum") ||
                                         (actualList[1] == "flags") ||
                                         (actualList[1] == "delegate") ||
                                         (targetList[1] == "enum") ||
                                         (targetList[1] == "flags") ||
                                         (targetList[1] == "delegate");
                }
                if (actualList[0] == "ref")
                {
                    localUpCastAllowed = false;
                }
                if (actualList[1] != targetList[1]) // type
                {
                    if (IsDebugger)
                    {
                        if (localUpCastAllowed)
                        {
                            Die(0x0B);
                        }
                        equal = false;
                        break;
                    }
                    else
                    {
                        if (!localUpCastAllowed)
                        {
                            equal = false;
                            break;          
                        }
                        else if (AutomaticUpCast(actualList[1], targetList[1], false, false))
                        {
                            // match
                        }
                        else
                        {
                            string targetString = targetList[1];
                            if ( (targetString == "K") 
                              || (targetString == "V")
                              || (targetString == "<K,V>")
                              || (targetString == "<V>")
                              || (targetString == "V[]")
                               )
                            {
                                // generics : TODO : better matching?
                            }
                            else if (targetString == "byte[]")
                            {
                                // for System.Call(byte[] code)
                                string actualString = actualList[1];
                                if (!actualString.StartsWith("byte["))
                                {
                                    equal = false;
                                    break;
                                }
                            }
                            else if (Types.IsEnum(actualList[1]) && Types.IsEnum(targetList[1]))
                            {
                                // TODO: compare fully qualified
                                Die(0x0A);
                                equal = false;
                                break;
                            }
                            else if (Types.IsFlags(actualList[1]) && Types.IsFlags(targetList[1]))
                            {
                                // TODO: compare fully qualified
                                Die(0x0A);
                                equal = false;
                                break;
                            }
                            else if (Types.IsDelegate(actualList[1]) && Types.IsDelegate(targetList[1]))
                            {
                                // compare fully qualified
                                string leftType  = Symbols.QualifyDelegate(actualList[1], currentNamespace);
                                string rightType = Symbols.QualifyDelegate(targetList[1], currentNamespace);
                                if (leftType == rightType)
                                {
                                    // match
                                }
                                else
                                {
                                    equal = false;
                                    break;
                                }
                            }
                            else 
                            {
                                equal = false;
                                break;
                            }
                        }
                    } // !IsDebugger

                }
            }
            break;
        }
        return equal;
    }
    uint FindVisibleOverload(string functionName, < <string> > arguments, ref string returnType)
    {
        uint iOverloadFound;
        uint iDot;
        char ch;
        loop
        {
            if (!functionName.Contains('.'))
            {
                functionName = currentNamespace + "." + functionName;
            }
            if (functionName.IndexOf('.', ref iDot))
            {
                ch = functionName[iDot+1];
                if (ch.IsLower())
                {
                    if (!functionName.StartsWith(currentNamespace + "."))
                    {
                        Parser.Error("'" + functionName + "' is private"); 
                        break;
                    }
                }
            }
            iOverloadFound = FindOverload(functionName, arguments, ref returnType);
            break;
        }
        return iOverloadFound;
    }
    
    
    uint FindOverload(string functionName, < <string> > arguments, ref string returnType)
    {
        uint iOverloadFound;
        
        loop
        {
            uint fIndex;
            bool found = false;
            bool foundName = false;
            if (Symbols.GetFunctionIndex(functionName, ref fIndex))
            {
                foundName = true;
                <uint> overloads = Symbols.GetFunctionOverloads(fIndex);
                for (uint pass = 0; pass < 3; pass++)
                {
                    foreach (var iOverload in overloads)
                    {
                        < < string > > overloadArguments = Symbols.GetOverloadArguments(iOverload);
                        if (ArgumentsEqual(arguments, overloadArguments, (pass == 2), (pass != 0)))
                        {
                            found = true;
                            iOverloadFound = iOverload;
                            returnType = GetOverloadReturnType(iOverload);
                            //if (functionName == "CodeStream.AddInstruction")
                            //{
                            //    OutputDebug(functionName + ":" + pass.ToString() + ":FOUND:" + iOverloadFound.ToHexString(4));
                            //    OutputDebug(arguments);
                            //    OutputDebug(overloadArguments);    
                            //}
                            break;
                       }
                    }
                    if (found)
                    {
                        break;
                    }
                }
            }
            if (!found)
            {
                // consider delegates (variables, not methods)
                string qualifiedName;
                string variableType = Types.GetTypeString(functionName, false, ref qualifiedName);
                if (variableType.Length > 0)
                {
                    if (!Types.IsDelegate(variableType))
                    {
                        Parser.Error("function expected, (not variable '" + functionName + "')");
                        break;
                    }
                    uint fdIndex;
                    //string qualifiedName = QualifyDelegateName(functionName);
                    if (!Symbols.GetFunctionDelegateIndex(variableType, ref fdIndex))
                    {
                        Parser.Error("delegate definition not found");
                        break;
                    }
                    <uint> overloads = Symbols.GetFunctionDelegateOverloads(fdIndex);
                    for (uint pass = 0; pass < 3; pass++)
                    {
                        foreach (var iOverload in overloads)
                        {
                            < < string > > overloadArguments = Symbols.GetDelegateArguments(iOverload);
                            if (ArgumentsEqual(arguments, overloadArguments, (pass == 2), (pass != 0)))
                            {
                                found = true;
                                iOverloadFound = iOverload;
                                returnType = GetDelegateReturnType(iOverload);
                                break;
                           }
                        }
                        if (found)
                        {
                            break;
                        }
                    }
                    if (found)
                    {
                        break;
                    }
                    Parser.Error("no matching delegate '"+ variableType +"' overload found for '" + functionName + "'");
                }
                else if (foundName)
                {
                    Parser.Error("no matching overload found for '" + functionName + "'");
                    //Die(0x0A);
                }
                else
                {
                    Parser.Error("'" + functionName + "' is not defined");
                }
            }
            break;   
        }
        return iOverloadFound;
    }
    
    string GetTypeString(string variableName, bool mustExist, ref string fullName)
    {
        // walk the block stack to find local or global and determine type
        string variableType = Block.GetType(variableName);
        if (variableType.Length > 0)
        {
            fullName = variableName; // local? 
        }
        if ((variableType.Length == 0) && !variableName.Contains('.') && (variableName.Length > 0))
        {
            char f = variableName[0];
            bool private = f.IsLower();
            
            // try current namespace first
            string qualifiedName = currentNamespace + "." + variableName;
            string qVariableType = Block.GetType(qualifiedName);
            if (qVariableType.Length > 0)
            {
                variableType = qVariableType;
                fullName = qualifiedName;
            }   
            else if (!private)
            {
                // we can only use the other namespaces for public names
                int winners = 0;
                <string> nameSpaces = Symbols.GetNameSpaces();
                foreach (var nameSpace in nameSpaces)
                {
                    qualifiedName = nameSpace + "." + variableName;
                    qVariableType = Block.GetType(qualifiedName);
                    if (qVariableType.Length > 0)
                    {
                        variableType = qVariableType;
                        fullName = qualifiedName;
                        winners++;          
                    }
                }
                if (winners > 1)
                {
                    Parser.Error("ambiguous unqualified identifier '" + variableName + "'");
                }
            }
        }
        if (mustExist && (variableType.Length == 0))
        {
            Parser.Error("undefined identifier '" + variableName + "'");
        }
        return variableType;
    }
    
    
    RuntimeValueCast(string castToType)
    {
        byte vt = Types.ToByte(castToType);
                        
        CodeStream.AddInstruction(Instruction.DUP, byte(0)); // copy of expression result
#ifndef JSONEXPRESS
        CodeStream.AddInstruction(Instruction.DUP, byte(0)); // 2nd copy
#endif
        CodeStream.AddInstructionSysCall0("Types", "TypeOf");
        CodeStream.AddInstructionPUSHI(byte(variant));
        CodeStream.AddInstruction(Instruction.EQ);           // is it a variant?
        uint jumpValue = CodeStream.NextAddress;
        CodeStream.AddInstructionJump(Instruction.JZB); // if not, jump past to valuetype
#ifndef JSONEXPRESS
        CodeStream.AddInstructionSysCall0("Types", "ValueTypeOf");// get the type of the variant value
        CodeStream.AddInstructionPUSHI(vt);
        CodeStream.AddInstruction(Instruction.EQ);
        CodeStream.AddInstructionJumpOffset(Instruction.JNZB, byte(4)); 
        CodeStream.AddInstruction(Instruction.DIE, byte(0x09)); // invalid variant type (incorrect boxed type)
#endif
        CodeStream.AddInstructionSysCall0("Variant", "UnBox");
        uint jumpEnd = CodeStream.NextAddress;
        CodeStream.AddInstructionJump(Instruction.JB);
        
// valuetype: 
        uint valueAddress = CodeStream.NextAddress;
        CodeStream.PatchJump(jumpValue, valueAddress);
#ifndef JSONEXPRESS
        CodeStream.AddInstructionSysCall0("Types", "TypeOf");
        CodeStream.AddInstructionPUSHI(vt);
        CodeStream.AddInstruction(Instruction.EQ);
        CodeStream.AddInstructionJumpOffset(Instruction.JNZB, byte(4)); 
        CodeStream.AddInstruction(Instruction.DIE, byte(0x08)); // failed dynamic cast       
#endif
// end        
        uint endAddress = CodeStream.NextAddress;
        CodeStream.PatchJump(jumpEnd, endAddress);
    }
    
    VerifyTopValueType(string variableType, string typeName) // typeName = "list" or "dictionary"
    {
        if (CodeStream.CheckedBuild)
        {
            byte vt = Types.ToByte(variableType);
            
            CodeStream.AddInstruction(Instruction.DUP, byte(0)); // copy of [top]
            CodeStream.AddInstruction(Instruction.DUP, byte(0)); // 2nd copy
            CodeStream.AddInstructionSysCall0("Types", "ValueTypeOf");
            CodeStream.AddInstructionPUSHI(byte(variant));
            CodeStream.AddInstruction(Instruction.EQ);           // is it a variant?
            uint jumpValue = CodeStream.NextAddress;
            CodeStream.AddInstructionJump(Instruction.JZB); // if not, jump past to valuetype
            
            CodeStream.AddInstructionPUSHI(vt);
            CodeStream.AddInstructionSysCall0("Types", "VerifyValueTypes");
            uint jumpEnd = CodeStream.NextAddress;
            CodeStream.AddInstructionJump(Instruction.JNZB); 
            CodeStream.AddInstruction(Instruction.DIE, byte(0x09)); // invalid value type/s
                                        
            // TODO : support trivial automatic upcasts like 'byte' -> 'uint'
// valuetype: 
            uint valueAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpValue, valueAddress);       
            CodeStream.AddInstructionSysCall0("Types", "ValueTypeOf");            
            CodeStream.AddInstructionPUSHI(vt);
            CodeStream.AddInstruction(Instruction.EQ);
            CodeStream.AddInstructionJumpOffset(Instruction.JNZB, byte(4)); 
            CodeStream.AddInstruction(Instruction.DIE, byte(0x09)); // invalid variant type
// end        
            uint endAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpEnd, endAddress);            
        }
    }
    
    DynamicCastIntToUInt() // verify that the top/sign bit is not set (like 'int' -> enum)
    {
        if (CodeStream.CheckedBuild)
        {
            // DUP [top]
            CodeStream.AddInstruction(Instruction.DUP, byte(0)); 
            
            // [top] >> 15
            CodeStream.AddInstructionPUSHI(0x0F);
            CodeStream.AddInstruction(Instruction.BITSHR); 
            
            // MSB == 0 -> ok
            CodeStream.AddInstructionJumpOffset(Instruction.JZB, byte(4)); 
            CodeStream.AddInstruction(Instruction.DIE, byte(0x08)); // failed dynamic cast
        }
        CodeStream.AddInstruction(Instruction.CAST, byte(uint)); // cast top of stack to UInt
    }
    
    DynamicCastToByte() // works for 'uint' -> 'byte' and 'int' -> 'byte'
    {
        if (CodeStream.CheckedBuild)
        {
            // DUP [top]
            CodeStream.AddInstruction(Instruction.DUP, byte(0)); 
            
            // [top] >> 8
            CodeStream.AddInstructionPUSHI(0x08);
            CodeStream.AddInstruction(Instruction.BITSHR); 
            
            // MSB == 0 -> ok
            CodeStream.AddInstructionJumpOffset(Instruction.JZB, byte(4)); 
            CodeStream.AddInstruction(Instruction.DIE, byte(0x08)); // failed dynamic cast
        }
        CodeStream.AddInstruction(Instruction.CAST, byte(byte)); // cast top of stack to byte
    }
    DynamicCastToType() // works for 'byte' -> 'type'
    {
        if (CodeStream.CheckedBuild)
        {
            // DUP [top]
            CodeStream.AddInstruction(Instruction.DUP, byte(0)); 
            
            // byte > 25?
            CodeStream.AddInstructionPUSHI(0x19); // tList = 25
            CodeStream.AddInstruction(Instruction.GT); 
            
            // 0 -> ok
            CodeStream.AddInstructionJumpOffset(Instruction.JZB, byte(4)); 
            CodeStream.AddInstruction(Instruction.DIE, byte(0x08)); // failed dynamic cast
        }
        CodeStream.AddInstruction(Instruction.CAST, byte(byte)); // cast top of stack to byte
    }
    
    VerifyTopType(string variableType, string prefix)
    {
        if (CodeStream.CheckedBuild)
        {
            CodeStream.AddInstruction(Instruction.DUP, byte(0)); // copy of [top]
            CodeStream.AddInstructionSysCall0("Types", prefix + "TypeOf");
            
            // TODO : support trivial automatic upcasts like 'byte' -> 'uint'
            byte vt = Types.ToByte(variableType);
            CodeStream.AddInstructionPUSHI(vt);
            CodeStream.AddInstruction(Instruction.EQ);
            CodeStream.AddInstructionJumpOffset(Instruction.JNZB, byte(4)); 
            CodeStream.AddInstruction(Instruction.DIE, byte(0x09)); // invalid variant type
        }
    }
    
    uint IntToUInt(int value)
    {
        <byte> bytes = value.ToBytes();
        return uint(bytes[0]) + uint(bytes[1]) << 8;
    }
    
    int UIntToInt(uint value)
    {
        if ((value & 0x8000) != 0) // sign bit set?
        {
            // two's complement
            value = ~value; // 0xFFFF -> 0x0000, 0xFFFE -> 0x0001
            long lvalue = 0 - long(value) - 1;
            return int(lvalue);
        }
        else
        {
            return int(value); // '+int'
        }
    }
    
    
}
