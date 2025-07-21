unit Constant
{
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    
    uses "Tokens/Token"
    uses "Tokens/Scanner"
    uses "Tokens/Parser"
    
    uses "Symbols"
    uses "Types"
    uses "Collection"
    
    string constantOperation(string leftValue, string rightValue, string typeExpected, HopperToken operation)
    {   
        uint ul;
        uint ur;
        string result;
        long   lresult;
        float  fresult;
        
        if (leftValue.StartsWith("unresolved,"))
        {
            // "unresolved,lexeme,line,source,identifier"
            <string> parts = leftValue.Split(',');
            string identifier = parts[4];
            if (!Symbols.ConstantExists(identifier))
            {
                Parser.ErrorAtCurrent("undefined constant identifier '" + identifier + "'");
                return result;
            }
        }
        if (rightValue.StartsWith("unresolved,"))
        {
            // "unresolved,lexeme,line,source,identifier"
            <string> parts = rightValue.Split(',');
            string identifier = parts[4];
            if (!Symbols.ConstantExists(identifier))
            {
                Parser.ErrorAtCurrent("undefined constant identifier '" + identifier + "'");
                return result;
            }
        }
        
        string message = typeExpected + "." + HopperTokenToString(operation) + "(" + leftValue + ", " + rightValue + ")";
        loop
        {
            switch (typeExpected)
            {
                case "float":
                {
                    float left;
                    float right;
                    if (!Float.TryParse(leftValue, ref left) || !Float.TryParse(rightValue, ref right))
                    {
                        Parser.ErrorAtCurrent(message + " failed");
                    } 
                    switch (operation)
                    {
                        case HopperToken.Add:
                        {
                            fresult = left + right;
                        }
                        case HopperToken.Subtract:
                        {
                            fresult = left - right;
                        }
                        case HopperToken.Multiply:
                        {
                            fresult = left * right;
                        }
                        case HopperToken.Divide:
                        {
                            if (right == 0)
                            {
                                Parser.ErrorAtCurrent("division by zero in constant");
                            }
                            else
                            {
                                fresult = left / right;
                            }
                        }
                        default:
                        {
                            Parser.ErrorAtCurrent(message + " not implemented");
                        }
                    }
                    
                }
                case "byte":
                case "uint":
                case "int":
                case "long":
                {
                    long left;
                    long right;
                    if (!Long.TryParse(leftValue, ref left) || !Long.TryParse(rightValue, ref right))
                    {
                        Parser.ErrorAtCurrent(message + " failed");
                    } 
                    switch (operation)
                    {
                        case HopperToken.ShiftLeft:
                        {
                            ul = uint(left);
                            ur = uint(right);
                            lresult = (ul << ur);
                        }
                        case HopperToken.ShiftRight:
                        {
                            ul = uint(left);
                            ur = uint(right);
                            lresult = (ul >> ur);
                        }
                        case HopperToken.BitOr:
                        {
                            ul = uint(left);
                            ur = uint(right);
                            lresult = (ul | ur);
                        }
                        case HopperToken.BitAnd:
                        {
                            ul = uint(left);
                            ur = uint(right);
                            lresult = (ul & ur);
                        }
                        case HopperToken.Add:
                        {
                            lresult = left + right;
                        }
                        case HopperToken.Subtract:
                        {
                            lresult = left - right;
                        }
                        case HopperToken.Multiply:
                        {
                            lresult = left * right;
                        }
                        case HopperToken.Divide:
                        {
                            if (right == 0)
                            {
                                Parser.ErrorAtCurrent("division by zero in constant");
                            }
                            else
                            {
                                lresult = left / right;
                            }
                        }
                        case HopperToken.Modulus:
                        {
                            if (right == 0)
                            {
                                Parser.ErrorAtCurrent("division by zero in constant");
                            }
                            else
                            {
                                lresult = left % right;
                            }
                        }
                        default:
                        {
                            Parser.ErrorAtCurrent(message + " not implemented");
                        }
                    }
                }
                default:
                {
                    Parser.ErrorAtCurrent(message + " not implemented");
                }
            }
            if (Parser.HadError)
            {
                break;
            }
        
            if (typeExpected == "byte")
            {
                if ((lresult < 0) || (lresult > 255))
                {
                    Parser.ErrorAtCurrent("'byte' constant out of range");           
                    break;
                }
                result = lresult.ToString();
            }
            else if (typeExpected == "int")
            {
                if ((lresult < -32768) || (lresult > 32767))
                {
                    Parser.ErrorAtCurrent("'int' constant out of range");           
                    break;
                }
                result = lresult.ToString();
            }
            else if (typeExpected == "uint")
            {
                if ((lresult < 0) || (lresult > 0xFFFF))
                {
                    Parser.ErrorAtCurrent("'uint' constant out of range");           
                    break;
                }
                result = lresult.ToString();    
            }
            else if (typeExpected == "long")
            {
                result = lresult.ToString();
            }
            else if (typeExpected == "float")
            {
                result = fresult.ToString();
            }
            break;
        } // loop
        return result;
    }
    
    bool validateIntegralConstant(string typeExpected, string identifierType, ref string actualType, string value, <string,string> token)
    {
        uint ui;
        int i;
        bool success;
        loop
        {
            string integralTypes = "|uint|int|byte|char|";
            string keyString = "|" + identifierType + "|";
            if (!integralTypes.Contains(keyString))
            {
                Parser.ErrorAt(token, "integral constant expected");
                break;
            }
        
            switch (typeExpected)
            {
                case "char":
                {
                    if (value.Length != 1)   
                    {
                        Parser.ErrorAt(token, "char constant out of range");
                        break;
                    }
                    actualType = "char";
                }
                case "byte":
                {
                    if (!UInt.TryParse(value, ref ui) || (ui > 255))   
                    {
                        Parser.ErrorAt(token, "constant out of range");
                        break;
                    }
                    actualType = "byte";
                }
                case "uint":
                {
                    if (!UInt.TryParse(value, ref ui))   
                    {
                        Parser.ErrorAt(token, "constant out of range");           
                        break;
                    }
                    actualType = "uint";
                }
                case "int":
                {
                    if (!Int.TryParse(value, ref i))   
                    {
                        Parser.ErrorAt(token, "constant out of range");           
                        break;
                    }
                    actualType = "int";
                }
                default:
                {
                    Parser.ErrorAt(token, "constant identifier case not implemented");
                }
            }
            success = true;
            break;
        } // loop
        return success;
    }

    string parseConstantPrimary(string typeExpected, ref string actualType, bool weakEnums)
    {
        <string,string> currentToken = Parser.CurrentToken;
        string value;
        HopperToken ttype = Token.GetType(currentToken);
        if (ttype == HopperToken.DottedIdentifier)
        {
            ttype = HopperToken.Identifier;
        }
        
        loop
        {
            switch(ttype)
            {
                case HopperToken.Bool:
                {
                    if ((currentToken["lexeme"] == "true") || (currentToken["lexeme"] == "false"))
                    {
                        value = currentToken["lexeme"];
                        Parser.Advance();
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("unexpected boolean literal");
                        break;           
                    }
                    actualType = "bool";
                }
                case HopperToken.StringConstant:
                {
                    Parser.Advance();
                    value = currentToken["lexeme"];
                    actualType = "string";
                }
                case HopperToken.LBrace:
                {
                    value = Collection.ParseConstant(typeExpected, ref actualType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                }
                case HopperToken.Char:
                {
                    Parser.Advance();
                    value = currentToken["lexeme"];
#ifdef ASSEMBLER
                    actualType = "byte";
                    value = (byte(value[0])).ToString();
#else
                    if (IsAssembly && weakEnums && (typeExpected == "byte"))
                    {
                        actualType = "byte";
                        value = (byte(value[0])).ToString();
                    }
                    else
                    {
                        actualType = "char";
                    }
#endif
                                    }
                case HopperToken.Float:
                {
                    float f;
                    if (!Float.TryParse(currentToken["lexeme"], ref f))
                    {
                        Parser.ErrorAtCurrent("invalid float literal");
                        break;           
                    }
                    actualType = "float";
                    value = currentToken["lexeme"];
                    Parser.Advance(); 
                }
                case HopperToken.Integer:
                {
                    long l;
                    if (!Long.TryParse(currentToken["lexeme"], ref l))
                    {
                        Parser.ErrorAtCurrent("invalid integer literal");
                        break;           
                    }
                    if (l >= 0)
                    {
                        if (typeExpected == "byte")
                        {
                            if (l > 255)
                            {
                                Parser.ErrorAtCurrent("'byte' constant out of range");           
                                break;
                            }
                            actualType = "byte";
                        }
                        else if (typeExpected == "int")
                        {
                            if (l > 32767)
                            {
                                Parser.ErrorAtCurrent("'int' constant out of range");           
                                break;
                            }
                            actualType = "int";    
                        }
                        else if (typeExpected == "uint")
                        {
                            if (l > 0xFFFF)
                            {
                                Parser.ErrorAtCurrent("'uint' constant out of range");           
                                break;
                            }
                            actualType = "uint";    
                        }
                        else if (typeExpected == "float")
                        {
                            float f;
                            if (!Float.TryParse(currentToken["lexeme"], ref f))
                            {
                                Parser.ErrorAtCurrent("invalid float literal");
                                break;           
                            }
                            actualType = "float";
                        }
                        else
                        {
                            actualType = "long";
                        }
                    }
                    else // l < 0
                    {
                        if (typeExpected == "int")
                        {
                            if (l < -32768)
                            {
                                Parser.ErrorAtCurrent("'int' constant out of range");           
                                break;
                            }
                            actualType = "int";    
                        }    
                        else
                        {
                            actualType = "long";
                        }
                    }
                    value = currentToken["lexeme"];
                    Parser.Advance();
                }
                
                case HopperToken.Identifier:
                {
                    string name = currentToken["lexeme"];
#ifdef ASSEMBLER
                    if (name.StartsWith('#')) // immediate addressing mode, not directive identifier
                    {
                        name = name.Substring(1);
                    }
#endif    
                    string typeName;
                    string valueName;
                    uint ivalue;
                    byte pass = 0;
                    bool outerLoopBreak = false;
                    bool isCount;
                    loop
                    {
                        if (Types.EnumValue(name, ref typeName, ref valueName, ref ivalue))
                        {
                            Parser.Advance();
                            actualType = typeName;
                            value = ivalue.ToString();
                            
                            if (weakEnums && (typeExpected != actualType))
                            {
                                actualType = "uint";
                                if (((typeExpected == "byte") || (typeExpected == "uint")) && (ivalue >= 0) && (ivalue <= 255))
                                {
                                    actualType = "byte";
                                }
                            }
                            outerLoopBreak = true;
                            break;
                        }
                        if (Types.FlagsValue(name, ref typeName, ref valueName, ref ivalue))
                        {
                            // Key
                            // Keyboard.Key
                            // Key.Escape
                            // Keyboard.Key.Escape
                            Parser.Advance();
                            actualType = typeName;
                            value = ivalue.ToString();
                            if (weakEnums && (typeExpected != actualType))
                            {
                                actualType = "uint";
                                if (((typeExpected == "byte") || (typeExpected == "uint")) && (ivalue >= 0) && (ivalue <= 255))
                                {
                                    actualType = "byte";
                                }
                            }
                            outerLoopBreak = true;
                            break;
                        }
                        string constantIdentifier = name;
                        if (constantIdentifier.EndsWith(".Count"))
                        {
                            constantIdentifier = constantIdentifier.Replace(".Count", "");
                            isCount = true;
                        }
                        constantIdentifier = Types.QualifyConstantIdentifier(constantIdentifier);
                        if (Symbols.ConstantExists(constantIdentifier))
                        {
                            if (!IsVisibleConstant(constantIdentifier))
                            {
                                Parser.ErrorAtCurrent("'" + constantIdentifier + "' is private");
                                outerLoopBreak = true;
                                break;
                            }
                            name = constantIdentifier; 
                            break;
                        }
                        else
                        {
                            
                            if (pass == 0)
                            {
                                //  Keyboard.Key ->  Keyboard.Key.Escape
                                if (Parser.PeekTokenType() == HopperToken.Dot)
                                {
                                    Parser.Advance(); // name
                                    Parser.Advance(); // dot
                                    <string, string> current = Parser.CurrentToken;
                                    name = constantIdentifier + "." + current["lexeme"];
                                    pass++;
                                    continue;
                                }
                            }
#ifdef ASSEMBLER
                            string qualifiedName;
                            string identifierTypeString = Types.GetTypeString(currentToken["lexeme"], false, ref qualifiedName);
                            if (Parser.HadError)
                            {
                                outerLoopBreak = true;
                                break;
                            }
                            if (identifierTypeString.Length != 0)
                            {
                                if (Symbols.GlobalMemberExists(qualifiedName))
                                {
                                    switch (identifierTypeString)
                                    {
                                        case "byte":
                                        case "uint":
                                        {
                                            value = (Symbols.GetGlobalByteAddress(qualifiedName)).ToString();
                                            if (!validateIntegralConstant(typeExpected, identifierTypeString, ref actualType, value, CurrentToken))
                                            {
                                                outerLoopBreak = true;
                                                break;
                                            }
                                            Parser.Advance();
                                        }
                                        default:
                                        {
                                            PrintLn("identifierTypeString=" + identifierTypeString);
                                            Die(0x0A);
                                        }
                                    }
                                }
                            }
#else                            
                            if (typeExpected != "string")
                            {
                                <string, string> current = Parser.CurrentToken;
                                value = "unresolved," + current["lexeme"] + "," + current["line"] + "," + current["source"] + "," + constantIdentifier;
                                actualType = typeExpected;
                                Parser.Advance();
                            }
                            else
                            {
                                Parser.ErrorAtCurrent("undefined constant identifier '" + constantIdentifier + "'");
                            }
#endif                            
                            outerLoopBreak = true;
                            break;
                        }
                    } // loop
                    if (outerLoopBreak)
                    {
                        break;
                    }
                    value = Symbols.GetConstantValue(name);
                    string constantType = Symbols.GetConstantType(name);
                    if (isCount)
                    {
                        value = (value.Length).ToString();
                    }
#ifdef ASSEMBLER
                    if (constantType == "char")
                    {
                        constantType = "byte";
                        value = (byte(value[0])).ToString();
                    }
                    else if (constantType.StartsWith("byte[")) // also "string"?
                    {
                        value = (GetConstantAddress(name, value)).ToString();
                        constantType = "uint";
                    }
                    else if (constantType == "string")
                    {
                        if (!value.EndsWith(char(0x00)))
                        {
                            value = value + char(0x00); // null terminate it
                        }
                        value = (GetConstantAddress(name, value)).ToString();
                        constantType = "uint";
                    }
#endif
                    if (!validateIntegralConstant(typeExpected, constantType, ref actualType, value, CurrentToken))
                    {
                        break;
                    }
                    Parser.Advance();
                }            
                case HopperToken.Keyword:
                {
                    string name = currentToken["lexeme"];
                    if (Token.IsTypeKeyword(name))
                    {
                        Parser.Advance(); // type name
                        if (Types.IsSimpleType(name) && Parser.Check(HopperToken.LParen))
                        {
                            Parser.Advance(); // '('
                         
                            // simple static constant casts:
                            string supportedType = typeExpected;
                            switch (typeExpected)
                            {
                                case "char": { supportedType = "byte"; }
                                case "byte": { supportedType = "byte"; }
                                default:
                                {
                                    Parser.ErrorAtCurrent("invalid simple constant cast: '" + name + "'");
                                    break;
                                }
                            }
                            value = ParseConstantExpression(supportedType, ref actualType, weakEnums);
                            
                            switch (typeExpected)
                            {
                                case "char":
                                {
                                    uint bv;
                                    if (!UInt.TryParse(value, ref bv) || (bv > 255))
                                    {
                                        Parser.ErrorAtCurrent("failed to cast '" + value + "' to char");
                                        break;
                                    }
                                    value = "" + char(bv);
                                    actualType = "char";
                                }
                            }
                            if (Parser.HadError)
                            {
                                break;
                            }
                            Parser.Consume(HopperToken.RParen);
                        }
                        else
                        {
                            byte b = Types.ToByte(name);
                            value = b.ToString();
                            actualType = "type";
                        }
                    }
                    else
                    {    
                        if (!Parser.HadError)
                        {   
                            Parser.ErrorAtCurrent("constant expected");
                        }
                        break;           
                    }
                }
                case HopperToken.LParen:
                {
                    Parser.Advance(); // (
                    value = ParseConstantExpression(typeExpected, ref actualType, weakEnums);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    Parser.Consume(HopperToken.RParen);
                }
                default:
                {
                    if (!Parser.HadError)
                    {
                        Parser.ErrorAtCurrent("constant expected");
                    }
                    break;
                }
            } // switch
            break;
        } // loop
        
        if (!HadError && (typeExpected != actualType))
        {
            if (Types.IsFlags(typeExpected) && Types.IsFlags(actualType))
            {
                string flagsExpected = Types.QualifyFlags(typeExpected);
                string flagsActual   = Types.QualifyFlags(actualType);
                if (flagsExpected != flagsActual)
                {
                    Parser.ErrorAtCurrent("expected '" + flagsExpected + "' flags constant, (was '" + flagsActual + "')");
                }
            }
            else if (Types.IsEnum(typeExpected) && Types.IsEnum(actualType))
            {
                string enumExpected = Types.QualifyEnum(typeExpected);
                string enumActual   = Types.QualifyEnum(actualType);
                if (enumExpected != enumActual)
                {
                    Parser.ErrorAtCurrent("expected '" + enumExpected + "' enum constant, (was '" + enumActual + "')");
                }
            }
            else if (Types.CanInferArrayCast(actualType, typeExpected))
            {
                // ok
                //PrintLn("Constant: " + actualType + "->" + typeExpected);
            }
            else if ((typeExpected == "uint") && (actualType == "byte"))
            {
                // ok
            }
            else
            {
                string was;
                if (actualType.Length != 0)
                {
                    was = ", (was '" + actualType + "')";
                }
                Parser.ErrorAtCurrent("expected '" + typeExpected + "' constant expression" + was);
            }
        }
        return value;
    }
    
    string parseConstantFactor(string typeExpected, ref string actualType, bool weakEnums)
    {
        string value;
        loop
        {
            value = parseConstantPrimary(typeExpected, ref actualType, weakEnums);
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
                    
                    if (!Types.IsNumericType(typeExpected))
                    {
                        if ((operation == HopperToken.Modulus) && (typeExpected == "float"))
                        {
                            Parser.ErrorAtCurrent("modulus operation not legal for 'float'");
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("multiply, divide and modulus operations only legal for numeric types");
                            break;
                        }
                    }
                    Advance(); // *, /, %
                    
                    string rightValue = parseConstantPrimary(typeExpected, ref actualType, weakEnums);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    value = constantOperation(value, rightValue, typeExpected, operation);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return value;
    }
       
    string parseConstantShift(string typeExpected, ref string actualType, bool weakEnums)
    {
        string value;
        loop
        {
            value = parseConstantFactor(typeExpected, ref actualType, weakEnums);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.ShiftRight) || Parser.Check(HopperToken.ShiftLeft))
                {
                    <string,string> operationToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(operationToken);
                    
                    if ((typeExpected != "uint") && (typeExpected != "byte"))
                    {
                        Parser.ErrorAtCurrent("bitwise constant operations only legal for unsigned integral numeric types");
                        break;
                    }
                    Advance(); // <<, >>
                    
                    string rightValue = parseConstantFactor(typeExpected, ref actualType, weakEnums);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    value = constantOperation(value, rightValue, typeExpected, operation);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return value;
    }       
    
    string parseConstantTerm(string typeExpected, ref string actualType, bool weakEnums)
    {
        string value;
        loop
        {
            value = parseConstantShift(typeExpected, ref actualType, weakEnums);
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
                    
                    if (!Types.IsNumericType(typeExpected))
                    {
                        if ((operation == HopperToken.Add) && ((typeExpected == "string") || (typeExpected == "char")))
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
                    
                    string rightValue = parseConstantShift(typeExpected, ref actualType, weakEnums);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    value = constantOperation(value, rightValue, typeExpected, operation);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return value;
    }
    
    string parseConstantBitAnd(string typeExpected, ref string actualType, bool weakEnums)
    {
        string value;
        loop
        {
            value = parseConstantTerm(typeExpected, ref actualType, weakEnums);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BitAnd))
                {
                    <string,string> operationToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(operationToken);
                    
                    if ((typeExpected != "uint") && (typeExpected != "byte"))
                    {
                        Parser.ErrorAtCurrent("bitwise constant operations only legal for unsigned integral numeric types");
                        break;
                    }
                    Advance(); // &
                    
                    string rightValue = parseConstantTerm(typeExpected, ref actualType, weakEnums);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    value = constantOperation(value, rightValue, typeExpected, operation);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return value;
    }
    string parseConstantBitOr(string typeExpected, ref string actualType, bool weakEnums)
    {
        string value;
        loop
        {
            value = parseConstantBitAnd(typeExpected, ref actualType, weakEnums);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BitOr))
                {
                    <string,string> operationToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(operationToken);
                    
                    if ((typeExpected != "uint") && (typeExpected != "byte"))
                    {
                        Parser.ErrorAtCurrent("bitwise constant operations only legal for unsigned integral numeric types");
                        break;
                    }
                    Advance(); // |
                    
                    string rightValue = parseConstantBitAnd(typeExpected, ref actualType, weakEnums);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    value = constantOperation(value, rightValue, typeExpected, operation);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return value;
    }
    
    string ParseConstantExpression(string typeExpected, ref string actualType, bool weakEnums)
    {
        return parseConstantBitOr(typeExpected, ref actualType, weakEnums);
    }
    string ParseConstantExpression(string typeExpected, ref string actualType)
    {
        return parseConstantBitOr(typeExpected, ref actualType, false);
    }
    
    ParseByteRange(<byte,bool> rangeSet)
    {
        loop
        {
            string actualType;
            string caseConstant = ParseConstantExpression("byte", ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (actualType != "byte")
            {
                Parser.Error("'byte' constant expected");
                break;
            }
            uint ui;
            _ = UInt.TryParse(caseConstant, ref ui);
            byte constantByte1 = byte(ui);
            <string, string> currentToken = Parser.CurrentToken;
            HopperToken currentType = Token.GetType(currentToken);
            if (currentType != HopperToken.Dot)
            {
                rangeSet[constantByte1] = true;
                break;
            }
            Parser.Advance(); // .
            currentToken = Parser.CurrentToken;
            currentType = Token.GetType(currentToken);
            if (currentType != HopperToken.Dot)
            {
                break;
            }
            Parser.Advance(); // .
            caseConstant = ParseConstantExpression("byte", ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (actualType != "byte")
            {
                Parser.Error("'byte' constant expected");
                break;
            }
            _ = UInt.TryParse(caseConstant, ref ui);
            byte constantByte2 = byte(ui);
            
            if (constantByte1 > constantByte2)
            {
                Parser.Error("invalid 'byte' range: order");
                break;
            }
            byte currentByte = constantByte1;
            loop
            {
                if (currentByte > constantByte2)
                {
                    break;
                }
                rangeSet[currentByte] = true;
                currentByte++;
            }
            break;
        } // loop
    }
    
    ParseCharRange(<byte,bool> rangeSet)
    {
        loop
        {
            string actualType;
            string caseConstant = ParseConstantExpression("char", ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (actualType != "char")
            {
                Parser.Error("'char' constant expected");
                break;
            }
            char c1 = caseConstant[0];
            byte constantByte1 = byte(c1);
            <string, string> currentToken = Parser.CurrentToken;
            HopperToken currentType = Token.GetType(currentToken);
            if (currentType != HopperToken.Dot)
            {
                rangeSet[constantByte1] = true;
                break;
            }
            Parser.Advance(); // .
            currentToken = Parser.CurrentToken;
            currentType = Token.GetType(currentToken);
            if (currentType != HopperToken.Dot)
            {
                break;
            }
            Parser.Advance(); // .
            caseConstant = ParseConstantExpression("char", ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (actualType != "char")
            {
                Parser.Error("'char' constant expected");
                break;
            }
            char c2 = caseConstant[0];
            byte constantByte2 = byte(c2);
            if (constantByte1 > constantByte2)
            {
                Parser.Error("invalid 'char' range: order");
                break;
            }
            byte currentByte = constantByte1;
            loop
            {
                if (currentByte > constantByte2)
                {
                    break;
                }
                rangeSet[currentByte] = true;
                currentByte++;
            }
            break;
        } // loop
    }
    
    <byte> ParseRange(string expectedType)
    {
        <byte> range;
        <byte,bool> rangeSet;
        loop
        {
            if (expectedType == "char")
            {
                ParseCharRange(rangeSet);
            }
            else if (expectedType == "byte")
            {
                ParseByteRange(rangeSet);
            }
            else
            {
                Die(0x0B);
            }
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.Comma))
            {
                Parser.Advance(); // ,
                continue;
            }
            break;
        }
        if (!Parser.HadError)
        {
            foreach (var kv in rangeSet)
            {
                range.Append(kv.key);
            }
        }
        return range;
    }
    
    bool ResolveUnresolveds()
    {
        bool success = true;
        
        <string, string> cValues = Symbols.GetConstantValues();
        foreach (var kv in cValues)
        {
            string typeExpected = Symbols.GetConstantType(kv.key);
            if (typeExpected == "string") { continue; }
            string value = kv.value;
            if (value.StartsWith("unresolved,"))
            {
                // "unresolved,lexeme,line,source,identifier"
                <string> parts = value.Split(',');
                <string, string> token;
                token["lexeme"] = parts[1];
                token["line"] = parts[2];
                token["source"] = parts[3];
                
                string identifier = parts[4];
                if (!Symbols.ConstantExists(identifier))
                {
                    Parser.ErrorAt(token, "undefined constant identifier");
                    success = false;
                    break;
                }
                
                string identifierType = Symbols.GetConstantType(identifier);
                value                 = Symbols.GetConstantValue(identifier);
                
                string actualType;
                if (!validateIntegralConstant(typeExpected, identifierType, ref actualType, value, token))
                {
                    success = false;
                    break;
                }
                Symbols.SetConstantValue(kv.key, value);
            }
        }
        return success;
    }
}
