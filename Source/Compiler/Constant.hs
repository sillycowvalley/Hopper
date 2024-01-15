unit Constant
{
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Collection"
    
    string constantOperation(string leftValue, string rightValue, string typeExpected, HopperToken operation)
    {
        string result;
        long   lresult;
        float  fresult;
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
                            uint ul = uint(left);
                            uint ur = uint(right);
                            lresult = (ul << ur);
                        }
                        case HopperToken.ShiftRight:
                        {
                            uint ul = uint(left);
                            uint ur = uint(right);
                            lresult = (ul >> ur);
                        }
                        case HopperToken.BitOr:
                        {
                            uint ul = uint(left);
                            uint ur = uint(right);
                            lresult = (ul | ur);
                        }
                        case HopperToken.BitAnd:
                        {
                            uint ul = uint(left);
                            uint ur = uint(right);
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

    string parseConstantPrimary(string typeExpected, ref string actualType)
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
                    actualType = "char";
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
                                <string,string> peekToken = Parser.Peek();
                                if (peekToken["type"] == "Dot")
                                {
                                    Parser.Advance(); // name
                                    Parser.Advance(); // dot
                                    <string, string> current = Parser.CurrentToken;
                                    name = constantIdentifier + "." + current["lexeme"];
                                    pass++;
                                    continue;
                                }
                            }
                            Parser.ErrorAtCurrent("undefined constant identifier '" + constantIdentifier + "'");
                            outerLoopBreak = true;
                            break;
                        }
                    } // loop
                    if (outerLoopBreak)
                    {
                        break;
                    }
                    value = Symbols.GetConstantValue(name);
                    if (isCount)
                    {
                        value = (value.Length).ToString();
                    }
                    switch (typeExpected)
                    {
                        case "byte":
                        {
                            uint ui;
                            if (!UInt.TryParse(value, ref ui) || (ui > 255))   
                            {
                                Parser.ErrorAtCurrent("invalid identifier");
                                break;
                            }
                            actualType = "byte";
                            Parser.Advance();
                        }
                        case "uint":
                        {
                            uint ui;
                            if (!UInt.TryParse(value, ref ui))   
                            {
                                Parser.ErrorAtCurrent("invalid identifier");           
                                break;
                            }
                            actualType = "uint";
                            Parser.Advance();   
                        }
                        case "int":
                        {
                            int i;
                            if (!Int.TryParse(value, ref i))   
                            {
                                Parser.ErrorAtCurrent("invalid identifier");           
                                break;
                            }
                            actualType = "int";
                            Parser.Advance();       
                        }
                        default:
                        {
                            Parser.ErrorAtCurrent("constant identifier case not implemented");
                        }
                    }
                }            
                case HopperToken.Keyword:
                {
                    string name = currentToken["lexeme"];
                    if (Token.IsTypeKeyword(name))
                    {
                        byte b = Types.ToByte(name);
                        value = b.ToString();
                        actualType = "type";
                        Parser.Advance();       
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
                    value = ParseConstantExpression(typeExpected, ref actualType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    Parser.Consume(HopperToken.RParen, ')');
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
                string flagsActual = Types.QualifyFlags(actualType);
                if (flagsExpected != flagsActual)
                {
                    Parser.ErrorAtCurrent("expected '" + flagsExpected + "' flags constant, (was '" + flagsActual + "')");
                }
            }
            else if (Types.IsEnum(typeExpected) && Types.IsEnum(actualType))
            {
                string enumExpected = Types.QualifyEnum(typeExpected);
                string enumActual = Types.QualifyEnum(actualType);
                if (enumExpected != enumActual)
                {
                    Parser.ErrorAtCurrent("expected '" + enumExpected + "' enum constant, (was '" + enumActual + "')");
                }
            }
            else if (typeExpected.Contains("[]") && actualType.EndsWith(']'))
            {
                string baseType = typeExpected.Substring(0, typeExpected.Length-1);
                if (!actualType.StartsWith(baseType))
                {
                    Parser.ErrorAtCurrent("expected '" + typeExpected + "' constant expression, (was '" + actualType + "')");
                }
            }
            else
            {
                Parser.ErrorAtCurrent("expected '" + typeExpected + "' constant expression, (was '" + actualType + "')");
            }
        }
        return value;
    }
    
    string parseConstantFactor(string typeExpected, ref string actualType)
    {
        string value;
        loop
        {
            value = parseConstantPrimary(typeExpected, ref actualType);
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
                    
                    string rightValue = parseConstantPrimary(typeExpected, ref actualType);
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
       
    string parseConstantShift(string typeExpected, ref string actualType)
    {
        string value;
        loop
        {
            value = parseConstantFactor(typeExpected, ref actualType);
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
                    
                    string rightValue = parseConstantFactor(typeExpected, ref actualType);
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
    
    string parseConstantTerm(string typeExpected, ref string actualType)
    {
        string value;
        loop
        {
            value = parseConstantShift(typeExpected, ref actualType);
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
                    
                    string rightValue = parseConstantShift(typeExpected, ref actualType);
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
    
    string parseConstantBitAnd(string typeExpected, ref string actualType)
    {
        string value;
        loop
        {
            value = parseConstantTerm(typeExpected, ref actualType);
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
                    
                    string rightValue = parseConstantTerm(typeExpected, ref actualType);
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
    string parseConstantBitOr(string typeExpected, ref string actualType)
    {
        string value;
        loop
        {
            value = parseConstantBitAnd(typeExpected, ref actualType);
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
                    
                    string rightValue = parseConstantBitAnd(typeExpected, ref actualType);
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
    
    string ParseConstantExpression(string typeExpected, ref string actualType)
    {
        return parseConstantBitOr(typeExpected, ref actualType);
    }
}
