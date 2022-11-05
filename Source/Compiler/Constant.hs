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

    string parseConstantPrimary(string typeExpected)
    {
        <string,string> currentToken = Parser.CurrentToken;
        string value;
        string actualType;
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
                case HopperToken.Char:
                {
                    Parser.Advance();
                    value = currentToken["lexeme"];
                    actualType = "char";
                }
                case HopperToken.Float:
                {
                    float f;
                    if (!Token.TryParseFloat(currentToken["lexeme"], ref f))
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
                    if (!Token.TryParseLong(currentToken["lexeme"], ref l))
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
                    if (Types.EnumValue(name, ref typeName, ref valueName, ref ivalue))
                    {
                        Parser.Advance();
                        actualType = typeName;
                        value = ivalue.ToString();
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
                        break;
                    }
                    
                    if (!Symbols.ConstantExists(name))
                    {
                        Parser.ErrorAtCurrent("undefined identifier A");           
                        break;
                    }
                    value = Symbols.GetConstantValue(name);
                    switch (typeExpected)
                    {
                        case "byte":
                        {
                            uint ui;
                            if (!Token.TryParseUInt(value, ref ui) || (ui > 255))   
                            {
                                Parser.ErrorAtCurrent("invalid identifier B");           
                                break;
                            }
                            actualType = "byte";
                            Parser.Advance();
                        }
                        case "uint":
                        {
                            uint ui;
                            if (!Token.TryParseUInt(value, ref ui))   
                            {
                                Parser.ErrorAtCurrent("invalid identifier C");           
                                break;
                            }
                            actualType = "uint";
                            Parser.Advance();   
                        }
                        case "int":
                        {
                            int i;
                            if (!Token.TryParseInt(value, ref i))   
                            {
                                Parser.ErrorAtCurrent("invalid identifier D");           
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
                        Parser.ErrorAtCurrent("constant expected A");
                        break;           
                    }
                }
                default:
                {
                    Parser.ErrorAtCurrent("constant expected A");
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
            else
            {
                Parser.ErrorAtCurrent("expected '" + typeExpected + "' constant expression, (was '" + actualType + "')");
            }
        }
        return value;
    }
    
    string ParseConstantExpression(string typeExpected)
    {
        return parseConstantPrimary(typeExpected);
    }

}
