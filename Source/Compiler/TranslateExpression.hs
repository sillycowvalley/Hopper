unit TranslateExpression
{
    string TranslateType(string typename)
    {
        switch (typename)
        {
            case "uint":
            {
                typename = "UInt";    
            }
            case "int":
            {
                typename = "Int";    
            }
            case "byte":
            {
                typename = "Byte";    
            }
            case "char":
            {
                typename = "Char";    
            }
            case "bool":
            {
                typename = "Bool";    
            }
            case "long":
            {
                typename = "Long";    
            }
        }
        return typename;
    }
    string TranslateIdentifier(string identifier)
    {
        identifier = identifier.Replace('.', '_');
        if (identifier == "this")
        {
            identifier = "_this";
        }
        return identifier;
    }
    
    string translateKeywordPrimary(string expectedType, ref string actualType)
    {
        string content;
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
                        Parser.ErrorAt(currentToken, "not implemented in translateKeywordPrimary 1");
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
                        Parser.ErrorAt(currentToken, "not implemented in translateKeywordPrimary 2");
                    }
                }
            }
            
            // either:
            //    typeof(..) 
            // or stand-alone type keyword that is one of these:
            //    array|bool|byte|char|const|directory|delegate|dictionary|enum|file|flags|float|int|list|long|pair|ref|string|type|uint|variant|var
            
            string expressionContent;
            if (Parser.Check(HopperToken.LParen))
            {
                if (!IsValueType(typeName)) // bool|byte|char|uint|int|type|delegate|<enum-type-name>|<flags-type-name>|<delegate-type-name>
                {
                    // array|const|directory|dictionary|enum|file|flags|float|list|long|pair|ref|string|type|variant|var
                    
                    if (!doTypeOf)
                    {
                        Parser.ErrorAtCurrent("'" + typeName + "' is an invalid type for simple cast");
                    }
                }
                Parser.Advance(); // (
                
                // typeof(<expression>)  or bool|byte|char|delegate|uint|int|type (<expression>)
                expressionContent = TranslateExpression(expectedType, ref actualType);
                if (Parser.HadError)
                {
                    break;
                }
                Parser.Consume(HopperToken.RParen, ')'); // )
                if (Parser.HadError)
                {
                    break;
                }
                expressionContent = "(" + expressionContent + ")";
                typeId = false;
            }
            else if (typeId)
            {
                content = TranslateType(typeName);
                actualType = "type";
            }
            
            if (typeId)
            {
                // done already
            }            
            else if (doTypeOf)
            {
                // push the type of what's on the stack
                content = "typeof" + expressionContent;
                actualType = "type";
            }
            else //if (typeName != sourceType)
            {
                content = TranslateType(typeName) + expressionContent; // simple cast
                actualType = typeName;
            }
            //else
            //{
            //    content = expressionContent; // nop
            //}
            break;
        } // loop
        return content;
    }
    
    string translateVariableIdentifier(string identifier, string typeString, string qualifiedName, string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            uint fdIndex;
            if (Symbols.GetFunctionDelegateIndex(typeString, ref fdIndex))
            {
                if (Parser.Check(HopperToken.LParen))
                {
                    // delegate method call
                    content = translateFunctionCall(identifier, "", expectedType, ref actualType);
                    break;
                }
            }
            // regular variable identifier (including delegate variable with no (..))
            content = TranslateIdentifier(qualifiedName);
            actualType = typeString;
            break;
        } // loop
        return content;
    }
    
    string translateConstant(string constantName, string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = Symbols.GetConstantValue(constantName);
            break;
        }
        return content;
    }
    
    string translateNamedTypeCast(string identifier, ref string actualType)
    {
        string content;
        loop
        {
            Parser.Advance(); // (
            string expressionContent = TranslateExpression("uint", ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            
            Parser.Consume(HopperToken.RParen, ')');
            if (Parser.HadError)
            {
                break;
            }
            content = TranslateIdentifier(identifier) + "(" + expressionContent + ")";
            actualType = identifier;
            break;
        } // loop
        return content;
    }
    
    string translateDelegateExpression(string identifier, uint fdIndex, string expectedType, ref string actualType)
    {
        string content;
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
            
            // delegate was pushed onto stack so we need to compile it
            Symbols.OverloadToCompile(wiOverload); 
            
            // push function index to stack
            content = "&" + TranslateIdentifier(identifier);
            
            actualType = expectedType;
            break;
        } // loop
        return content;
    }
    
    
    string translateIdentifierPrimary(bool isDotted, string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            <string,string> currentToken = Parser.CurrentToken;
    
            Parser.Advance();
            string identifier = currentToken["lexeme"];
            
            string qualifiedName;
            string identifierTypeString = Types.GetTypeString(identifier, false, ref qualifiedName);
            if (Parser.HadError)
            {
                break;
            }
            
            if (identifierTypeString.Length != 0)
            {
                // <identifier>  or <delegate-identifier> (...)
                content = translateVariableIdentifier(identifier, identifierTypeString, qualifiedName, expectedType, ref actualType);
                break;
            } // (identifierTypeString.Length != 0)
            
            else // (identifierTypeString.Length == 0)
            {
                // named type casts
                
                if (Types.IsEnum(identifier) || Types.IsFlags(identifier) || Types.IsDelegate(identifier))
                {
                    if (Parser.Check(HopperToken.LParen))
                    {
                        content = translateNamedTypeCast(identifier, ref actualType);
                        break;
                    }        
                }
            }
            
            string constantIdentifier = Types.QualifyConstantIdentifier(identifier);
            if (Symbols.ConstantExists(constantIdentifier)) 
            {
                if (!IsVisibleConstant(constantIdentifier))
                {
                    Parser.ErrorAtCurrent("'" + constantIdentifier + "' is private");
                    break;
                }
                content    = translateConstant(constantIdentifier, expectedType, ref actualType);
                break;
            }
            
            uint fdIndex;
            if (Symbols.GetFunctionDelegateIndex(expectedType, ref fdIndex))
            {
                content = translateDelegateExpression(identifier, fdIndex, expectedType, ref actualType);
                break;
            }
            
            <string> nameSpaces = Symbols.GetNameSpaces();
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
                string functionName = parts[1];
                
                if (!Parser.Check(HopperToken.LParen))
                {
                    // no '(' : getter?
                    functionName = functionName + "_Get";
                }
                if (thisTypeString.Length != 0)
                {
                    // "this" case
                    if (Types.IsEnum(thisTypeString) || Types.IsFlags(thisTypeString))
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
                        content = translateFunctionCall(qualifiedName, thisVariable, expectedType, ref actualType);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        break;
                    }
                    break;
                } // "this" case
                
                else
                {
                    // getter?
                    
                    string namespaceAsTypeString = parts[0];
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
                        uint fIndex;
                        if (GetFunctionIndex(qualifiedName, ref fIndex))
                        {
                            // qualified by namespace name rather than by variable name:
                            content = translateFunctionCall(qualifiedName, "", expectedType, ref actualType);
                            if (Parser.HadError)
                            {
                                break;
                            }
                            break;
                        }
                    }
                    
                    // enums and flags
                    bool oneDot = true;
                    if (Parser.Check(HopperToken.Dot)) // next token is another '.'
                    {
                        // 2 dot enums like "Keyboard.Key.Delete"
                        oneDot = false;
                        Parser.Advance(); // consume '.'
                        
                        <string,string> memberNameToken = Parser.CurrentToken;
                        identifier = memberNameToken["lexeme"];
                        
                        HopperToken tokenType = Token.GetType(memberNameToken);
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
                            content = TranslateIdentifier(identifier);
                            actualType = "enum";
                            break;
                        }
                        if (Symbols.FindFlags(typeName, identifier, ref value))
                        {
                            content = TranslateIdentifier(identifier);
                            actualType = "flags";
                            break;
                        }             
                    }
                    if (oneDot)
                    {
                        content = "";
                        actualType = "";
                        
                        foreach (var nameSpace in nameSpaces)
                        {
                            string typeName = nameSpace + "." + parts[0];
                            string memberName = nameSpace + "." + identifier;
                            uint value;
                            if (Symbols.FindEnum(typeName, memberName, ref value))
                            {
                                content = identifier.Replace(".", "::e");    
                                actualType = "enum";
                                break;
                            }
                            if (Symbols.FindFlags(typeName, memberName, ref value))
                            {
                                content = identifier.Replace(".", "::e");    
                                actualType = "flags";
                                break;
                            }
                        }    
                        if (content.Length != 0)
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
                string qualifiedIdentifier = Types.QualifyMethodName(identifier);
                bool success = false;
                uint fIndex;
                if (GetFunctionIndex(qualifiedIdentifier, ref fIndex))
                {
                    // unqualified method call: we qualify using namespace scope rules
                    content = translateFunctionCall(qualifiedIdentifier, "", expectedType, ref actualType);
                    success = !Parser.HadError;
                    break;
                }
            }
            Parser.ErrorAtCurrent("'" + currentToken["lexeme"] + "' is not defined");
            break;
        } // loop
        return content;
    }
    string translatePrimary(string expectedType, ref string actualType)
    {
        string content;
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
                    content = currentToken["lexeme"];
                    Parser.Advance();
                    actualType = "bool";
                }
                case HopperToken.Char:
                {
                    Parser.Advance();
                    content = "'" + currentToken["lexeme"] + "'";
                    actualType = "char";
                }
                case HopperToken.Integer:
                {
                    Parser.Advance();
                    content = currentToken["lexeme"];
                    long l;
                    if (!Long.TryParse(currentToken["lexeme"], ref l))
                    {
                        Parser.ErrorAtCurrent("invalid integer token " + currentToken["type"]);
                    }
                    if (l < -32768)
                    {
                        actualType = "long";
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
                        content = "0x" + l.ToHexString(2);
                    }
                    else if (l < 32768)
                    {
                        actualType = "+int";
                        content = "0x" + l.ToHexString(4);
                    }
                    else if (l <= 65535)
                    {
                        actualType = "uint";
                        content = "0x" + l.ToHexString(4);
                    }
                    else
                    {
                        actualType = "long";
                    }
                    
                }
                case HopperToken.Identifier:
                {
                    content = translateIdentifierPrimary(isDotted, expectedType, ref actualType);
                }
                case HopperToken.Keyword:
                {
                    content = translateKeywordPrimary(expectedType, ref actualType);
                }
                case HopperToken.LParen:
                {
                    Parser.Advance(); // (
                    content = TranslateExpression(expectedType, ref actualType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    Parser.Consume(HopperToken.RParen, ')');
                    if (Parser.HadError)
                    {
                        break;
                    }
                    content = "(" + content + ")";
                }
                default:
                {
                    Parser.ErrorAtCurrent("translatePrimary not implemented for " + currentToken["type"]);
                }
            }
            break;
        } // loop
        return content;
    }
    
    string translateUnary(string expectedType, ref string actualType)
    {
        string content;
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
            
            content = translatePrimary(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (operation == HopperToken.Add)
            {
                content = "+" + content;
            }
            else if (operation == HopperToken.Subtract)
            {
                content = "-" + content;
            }
            else if (operation == HopperToken.BooleanNot)
            {
                content = "!" + content;
            }
            else if (operation == HopperToken.BitNot)
            {
                content = "~" + content;
            }
            break;
        } // loop
        return content;            
    }
    
    string translateFactor(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateUnary(expectedType, ref actualType);
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
                    Parser.Advance();
                    string rightContent = translateUnary(expectedType, ref actualType);
                    if (operation == HopperToken.Multiply)
                    {
                        content = content + " * " + rightContent;
                    }
                    else if (operation == HopperToken.Divide)
                    {
                        content = content + " / " + rightContent;
                    }
                    else if (operation == HopperToken.Modulus)
                    {
                        content = content + " % " + rightContent;
                    }
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    
    string translateShift(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateFactor(expectedType, ref actualType);
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
                    Parser.Advance();
                    string rightContent = translateFactor(expectedType, ref actualType);
                    if (operation == HopperToken.ShiftRight)
                    {
                        content = content + " >> " + rightContent;
                    }
                    else if (operation == HopperToken.ShiftLeft)
                    {
                        content = content + " << " + rightContent;
                    }
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    string translateTerm(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateShift(expectedType, ref actualType);
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
                    Parser.Advance();
                    string rightContent = translateShift(expectedType, ref actualType);
                    if (operation == HopperToken.Add)
                    {
                        content = content + " + " + rightContent;
                    }
                    else if (operation == HopperToken.Subtract)
                    {
                        content = content + " - " + rightContent;
                    }
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    
    string translateComparison(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateTerm(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.LT) || Parser.Check(HopperToken.LE) || Parser.Check(HopperToken.GT) || Parser.Check(HopperToken.GE))
            {
                <string,string> comparisonToken = Parser.CurrentToken;
                HopperToken operation = Token.GetType(comparisonToken);
                Advance(); // <, <=, >, >=
                string rightContent = translateTerm(expectedType, ref actualType);
                if (operation == HopperToken.LT)
                {
                    content = content + " < " + rightContent;
                    actualType = "bool";
                }
                else if (operation == HopperToken.GT)
                {
                    content = content + " > " + rightContent;
                    actualType = "bool";
                }
                else if (operation == HopperToken.LE)
                {
                    content = content + " <= " + rightContent;
                    actualType = "bool";
                }
                else if (operation == HopperToken.GE)
                {
                    content = content + " >= " + rightContent;
                    actualType = "bool";
                }
            }
            break;
        } // loop
        return content;
    }
                
    string translateBitAnd(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateComparison(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BitAnd))
                {
                    Parser.Advance();
                    string rightContent = translateComparison(expectedType, ref actualType);
                    content = content + " & " + rightContent;
                    actualType = "uint";
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    
    string translateBitXor(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateBitAnd(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BitXor))
                {
                    Parser.Advance();
                    string rightContent = translateBitAnd(expectedType, ref actualType);
                    content = content + " ^ " + rightContent;
                    actualType = "uint";
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    
    string translateBitOr(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateBitXor(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BitOr))
                {
                    Parser.Advance();
                    string rightContent = translateBitXor(expectedType, ref actualType);
                    content = content + " | " + rightContent;
                    actualType = "uint";
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    string translateBooleanAnd(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateBitOr(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BooleanAnd))
                {
                    Parser.Advance();
                    string rightContent = translateBitOr(expectedType, ref actualType);
                    content = content + " && " + rightContent;
                    actualType = "bool";
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    
    string translateBooleanOr(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateBooleanAnd(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.BooleanOr))
                {
                    Parser.Advance(); // ||
                    string rightContent = translateBooleanAnd(expectedType, ref actualType);
                    content = content + " || " + rightContent;
                    actualType = "bool";
                    continue;    
                }
                break;
            }
            break;
        } // loop
        return content;
    }
    
    string translateEquality(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateBooleanOr(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.EQ) || Parser.Check(HopperToken.NE))
                {
                    <string,string> equalityToken = Parser.CurrentToken;
                    HopperToken operation = Token.GetType(equalityToken);
                    
                    Parser.Advance(); // != or ==
                    string rightContent = translateBooleanOr(expectedType, ref actualType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (operation == HopperToken.EQ)
                    {
                        content = content + " == " + rightContent;
                        actualType = "bool";
                    }
                    else
                    {
                        content = content + " != " + rightContent;
                        actualType = "bool";
                    }
                }
                break;
            }
            break;
        } // loop
        return content; 
    }
    
    string translateTernary(string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = translateEquality(expectedType, ref actualType);
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.Question)) // ?
            {
                if (actualType != "bool")
                {
                    Parser.ErrorAtCurrent("ternary test expression must be boolean");
                    break;
                }
                
                content = "(" + content + ") ? ";
                Parser.Advance(); // ?
                
                string trueExpression = TranslateExpression(expectedType, ref actualType);
                
                Parser.Consume(HopperToken.Colon, ':');
                if (Parser.HadError)
                {
                    break;
                }
                
                string falseExpression = TranslateExpression(expectedType, ref actualType);
                if (Parser.HadError)
                {
                    break;
                }

                content = content + "(" + trueExpression + ") : (" + falseExpression + ")";
                if (Parser.HadError)
                {
                    break;
                }
                
            } // ?
            break;
        }
        return content;
    }
        
    string TranslateExpression(string expectedType, ref string actualType)
    {
        return translateTernary(expectedType, ref actualType);
    } 
    
    string TranslateReferenceArgument(ref string argumentType)
    {
        string content;
        argumentType = "";
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
                content = TranslateIdentifier(fullName);
            }   
            else
            {
                content = TranslateIdentifier(variableName);
            }
            break;
        }
        return content;
    }
    
    string translateFunctionCall(string functionName, string thisVariable, string expectedType, ref string actualType)
    {
        string content;
        loop
        {
            content = TranslateMethodCall(functionName, thisVariable, "", expectedType, ref actualType);
            break;
        } // loop
        return content;            
    }
    
    string TranslateMethodCall(string methodName, string thisVariable, string dotThisType, string expectedType, ref string actualType)
    {
        string content;
        string returnType;
        
        < <string > > arguments;
        bool isDelegateType = false;
        bool isLocal = false;

        bool hasThisArgument;
        string qualifiedThis;
        string thisType;
        
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
                content = TranslateIdentifier(methodName) + "(";
                if ((thisVariable.Length != 0) || (dotThisType.Length != 0))
                {
                    if (thisVariable.Length != 0) 
                    {
                        thisType = Types.GetTypeString(thisVariable, true, ref qualifiedThis);
                        hasThisArgument = true;
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
                    <string> argument = arguments[0];
                    content = content + TranslateType(argument[1]) + " " + TranslateIdentifier(argument[2]);
                }
                content = content + ")";
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
                    string typeString = Types.GetTypeString(thisVariable, false, ref qualifiedThis);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (Types.IsSimpleType(typeString))
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
            
            content = "";
        
            uint n = 0;
            if ( (thisVariable.Length != 0) || (dotThisType.Length != 0))
            {
                thisType = dotThisType;
                if (thisVariable.Length != 0)
                {
                    thisType = Types.GetTypeString(thisVariable, true, ref qualifiedThis);
                    content = content + TranslateIdentifier(qualifiedThis);
                }
                <string> argument;
                argument.Append(""); // no "ref"
                argument.Append(thisType);
                argument.Append("a" + n.ToString());
                arguments.Append(argument);
                n++;
            }
            bool hadRef = false;
            loop
            {
                if (Parser.HadError)
                {
                    break;
                }
                if (Parser.Check(HopperToken.RParen))
                {
                    Parser.Advance();
                    content = content + ")";
                    break;
                }
                if (n > 0)
                {
                    content = content + ", ";
                }
                <string> argument;
                bool isRef;
                if (Parser.Check(HopperToken.Keyword, "ref"))
                {
                    Parser.Advance();
                    //content = content + "&";
                    argument.Append("ref");
                    isRef = true;
                }
                else
                {
                    argument.Append(""); // no "ref"
                }
                string argumentContent;
                string argumentType;
                if (isRef)
                {
                    argumentContent = TranslateReferenceArgument(ref argumentType);
                    hadRef = true;
                }
                else  
                {  
                    argumentContent = TranslateExpression("", ref argumentType);
                }
                if (Parser.HadError)
                {
                    break;
                }
                content = content + argumentContent;
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
            if (hadRef)
            {
                content = TranslateIdentifier(methodName) + "_R(" + content;
            }
            else
            {
                content = TranslateIdentifier(methodName) + "(" + content;
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
            break;
        } // loop
        actualType = returnType;
        return content;
    }
    
}
