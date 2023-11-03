unit Expression
{
    
    uses "/Source/6502/Samples/GIBL/HopperCode"  // code related to Hopper VM byte code
    ExpressionType ParsePrimary(ref string eContent)
    {
        ExpressionType expressionType = ExpressionType.Undefined;
        loop // ParseUnary
        {
            char operator = Peek(ref eContent);
            if (   (operator == '+') || (operator == '-')
                || ((operator == 'N') && (PeekNth(ref eContent, 1) == 'O') && (PeekNth(ref eContent, 2) == 'T'))
               )
            {
                Consume(operator, ref eContent);
                if (operator == 'N')
                {
                    Consume('O', ref eContent);
                    Consume('T', ref eContent);
                    operator = '~';
                }
            }
            
            loop // ParsePrimary start
            {
                char token = eContent[0].ToUpper();
#ifdef HWM
                HopperCode.MeasureHWM();
#endif        
                loop // argumentless functions: TOP, RND, GET
                {
                    if (   (token == 'T')    
                        && (eContent.Length > 2)
                        && ((eContent[1] == 'O') || (eContent[1] == 'o'))
                        && ((eContent[2] == 'P') || (eContent[2] == 'p'))
                       )
                    {
                        TrimFront(3, ref eContent);
                        HopperCode.PushTOP();
                        return ExpressionType.Integer;
                    }
                    else if (   (token == 'G')    
                        && (eContent.Length > 2)
                        && ((eContent[1] == 'E') || (eContent[1] == 'e'))
                        && ((eContent[2] == 'T') || (eContent[2] == 't'))
                       )
                    {
                        TrimFront(3, ref eContent);
                        HopperCode.Get();
                        return ExpressionType.Integer;
                    }
                    else if (   (token == 'R')    
                        && (eContent.Length > 2)
                        && ((eContent[1] == 'N') || (eContent[1] == 'n'))
                        && ((eContent[2] == 'D') || (eContent[2] == 'd'))
                       )
                    {
                        TrimFront(3, ref eContent);
                        HopperCode.Rnd();
                        return ExpressionType.Integer;
                    }
                    
                    break;
                } // loop argumentless functions
                
                eContent = eContent.Substring(1);
                loop
                {
                    if (token.IsDigit())
                    {
                        // integer literal
                        uint iEnd;
                        char ch;
                        int integer;
                        string numberString;
                        String.Build(ref numberString, token);
                        while (iEnd < eContent.Length)
                        {
                            ch = eContent[iEnd];
                            if (!ch.IsDigit())
                            {
                                break;
                            }
                            iEnd++;
                            String.Build(ref numberString, ch);
                        }
                        
                        if (iEnd > 0)
                        {
                            eContent = eContent.Substring(iEnd);
                        }
                        
                        if (!Int.TryParse(numberString, ref integer))
                        {
                            Error(4, eContent[0]);  // Integer expected        
                            break;
                        }
                        HopperCode.PushInt(integer);
                        // integer literal
                        expressionType = ExpressionType.Integer;
                    }
                    else if (token.IsUpper())
                    {
                        HopperCode.GetVariable(byte(token) - 65);
                        expressionType = ExpressionType.Integer;
                    }
                    else if (token == '(')
                    {
                        expressionType = ParseExpression(ref eContent);
                        if (Runtime.Condition != Conditions.None) { break; }
                        TrimFront(0, ref eContent);
                        if (!eContent.StartsWith(')'))
                        {
                            Error(17); // ')' expected.
                            break;        
                        }
                        TrimFront(1, ref eContent);
                    }
                    else if ((token == '&'))
                    {
                        // & prefix for hex
                        int integer;
                        loop 
                        {
                            string hex;
                            loop
                            {
                                if (eContent.Length == 0)
                                {
                                    break;
                                }
                                char ch = eContent[0];
                                if (!ch.IsHexDigit())
                                {
                                    break;
                                }
                                eContent = eContent.Substring(1);
                                hex = hex + ch;    
                            }
                            if ((hex.Length == 0) || (hex.Length > 4))
                            {
                                Error(24); // Invalid hexadecimal literal
                                break;
                            }
                            uint ui;
                            if (!UInt.TryParse("0x" + hex, ref ui))
                            {
                                Error(24); // Invalid hexadecimal literal
                                break;
                            }
                            integer = UIntToInt(ui);
                            break;
                        } // loop
                        if (Runtime.Condition != Conditions.None) { break; }
                        HopperCode.PushInt(integer);
                        // integer literal
                        expressionType = ExpressionType.Integer;
                    }
                    else if ((token == '!') || (token == '?') || (token == '@'))
                    {
                        byte argumentsExpected = 1;
                        if (token == '@')
                        {
                            argumentsExpected = 2;
                        }
                        expressionType = ParseArguments(ref eContent, argumentsExpected);
                        if (Runtime.Condition != Conditions.None) { break; }
                        if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected
                        if (token == '!')
                        {
                            HopperCode.PeekW();
                        }
                        else if (token == '?')
                        {
                            HopperCode.PeekB();
                        }
                        else
                        {
                            HopperCode.PeekBit();
                        }
                    }
                    else
                    {
                        Error(3, token); // Unexpected character in expression
                    }
                    break;
                } // loop primaries
                break;
            } // loop ParsePrimary end
            
            if (Runtime.Condition != Conditions.None) { break; }
            if ((operator == '+') || (operator == '-') || (operator == '~'))
            {
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected
                if (operator == '-') 
                {
                    HopperCode.Negate();
                }
                else if (operator == '~') 
                {
                    HopperCode.Operation(Instruction.BITNOT);
                }
            }
            break;
        } // loop ParseUnary
        return expressionType;
    }
    
    ExpressionType ParseFactor(ref string eContent)
    {
        ExpressionType expressionType = ParsePrimary(ref eContent);
        loop
        {
            if (Runtime.Condition != Conditions.None) { break; }
            char operator = Peek(ref eContent);
            if (   (operator == '*') || (operator == '/') || (operator == '%'))
            {
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected    
                Consume(operator, ref eContent);
                expressionType = ParsePrimary(ref eContent);
                if (Runtime.Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected 
                if (operator == '*')
                {
                    HopperCode.Operation(Instruction.MULI);
                }
                else if (operator == '/')
                {
#ifdef CHECKED    
                    HopperCode.DivZeroCheck();
#endif
                    HopperCode.Operation(Instruction.DIVI);
                }
                else
                {
#ifdef CHECKED    
                    HopperCode.DivZeroCheck();
#endif
                    HopperCode.Operation(Instruction.MODI);
                }
                
                continue;
            }
            break;
        } // loop
        return expressionType;
    }
    ExpressionType ParseTerm(ref string eContent)
    {
        ExpressionType expressionType = ParseFactor(ref eContent);
        loop
        {
            if (Runtime.Condition != Conditions.None) { break; }
            char operator = Peek(ref eContent);
            if ((operator == '+') || (operator == '-'))
            {
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected
                Consume(operator, ref eContent);
                expressionType = ParseFactor(ref eContent);
                if (Runtime.Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected
                if (operator == '+')         { HopperCode.Operation(Instruction.ADDI); }
                else                         { HopperCode.Operation(Instruction.SUBI); }
                continue;
            }
            break;
        } // loop
        return expressionType;
    }
    ExpressionType ParseComparison(ref string eContent)
    {
#ifdef HWM
        HopperCode.MeasureHWM();
#endif                
        ExpressionType expressionType = ParseTerm(ref eContent); 
        loop
        {
            if (Runtime.Condition != Conditions.None) { break; }
            char operator = Peek(ref eContent);
            if ((operator == '<') || (operator == '>') || (operator == '='))
            {
                // =, <, >,
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected    
                Consume(operator, ref eContent);
                char operator2;
                if ((operator == '<') || (operator == '>'))
                {
                    operator2 = Peek(ref eContent);
                    if ((operator2 == '=') || (operator2 == '>'))
                    {
                        // <=, >=, <> 
                        Consume(operator2, ref eContent);
                    }
                }
                
                expressionType = ParseTerm(ref eContent);
                if (Runtime.Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Integer) { Error(15); } // integer expression expected
                if (operator == '<')
                {
                    if (operator2 == '=')      { HopperCode.Operation(Instruction.LEI); }
                    else if (operator2 == '>') { HopperCode.Operation(Instruction.NE); }
                    else                       { HopperCode.Operation(Instruction.LTI); }
                }
                else if (operator == '>')  
                {
                    if (operator2 == '=')      { HopperCode.Operation(Instruction.GEI); }
                    else                       { HopperCode.Operation(Instruction.GTI); }
                }
                else if (operator == '=')      { HopperCode.Operation(Instruction.EQ); }
                expressionType = ExpressionType.Boolean;
                break;
            }
            break;
        } // loop   
        return expressionType;
    }
    ExpressionType ParseAnd(ref string eContent)
    {
        ExpressionType expressionType = ParseComparison(ref eContent);
        loop
        {
            if (Runtime.Condition != Conditions.None) { break; }
            char operator = Peek(ref eContent);
            if (    ((operator == 'A') && (PeekNth(ref eContent, 1) == 'N') && (PeekNth(ref eContent, 2) == 'D'))
                 || ((operator == 'I') && (PeekNth(ref eContent, 1) == 'F')) // chained 'IF's behave like AND's
               )
            {
                if (expressionType != ExpressionType.Boolean) { Error(23); } // boolean expression expected
                Consume(operator, ref eContent);
                if (operator == 'A')
                {
                    Consume('N', ref eContent);
                    Consume('D', ref eContent);
                }
                else
                {
                    Consume('F', ref eContent);
                }
                expressionType = ParseComparison(ref eContent);
                if (Runtime.Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Boolean) { Error(23); } // boolean expression expected
                HopperCode.Operation(Instruction.BOOLAND);
                continue;
            }
            break;
        } // loop
        return expressionType;
    }
    ExpressionType ParseExpression(ref string eContent)
    {
        ExpressionType expressionType = ParseAnd(ref eContent);
        loop
        {
            if (Runtime.Condition != Conditions.None) { break; }
            char operator = Peek(ref eContent);
            if ((operator == 'O') && (PeekNth(ref eContent, 1) == 'R'))
            {
                if (expressionType != ExpressionType.Boolean) { Error(23); } // boolean expression expected
                Consume(operator, ref eContent);
                Consume('R', ref eContent);
                expressionType = ParseAnd(ref eContent);
                if (Runtime.Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Boolean) { Error(23); } // boolean expression expected
                HopperCode.Operation(Instruction.BOOLOR);
                continue;
            }
            break;
        } // loop
        return expressionType;
    }
    
    ExpressionType ParseArguments(ref string eContent, byte argumentsExpected)
    {
        ExpressionType expressionType = ExpressionType.Undefined;
        // Used by '!' and '?' for both Peek and Poke
        // Either the argument is in parens like:
        //   ?(M+L) = A  or A = ?(M+L)
        // Or the argument is a single primary like:
        //   !160=0 or PRINT !160
        // Calling ParseExpression(..) directly for this would continue
        // through the expression and confuse the assignment '=' with the
        // comparison operator for equality.
        char token = eContent[0].ToUpper();
        loop
        {
            if (token == '(')
            {
                eContent = eContent.Substring(1);
                expressionType = ParseExpression(ref eContent);
                if (Runtime.Condition != Conditions.None)
                {
                    break;
                }
                TrimFront(0, ref eContent);
                argumentsExpected--;
                
                if (argumentsExpected != 0)
                {
                    if (!eContent.StartsWith(','))
                    {
                        Error(26); // ',' expected.
                        break;        
                    }
                    continue; // next argument
                }
                if (!eContent.StartsWith(')'))
                {
                    Error(17); // ')' expected.
                    break;        
                }
                TrimFront(1, ref eContent);
            }
            else
            {
                expressionType = ParsePrimary(ref eContent);
            }
            break;
        } // loop
        return expressionType;
    } 
}
