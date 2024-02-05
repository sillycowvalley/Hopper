unit Directives
{
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    uses "/Source/Compiler/Symbols"
  
    <bool> defineNesting;
    
    New()
    {
        defineNesting.Clear();
    }
    
    bool IsStillOpen { get { return (defineNesting.Length != 0); }}
    
    bool IsAllDefined()
    {
        bool allDefined = true;
        if (defineNesting.Length > 0)
        {
            foreach (var isDefined in defineNesting)
            {
                if (!isDefined)
                {
                    allDefined = false;
                    break;
                }
            }    
        }
        return allDefined;
    }
    
    bool directivePrimary()
    {
        bool result;
        loop
        {
            if (Parser.Check(HopperToken.Directive, "defined"))
            {
                Parser.Advance(); // defined
                Parser.Consume(HopperToken.LParen, '(');
                if (Parser.HadError)
                {
                    break;
                }
                if (!Parser.Check(HopperToken.Identifier))
                {
                    Parser.ErrorAtCurrent("conditional symbol identifier expected");
                    break;
                }
                <string,string> idToken = Parser.CurrentToken;
                string symbol = idToken["lexeme"];
                result = Symbols.DefineExists(symbol);
                    
                Parser.Advance();
                Parser.Consume(HopperToken.RParen, ')');
                if (Parser.HadError)
                {
                    break;
                }
            }
            else if (Parser.Check(HopperToken.LParen))
            {
                Parser.Advance(); // (
                result = directiveExpression();
                if (Parser.HadError)
                {
                    break;
                }
                Parser.Consume(HopperToken.RParen, ')');
            }
            else
            {
                Parser.ErrorAtCurrent("'defined' expected");
            }
            break;
        } // loop
        return result;
    }
    
    bool directiveUnary()
    {
        bool result;
        loop
        {
            HopperToken operation = HopperToken.Undefined;
            if (Parser.Check(HopperToken.BooleanNot))
            {
                Parser.Advance();
                operation = HopperToken.BooleanNot;
            }
            result = directivePrimary();
            if (Parser.HadError)
            {
                break;
            }
            if (operation == HopperToken.BooleanNot)
            {
                result = !result;
            }
            break;
        }
        return result;
    }
    bool directiveBooleanAnd()
    {
        bool result;
        loop
        {
            result = directiveUnary();
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (!Parser.Check(HopperToken.BooleanAnd))
                {
                    break;
                }
                Parser.Advance(); // &&
                bool rightResult = directiveUnary();
                result = result && rightResult;
                if (Parser.HadError)
                {
                    break;
                }
                continue;
            } // loop
            break;
        } // loop
        return result; 
    }
    bool directiveBooleanOr()
    {
        bool result;
        loop
        {
            result = directiveBooleanAnd();
            if (Parser.HadError)
            {
                break;
            }
            loop
            {
                if (!Parser.Check(HopperToken.BooleanOr))
                {
                    break;
                }
                Parser.Advance(); // ||
                bool rightResult = directiveBooleanAnd();
                result = result || rightResult;
                if (Parser.HadError)
                {
                    break;
                }
                continue;
            } // loop
            break;
        } // loop
        return result;
    }
    bool directiveExpression()
    {
        return directiveBooleanOr();
    }
    
    Declaration()
    {
        <string,string> currentToken = Parser.CurrentToken;
        string ln = currentToken["line"];
        loop
        {
            Parser.Advance();
            
            Parser.Consume(HopperToken.Identifier, "identifier expected");
            if (Parser.HadError)
            {
                break;
            }
            <string,string> idToken = Parser.PreviousToken;
            if (idToken["line"] != ln)
            {
                Parser.ErrorAtCurrent("preprocessor directive must be on one line");
                break;
            }
            string value = "true"; // "defined"
            <string,string> nextToken = Parser.CurrentToken;
            if (nextToken["line"] == ln)
            {
                Parser.ErrorAtCurrent("unexpected content on same line as directive declaration");
                break;
                
                // is there an expression following the identifier on the same line?
                //value = expression(ln);
                //if (Parser.HadError)
                //{
                //    break;
                //}
            }
            bool allDefined = Directives.IsAllDefined();
            if (allDefined) // otherwise ignore it
            {
                if (Symbols.DefineExists(idToken["lexeme"]))
                {
                    Parser.ErrorAtCurrent("preprocessor symbol can only be defined once");
                    break;
                }
                Symbols.AddDefine(idToken["lexeme"], value);
                Symbols.AddLocation(idToken["lexeme"], idToken["source"] + ":" + idToken["line"]);
            }
            break;
        }
    }
    NestingAppend(string symbol, bool positive)
    {
        if (positive) // #ifdef
        {
            defineNesting.Append(Symbols.DefineExists(symbol));
        }
        else // #ifdef
        {
            defineNesting.Append(!Symbols.DefineExists(symbol));
        }
    }
    bool NestingPopTail()
    {
        if (defineNesting.Length != 0)
        {
            defineNesting.Remove(defineNesting.Length-1);
            return true;
        }
        return false;
    }
    bool NestingFlipTail()
    {
        if (defineNesting.Length != 0)
        {
            defineNesting.SetItem(defineNesting.Length-1, !defineNesting[defineNesting.Length-1]);
            return true;
        }
        return false;
    }
    
    
    
    Directive()
    {
        <string,string> currentToken = Parser.CurrentToken;
        string ln = currentToken["line"];
        string directive = currentToken["lexeme"];
        loop
        {
            Parser.Advance(); // #ifdef, etc
            if (directive == "#ifdef")
            {
                <string,string> idToken = Parser.CurrentToken;
                if (idToken["line"] != ln)
                {
                    Parser.ErrorAtCurrent("preprocessor directive must be on one line");
                    break;
                }
                Parser.Advance();
                NestingAppend(idToken["lexeme"], true);
            }
            else if (directive == "#ifndef")
            {
                <string,string> idToken = Parser.CurrentToken;
                if (idToken["line"] != ln)
                {
                    Parser.ErrorAtCurrent("preprocessor directive must be on one line");
                    break;
                }
                Parser.Advance();
                NestingAppend(idToken["lexeme"], false);
            }
            else if (directive == "#if")
            {
                bool value = directiveExpression();
                defineNesting.Append(value);
                <string,string> idToken = Parser.PreviousToken;
                if (idToken["line"] != ln)
                {
                    Parser.ErrorAtCurrent("preprocessor directive must be on one line");
                    break;
                }
            }
            else if (directive == "#else")
            {
                if (!NestingFlipTail())
                {
                    Parser.ErrorAtCurrent("unexpected '#else'");
                    break;
                }
            }
            else if (directive == "#endif")
            {
               if (!NestingPopTail())
               {
                   Parser.ErrorAtCurrent("unexpected '#endif'");
                   break;
               }
            }
            else if (directive == "#error")
            {
                string actualType;
                string message = ParseConstantExpression("string", ref actualType);
                if (actualType != "string")
                {
                    Parser.ErrorAtCurrent("string constant expected");
                    break;
                }
                if (IsAllDefined())
                {
                    Parser.Error(message, false);
                }
                break;
            }
            <string,string> currentToken = Parser.CurrentToken;
            if (currentToken["line"] == ln)
            {
                Parser.ErrorAtCurrent("unexpected content after directive");
            }
            break;
        } // loop
    }
}
