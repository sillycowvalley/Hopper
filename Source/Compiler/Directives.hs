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
    
    bool IsStillOpen { get { return (defineNesting.Length > 0); }}
    
    bool IsAllDefined()
    {
        bool allDefined = true;
        if (defineNesting.Length > 0)
        {
            foreach (var defined in defineNesting)
            {
                if (!defined)
                {
                    allDefined = false;
                    break;
                }
            }    
        }
        return allDefined;
    }
    
    string expression(string ln)
    {
        string value;
        loop
        {
            Parser.ErrorAtCurrent("preprocessorExpression not yet implemented");
            break;
        }
        return value;
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
            if (Symbols.DefineExists(idToken["lexeme"]))
            {
                Parser.ErrorAtCurrent("preprocessor symbol can only be defined once");
                break;
            }
            Symbols.AddDefine(idToken["lexeme"], value);
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
                string value = expression(ln);
                defineNesting.Append(false); // TODO
                Parser.ErrorAtCurrent("preprocessorDirective '" + directive + "' not yet implemented");
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
            <string,string> currentToken = Parser.CurrentToken;
            if (currentToken["line"] == ln)
            {
                Parser.ErrorAtCurrent("unexpected content after directive");
            }
            break;
        }
    }
}
