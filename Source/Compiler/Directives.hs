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
    
    bool defined(string ln)
    {
        bool value;
        loop
        {
            <string,string> idToken = Parser.CurrentToken;
            if (idToken["line"] != ln)
            {
                Parser.ErrorAtCurrent("preprocessor directive must be on one line");
                break;
            }
            value = false;
            if (Symbols.DefineExists(idToken["lexeme"]))
            {
                value = true;
            }
            Parser.Advance();
            break;
        }
        return value;
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
                // is there an expression following the identifier on the same line?
                value = expression(ln);
                if (Parser.HadError)
                {
                    break;
                }
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
                bool value = defined(ln);
                defineNesting.Append(value);
            }
            else if (directive == "#ifndef")
            {
                bool value = !defined(ln);
                defineNesting.Append(value);
            }
            else if (directive == "#if")
            {
                string value = expression(ln);
                defineNesting.Append(false); // TODO
                Parser.ErrorAtCurrent("preprocessorDirective '" + directive + "' not yet implemented");
            }
            else if (directive == "#else")
            {
                if (defineNesting.Length == 0)
                {
                    Parser.ErrorAtCurrent("unexpected '#else'");
                    break;
                }
                defineNesting.SetItem(defineNesting.Length-1, !defineNesting[defineNesting.Length-1]);
            }
            else if (directive == "#endif")
            {
               if (defineNesting.Length == 0)
               {
                   Parser.ErrorAtCurrent("unexpected '#endif'");
                   break;
               }
               defineNesting.Remove(defineNesting.Length-1);
            }
            break;
        }
    }
}
