unit Highlighter
{
    uses "/Source/System/Color"
    uses "/Source/Compiler/Tokens/Token"
    
    string delimiters = ";,:?(){}[]<>=!&|~^+-*/%";
    
    uint HopperWord(string word)
    {
        uint colour;
        
        if (IsStatementKeyword(word))
        {
            colour = Statement;
        }
        else if (IsDirectiveKeyword(word))
        {
            colour = Directive;
        }
        else if (IsTypeKeyword(word) || IsReservedIdentifier(word))
        {
            colour = Type;
        }
        else
        {
            long l;
            uint h;
            if (word.StartsWith('"'))
            {
                colour = Constant;
            }
            else if (word.StartsWith('\''))
            {
                colour = Constant;
            }
            else if (delimiters.Contains(word))
            {
                colour = Delimiter;
            }
            else if (Token.TryParseLong(word, ref l))
            {
                colour = Constant;
            }
            else if (Token.TryParseHex(word, ref h))
            {
                colour = Constant;
            }
            else
            {
                char w0 = word.GetChar(0);
                if (w0.IsUpper())
                {
                    colour = Public;
                }
                else
                {
                    colour = Private;
                }
            }
        }
        return colour;
    }
    
    <uint> Hopper(string ln, uint backColor)
    {
        <uint> colours;
        uint colour;
        
        uint length = ln.Length;
        
        string word;
        bool inChar = false;
        bool inString = false;
        bool inComment = false;
        for (uint i=0; i < length; i++)
        {
            char c = ln[i];
            
            if (inComment)
            {
                colours.Append(Comment);
                continue;
            }
            switch (c)
            {
            case ' ':
                {
                    if (inChar || inString)
                    {
                        word = word + c;
                    }
                    else
                    {
                        if (word.Length > 0)
                        {
                            colour = HopperWord(word);
                            foreach (var ch in word)
                            {
                                colours.Append(colour);
                            }
                            inString = false;
                            inChar = false;
                            word = "";
                        }
                        colours.Append(backColor);
                    }
                }
            case '"':
                {
                    word = word + c;
                    if (inString)
                    {
                        colour = HopperWord(word);
                        foreach (var ch in word)
                        {
                            colours.Append(colour);
                        }
                        inString = false;
                        inChar = false;
                        word = "";
                    }
                    else
                    {
                        inString = true;
                    }
                }
            case '\'':
                {
                    word = word + c;
                    if (inChar)
                    {
                        colour = HopperWord(word);
                        foreach (var ch in word)
                        {
                            colours.Append(colour);
                        }
                        inString = false;
                        inChar = false;
                        word = "";
                    }
                    else
                    {
                        inChar = true;
                    }
                }
            case '\\':
                {
                    word = word + c;
                    if (inChar || inString)
                    {
                        // do nothing
                    }
                    else
                    {
                        colour = HopperWord(word);
                        foreach (var ch in word)
                        {
                            colours.Append(colour);
                        }
                        inString = false;
                        inChar = false;
                        word = "";
                    }
                }
            default:
                {
                    if (inChar || inString)
                    {
                        word = word + c;
                    }
                    else if (c == '/')
                    {
                        if (word == "/")
                        {
                            colours.Append(Comment);
                            colours.Append(Comment);
                            inComment = true;
                            word = "";
                        }
                        else 
                        {
                            if (word.Length > 0)
                            {
                                colour = HopperWord(word);
                                foreach (var ch in word)
                                {
                                    colours.Append(colour);
                                }
                            }
                            word = "/";
                        }
                    }
                    else if (delimiters.Contains(c) || (word == "/"))
                    {
                        if (word.Length > 0)
                        {
                            colour = HopperWord(word);
                            foreach (var ch in word)
                            {
                                colours.Append(colour);
                            }
                        }
                        colour = HopperWord(c.ToString());
                        colours.Append(colour);
                        inString = false;
                        inChar = false;
                        word = "";
                    }
                    else
                    {
                        word = word + c;
                    }
                }
            }
        } // foreach
        if (word.Length > 0)
        {
            colour = HopperWord(word);
            foreach (var ch in word)
            {
                colours.Append(colour);
            }
        }
        return colours;
    }
    
}
