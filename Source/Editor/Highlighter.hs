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
        else if (IsTypeKeyword(word))
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
            else if (Long.TryParse(word, ref l))
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
    
    <uint> Hopper(string ln, uint backColor, ref uint blockCommentNesting)
    {
        <uint> colours;
        uint colour;
        
        uint length = ln.Length;
        
        string word;
        
        
        uint i=0;
        loop 
        {
            if (i >= length) { break; }
            char c = ln[i];
            switch (c)
            {
            case ' ':
                {
                    if (word.Length > 0)
                    {
                        colour = HopperWord(word);
                        foreach (var ch in word)
                        {
                            colours.Append((blockCommentNesting == 0) ? colour : Color.Comment);
                        }
                        word = "";
                    }
                    colours.Append(backColor);
                }
            case '"':
                {
                    uint strLength = 1; // opening "
                    loop
                    {
                        i++;
                        if (i >= length) { break; }
                        strLength++;
                        if (ln[i] == '\\')
                        {
                            i++;
                            strLength++;
                        }
                        else if (ln[i] == '"')
                        {
                            break; // closing  "
                        }
                    }
                    while (strLength > 0)
                    {
                        colours.Append((blockCommentNesting == 0) ? Color.Constant : Color.Comment);
                        strLength--;
                    }
                    word = "";
                    i++;
                    continue;
                }
            case '\'':
                {
                    word += c;
                    // process 'x' or '\x'
                    uint charLength = 1; // opening '
                    i++;
                    if (i < length)
                    {
                        charLength++; // x or \
                        if (ln[i] == '\\')
                        {
                            i++;
                            if (i < length)
                            {
                                charLength++; // x
                            }
                        }   
                    }
                    charLength++; // closing '
                    i++;
                    while (charLength > 0)
                    {
                        colours.Append((blockCommentNesting == 0) ? Color.Constant : Color.Comment);
                        charLength--;
                    }
                    word = "";
                    i++;
                    continue;
                }
            case '\\':
                {
                    word = word + c;
                    colour = HopperWord(word);
                    foreach (var ch in word)
                    {
                        colours.Append((blockCommentNesting == 0) ? colour : Color.Comment);
                    }
                    word = "";
                }
            default:
                {
                    if (c == '/')
                    {
                        if (word == "/") // //
                        {
                            colours.Append(Comment); 
                            colours.Append(Comment); 
                            word = "";
                            loop
                            {
                                i++;
                                if (i >= length) { break; }
                                colours.Append(Color.Comment);
                            }
                        }
                        else if ((i+1 < length) && (ln[i+1] == '*'))
                        {
                            // /*
                            colours.Append(Color.Comment);
                            colours.Append(Color.Comment);
                            blockCommentNesting++;
                            i++;
                        }
                        else 
                        {
                            if (word.Length > 0)
                            {
                                colour = HopperWord(word);
                                foreach (var ch in word)
                                {
                                    colours.Append((blockCommentNesting == 0) ? colour : Color.Comment);
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
                                colours.Append((blockCommentNesting == 0) ? colour : Color.Comment);
                            }
                        }
                        word = "";
                        if ((c == '*') && (i+1 < length) && (ln[i+1] == '/'))
                        {
                            // */
                            colours.Append(Color.Comment);
                            colours.Append(Color.Comment);
                            if (blockCommentNesting > 0)
                            {
                                blockCommentNesting--;
                            }
                            i++;
                        }
                        else
                        {
                            colour = HopperWord(c.ToString());
                            colours.Append((blockCommentNesting == 0) ? colour : Color.Comment);
                        }
                    }
                    else
                    {
                        word = word + c;
                    }
                }
            }
            i++;
        } // loop
        if (word.Length > 0)
        {
            colour = HopperWord(word);
            foreach (var ch in word)
            {
                colours.Append((blockCommentNesting == 0) ? colour : Color.Comment);
            }
        }
        return colours;
    }
    
}
