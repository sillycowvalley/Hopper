unit Highlighter
{
    uses "/Source/System/Colour"
    uses "/Source/Compiler/Tokens/Token"
    
    string delimiters = ";,:?(){}[]<>=!&|~^+-*/%";
    
    bool isAssembler;
    
    uint HopperWord(string word, string selectedWord)
    {
        uint colour;
        if (isAssembler && word.StartsWith('#'))
        {
            if (word.Length == 1)
            {
                colour = Colour.Constant;
            }
            else if (IsDirectiveKeyword(word))
            {
                colour = Colour.Directive;
            }
            else
            {
                string subWord = word.Substring(1);
                return HopperWord(subWord, selectedWord);
            }
        }
        else if (IsStatementKeyword(word))
        {
            colour = Colour.Statement;
        }
        else if (IsDirectiveKeyword(word))
        {
            colour = Colour.Directive;
        }
        else if (IsTypeKeyword(word))
        {
            colour = Colour.Type;
        }
        else if (isAssembler && IsInstructionKeyword(word))
        {
            colour = Colour.Instruction;
        }
        else if (isAssembler && IsRegisterKeyword(word))
        {
            colour = Colour.Register;
        }
        else if (isAssembler && IsConditionKeyword(word))
        {
            colour = Colour.Condition;
        }
        else
        {
            long l;
            uint ui;
            if (word.StartsWith('"'))
            {
                colour = Colour.Constant;
            }
            else if (word.StartsWith('\''))
            {
                colour = Colour.Constant;
            }
            else if (delimiters.Contains(word))
            {
                colour = Colour.Delimiter;
            }
            else if (Long.TryParse(word, ref l))
            {
                colour = Colour.Constant;
            }
            else if (UInt.TryParse(word, ref ui))
            {
                colour = Colour.Constant;
            }
            else
            {
                char w0 = word.GetChar(0);
                if (w0.IsUpper())
                {
                    colour = Colour.Public;
                }
                else
                {
                    colour = Colour.Private;
                }
            }
        }
        if ((selectedWord.Length != 0) && (word == selectedWord))
        {
            colour = colour | Colour.Selected;
        }
        return colour;
    }
    
    <uint> HopperSource(string ln, string selectedWord, uint backColor, bool isAssemblerSource, ref uint blockCommentNesting)
    {
        <uint> colours;
        uint colour;
        
        isAssembler = isAssemblerSource;
        
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
                    if (word.Length != 0)
                    {
                        colour = HopperWord(word, selectedWord);
                        foreach (var ch in word)
                        {
                            colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
                        }
                        word = "";
                    }
                    colours.Append(backColor);
                }
            case '"':
                {
                    if (word.Length != 0)
                    {
                        colour = HopperWord(word, selectedWord);
                        foreach (var ch in word)
                        {
                            colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
                        }
                        word = "";
                    }
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
                        colours.Append((blockCommentNesting == 0) ? Colour.Constant : Colour.Comment);
                        strLength--;
                    }
                    word = "";
                    i++;
                    continue;
                }
            case '\'':
                {
                    if (word.Length != 0)
                    {
                        colour = HopperWord(word, selectedWord);
                        foreach (var ch in word)
                        {
                            colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
                        }
                        word = "";
                    }
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
                        colours.Append((blockCommentNesting == 0) ? Colour.Constant : Colour.Comment);
                        charLength--;
                    }
                    word = "";
                    i++;
                    continue;
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
                                colours.Append(Colour.Comment);
                            }
                        }
                        else if ((i+1 < length) && (ln[i+1] == '*'))
                        {
                            // /*
                            colours.Append(Colour.Comment);
                            colours.Append(Colour.Comment);
                            blockCommentNesting++;
                            i++;
                        }
                        else 
                        {
                            if (word.Length != 0)
                            {
                                colour = HopperWord(word, selectedWord);
                                foreach (var ch in word)
                                {
                                    colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
                                }
                            }
                            word = "/";
                        }
                    }
                    else if (delimiters.Contains(c) || (word == "/"))
                    {
                        if (word.Length != 0)
                        {
                            colour = HopperWord(word, selectedWord);
                            foreach (var ch in word)
                            {
                                colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
                            }
                        }
                        word = "";
                        if ((c == '*') && (i+1 < length) && (ln[i+1] == '/'))
                        {
                            // */
                            colours.Append(Colour.Comment);
                            colours.Append(Colour.Comment);
                            if (blockCommentNesting > 0)
                            {
                                blockCommentNesting--;
                            }
                            i++;
                        }
                        else
                        {
                            colour = HopperWord(c.ToString(), selectedWord);
                            colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
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
        if (word.Length != 0)
        {
            colour = HopperWord(word, selectedWord);
            foreach (var ch in word)
            {
                colours.Append((blockCommentNesting == 0) ? colour : Colour.Comment);
            }
        }
        return colours;
    }
    
}
