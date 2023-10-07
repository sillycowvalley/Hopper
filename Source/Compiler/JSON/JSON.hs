unit JSON
{
    uses "/Source/System/System"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    
    string eol()
    {
        return "" + char(0x0A);
    }
    string spaces(uint count)
    {
        string spaces;
        spaces = spaces.Pad(' ', count);
        return spaces;
    }
    
    // Serialize list to a string of .json
    ExportList(file jsonFile, <variant> lst, uint indent)
    {
        uint entries;
        string content;
        string comma;
        indent = indent + 2;
        content = spaces(indent) + '[';
        foreach (var vv in lst)
        {
            String.Build(ref content, comma);
            switch(typeof(vv))
            {
                case byte:
                {
                    byte v = byte(vv);
                    entries++;
                    if ((entries % 16) == 0)
                    {
                        jsonFile.Append(content + eol());
                        content = spaces(indent+1);
                    }
                    String.Build(ref content, "0x");
                    String.Build(ref content, v.ToHexString(2));
                }
                case uint:
                {
                    uint v = uint(vv);
                    String.Build(ref content, v.ToString());
                }
                case int:
                {
                    int v = int(vv);
                    String.Build(ref content, v.ToString());
                }
                case long:
                {
                    long v = long(vv);
                    String.Build(ref content, v.ToString());
                }
                case float:
                {
                    float v = float(vv);
                    String.Build(ref content, v.ToString());
                }
                case string:
                {
                    string v = vv;
                    String.Build(ref content, '"');
                    String.Build(ref content, v);
                    String.Build(ref content, '"');
                }
                case bool:
                {
                    bool v = bool(vv);
                    if (v)
                    {
                        String.Build(ref content, "true");
                    }
                    else
                    {
                        String.Build(ref content, "false");
                    }
                }
                default:
                {
#ifndef JSONEXPRESS
                    type tt = typeof(vv);
                    string ts = tt.ToString();
                    String.Build(ref content, "unsupported '");
                    String.Build(ref content, ts);
                    String.Build(ref content, "' in <variant>");
#endif
                }
            } 
            comma = ", ";
        }
        String.Build(ref content, ']');
        String.Build(ref content, char(0x0A));
        jsonFile.Append(content);
    }
    
    // Serialize dictionary to a string of .json
    ExportDictionary(file jsonFile, <string, variant> dict, uint indent)
    {
        string content = spaces(indent) + '{';
        jsonFile.Append(content + eol());
        content = "";
        
        indent = indent + 2;
        string comma = "  ";
        foreach (var kv in dict)
        {
            String.Build(ref content, spaces(indent));
            String.Build(ref content, comma);
            String.Build(ref content, '"');
            String.Build(ref content, kv.key);
            String.Build(ref content, "\": ");
            switch(typeof(kv.value))
            {
                // Note: char is deliberately excluded
                case int:
                {
                    int v = int(kv.value);
                    String.Build(ref content, v.ToString());
                }
                case uint:
                {
                    uint v = uint(kv.value);
                    String.Build(ref content, v.ToString());
                }
                case byte:
                {
                    byte v = byte(kv.value);
                    String.Build(ref content, v.ToString());
                }
                case long:
                {
                    long v = long(kv.value);
                    String.Build(ref content, v.ToString());
                }
                case float:
                {
                    float v = float(kv.value);
                    String.Build(ref content, v.ToString());
                }
                case string:
                {
                    string v = kv.value;
                    String.Build(ref content, '"');
                    String.Build(ref content, v);
                    String.Build(ref content, '"');
                }
                case bool:
                {
                    bool v = bool(kv.value);
                    if (v)
                    {
                        String.Build(ref content, "true");
                    }
                    else
                    {
                        String.Build(ref content, "false");
                    }
                }
                case list:
                {
                    <variant> v = kv.value;
                    String.Build(ref content, char(0x0A));
                    jsonFile.Append(content);
                    content = "";
                    ExportList(jsonFile, v, indent);
                }
                case dictionary:
                {
                    <string, variant> v = kv.value;
                    String.Build(ref content, char(0x0A));
                    jsonFile.Append(content);
                    content = "";
                    ExportDictionary(jsonFile, v, indent);
                }
                default:
                {
#ifndef JSONEXPRESS
                    type tt = typeof(kv.value);
                    uint ut = uint(tt);
                    string ts = tt.ToString();
                    content = content + "unsupported '" + ts + "' (0x" + ut.ToHexString(2) + ") in <string, variant>, (key=" + kv.key + ")";
#endif
                }
            }
            if (content.Length > 0)
            {
                String.Build(ref content, char(0x0A));
                jsonFile.Append(content);
                content = "";
            }
            comma = ", ";
        }
        indent = indent - 2;
        String.Build(ref content, spaces(indent));
        String.Build(ref content, '}');
        String.Build(ref content, char(0x0A));
        jsonFile.Append(content);
    }
    
    // Export the dictionary (recursively) to the .json file format
    bool Write(string path, <string, variant> dict)
    {
        if (File.Exists(path))
        {
            File.Delete(path);
        }
        file jsonFile = File.Create(path);
        ExportDictionary(jsonFile, dict, 0);
        jsonFile.Flush();
        return true;
    }
    
    <string> ReadList()
    {
        <string> lst;
        Parser.Consume(HopperToken.LBracket, '[');
        bool first = true;
        loop
        {
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            if (Parser.Check(HopperToken.RBracket))
            {
                Parser.Advance(); // ]
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma, ',');
            }
            first = false;
            <string,string> currentToken = CurrentToken;
            if (Parser.Check(HopperToken.StringConstant))
            {
                Parser.Advance();
                string v = currentToken["lexeme"];
                lst.Append(v);
            }
            else
            {
#ifndef JSONEXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in ReadList");
#endif
            }
        }
        return lst;
    }
    <string, variant> ReadDictionary()
    {
        <string, variant> dict;
        Parser.Consume(HopperToken.LBrace, '{');
        bool first = true;
        loop
        {
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            if (Parser.Check(HopperToken.RBrace))
            {
                Parser.Advance(); // }
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma, ',');
            }
            first = false;
            Parser.Consume(HopperToken.StringConstant, "key string expected");
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> previousToken = PreviousToken;
            string name = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, ':');
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> currentToken = CurrentToken;
            if (Parser.Check(HopperToken.StringConstant))
            {
                Parser.Advance();
                dict[name] = currentToken["lexeme"];
            }
            else if (Parser.Check(HopperToken.Integer))
            {
                Parser.Advance();
                long l;
                if (Token.TryParseLong(currentToken["lexeme"], ref l))
                {
                    dict[name] = l.ToString();
                }
                else
                {
#ifndef JSONEXPRESS
                    Parser.ErrorAt(currentToken, "integer expected");
#endif
                }
            }
            else if (Parser.Check(HopperToken.LBracket))
            {
                <string> lst = ReadList();
                dict[name] = lst;
            }
            else if (Parser.Check(HopperToken.LBrace))
            {
                <string, variant> subDict = ReadDictionary();
                dict[name] = subDict;
            }
            else
            {
#ifndef JSONEXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in ReadDictionary");
#endif
            }
        }
        return dict;
    }
    
    // reverse of Export above
    bool Read(string path, ref <string, variant> dict)
    {
        Scanner.New();
        Parser.Reset();
        Scanner.Load(path);
        long pos = 0;
        Scanner.Reset(pos, 1, path);
        
        Parser.Advance(); // load up first token
        Parser.Consume(HopperToken.LBrace, '{');
        bool first = true;
        loop
        {
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            if (Parser.Check(HopperToken.RBrace))
            {
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma, ',');
            }
            first = false;
            Parser.Consume(HopperToken.StringConstant, "section name expected");
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> previousToken = PreviousToken;
            string sectionName = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, ':');
#ifndef JSONEXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> currentToken = CurrentToken;
            if (Parser.Check(HopperToken.LBrace))
            {
                <string, variant> section = ReadDictionary();
                dict[sectionName] = section;
            }
            else if (Parser.Check(HopperToken.LBracket))
            {
                <string> section = ReadList();
                dict[sectionName] = section;
            }
            else
            {
#ifndef JSONEXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in Read");
#endif
            }

        }   
        bool success = !Parser.HadError;
#ifndef JSONEXPRESS
        if (Parser.HadError)
        {
            DumpPrevious();
            DumpCurrent();
        }
#endif
        return success;
    }
}
