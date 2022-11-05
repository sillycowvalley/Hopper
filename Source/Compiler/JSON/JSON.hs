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
    
    // Serialize dictionary to a string of .json
    ExportList(file jsonFile, <variant> lst, uint indent)
    {
        string content;
        indent = indent + 2;
        content = spaces(indent) + "[";
        string comma;
        uint entries = 0;
        foreach (var vv in lst)
        {
            content = content + comma;
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
                    content = content + "0x" + v.ToHexString(2);
                }
                case uint:
                {
                    uint v = uint(vv);
                    content = content + v.ToString();
                }
                case int:
                {
                    int v = int(vv);
                    content = content + v.ToString();
                }
                case long:
                {
                    long v = long(vv);
                    content = content + v.ToString();
                }
                case float:
                {
                    float v = float(vv);
                    content = content + v.ToString();
                }
                case string:
                {
                    string v = vv;
                    content = content + '"' + v + '"';
                }
                case bool:
                {
                    bool v = bool(vv);
                    if (v)
                    {
                        content = content + "true";
                    }
                    else
                    {
                        content = content + "false";
                    }
                }
                default:
                {
                    type tt = typeof(vv);
                    string ts = tt.ToString();
                    content = content + "unsupported '" + ts + "' in <variant>";
                }
            } 
            comma = ", ";
        }
        content = content + "]";
        jsonFile.Append(content + eol());
    }
    
    // Serialize dictionary to a string of .json
    ExportDictionary(file jsonFile, <string, variant> dict, uint indent)
    {
        string content = spaces(indent) + "{";
        jsonFile.Append(content + eol());
        content = "";
        
        indent = indent + 2;
        string comma = "  ";
        foreach (var kv in dict)
        {
            content = content + spaces(indent) + comma + '"' + kv.key + "\": ";
            switch(typeof(kv.value))
            {
                // Note: char is deliberately excluded
                case int:
                {
                    int v = int(kv.value);
                    content = content + v.ToString();
                }
                case uint:
                {
                    uint v = uint(kv.value);
                    content = content + v.ToString();
                }
                case byte:
                {
                    byte v = byte(kv.value);
                    content = content + v.ToString();
                }
                case long:
                {
                    long v = long(kv.value);
                    content = content + v.ToString();
                }
                case float:
                {
                    float v = float(kv.value);
                    content = content + v.ToString();
                }
                case string:
                {
                    string v = kv.value;
                    content = content + '"' + v + '"';
                }
                case bool:
                {
                    bool v = bool(kv.value);
                    if (v)
                    {
                        content = content + "true";
                    }
                    else
                    {
                        content = content + "false";
                    }
                }
                case list:
                {
                    <variant> v = kv.value;
                    jsonFile.Append(content + eol());
                    content = "";
                    ExportList(jsonFile, v, indent);
                }
                case dictionary:
                {
                    <string, variant> v = kv.value;
                    jsonFile.Append(content + eol());
                    content = "";
                    ExportDictionary(jsonFile, v, indent);
                }
                default:
                {
                    type tt = typeof(kv.value);
                    uint ut = uint(tt);
                    string ts = tt.ToString();
                    content = content + "unsupported '" + ts + "' (0x" + ut.ToHexString(2) + ") in <string, variant>, (key=" + kv.key + ")";
                }
            }
            if (content.Length > 0)
            {
                jsonFile.Append(content + eol());
                content = "";
            }
            comma = ", ";
        }
        indent = indent - 2;
        content = content + spaces(indent) + "}";
        jsonFile.Append(content + eol());
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
        Parser.Consume(HopperToken.LBracket, "'[' expected");
        bool first = true;
        loop
        {
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.RBracket))
            {
                Parser.Advance(); // ]
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma, "',' expected");
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
                Parser.ErrorAt(currentToken, "value type not implemented in ReadList");
            }
        }
        return lst;
    }
    <string, variant> ReadDictionary()
    {
        <string, variant> dict;
        Parser.Consume(HopperToken.LBrace, "'{' expected");
        bool first = true;
        loop
        {
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.RBrace))
            {
                Parser.Advance(); // }
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma, "',' expected");
            }
            first = false;
            Parser.Consume(HopperToken.StringConstant, "key string expected");
            if (Parser.HadError)
            {
                break;
            }
            <string,string> previousToken = PreviousToken;
            string name = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, "':' expected");
            if (Parser.HadError)
            {
                break;
            }
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
                    Parser.ErrorAt(currentToken, "integer expected");
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
                Parser.ErrorAt(currentToken, "value type not implemented in ReadDictionary");
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
        Parser.Consume(HopperToken.LBrace, "'{' expected");
        bool first = true;
        loop
        {
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.RBrace))
            {
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma, "',' expected");
            }
            first = false;
            Parser.Consume(HopperToken.StringConstant, "section name expected");
            if (Parser.HadError)
            {
                break;
            }
            <string,string> previousToken = PreviousToken;
            string sectionName = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, "':' expected");
            if (Parser.HadError)
            {
                break;
            }
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
                Parser.ErrorAt(currentToken, "value type not implemented in Read");
            }
        }   
        bool success = !Parser.HadError;
        if (Parser.HadError)
        {
            DumpPrevious();
            DumpCurrent();
        }
        return success;
    }
}
