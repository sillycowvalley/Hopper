unit JSON
{
    uses "/Source/System/System"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    
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
    exportList(file jsonFile, <variant> lst, uint indent)
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
#ifndef JSON_EXPRESS
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
    exportDictionary(file jsonFile, <string, variant> dict, uint indent)
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
                    bool isBinary;
                    foreach (var c in v)
                    {
                        if (byte(c) < 32)
                        {
                            isBinary = true;
                            break;
                        }
                    }
                    if (isBinary)
                    {
                        String.Build(ref content, '[');
                        bool next;
                        foreach (var c in v)
                        {
                            if (next)
                            {
                                String.Build(ref content, ", ");   
                            }
                            byte b = byte(c);
                            String.Build(ref content, "0x" + b.ToHexString(2));   
                            next = true; 
                        }
                        String.Build(ref content, "]");
                    }
                    else
                    {
                        String.Build(ref content, '"');
                        String.Build(ref content, v);
                        String.Build(ref content, '"');
                    }
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
                    exportList(jsonFile, v, indent);
                }
                case dictionary:
                {
                    <string, variant> v = kv.value;
                    String.Build(ref content, char(0x0A));
                    jsonFile.Append(content);
                    content = "";
                    exportDictionary(jsonFile, v, indent);
                }
                default:
                {
#ifndef JSON_EXPRESS
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
        exportDictionary(jsonFile, dict, 0);
        jsonFile.Flush();
        return true;
    }
    
    <string> readList()
    {
        <string> lst;
        Parser.Consume(HopperToken.LBracket, '[');
        bool first = true;
        loop
        {
#ifndef JSON_EXPRESS
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
#ifndef JSON_EXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in readList");
#endif
            }
        }
        return lst;
    }
    string readBinaryString()
    {
        string str;
        Parser.Consume(HopperToken.LBracket, '[');
        bool first = true;
        loop
        {
#ifndef JSON_EXPRESS
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
            if (Parser.Check(HopperToken.Integer))
            {
                Parser.Advance();
                string v = currentToken["lexeme"];
                uint ui;
                if (UInt.TryParse(v, ref ui) && (ui <= 255))
                {
                    char c = char(ui);
                    str += c;
                }
                else
                {
                    Parser.ErrorAt(currentToken, "bad binary value in readBinaryString");
                }
            }
            else
            {
#ifndef JSON_EXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in readBinaryString");
#endif
            }
        }
        return str;
    }
    <string, variant> readDictionary(string dictionaryName)
    {
        <string, variant> dict;
        Parser.Consume(HopperToken.LBrace, '{');
        bool first = true;
        loop
        {
#ifndef JSON_EXPRESS
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
#ifndef JSON_EXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> previousToken = PreviousToken;
            string name = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, ':');
#ifndef JSON_EXPRESS
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
                if (Long.TryParse(currentToken["lexeme"], ref l))
                {
                    dict[name] = l.ToString();
                }
                else
                {
#ifndef JSON_EXPRESS
                    Parser.ErrorAt(currentToken, "integer expected");
#endif
                }
            }
            else if (Parser.Check(HopperToken.Bool))
            {
                Parser.Advance();
                if (currentToken["lexeme"] == "true")
                {
                    dict[name] = true;
                }
                else if (currentToken["lexeme"] == "false")
                {
                    dict[name] = false;
                }
                else
                {
#ifndef JSON_EXPRESS
                    Parser.ErrorAt(currentToken, "bool expected");
#endif
                }
            }
            else if (Parser.Check(HopperToken.LBracket))
            {
                if (dictionaryName == "constants") // seriously cheating!
                {
                    string str = readBinaryString();
                    dict[name] = str;
                }
                else
                {
                    <string> lst = readList();
                    dict[name] = lst;
                }
            }
            else if (Parser.Check(HopperToken.LBrace))
            {
                <string, variant> subDict = readDictionary(name);
                dict[name] = subDict;
            }
            else
            {
#ifndef JSON_EXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in ReadDictionary for '" + dictionaryName + "'");
#endif
            }
        }
        return dict;
    }
    
    // reverse of Export above
    bool read(string path, ref <string, variant> dict)
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
#ifndef JSON_EXPRESS
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
#ifndef JSON_EXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> previousToken = PreviousToken;
            string sectionName = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, ':');
#ifndef JSON_EXPRESS
            if (Parser.HadError)
            {
                break;
            }
#endif
            <string,string> currentToken = CurrentToken;
            if (Parser.Check(HopperToken.LBrace))
            {
                <string, variant> section = readDictionary(sectionName);
                dict[sectionName] = section;
            }
            else if (Parser.Check(HopperToken.LBracket))
            {
                <string> section = readList();
                dict[sectionName] = section;
            }
            else if (Parser.Check(HopperToken.StringConstant))
            {
                Parser.Advance();
                dict[sectionName] = currentToken["lexeme"];
            }
            else if (Parser.Check(HopperToken.Integer))
            {
                Parser.Advance();
                long l;
                if (Long.TryParse(currentToken["lexeme"], ref l))
                {
                    dict[sectionName] = l; //.ToString();
                }
                else
                {
#ifndef JSON_EXPRESS
                    Parser.ErrorAt(currentToken, "integer expected");
#endif
                }
            }
            else if (Parser.Check(HopperToken.Bool))
            {
                Parser.Advance();
                if (currentToken["lexeme"] == "true")
                {
                    dict[sectionName] = true;
                }
                else if (currentToken["lexeme"] == "false")
                {
                    dict[sectionName] = false;
                }
                else
                {
#ifndef JSON_EXPRESS
                    Parser.ErrorAt(currentToken, "bool expected");
#endif
                }
            }
            else
            {
#ifndef JSON_EXPRESS
                Parser.ErrorAt(currentToken, "value type not implemented in Read");
#endif
            }

        }   
        bool success = !Parser.HadError;
#ifndef JSON_EXPRESS
        if (Parser.HadError)
        {
            DumpPrevious();
            DumpCurrent();
        }
#endif
        return success;
    }
    
    bool Read(string content, ref <string, variant> dict)
    {
        if (content.Contains('{'))
        {
            uint iBrace;
            if (content.IndexOf('{', ref iBrace))
            {
                content = content.Substring(iBrace);
            }
            content = "{ \"fakesection\": " + content + "}";
            string tempPath = "/temp/jsoncontent.json";
            file t = File.Create(tempPath);
            t.Append(content);
            t.Flush();
            if (read(tempPath, ref dict))
            {
                dict = dict["fakesection"];
                File.Delete(tempPath);
                return true;
            }
        }
        else if (File.Exists(content))
        {
            return read(content, ref dict);
        }
        return false;
    }
}
