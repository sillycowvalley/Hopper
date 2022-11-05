unit Code
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "/Source/Compiler/JSON/JSON"
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    
    uint entryIndex;
    <uint, <byte> > methodCode;
    <uint, uint > methodSize;
    <uint, uint > methodStart;
    <byte> constantData;
    
    <string, <string,variant> > debugSymbols;
    
    <string, <uint, uint> > lineMap;
    
    MapSource()
    {
        foreach (var kv in debugSymbols)
        {
            <string,variant> methodSymbols = kv.value;
            string path = methodSymbols["source"];
            <uint,uint> map;
            if (lineMap.Contains(path))
            {
                map = lineMap[path];
            }
            <string,string> smap = methodSymbols["debug"];
            foreach (var kv2 in smap)
            {
                uint address;
                uint ln;
                if (Token.TryParseUInt(kv2.key, ref address))
                {
                }
                if (Token.TryParseUInt(kv2.value, ref ln))
                {
                }
                //OutputDebug(path + ":" + kv2.value + " " + kv2.key);
                map[ln] = address;
            }
            lineMap[path] = map;
        }
    }
    
    <byte> GetConstantData()
    {
        return constantData;
    }
    uint GetEntryIndex()
    {
        return entryIndex;
    }
    
    uint GetMethodCount()
    {
        return methodCode.Count;
    }
    uint GetMethodSize(uint methodIndex)
    {
        return methodSize[entryIndex];
    }
    <uint, uint> GetMethodSizes()
    {
        return methodSize;
    }    
    <byte> GetMethodCode(uint methodIndex)
    {
        return methodCode[methodIndex];
    }
    
    <string,variant> GetMethodSymbols(string methodIndex)
    {
        <string,variant> methodSymbols;
        if (debugSymbols.Contains(methodIndex))
        {
            methodSymbols = debugSymbols[methodIndex];
        }
        return methodSymbols;
    }
        
    bool ParseMethod(string methodIndex, bool keepCode, bool keepSymbols, ref string methodName, ref <byte> code)
    {
        <string,variant> methodDictionary;
        
        bool first = true;
        bool isData;
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
                Parser.Consume(HopperToken.Comma, "',' comma expected A");
                if (Parser.HadError)
                {
                    break;
                }
            }
            Parser.Consume(HopperToken.StringConstant, "key name expected");
            if (Parser.HadError)
            {
                break;
            }
            <string,string> previousToken = PreviousToken;
            string keyName = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon, "':' expected");
            if (Parser.HadError)
            {
                break;
            }
            if (keyName == "data")
            {
                isData = true;
                keyName = "code";
                methodName = "const";
            }
            switch (keyName)
            {
                case "name":
                {
                    Parser.Consume(HopperToken.StringConstant, "method name expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodName = previousToken["lexeme"];
                    methodDictionary["name"] = previousToken["lexeme"];
                }
                case "source":
                {
                    Parser.Consume(HopperToken.StringConstant, "source path expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodDictionary["source"] = previousToken["lexeme"];
                }
                case "line":
                {
                    Parser.Consume(HopperToken.Integer, "line number expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodDictionary["line"] = previousToken["lexeme"];
                }
                case "hits":
                {
                    Parser.Consume(HopperToken.Integer, "hits expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                }
                case "locals":
                {
                    Parser.Consume(HopperToken.LBrace, "'{' expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
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
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma, "',' comma expected C");
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "BP offset expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.Colon, "':' expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "local name expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        fc = false;
                    } // loop
                }
                case "debug":
                {
                    <string,string> debugInfo;
                    Parser.Consume(HopperToken.LBrace, "'{' expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
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
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma, "',' comma expected C");
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                            
                        Parser.Consume(HopperToken.StringConstant, "offset key expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string offset = previousToken["lexeme"];
                        
                        Parser.Consume(HopperToken.Colon, "':' expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "line value expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string ln = previousToken["lexeme"];
                        
                        debugInfo[offset] = ln;
                        
                        fc = false;
                    } // loop   
                    if (keepSymbols)
                    {
                        methodDictionary["debug"] = debugInfo;
                    }
                }
                case "code":
                {
                    Parser.Consume(HopperToken.LBracket, "'[' expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
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
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma, "',' comma expected B");
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        Parser.Consume(HopperToken.Integer, "code expected");
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (keepCode)
                        {
                            uint hc;
                            previousToken = PreviousToken;
                            string hexcode = previousToken["lexeme"];
                            if (Token.TryParseUInt(hexcode, ref hc))
                            {
                            }
                            code.Append(byte(hc));
                        }
                        fc = false;
                    } // loop
                }
                default:
                {
                    Parser.Error("unexpected key '" + keyName + "'");
                    break;
                }
            }
            first = false;
        } // loop
        
        if (!isData && keepSymbols)
        {
            uint iMethod;
            if (Token.TryParseUInt(methodIndex, ref iMethod))
            {
            }
            string name = "0x" + iMethod.ToHexString(4);
            string mname = methodDictionary["name"];
            if (mname.EndsWith(".main"))
            {
                name = "main";
            }
            debugSymbols[name] = methodDictionary;
        }
        return !Parser.HadError;
    }
    
    bool ParseCode(string codePath, bool keepCode, bool keepSymbols)
    {
        bool success = false;
        loop
        {
            Scanner.New();
            Parser.Reset();
            Scanner.Load(codePath);
            long pos = 0;
            Scanner.Reset(pos, 1, codePath);
    
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
                    Parser.Consume(HopperToken.Comma, "',' expected C");
                }
                first = false;
                Parser.Consume(HopperToken.StringConstant, "method index expected");
                if (Parser.HadError)
                {
                    break;
                }
                <string,string> previousToken = PreviousToken;
                string methodIndex = previousToken["lexeme"];
                Parser.Consume(HopperToken.Colon, "':' expected");
                if (Parser.HadError)
                {
                    break;
                }
                Parser.Consume(HopperToken.LBrace, "'{' expected");
                if (Parser.HadError)
                {
                    break;
                }
                
                if (methodIndex == "globals")
                {
                    bool fc = true;
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
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma, "',' comma expected C");
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "global offset expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.Colon, "':' expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "global name expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        fc = false;
                    } // loop
                }
                else
                {
                    string name;
                    <byte> code;
                    if (!ParseMethod(methodIndex, keepCode, keepSymbols, ref name, ref code))
                    {
                        break;
                    }
                    uint size = code.Length;
                    if (methodIndex == "const")
                    {
                        if (keepCode)
                        {
                            constantData = code;
                        }
                    }
                    else
                    {
                        uint index;
                        if (Token.TryParseUInt(methodIndex, ref index))
                        {
                        }
                        if (name.EndsWith(".main"))
                        {
                            entryIndex = index;
                        }
                        if (keepCode)
                        {
                            methodCode[index] = code;
                        }
                        methodSize[index] = size;
                    }
                    Parser.Consume(HopperToken.RBrace, "'}' expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                }
                Parser.ProgressTick(".");
            } // loop  
            if (Parser.HadError)
            {
                break;
            }
            success = true;
            break;
        } // loop
        if (keepSymbols)
        {
            MapSource();
        }
        return success;
    }
    
}
