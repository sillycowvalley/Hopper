unit Dependencies
{
    uses "/Source/System/File"
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/Directives"
    
    // Examples:
    //    '!defined(SERIAL_CONSOLE)&&defined(MCU)'  
    //    '!defined(DISPLAY_DRIVER)'
    
    bool parsePrimary(ref string expression, ref bool condition)
    {
        //OutputDebug("parsePrimary: " + expression);
        bool result;
        loop
        {
            if (expression.StartsWith("defined"))
            {
                expression = expression.Substring(7);
                if (!expression.StartsWith("("))
                {
                    break;
                }
                expression = expression.Substring(1);
                uint iClose;
                if (!expression.IndexOf(')', ref iClose))
                {
                    break;
                }
                string symbol = expression.Substring(0, iClose);
                expression = expression.Substring(iClose+1);
                condition = Symbols.DefineExists(symbol);
                //OutputDebug(symbol + " " + (condition ? "TRUE" : "FALSE"));
                result = true;
            }
            else if (expression.StartsWith("("))
            {
                expression = expression.Substring(1);
                result = parseExpression(ref expression, ref condition);
                if (!result)
                {
                    break;
                }
                if (!expression.StartsWith(")"))
                {
                    result = false;
                    break;
                }
                expression = expression.Substring(1);
            }
            break;
        } // loop
        return result;
    }  
    bool parseUnary(ref string expression, ref bool condition)
    {
        //OutputDebug("parseUnary: " + expression);
        bool result;
        loop
        {
            bool isNot = false;
            if (expression.StartsWith("!"))
            {
                expression = expression.Substring(1);
                isNot = true;
            }
            result = parsePrimary(ref expression, ref condition);
            if (!result)
            {
                break;
            }
            if (isNot)
            {
                condition = !condition;
            }
            break;
        } // loop
        return result;
    }
    
    bool parseBooleanAnd(ref string expression, ref bool condition)
    {
        //OutputDebug("parseBooleanAnd: " + expression);
        bool result;
        loop
        {
            result = parseUnary(ref expression, ref condition);
            if (!result)
            {
                break;
            }
            loop
            {
                if (!expression.StartsWith("&&"))
                {
                    break;
                }
                expression = expression.Substring(2);
                bool rightCondition;
                result = parseUnary(ref expression, ref rightCondition);
                condition = condition && rightCondition;
                if (!result)
                {
                    break;
                }
                continue;
            } // loop
            break;
        } // loop
        return result;
    }
    
    bool parseBooleanOr(ref string expression, ref bool condition)
    {
        //OutputDebug("parseBooleanOr: " + expression);
        bool result;
        loop
        {
            result = parseBooleanAnd(ref expression, ref condition);
            if (!result)
            {
                break;
            }
            loop
            {
                if (!expression.StartsWith("||"))
                {
                    break;
                }
                expression = expression.Substring(2);
                bool rightCondition;
                result = parseBooleanAnd(ref expression, ref rightCondition);
                condition = condition || rightCondition;
                if (!result)
                {
                    break;
                }
                continue;
            } // loop
            break;
        } // loop
        return result;
    }
    bool parseExpression(ref string expression, ref bool condition)
    {
        //OutputDebug("parseExpression: " + expression);
        return parseBooleanOr(ref expression, ref condition);
    }
    
    bool TryGetYoungest(<string> sources, ref string youngestPath, ref long youngest)
    {
        youngest = 0;
        string youngestStr = youngest.ToHexString(8);
        foreach (var path in sources)
        {
            long fileTime = File.GetTimeStamp(path);
            string fileTimeStr = fileTime.ToHexString(8);
            int cmp = String.Compare(fileTimeStr, youngestStr);
            //OutputDebug(path + " " + fileTimeStr + " " + youngestStr + " " + cmp.ToString());
            if ((fileTimeStr >= youngestStr) || (youngest == 0))
            {
                youngest = fileTime;
                youngestStr = youngest.ToHexString(8);
                youngestPath = path;
                if (youngest == 0)
                {
                    return false;           
                }
            }
        }
        return true;
    }
    
    string ResolveRelativePath(string hsPath, string currentPath, string projectPath)
    {
        // same search path logic as in usesDeclaration(..) in PreProcess
        if (!File.Exists(hsPath))
        {
            string tryFile = hsPath;
            uint removeLevels = 0;
            if (tryFile.StartsWith("./"))
            {
                tryFile = tryFile.Substring(2);
            }
            while (tryFile.StartsWith("../"))
            {
                tryFile = tryFile.Substring(3);
                removeLevels++;
            }
            if (!tryFile.StartsWith("/"))
            {
                // first try relative to current source file:
                string currentDirectory = Path.GetDirectoryName(currentPath);
                while (removeLevels > 0)
                {
                    currentDirectory = Path.GetDirectoryName(currentDirectory);
                    removeLevels--;
                }
                string tryPath = Path.Combine(currentDirectory, tryFile);
                if (File.Exists(tryPath))
                {
                    hsPath = tryPath;
                }
                else
                {
                    // then try relative to main project file
                    string projectDirectory = Path.GetDirectoryName(projectPath);
                    tryPath = Path.Combine(projectDirectory, tryFile);
                    if (File.Exists(tryPath))
                    {
                        hsPath = tryPath;
                    }
                }
            }
        }
        return hsPath;
    }
    
    bool TryGetSources(string primaryPath, <string> sources)
    {
        //OutputDebug("TryGetSources: " + primaryPath);
        
        sources.Clear();
        Directives.New();
        <string,bool> usesPathsToParse;
        <string,string> usesPathsSource;
        primaryPath = primaryPath.ToLower();
        usesPathsToParse[primaryPath] = true;
        usesPathsSource[primaryPath] = primaryPath;
        sources.Append(primaryPath);
        string usesPrefix = "uses" + '"';
        loop
        {
            string nextPath;
            string nextSourcePath;
            foreach (var kv in usesPathsToParse)
            {
                if (kv.value)
                {
                    nextPath = kv.key;
                    nextSourcePath = usesPathsSource[nextPath];
                    break;
                }
            }
            if (nextPath.Length == 0)
            {
                break; // done
            }
            usesPathsToParse[nextPath] = false; // we are parsing it
            
            nextPath = ResolveRelativePath(nextPath, nextSourcePath, Editor.ProjectPath);
            
            file textFile = File.Open(nextPath);
            //OutputDebug("TryGetSources: Open: " + nextPath);
            if (!textFile.IsValid())
            {
                OutputDebug("TryGetSources Failed A: " + nextPath + " (from " + nextSourcePath + ")");
                return false;
            }
            bool isAllDefined = true; // reset for each new file
            uint lineNumber;
            loop
            {
                string ln = textFile.ReadLine();
                if (ln.Length == 0)
                {
                    if (!textFile.IsValid())
                    {
                        break;
                    }
                }
                lineNumber++;
                ln = ln.Replace(" ", "");
                uint iComment;
                if (ln.IndexOf("//", ref iComment))
                {
                    ln = ln.Substring(0, iComment);
                }
                
                if (ln.StartsWith(usesPrefix))
                {
                    ln = ln.Substring(5);
                    ln = ln.Substring(0, ln.Length-1);
                    ln = ln.ToLower();
                    
                    string extension = Path.GetExtension(ln);
                    if (extension == ".")
                    {
                        ln = ln + ".hs";
                    }
                    if (!usesPathsToParse.Contains(ln))
                    {
                        if (isAllDefined)
                        {
                            usesPathsToParse[ln] = true;
                            usesPathsSource[ln] = nextPath;
                            sources.Append(ln);
                        }
                    }
                }
                else if (ln.StartsWith('#'))
                {
                    if (ln.StartsWith("#define"))
                    {
                        string symbol = ln.Substring(7);
                        Symbols.AddDefine(symbol, "true");
                    }
                    else if (ln.StartsWith("#ifdef"))
                    {
                        string symbol = ln.Substring(6);
                        Directives.NestingAppend(symbol, true);
                    }
                    else if (ln.StartsWith("#ifndef"))
                    {
                        string symbol = ln.Substring(7);
                        Directives.NestingAppend(symbol, false);
                    }
                    else if (ln.StartsWith("#if"))
                    {
                        bool condition;
                        string expression = ln.Substring(3);
                        string expressionBefore = expression;
                        if (!parseExpression(ref expression, ref condition))
                        {
                            OutputDebug("TryGetSources Failed D: '" + expressionBefore + "' " + nextPath + ":" + lineNumber.ToString());
                            return false;
                        }
                        //OutputDebug(expressionBefore + " " + (condition ? "TRUE" : "FALSE"));
                        NestingAppend(condition);
                    }
                    else if (ln.StartsWith("#else"))
                    {
                        if (!Directives.NestingFlipTail())
                        {
                            OutputDebug("TryGetSources Failed B: " + nextPath + ":" + lineNumber.ToString());
                            return false;
                        }
                    }
                    else if (ln.StartsWith("#endif"))
                    {
                        if (!Directives.NestingPopTail())
                        {
                            OutputDebug("TryGetSources Failed C: " + nextPath + ":" + lineNumber.ToString());
                            return false;
                        }
                    }
                    isAllDefined = Directives.IsAllDefined();
                }
            }
        }
        return true;
    }
}
