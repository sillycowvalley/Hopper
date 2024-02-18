unit Dependencies
{
    uses "/Source/System/File"
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/Directives"
    
    bool TryGetYoungest(<string> sources, ref string youngestPath, ref long youngest)
    {
        youngest = 0;
        string youngestStr = youngest.ToHexString(8);
        foreach (var path in sources)
        {
            long fileTime = File.GetTime(path);
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
    
    bool TryGetSources(string primaryPath, <string> sources)
    {
        //OutputDebug("TryGetSources: " + primaryPath);
        
        sources.Clear();
        Directives.New();
        <string,bool> usesPathsToParse;
        primaryPath = primaryPath.ToLower();
        usesPathsToParse[primaryPath] = true;
        sources.Append(primaryPath);
        string usesPrefix = "uses" + '"';
        loop
        {
            string nextPath;
            foreach (var kv in usesPathsToParse)
            {
                if (kv.value)
                {
                    nextPath = kv.key;
                    break;
                }
            }
            if (nextPath.Length == 0)
            {
                break; // done
            }
            usesPathsToParse[nextPath] = false; // we are parsing it
            file textFile = File.Open(nextPath);
            //OutputDebug("TryGetSources: Open: " + nextPath);
            if (!textFile.IsValid())
            {
                return false;
            }
            bool isAllDefined = true; // reset for each new file
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
                    else if (ln.StartsWith("#else"))
                    {
                        if (!Directives.NestingFlipTail())
                        {
                            return false;
                        }
                    }
                    else if (ln.StartsWith("#endif"))
                    {
                        if (!Directives.NestingPopTail())
                        {
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
