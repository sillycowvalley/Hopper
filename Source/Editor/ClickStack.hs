unit ClickStack
{
    uses "/Source/System/System"
    
    uses "/Source/Debugger/Source" // to load symbols
    
    <string> clickStack;
    
    string poppedFromLocation;
    
    Push(string path, uint ln, uint column)
    {
        string location = path + ":" + ln.ToString() + "," + column.ToString();
        clickStack.Append(location);
        poppedFromLocation = "";
    }
    Pop(string path, uint ln, uint column)
    {
        uint length = clickStack.Count;
        if (0 != length)
        {
            string toLocation = clickStack[length-1];
            clickStack.Remove(length-1);
            ClickStack.Load(toLocation);
            poppedFromLocation = path + ":" + ln.ToString() + "," + column.ToString();
        }
    }
    bool Flip(string path, uint ln, uint column)
    {
        bool flipped;
        string fromLocation = path + ":" + ln.ToString() + "," + column.ToString();
        if (poppedFromLocation.Length != 0)
        {
            // revert the last pop instead of flipping
            clickStack.Append(fromLocation);
            ClickStack.Load(poppedFromLocation);
            poppedFromLocation = "";
            flipped = true;
        }
        else if (clickStack.Count > 0)
        {
            uint length = clickStack.Count;
            string toLocation = clickStack[length-1];
            clickStack[length-1] = fromLocation; // the flip
            ClickStack.Load(toLocation);
            flipped = true;
        }
        return flipped;
    }
    Load(string location)
    {
        <string> parts = location.Split(':');
        if (parts.Count == 2)
        {
            string sourcePath = parts[0];
            string clickCurrentPath = Editor.CurrentPath;
            uint ln;
            uint col;
            bool gotoLine = false;
            bool gotoColumn = false;
            <string> locationParts = (parts[1]).Split(',');
            if (UInt.TryParse(locationParts[0], ref ln))
            {
                gotoLine = true;
            }
            if ((locationParts.Count == 2) && UInt.TryParse(locationParts[1], ref col))
            {
                gotoColumn = true;
            }
            //OutputDebug("Load: " + ln.ToString() + "," + col.ToString() + 
            //           (gotoLine ? " gotoLine" : "") + (gotoColumn ? " gotoColumn" :""));
            if (sourcePath.ToLower() != clickCurrentPath.ToLower())
            {
                if (Editor.CanUndo())
                {
                    // OpenPath(string suggestedPath, bool openPrompt, bool pushClick)
                    Editor.OpenPath(sourcePath, false, false); // offer undo
                }
                else
                {
                    // LoadFile(string path, uint gotoLine, bool defaultLine, bool pushClick)
                    Editor.LoadFile(sourcePath, 0, true, false);
                }
                clickCurrentPath = Editor.CurrentPath;
                gotoLine = (sourcePath.ToLower() == clickCurrentPath.ToLower());
            }
            if (gotoLine)
            {
                //
                // GotoLineNumber(uint gotoLine,      -1 : TextBuffer.GetLineCount() / EOF
                //                uint gotoColumn,    actual column or ~10000 to mean EOL
                //                bool defaultLine,   true: EOF
                //                bool defaultColumn) true: first non-space character on the line
                //
                if (Editor.GotoLineNumber(ln, col, false, !gotoColumn))
                {
                }
                if (gotoColumn)
                {
                    //OutputDebug("Loaded: " + ln.ToString() + "," + col.ToString() + ", clickStack.Count=" + (clickStack.Count).ToString());
                }
                else
                {
                    //OutputDebug("Loaded: " + ln.ToString()+ ", clickStack.Count=" + (clickStack.Count).ToString());
                }
            }
            else 
            {
                //OutputDebug("Loaded: no gotoLine" );
            }
        }
    }
    bool ContextClick(string contextWord, string beforeWord, string afterWord, uint clickPos, uint clickLine, uint clickColumn)
    {
        bool clickedThrough;
        string location;
        string clickLocation;
        loop
        {
            // Bypass symbol loading to:
            // - jump instantly on 'uses' paths
            // - allow 'uses' jumps to work even if .sym does not exist
            string clickLineFragment = beforeWord + contextWord + afterWord;
            clickLineFragment = clickLineFragment.Trim();
            if (clickLineFragment.StartsWith(IncludeToken))
            {
                string candidate = contextWord.Replace("\"", "");
                candidate = ResolveUsesPath(clickLineFragment, candidate);
                if (candidate.Length != 0)
                {
                    location = candidate + ":1";
                    ClickStack.Push(Editor.CurrentPath, clickLine, clickColumn);
                    ClickStack.Load(location);
                    return true;
                }
            }
            
            if (!Source.DefinitionSymbolsLoaded)
            {
                string clickProjectPath = Editor.ProjectPath;
                string extension = Path.GetExtension(clickProjectPath);
                string jsonPath  = clickProjectPath.Replace(extension, ".sym");
                jsonPath = Path.GetFileName(jsonPath);
                jsonPath = Path.Combine("/Debug/Obj/", jsonPath);
                if (!File.Exists(jsonPath))
                {
                    Editor.SetStatusBarText("'" + Path.GetFileName(jsonPath) + "' not found. Successful pre-process run required for <right><click> -> definition.");
                    break;
                }
                if (!Source.LoadDefinitions(jsonPath))
                {
                    Editor.SetStatusBarText("Failed to load definitions from '" + Path.GetFileName(jsonPath) + "'");
                    break;
                }
                // successfully loaded symbols (which called Scanner.New()) so we 
                // need to reload assembler keywords if isAssembler
                Editor.CheckAssemblerSource(false);
            }
            string currentNamespace = Symbols.GetNamespace(Editor.CurrentPath);
            clickLocation = (Editor.CurrentPath).ToLower() + ":" + clickLine.ToString();
            
            uint iFirst; uint iLast;
            if (contextWord.IndexOf('.', ref iFirst) && contextWord.LastIndexOf('.', ref iLast) && (iFirst != iLast))
            {
                contextWord = contextWord.Substring(iFirst+1); // "Keyboard.Key.Backspace" -> "Key.Backspace":
            }
            
            uint iDot;
            bool firstClick = false;
            bool secondClick = false;
            string firstWord = contextWord;
            string secondWord = contextWord;
            if (contextWord.IndexOf('.', ref iDot))
            {
                firstClick  = clickPos <  iDot;
                secondClick = clickPos >= iDot;
                firstWord   = contextWord.Substring(0, iDot);
                secondWord  = contextWord.Substring(iDot+1);
                if ((firstWord.Length != 0) && (secondWord.Length != 0))
                {
                    if (firstClick && (firstWord[0]).IsUpper())
                    {
                        string path = Symbols.GetNamespaceLocation(firstWord);
                        if (path.Length != 0)
                        {
                            // clicked on the namespace qualifier part of an identifier
                            location = path + ":1";
                            break;
                        }
                    }
                    if ((firstWord[0]).IsLower())
                    {                 
                        // probably a 'this' variable -> replace with type/namespace
                        if (secondClick)
                        {
                            string idType;
                            
                            // try arguments first
                            uint iOverload;
                            if ((firstWord[0]).IsLower() && Symbols.TryFindMethod(clickLocation, ref iOverload))
                            {
                                < < string > > arguments = Symbols.GetOverloadArguments(iOverload);
                                foreach (var argument in arguments)
                                {
                                    if (argument[2] == firstWord)
                                    {
                                        idType = argument[1];
                                        break;
                                    }
                                }
                            }
                            
                            // then globals
                            if (idType.Length == 0)
                            {
                                string globalCandidate = currentNamespace + "." + firstWord;
                                if (Symbols.GlobalMemberExists(globalCandidate))
                                {
                                    uint gIndex = Symbols.GetGlobalMemberIndex(globalCandidate);
                                    idType = Symbols.GetGlobalType(gIndex);
                                }
                            }
                            if (idType.Length != 0)
                            {
                                string thisNamespace = Types.ToNamespace(idType);
                                contextWord = thisNamespace + "." + secondWord;
                                // fall through to TryFindMethod(..) below ..
                            }
                        } // secondClick
                    }
                    if ((secondWord[0]).IsLower() && (firstWord == currentNamespace))
                    {
                        firstWord = secondWord;
                        secondWord = "";
                        firstClick = true;
                        secondClick = false;
                        // fall through to firstClick handling below for constants and globals in this unit
                    }
                    if ((firstWord[0]).IsUpper() && (secondWord[0]).IsUpper())
                    {
                        location = Symbols.GetLocation(contextWord);
                        if (location.Length != 0)
                        {
                            // clicked on a qualified public symbol like an enum or a flag or a qualified public constant
                            break;
                        }
                    }
                }
            }
            else // no '.'
            {
                if (beforeWord.StartsWith('#'))
                {
                    location = Symbols.GetLocation(contextWord);
                    if (location.Length != 0)
                    {
                        // clicked on a preprocessor symbol
                        break;
                    }
                }
                firstWord = contextWord;
                firstClick = true;
            }
            if (firstClick && (firstWord.Length != 0))
            {
                if (Token.IsTypeKeyword(firstWord))
                {
                    string namespace = Types.ToNamespace(firstWord);
                    string path = Symbols.GetNamespaceLocation(namespace);
                    
                    if (path.Length != 0)
                    {
                        // clicked on a simply type like 'byte' -> uint Byte
                        location = path + ":1";
                        break;
                    }
                }
                uint iOverload;
                if ((firstWord[0]).IsLower() && Symbols.TryFindMethod(clickLocation, ref iOverload))
                {
                    
                    < < string > > arguments = Symbols.GetOverloadArguments(iOverload);
                    foreach (var argument in arguments)
                    {
                        if (argument[2] == firstWord)
                        {
                            location = Symbols.GetMethodLocation(iOverload);
                            break;
                        }
                    }
                }
                if (location.Length != 0)
                {
                    // clicked on an argument, goto the function definition
                    break;
                }
                
                // single word symbol: constant, define ..
                string symbolCandidate = currentNamespace + "." + firstWord;
                if (Symbols.GlobalMemberExists(symbolCandidate))
                {
                    uint gIndex = Symbols.GetGlobalMemberIndex(symbolCandidate);
                    
                    // clicked on a global variable in this unit
                    location = Symbols.GetGlobalLocation(gIndex);
                    break;
                }
                symbolCandidate = Symbols.QualifyConstantIdentifier(firstWord, currentNamespace);
                location = Symbols.GetLocation(symbolCandidate);
                if (location.Length != 0)
                {
                    // clicked on a constant
                    break;
                }
                
                symbolCandidate = QualifiedNamedType(firstWord, currentNamespace);
                location = Symbols.GetLocation(symbolCandidate);
                if (location.Length != 0)
                {
                    // clicked on an enum, flags or delegate type (not member)
                    break;
                }
            }
            
            bool setter;
            bool getter;
            if (Symbols.TryFindMethod(currentNamespace, ref contextWord, ref setter, ref getter))
            {
                if (getter && setter)
                {
                    if (afterWord.StartsWith('='))
                    {
                        // setter rather than getter?
                        getter = false;
                    }
                    else //if (afterWord.StartsWith(';') || beforeWord.EndsWith('='))
                    {
                        // getter rather than setter?
                        setter = false;
                    }
                }
                if (getter)
                {
                    contextWord += "_Get";
                }
                else if (setter)
                {
                    contextWord += "_Set";
                }
                uint fIndex;
                if (!Symbols.GetFunctionIndex(contextWord, ref fIndex))
                {
                    Editor.SetStatusBarText("Failed function index for '" + contextWord + "'");
                    break;
                }
                <uint> overloads = Symbols.GetFunctionOverloads(fIndex);
                if (overloads.Count == 0)
                {
                    Editor.SetStatusBarText("Failed overloads for '" + contextWord + "'");
                    break;
                }
                
                if (overloads.Count > 1)
                {
                    Editor.SetStatusBarText((overloads.Count).ToString() + " overloads found for '" + contextWord + "', jumping to first overload.");
                    //break;
                }
                // found a method call, setter or getter (source, syscall or libcall)
                location = GetMethodLocation(overloads[0]);
                break;
            }
            
            string candidate = ResolveUsesPath(beforeWord + contextWord + afterWord, contextWord);
            if (candidate.Length != 0)
            {
                //OutputDebug("Uses candidate: '" + candidate + "'");
                location = candidate + ":1";
                break;
            }
            OutputDebug("Unresolved ContextClick: '"+ beforeWord + "' '" + contextWord + "' '" + afterWord + "' (" + currentNamespace + ")");                
            break;
        }
        if (location.Length != 0)
        {
            if (clickLocation != location)
            {
                string cursorLocation = (Editor.CurrentPath).ToLower() + ":" + Editor.GetCurrentLineNumber().ToString();
                if (cursorLocation != location)
                {
                    ClickStack.Push(Editor.CurrentPath, clickLine, clickColumn);
                    ClickStack.Load(location);
                    clickedThrough = true;
                }
            }
        }
        return clickedThrough;
    }
}
