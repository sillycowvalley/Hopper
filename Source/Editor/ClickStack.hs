unit ClickStack
{
    uses "/Source/System/System"
    
    uses "/Source/Debugger/Source" // to load symbols
    
    <string> clickStack;
    
    Push(string path, uint ln)
    {
        string location = path + ":" + ln.ToString();
        //OutputDebug("Push:" + location);
        clickStack.Append(location);
    }
    Pop()
    {
        uint length = clickStack.Count;
        if (0 != length)
        {
            string location = clickStack[length-1];
            //OutputDebug("Pop:" + location);
            clickStack.Remove(length-1);
            ClickStack.Load(location);
            
            // don't keep this Load(..) in the stack:
            length = clickStack.Count;
            if (0 != length)
            {
                clickStack.Remove(length-1);
            }
        }
    }
    Load(string location)
    {
        <string> parts = location.Split(':');
        if (parts.Count == 2)
        {
            string sourcePath = parts[0];
            string currentPath = Editor.GetCurrentPath();
            uint ln;
            bool gotoLine = false;
            if (UInt.TryParse(parts[1], ref ln))
            {
                gotoLine = true;
            }
            if (sourcePath.ToLower() != currentPath.ToLower())
            {
                if (Editor.CanUndo())
                {
                    Editor.OpenPath(sourcePath); // offer undo
                }
                else
                {
                    Editor.LoadFile(sourcePath);
                }
                currentPath = Editor.GetCurrentPath();
                gotoLine = (sourcePath.ToLower() == currentPath.ToLower());
            }
            if (gotoLine)
            {
                //
                // GotoLineNumber(uint gotoLine,      -1 : TextBuffer.GetLineCount() / EOF
                //                uint gotoColumn,    actual column or ~10000 to mean EOL
                //                bool defaultLine,   true: EOF
                //                bool defaultColumn) true: first non-space character on the line
                //
                if (Editor.GotoLineNumber(ln, 0, false, true))
                {
                }
            }
        }
    }
    bool ContextClick(string contextWord, string beforeWord, string afterWord, uint clickPos, uint clickLine)
    {
        bool clickedThrough;
        string location;
        string clickLocation;
        loop
        {
            if (!Source.DefinitionSymbolsLoaded)
            {
                string projectPath = Editor.GetProjectPath();
                string extension = Path.GetExtension(projectPath);
                string jsonPath  = projectPath.Replace(extension, ".json");
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
            }
            string currentNamespace = Symbols.GetNamespace(Editor.GetCurrentPath());
            clickLocation = Editor.GetCurrentPath().ToLower() + ":" + clickLine.ToString();
            
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
                if ((firstWord.Length > 0) && (secondWord.Length > 0))
                {
                    if (firstClick && (firstWord[0]).IsUpper())
                    {
                        string path = Symbols.GetNamespaceLocation(firstWord);
                        if (path.Length > 0)
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
                if (IsSimpleType(firstWord))
                {
                    string path = Symbols.GetNamespaceLocation(Types.ToNamespace(firstWord));
                    if (path.Length > 0)
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
            
            OutputDebug("Unresolved ContextClick: '"+ beforeWord + "' '" + contextWord + "' '" + afterWord + "' (" + currentNamespace + ")");                
            break;
        }
        if (location.Length != 0)
        {
            if (clickLocation != location)
            {
                string cursorLocation = Editor.GetCurrentPath().ToLower() + ":" + Editor.GetCurrentLineNumber().ToString();
                if (cursorLocation != location)
                {
                    ClickStack.Push(Editor.GetCurrentPath(), Editor.GetCurrentLineNumber());
                    ClickStack.Load(location);
                    clickedThrough = true;
                }
            }
        }
        return clickedThrough;
    }
}
