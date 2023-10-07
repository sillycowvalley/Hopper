program Shell
{
        
//#define TINYHOPPER  

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/EditControl"
    uses "/Source/System/Runtime"
    
    bool exiting;
    bool loaded = false;
    
    string ExtendPath(string command)
    {
        string extension = Path.GetExtension(command);
        extension = extension.ToLower();
        if (extension == ".hexe")
        {
            // good
        }
        else if (extension == ".cmd")
        {
            // good
        }
        else if (extension == ".")
        {
            string binaryname = command; // full path?
            if (File.Exists(binaryname + hexeExtension))
            {
                 command = binaryname + hexeExtension;
            }
            else if (File.Exists(binaryname + ".cmd"))
            {
                command = binaryname + ".cmd";        
            }
            else
            {
                binaryname = Path.Combine(CurrentDirectory, command);
                if (File.Exists(binaryname + hexeExtension))
                {
                    command = binaryname + hexeExtension;
                }
                else if (File.Exists(binaryname + ".cmd"))
                {
                    command = binaryname + ".cmd";        
                }
                else
                {
                    binaryname = Path.Combine("/bin", command);
                    if (File.Exists(binaryname + hexeExtension))
                    {
                        command = binaryname + hexeExtension;
                    }
                    else if (File.Exists(binaryname + ".cmd"))
                    {
                        command = binaryname + ".cmd";        
                    }   
                }
            }
        }
        return command;        
    }
    
    bool Load(string command, <string> arguments)
    {
        bool success = false;
        command = ExtendPath(command);
        string extension = Path.GetExtension(command);
        extension = extension.ToLower();
        loaded = false;
        if (extension == hexeExtension)
        {
            success = Runtime.Load(command, arguments);
            if (success)
            {
                uint bytesLoaded = Runtime.BytesLoaded;
                PrintLn(bytesLoaded.ToString() + " bytes loaded.");
                loaded = true;
            }
        }
        return success;
    }
    
    bool Run(string command, <string> arguments)
    {
        bool success = false;
        loop
        {
            if (!File.Exists(command)) // fullpath already?
            {
                break;
            }
            uint result = System.Execute(command, arguments);
            
            // TODO REMOVE
            if (result != 0)
            {
                string content = result.ToString() + " " + command;
                OutputDebug(content); 
            }
            success = (result == 0);
            break;
        }
        return success;
    }
    
    bool RunBatch(string command, <string> options, <string> arguments)
    {
        bool success = false;
        loop
        {
            if (options.Length > 0)
            {
                PrintLn("Batch scripts don't support options.");
                break;
            }
            if (arguments.Length > 0)
            {
                PrintLn("Batch scripts don't support arguments.");
                break;
            }
            if (!File.Exists(command))
            {
                break;
            }
            file cmdFile = File.Open(command);
            if (!cmdFile.IsValid())
            {
                break;
            }
            success = true;
            loop
            {
                string commandLine = cmdFile.ReadLine();
                
                uint iComment;
                if (commandLine.IndexOf("//", ref iComment))
                {
                    commandLine = commandLine.Substring(0, iComment);
                }
                commandLine = commandLine.Trim();
                
                string args;
                if (commandLine.Length == 0)
                {
                    if (!cmdFile.IsValid())
                    {
                        break;
                    }
                    continue; // ignore blank lines
                }
                string currentDirectory = CurrentDirectory;
                Print(currentDirectory + ">", Color.MatrixBlue, Color.Black); // colour just to help with testing for now
                Print(commandLine);
                if (!RunCommandLine(commandLine))
                {
                    PrintLn("Failure in batch script:");
                    PrintLn("  '" + commandLine + "' ");
                    break;    
                }
            }
            break;
        }
        return success;
    }    
    
    bool RunCommandLine(string commandLine)
    {
        bool success = true;
        PrintLn();
        string command = commandLine;
        string arguments;
        uint iSpace;
        if (commandLine.IndexOf(' ', ref iSpace))
        {
            arguments = commandLine.Substring(iSpace+1);
            command = commandLine.Substring(0, iSpace);
        }
        <string> rawargs = arguments.Split(' ');
        <string> options;
        <string> args;
        foreach (var arg in rawargs)
        {
            if ((arg.Length == 2) && arg.StartsWith('-'))
            {
                options.Append(arg.ToLower());
            }
            else
            {
                args.Append(arg);
            }
        }
        switch (command.ToUpper())
        {
            // built-ins:
            case "CLS":
            {
                ClearScreen(options, args);
            }
            case "EXIT":
            {
                ClearScreen(options, args);
                exiting = true;
            }
            case "CD":
            {
                ChangeDirectory(options, args);
            }
            case "RUN":
            {
                if (!loaded)
                {
                    PrintLn("Nothing is loaded.");
                }
                else
                {
                    Runtime.SetVisibility(true);
                    Runtime.Run();
                }    
            }
            case "LOAD":
            {
                if (rawargs.Length == 0)
                {
                    
                }
                else
                {
                    command = rawargs[0];
                    args.Clear();
                    for (uint i=1; i < rawargs.Length; i++)
                    {
                        args.Append(rawargs[i]);
                    }
                    if (!Load(command, args))
                    {
                        PrintLn("Failed to load '" + command + "'");
                        success = false;
                    }
                }
            }
            
            // hexes and cmds:
            default:
            {
                command = ExtendPath(command);
                string extension = Path.GetExtension(command);
                extension = extension.ToLower();
                if (extension == hexeExtension)
                {
                    if (!Run(command, rawargs))
                    {
                        PrintLn("Failed to run '" + command + "'");
                        success = false;
                    }
                }
                else if (extension == ".cmd")
                {
                    if (!RunBatch(command, options, args))
                    {
                        PrintLn("Failed to run '" + command + "'");
                        success = false;
                    }
                }
                else
                {
                    PrintLn("Unknown command '" + command + "'");
                    success = false;
                }
            }
        } // switch (command.ToUpper())
        return success;
    }
    
    ChangeDirectory(<string> options, <string> arguments)
    {
        loop
        {
            bool found = false;
            if ((arguments.Length != 1) || (options.Length != 0))
            {
                // zero arguments or spaces between more than one part
                PrintLn("Invalid arguments for CD.");
                break;
            }
            string path = arguments[0];
            if (!path.EndsWith('/'))
            {
                path = path + '/';
            }
            
            if (path == "./")
            {
                break;
            }
            if (path == "../")
            {
                string upPath = CurrentDirectory;
                upPath = Path.GetDirectoryName(upPath);
                if (upPath.Length == 0)
                {
                    break;
                }
                else if (Directory.Exists(upPath))
                {
                    path = upPath;
                    found = true;
                }
                else
                {
                    break; // do nothing?
                }
            }
            if (!found && !path.StartsWith('/'))
            {
                string fullPath = Path.Combine(CurrentDirectory, path);
                if (Directory.Exists(fullPath))
                {
                    path = fullPath;
                    found = true;
                }
            }
            if (!found && Directory.Exists(path))
            {
                found = true;
            }
            if (found)
            {
                CurrentDirectory = path;
            }
            else
            {
                PrintLn("Invalid arguments for CD."); // System.Beep();
            }
            break;
        }
        
    }
    
    ClearScreen(<string> options, <string> arguments)
    {
        if ((arguments.Length != 0) || (options.Length != 0))
        {
            PrintLn("Invalid arguments.");
        }
        else
        {
            Screen.Clear();
        }
    }
    
    bool ValidCommandLineCharacter(char c)
    {
        if (IsValidPathCharacter(c) 
          || (c == ' ') 
          || (c == '-')
          || (c == '*')
           )
        {
            return true;
        }
        return false;
    }
    
    
    {
        string currentDirectory = CurrentDirectory;
        
        EditControl.ValidEditCharacter validator = ValidCommandLineCharacter;
        EditControl.SetValidation(validator);
        EditControl.SetColours(Color.MatrixGreen, Color.Black);
        
        < string > previousCommands;
        uint currentPreviousCommand = 0;
        
        Print(currentDirectory + ">", Color.MatrixBlue, Color.Black); // colour just to help with testing for now
        uint x = currentDirectory.Length+1;
        uint w = Screen.Columns;
        uint current = x;
        string commandLine;
        loop
        {
            bool redraw = false;
            Key  key = ReadKey();
            switch (key)
            {
                // Command keys:
                case Key.Enter:
                {
                    commandLine = commandLine.Trim();
                    if (commandLine.Length != 0)
                    {
                        // if it exists in the list already, move it to the front
                        uint iFound = 0;
                        bool wasFound = false;
                        for (uint i=0; i < previousCommands.Length; i++)
                        {
                            string currentString = previousCommands[i];
                            if (commandLine == currentString)
                            {
                                iFound = i;
                                wasFound = true;
                                break;
                            }
                        }
                        if (wasFound)
                        {
                            // move it to the front
                            if (iFound != 0) // already the front?
                            {
                                previousCommands.Remove(iFound);
                                previousCommands.Insert(0, commandLine);
                            }
                        }
                        else
                        {   
                            // insert new commandline in front
                            previousCommands.Insert(0, commandLine);
                        }
                        currentPreviousCommand = 0; // reset for <up> and <down>
                    }
                    
                    if (commandLine.Length != 0)
                    {
                        if (RunCommandLine(commandLine))
                        {
                        }
                        if (exiting)
                        {
                            break;
                        }
                        commandLine = "";
                    }
                    else
                    {
                        PrintLn();
                    }
                    currentDirectory = CurrentDirectory;
                    Print(currentDirectory + ">", Color.MatrixBlue, Color.Black); // colour just to help with testing for now
                    x = currentDirectory.Length+1;
                    current = x;
                }
                case Key.Up:
                {
                    // <up> - clear the current commandLine and replace with previous command
                    if (previousCommands.Length > 0) // previous commands exist
                    {
                        loop
                        {
                            uint length = previousCommands.Length;
                            if (currentPreviousCommand < length)
                            {
                                string currentString = previousCommands[currentPreviousCommand];
                                currentPreviousCommand++;
                                if ((length > 1) && (commandLine == currentString))
                                {
                                    continue; // try again : probably switching between <up> and <down>
                                }
                                commandLine = currentString;
                                redraw = true;
                            }
                            break;
                        }
                    }
                }
                case Key.Down:
                {
                    // <down> - clear the current commandLine and replace with next previous command
                    if (previousCommands.Length > 0) // previous commands exist
                    {
                        loop
                        {
                            uint length = previousCommands.Length;
                            if ((currentPreviousCommand > 0) && (currentPreviousCommand-1 < length))
                            {
                                currentPreviousCommand--;
                                string currentString = previousCommands[currentPreviousCommand];
                                if ((length > 1) && (commandLine == currentString))
                                {
                                    continue; // try again : probably switching between <up> and <down>
                                }
                                commandLine = currentString;
                                redraw = true;
                            }
                            break;
                        }
                    }
                }
                case Key.Escape:
                {
                    currentPreviousCommand = 0;
                    redraw = true;
                    commandLine = "";
                }
                
                // Edit keys:
                default:
                {
                    if (!EditControl.OnKey(key, x, w-x, ref commandLine, ref current))
                    {
                        // not consumed
                    }
                } // default
                
            } // switch (key)
            
            if (redraw)
            {
#ifdef TINYHOPPER                
                while (current > x)
                {
                    Print(char(0x08) + " " + char(0x08));
                    current--;
                }
                Print(commandLine);
                current = current + commandLine.Length;
#else
                Screen.Suspend();
                uint y = Screen.CursorY;
                uint padWidth = 0;
                if (w > x)
                {
                    padWidth = w - x;
                    if (padWidth > 0)
                    {
                        padWidth--;
                    }   
                }
                string paddedText = commandLine.Pad(' ', padWidth);
                Screen.SetCursor(x, y);
                Screen.Print(paddedText);
                current = x + commandLine.Length;
                Screen.SetCursor(current, y);
                Screen.Resume(true);
#endif                
                
            }
        } // loop
    }
}
