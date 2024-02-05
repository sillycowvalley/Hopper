program Shell
{
        
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/EditControl"
    uses "/Source/System/Runtime"
    
    uses "/Source/Shell/Common"
    
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
            if (File.Exists(binaryname + HexeExtension))
            {
                 command = binaryname + HexeExtension;
            }
            else if (File.Exists(binaryname + ".cmd"))
            {
                command = binaryname + ".cmd";        
            }
            else
            {
                binaryname = Path.Combine(CurrentDirectory, command);
                if (File.Exists(binaryname + HexeExtension))
                {
                    command = binaryname + HexeExtension;
                }
                else if (File.Exists(binaryname + ".cmd"))
                {
                    command = binaryname + ".cmd";        
                }
                else
                {
                    binaryname = Path.Combine("/bin", command);
                    if (File.Exists(binaryname + HexeExtension))
                    {
                        command = binaryname + HexeExtension;
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
    
    bool Run(string command, <string> arguments)
    {
        bool success = false;
        loop
        {
            if (!File.Exists(command)) // fullpath already?
            {
                break;
            }
            uint result = Runtime.Execute(command, arguments);
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
            if (options.Count != 0)
            {
                PrintLn("Batch scripts don't support options.", MatrixRed, Black);
                break;
            }
            if (arguments.Count != 0)
            {
                PrintLn("Batch scripts don't support arguments.", MatrixRed, Black);
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
                Print(currentDirectory + ">", Colour.MatrixBlue, Colour.Black); // colour just to help with testing for now
                Print(commandLine);
                if (!RunCommandLine(commandLine, true))
                {
                    PrintLn("Failure in batch script '" + command + "':", MatrixRed, Black);
                    PrintLn("  '" + commandLine + "' ", MatrixRed, Black);
                    break;    
                }
            }
            break;
        }
        return success;
    }    
    
    bool RunCommandLine(string commandLine, bool inBatch)
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
        command = Common.SwitchAlias(command);
        switch (command)
        {
            // built-ins:
            case "exit":
            {
                Screen.Clear();
                exiting = true;
            }
            
            // hexes and cmds:
            default:
            {
                command = ExtendPath(command);
                string extension = Path.GetExtension(command);
                extension = extension.ToLower();
                if (extension == HexeExtension)
                {
                    if (!Run(command, rawargs))
                    {
                        if (!inBatch)
                        {
                            PrintLn("Failed to run '" + command + "'", MatrixRed, Black);
                        }
                        success = false;
                    }
                }
                else if (extension == ".cmd")
                {
                    if (!RunBatch(command, options, args))
                    {
                        if (!inBatch)
                        {
                            PrintLn("Failed to run '" + command + "'", MatrixRed, Black);
                        }
                        success = false;
                    }
                }
                else
                {
                    if (!inBatch)
                    {
                        PrintLn("Unknown command '" + command + "'", MatrixRed, Black);
                    }
                    success = false;
                }
            }
        } // switch (command.ToUpper())
        return success;
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
        EditControl.SetColours(Colour.MatrixGreen, Colour.Black);
        
        < string > previousCommands;
        uint currentPreviousCommand = 0;
        
        Print(currentDirectory + ">", Colour.MatrixBlue, Colour.Black); // colour just to help with testing for now
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
                        for (uint i=0; i < previousCommands.Count; i++)
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
                        if (RunCommandLine(commandLine, false))
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
                    Print(currentDirectory + ">", Colour.MatrixBlue, Colour.Black); // colour just to help with testing for now
                    x = currentDirectory.Length+1;
                    current = x;
                }
                case Key.Up:
                {
                    // <up> - clear the current commandLine and replace with previous command
                    if (previousCommands.Count != 0) // previous commands exist
                    {
                        loop
                        {
                            uint length = previousCommands.Count;
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
                    if (previousCommands.Count != 0) // previous commands exist
                    {
                        loop
                        {
                            uint length = previousCommands.Count;
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
                    _ = EditControl.OnKey(key, x, w-x, ref commandLine, ref current);
                } // default
                
            } // switch (key)
            
            if (redraw)
            {
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
            }
        } // loop
    }
}
