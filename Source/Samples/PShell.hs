program Shell
{
        
#define SERIAL_CONSOLE

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "/Source/System/IO"
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
            if (options.Length != 0)
            {
                PrintLn("Batch scripts don't support options.", MatrixRed, Black);
                break;
            }
            if (arguments.Length != 0)
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
    
    ChangeDirectory(<string> options, <string> arguments)
    {
        loop
        {
            bool found = false;
            if ((arguments.Length != 1) || (options.Length != 0))
            {
                // zero arguments or spaces between more than one part
                PrintLn("Invalid arguments for CD.", MatrixRed, Black);
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
                PrintLn("Invalid arguments for CD.", MatrixRed, Black); // System.Beep();
            }
            break;
        }
        
    }
    
    ClearScreen(<string> options, <string> arguments)
    {
        if ((arguments.Length != 0) || (options.Length != 0))
        {
            PrintLn("Invalid arguments.", MatrixRed, Black);
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
        EditControl.SetColours(Colour.MatrixGreen, Colour.Black);
        
        < string > previousCommands;
        uint currentPreviousCommand = 0;
        
        Print(currentDirectory + ">", Colour.MatrixBlue, Colour.Black); // colour just to help with testing for now
        uint x = currentDirectory.Length+1;
        uint w = 120;
        uint current = x;
        string commandLine;
        loop
        {
            bool redraw = false;

            char ch = IO.Read();
            if (ch == char(0x0D))
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
            else if (ch == char(0x1B)) // Escape
            {
                currentPreviousCommand = 0;
                redraw = true;
                commandLine = "";
            }
            else // Edit keys:
            {
                if (!EditControl.OnKey(ch, x, w-x, ref commandLine, ref current))
                {
                    // not consumed
                }
            }

            if (redraw)
            {
                while (current > x)
                {
                    Print(char(0x08) + " " + char(0x08));
                    current--;
                }
                Print(commandLine);
                current = current + commandLine.Length;
            }
        } // loop
    }
}
