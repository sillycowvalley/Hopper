program MCUShell
{
        
#define SERIAL_CONSOLE

    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    uses "/Source/System/IO"
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/EditControl"
    
    uses "/Source/Shell/Common"
    
    const uint consoleColumns = 80;
    
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
     
    bool RunScript(string command, <string> options, <string> arguments, ref bool wasExit)
    {
        bool success = false;
        loop
        {
            if (options.Count != 0)
            {
                IO.WriteLn("Scripts don't support options.");
                break;
            }
            if (arguments.Count != 0)
            {
                IO.WriteLn("Scripts don't support arguments.");
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
                IO.Write(currentDirectory + ">");
                IO.Write(commandLine);
                if (!RunCommandLine(commandLine, true, ref wasExit))
                {
                    IO.WriteLn("Failure in batch script '" + command + "':");
                    IO.WriteLn("  '" + commandLine + "' ");
                    break;    
                }
                if (wasExit)
                {
                    break;
                }
            }
            break;
        }
        return success;
    }     
    
    bool RunCommandLine(string commandLine, bool inScript, ref bool wasExit)
    {
        bool success = true;
        IO.WriteLn();
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
                IO.WriteLn("Shell exited");
                wasExit = true;
            }
            
            // hexes and cmds:
            default:
            {
                command = ResolveCommandPath(command);
                string extension = Path.GetExtension(command);
                extension = extension.ToLower();
                if (extension == HexeExtension)
                {
                    if (!Run(command, rawargs))
                    {
                        if (!inScript)
                        {
                            IO.WriteLn("Failed to run '" + command + "'");
                        }
                        success = false;
                    }
                }
                else if (extension == ".cmd")
                {
                    if (!RunScript(command, options, args, ref wasExit))
                    {
                        if (!inScript)
                        {
                            IO.WriteLn("Failed to run '" + command + "'");
                        }
                        success = false;
                    }
                }
                else
                {
                    if (!inScript)
                    {
                        IO.WriteLn("Unknown command '" + command + "'");
                    }
                    success = false;
                }
            }
        } // switch (command.ToUpper())
        return success;
    } 
    bool ValidCommandLineCharacter(char c)
    {
        return (IsValidPathCharacter(c) || (c == ' ') || (c == '-') || (c == '*'));
    }
    
    {
        string currentDirectory = CurrentDirectory;
        
        EditControl.ValidEditCharacter validator = ValidCommandLineCharacter;
        EditControl.SetValidation(validator);
        
        < string > previousCommands;
        uint currentPreviousCommand = 0;
        
        IO.Write(currentDirectory + ">");
        uint x = currentDirectory.Length+1;
        uint w = consoleColumns;
        uint current = x;
        string commandLine;
        loop
        {
            bool redraw = false;
            char maker;
            char ch2;
            char ch = IO.Read();
            if (ch == char(0xE0))
            {
                ch2 = IO.Read();
                maker = ch;  
                ch = ch2;
            }
            Key key = Keyboard.FromSerial(ch, maker);
            switch (key)
            {
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
                                    // try again : probably switching between <up> and <down>
                                    continue;
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
                    // <down> - clear the current commandLine and replace with next command
                    if (previousCommands.Count != 0) // previous commands exist
                    {
                        loop
                        {
                            uint length = previousCommands.Count;
                            if ((currentPreviousCommand > 0) && 
                                (currentPreviousCommand-1 < length)
                               )
                            {
                                currentPreviousCommand--;
                                string currentString = previousCommands[currentPreviousCommand];
                                if ((length > 1) && (commandLine == currentString))
                                {
                                    // try again : probably switching between <up> and <down>
                                    continue;
                                }
                                commandLine = currentString;
                                redraw = true;
                            }
                            break;
                        }
                    }
                }
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
                        bool wasExit;
                        if (RunCommandLine(commandLine, false, ref wasExit))
                        {
                        }
                        if (wasExit)
                        {
                            break;
                        }
                        commandLine = "";
                    }
                    else
                    {
                        IO.WriteLn();
                    }
                    currentDirectory = CurrentDirectory;
                    IO.Write(currentDirectory + ">");
                    x = currentDirectory.Length+1;
                    current = x;
                }
                case Key.Escape:
                {
                    currentPreviousCommand = 0;
                    redraw = true;
                    commandLine = "";
                }
                default:
                {
                    //_ = EditControl.OnKey(ch, x, w-x, ref commandLine, ref current);
                    _ = EditControl.OnKey(key, x, w-x, ref commandLine, ref current);
                }
            } // switch
            if (redraw)
            {
                while (current > x)
                {
                    IO.Write(char(0x08) + " " + char(0x08));
                    current--;
                }
                IO.Write(commandLine);
                current = current + commandLine.Length;
            }
        } // loop
    }
}
