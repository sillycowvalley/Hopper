program Shell
{
        
#define SERIAL_CONSOLE

    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/IO"
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
        
        IO.Write(currentDirectory + ">");
        uint x = currentDirectory.Length+1;
        uint w = consoleColumns;
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
            else if (ch == char(0x1B)) // Escape
            {
                redraw = true;
                commandLine = "";
            }
            else // Edit keys:
            {
                _ = EditControl.OnKey(ch, x, w-x, ref commandLine, ref current);
            }

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
