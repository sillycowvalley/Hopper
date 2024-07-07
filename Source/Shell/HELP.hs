program Help
{
//#define SERIAL_CONSOLE
    uses "Common"
    
    {
        <string> args = Arguments;
        if (args.Count == 0)
        {
            // Inspiration:
            // https://static-bcrf.biochem.wisc.edu/tutorials/unix/basic_unix/dos_and_unix_commands.html
            WriteLn("For more information about a specific command, type HELP <command name>");
            WriteLn("  Command:   Alias:");
            WriteLn("    CD         CHDIR    Displays the name of or changes the current directory.");
            WriteLn("    CLS        CLEAR    Clears the screen.");
            WriteLn("    CMD        SHELL    Starts a new instance of the Hopper Shell.");
            WriteLn("    CP         COPY     Copies on or more files between directories.");
            WriteLn("    DEL        RM       Deletes on or more files.");
            WriteLn("    DIR        LS       Lists files and subdirectories of a directory.");
            WriteLn("    HELP       MAN      Provides help information for Hopper Shell commands.");
#ifdef SERIAL_CONSOLE
            WriteLn("    MORE       TYPE     Echos file content to console.");
#else
            WriteLn("    MORE       TYPE     Displays syntax highlighted file one screen at a time.");
#endif
            WriteLn("    MD         MKDIR    Create new directory.");
            WriteLn("    RD         RMDIR    Remove empty directory.");
            WriteLn("    PORT                Select the current COM port (for DEBUG and HM).");
            WriteLn("    BAUD                Select the baud rate for COM ports (for DEBUG and HM).");
            WriteLn("    SHELL               Launch a nested Hopper Shell within the current one.");
            WriteLn("    EXIT                Quit the current Hopper Shell.");
            // TODO
            //           MEM        FREE
            //           DATE
            //           TIME
            //           REN        MV
        }
        
        else if (args.Count == 1)
        {
            string command = (args[0]).ToLower();
            command = SwitchAlias(command);
            switch (command)
            {
                case "cd":
                case "cls":
                case "copy":
                case "del":
                case "dir":
                case "mkdir":
                case "rmdir":
                case "show":
                {
                    args.Clear();
                    args.Append("-h");
                    
                    command = Common.ResolveCommandPath(command);
                    _ = Runtime.Execute(command, args);
                }
                default: { WriteLn("no help for command '" + command +"'", Colour.MatrixRed); }
            }
            
        }
        else
        {
            WriteLn("unsupported arguments", Colour.MatrixRed);
        }
    }
}
