program Help
{
//#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"
    
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
            WriteLn("    DEL        RM       Deletes on or more files.");
            WriteLn("    DIR        LS       Lists files and subdirectories of a directory.");
            WriteLn("    HELP       MAN      Provides help information for Hopper Shell commands.");
#ifndef SERIAL_CONSOLE
            WriteLn("    MORE       TYPE     Displays syntax highlighted file one screen at a time.");
#endif
            WriteLn("    SHELL               Launch a nested Hopper Shell within the current one.");
            WriteLn("    EXIT                Quit the current Hopper Shell.");
            // TODO
            //           MEM        FREE
            //           DATE
            //           TIME
            //           REN        MV
            //           MD         MKDIR
            //           RD         RMDIR
            //           CP         COPY
            // 
        }
        
        else if (args.Count == 1)
        {
            string command = (args[0]).ToLower();
            command = SwitchAlias(command);
            switch (command)
            {
                case "cd":
                case "cls":
                case "del":
                case "dir": 
                {
                    args.Clear();
                    args.Append("-h");
                    
                    command = Common.ResolveCommandPath(command);
                    _ = Runtime.Execute(command, args);
                }
                default: { WriteLn("unknown command '" + command +"'", Colour.MatrixRed); }
            }
            
        }
        else
        {
            WriteLn("unsupported arguments", Colour.MatrixRed);
        }
    }
}
