unit ExitCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/Editor/Commands"
    
    bool exiting = false;
    
    Register()
    {
        Commands.CommandExecuteDelegate exitCommand = ExitCommand.Execute;
        Commands.CommandEnabledDelegate exitEnabled = ExitCommand.Enabled;
        
        Key key = (Key.Alt | Key.F4);
        InstallCommand("Exit", "E&xit", exitCommand, exitEnabled, key);
    }
    Execute()
    {
        loop
        {
            if (Editor.CanUndo())
            {
                string result = Editor.OfferSave();
                if (result == "Cancel")
                {
                    break;
                }
            }
            exiting = true;
            Screen.Clear();
            SetCursor(0,0);
            if (IsDebugger)
            {
                PrintLn("Hopper Debugger exited.", Color.MatrixGreen, Color.Black);
            }
            else
            {
                PrintLn("Hopper Editor exited.", Color.MatrixGreen, Color.Black);
            }
            break;
        }
    }
    
    bool Enabled()
    {
        return true;
    }
    bool IsExiting()
    {
        return exiting;
    }
}

