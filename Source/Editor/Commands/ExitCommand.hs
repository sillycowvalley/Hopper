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
        
        Key key = (Key.Alt | Key.ModX);
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
            if (Editor.IsEditor)
            {
                Screen.Clear();
                SetCursor(0,0);
                PrintLn("Hopper Editor exited.", Colour.MatrixGreen, Colour.Black);
            }
            else 
            {
                // debugger
                if (IsInteractive)
                {
                    // launched from editor
                }
                else
                {
                    // launched from command line
                    Screen.Clear();
                    SetCursor(0,0);
                    PrintLn("Hopper Debugger exited.", Colour.MatrixGreen, Colour.Black);
                }
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

