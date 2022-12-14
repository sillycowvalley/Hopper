unit Commands
{
    uses "/Source/System/System"
    uses "/Source/System/Keyboard"

    uses "/Source/Editor/Commands/ExitCommand"
    uses "/Source/Editor/Commands/BuildCommand"
    
    delegate CommandExecuteDelegate();
    delegate bool CommandEnabledDelegate();
    
    <string, Key> acceleratorKey;
    <string, string> menuText;
    <string, CommandExecuteDelegate> commandsExecute;
    <string, CommandEnabledDelegate> commandsEnabled;
    
    
    Initialize()
    {
        ExitCommand.Register();
        BuildCommand.Register();
        Editor.RegisterCommands();
        
        // TODO : these should all eventually be Commands.Register() calls
        
        acceleratorKey["Replace"] = (Key.Control | Key.ModH);
        menuText["Replace"] = "Rep&lace";
        
        //acceleratorKey["StartDebugging"] = Key.F5;
        //menuText["StartDebugging"] = "&Start Debugging";

        //acceleratorKey["StartWithoutDebugging"] = (Key.Control | Key.F5);
        //menuText["StartWithoutDebugging"] = "Start Wit&hout Debugging";

        //acceleratorKey["StopDebugging"] = (Key.Shift | Key.F5);
        //menuText["StopDebugging"] = "Stop &Debugging";

        //acceleratorKey["StepInto"] = Key.F11;
        //menuText["StepInto"] = "Step &Into";

        //acceleratorKey["StepOver"] = Key.F10;
        //menuText["StepOver"] = "Step &Over";

        //acceleratorKey["StepOut"] = (Key.Shift | Key.F11);
        //menuText["StepOut"] = "Step Ou&t";

        //acceleratorKey["ToggleBreakpoint"] = Key.F9;
        //menuText["ToggleBreakpoint"] = "To&ggle Breakpoint";

        //acceleratorKey["DeleteAllBreakpoints"] = (Key.Control | Key.Shift | Key.F9);
        //menuText["DeleteAllBreakpoints"] = "Delete &All Breakpoints";
        
        //acceleratorKey["Build"] = Key.F7;
        //menuText["Build"] = "&Build";
    }
    
    string cleanName(string name)
    {
        name = name.Replace("&", "");
        return name.Replace(" ", "");
    }
    InstallCommand(string name, string menu, CommandExecuteDelegate command, CommandEnabledDelegate enabled, Key key)
    {
        menuText[name] = menu;
        commandsExecute[name] = command;
        commandsEnabled[name] = enabled;
        if (key != Key.NoKey)
        {
            acceleratorKey[name] = key;
        }
    }
    
    bool IsEnabled(string name)
    {
        bool enabled = false;
        name = cleanName(name);
        if (commandsEnabled.Contains(name))
        {
            CommandEnabledDelegate command = commandsEnabled[name];
            enabled = command();
        }
        return enabled;
    }
    Execute(string name)
    {
        name = cleanName(name);
        if (commandsExecute.Contains(name))
        {
            CommandExecuteDelegate command = commandsExecute[name];
            command();
        }
        else
        {
            System.Beep();
        }
    }
    
    string GetMenuText(string name)
    {
        string result;
        name = cleanName(name);
        if (menuText.Contains(name))
        {
            result = menuText[name];
        }
        return result;
    }
    
    string GetAcceleratorName(string name)
    {
        string content;
        name = cleanName(name);
        if (acceleratorKey.Contains(name))
        {
            Key key = acceleratorKey[name];
            if (Key.NoKey != (key & (Key.Control | Key.Shift | Key.Alt)))
            {
                content = content + Keyboard.ModifiersToString(key) + "+";
                content = content.Replace('|', '+');
                content = content.Replace("Control", "Ctrl");
            }
            content = content + Keyboard.KeyToString(key);
            content = content.Replace("'", "");
        }
        return content;
    }
    
    string KeyToCommand(Key candidateKey)
    {
        string winner;
        foreach (var kv in acceleratorKey)
        {
            if (kv.value == candidateKey)
            {
                winner = kv.key;
                break; // winner
            }
        }
        if (winner.Length > 0)
        {
            if (!IsEnabled(winner))
            {
                winner = "";
            }
        }
        return winner;
    }
                    
}
