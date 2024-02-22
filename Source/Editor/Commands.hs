unit Commands
{
    uses "/Source/System/System"
    uses "/Source/System/Keyboard"

    uses "/Source/Editor/Commands/ExitCommand"
#ifdef DEBUGGER
    uses "/Source/Debugger/DebugCommand"
    uses "/Source/Debugger/DebugOptions"
#else
    uses "/Source/Editor/Commands/BuildCommand"
    uses "/Source/Editor/Commands/BuildOptions"
#endif    
    
    delegate CommandExecuteDelegate();
    delegate bool CommandEnabledDelegate();
    delegate bool CommandCheckedDelegate();
    
    <string, Key> acceleratorKey;
    <string, string> menuText;
    <string, CommandExecuteDelegate> commandsExecute;
    <string, CommandEnabledDelegate> commandsEnabled;
    <string, CommandCheckedDelegate> commandsChecked;
    
    
    Initialize()
    {
        ExitCommand.Register();
#ifndef DEBUGGER        
        BuildCommand.Register();
        BuildOptions.Register();   
#else
        DebugCommand.Register();
        DebugOptions.Register();
        
#endif        
        Editor.RegisterCommands();
        
        acceleratorKey["Replace"] = (Key.Control | Key.ModH);
        menuText["Replace"] = "Rep&lace";
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
    InstallChecked(string name, CommandCheckedDelegate checked)
    {
        commandsChecked[name] = checked;
    }
    bool IsChecked(string name)
    {
        bool checked = false;
        name = cleanName(name);
        if (commandsChecked.Contains(name))
        {
            CommandCheckedDelegate command = commandsChecked[name];
            checked = command();
        }
        return checked;
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
        if (winner.Length != 0)
        {
            if (!IsEnabled(winner))
            {
                winner = "";
            }
        }
        return winner;
    }
                    
}
