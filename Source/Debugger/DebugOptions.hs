unit DebugOptions
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    
    // to load and save options
    uses "/Source/Compiler/JSON/JSON"
    
    <string, string> debugOptions;
    bool IsHexDisplay()     { return debugOptions.Contains("hexDisplay") && (debugOptions["hexDisplay"] != "false"); }
    bool IsHexDisplayMode { get { return IsHexDisplay(); } }
    
    bool IsCaptureConsole()     { return debugOptions.Contains("captureConsole") && (debugOptions["captureConsole"] != "false"); }
    bool IsCaptureConsoleMode { get { return IsCaptureConsole(); } }
    
    Register()
    {
        debugOptions["hexDisplay"]    = "false";
        debugOptions["captureConsole"]    = "false";
        loadOptions();
        
        Commands.CommandExecuteDelegate hexDisplayCommand   = DebugOptions.HexDisplay;
        Commands.CommandEnabledDelegate hexDisplayEnabled   = DebugOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate hexDisplayIsChecked = DebugOptions.IsHexDisplay;
        
        Commands.CommandExecuteDelegate captureConsoleCommand   = DebugOptions.CaptureConsole;
        Commands.CommandEnabledDelegate captureConsoleEnabled   = DebugOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate captureConsoleIsChecked = DebugOptions.IsCaptureConsole;
        
        InstallCommand("HexDisplay", "[ ] &Hexadecimal Display", hexDisplayCommand, hexDisplayEnabled, Key.NoKey);
        InstallChecked("HexDisplay", hexDisplayIsChecked);
        InstallCommand("CaptureConsole", "[ ] &Capture Console Output", captureConsoleCommand, captureConsoleEnabled, Key.NoKey);
        InstallChecked("CaptureConsole", captureConsoleIsChecked);
        
    }
    loadOptions()
    {
        string optionsPath = Path.Combine("/Bin/", "Debug.options");
        if (File.Exists(optionsPath))
        {
            <string, variant> dict;
            if (JSON.Read(optionsPath, ref dict))
            {
                debugOptions = dict["debugoptions"];
            }
        }
    }
    saveOptions()
    {
        string optionsPath = Path.Combine("/Bin/", "Debug.options");
        File.Delete(optionsPath);
        <string, variant> dict;
        dict["debugoptions"] = debugOptions;
        if (JSON.Write(optionsPath, dict))
        {
        }
    }
    bool AlwaysEnabled()
    {
        return true;
    }
    HexDisplay()
    {
        if (debugOptions["hexDisplay"] == "false")
        {
            debugOptions["hexDisplay"] = "true";
        }
        else
        {
            debugOptions["hexDisplay"] = "false";
        }
        saveOptions();
        DebugCommand.RefreshWatch();
    }
    CaptureConsole()
    {
        if (debugOptions["captureConsole"] == "false")
        {
            debugOptions["captureConsole"] = "true";
        }
        else
        {
            debugOptions["captureConsole"] = "false";
        }
        saveOptions();
        ConsoleCapture.ClearLog();
    }
}
