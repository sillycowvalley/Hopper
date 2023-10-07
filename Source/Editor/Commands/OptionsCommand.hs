unit OptionsCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    
    // to load and save options
    uses "/Source/Compiler/JSON/JSON"
    
    <string, string> buildOptions;
    bool IsCheckedEnabled()     { return buildOptions["checkedBuild"] != "false"; }
    bool IsOptimizeEnabled()    { return buildOptions["runOptimizer"] != "false"; }
    bool IsDisassembleEnabled() { return buildOptions["runDisassembler"] != "false"; }
    
    Register()
    {
        buildOptions["checkedBuild"] = "false";
        buildOptions["runOptimizer"] = "false";
        buildOptions["runDisassembler"] = "false";
        loadOptions();
        
        Commands.CommandExecuteDelegate checkedCommand = OptionsCommand.Checked;
        Commands.CommandEnabledDelegate checkedEnabled = OptionsCommand.AlwaysEnabled;
        Commands.CommandCheckedDelegate checkedIsChecked = OptionsCommand.IsCheckedEnabled;
        Commands.CommandExecuteDelegate optimizeCommand = OptionsCommand.Optimized;
        Commands.CommandEnabledDelegate optimizeEnabled = OptionsCommand.AlwaysEnabled;
        Commands.CommandCheckedDelegate optimizeIsChecked = OptionsCommand.IsOptimizeEnabled;
        Commands.CommandExecuteDelegate disassembleCommand = OptionsCommand.Disassemble;
        Commands.CommandEnabledDelegate disassembleEnabled = OptionsCommand.AlwaysEnabled;
        Commands.CommandCheckedDelegate disassembleIsChecked = OptionsCommand.IsDisassembleEnabled;
        
        InstallCommand("Checked",     "[ ] &Create Checked Build", checkedCommand, checkedEnabled, Key.NoKey);
        InstallChecked("Checked", checkedIsChecked);
        InstallCommand("Optimize",    "[ ] Run &Optimizer", optimizeCommand, optimizeEnabled, Key.NoKey);
        InstallChecked("Optimize", optimizeIsChecked);
        InstallCommand("Disassemble", "[ ] Run &Disassembler", disassembleCommand, disassembleEnabled, Key.NoKey);
        InstallChecked("Disassemble", disassembleIsChecked);
        
        
    }
    loadOptions()
    {
        string optionsPath = Path.Combine("/Bin/", "Edit.options");
        if (File.Exists(optionsPath))
        {
            <string, variant> dict;
            if (JSON.Read(optionsPath, ref dict))
            {
                buildOptions = dict["buildoptions"];
            }
        }
    }
    saveOptions()
    {
        string optionsPath = Path.Combine("/Bin/", "Edit.options");
        File.Delete(optionsPath);
        <string, variant> dict;
        dict["buildoptions"] = buildOptions;
        if (JSON.Write(optionsPath, dict))
        {
        }
    }
    bool AlwaysEnabled()
    {
        return true;
    }
    Checked()
    {
        if (buildOptions["checkedBuild"] == "false")
        {
            buildOptions["checkedBuild"] = "true";
        }
        else
        {
            buildOptions["checkedBuild"] = "false";
        }
        saveOptions();
    }
    Optimized()
    {
        if (buildOptions["runOptimizer"] == "false")
        {
            buildOptions["runOptimizer"] = "true";
        }
        else
        {
            buildOptions["runOptimizer"] = "false";
        }
        saveOptions();
    }
    Disassemble()
    {
        if (buildOptions["runDisassembler"] == "false")
        {
            buildOptions["runDisassembler"] = "true";
        }
        else
        {
            buildOptions["runDisassembler"] = "false";
        }
        saveOptions();
    }
    
}
