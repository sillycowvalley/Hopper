unit BuildOptions
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    
    // to load and save options
    uses "/Source/Compiler/JSON/JSON"
    
    <string, string> buildOptions;
    bool IsCheckedEnabled()      { return buildOptions["checkedBuild"]        != "false"; }
    bool IsOptimizeEnabled()     { return buildOptions["runOptimizer"]        != "false"; }
    bool IsDisassembleEnabled()  { return buildOptions["runDisassembler"]     != "false"; }
    bool IsAutoSaveEnabled()     { return buildOptions["autoSaveOnBuild"]     != "false"; }
    
    Register()
    {
        buildOptions["checkedBuild"]        = "false";
        buildOptions["runOptimizer"]        = "true";  // default to Optimizing if no .options file
        buildOptions["runDisassembler"]     = "false";
        buildOptions["autoSaveOnBuild"]     = "false";
        loadOptions();
        
        Commands.CommandExecuteDelegate checkedCommand = BuildOptions.Checked;
        Commands.CommandEnabledDelegate checkedEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate checkedIsChecked = BuildOptions.IsCheckedEnabled;
        Commands.CommandExecuteDelegate optimizeCommand = BuildOptions.Optimized;
        Commands.CommandEnabledDelegate optimizeEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate optimizeIsChecked = BuildOptions.IsOptimizeEnabled;
        Commands.CommandExecuteDelegate disassembleCommand = BuildOptions.Disassemble;
        Commands.CommandEnabledDelegate disassembleEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate disassembleIsChecked = BuildOptions.IsDisassembleEnabled;
        Commands.CommandExecuteDelegate autosaveCommand = BuildOptions.AutoSave;
        Commands.CommandEnabledDelegate autosaveEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate autosaveIsChecked = BuildOptions.IsAutoSaveEnabled;
        
        InstallCommand("Checked",     "[ ] &Create Checked Build", checkedCommand, checkedEnabled, Key.NoKey);
        InstallChecked("Checked", checkedIsChecked);
        InstallCommand("Optimize",    "[ ] Run &Optimizer", optimizeCommand, optimizeEnabled, Key.NoKey);
        InstallChecked("Optimize", optimizeIsChecked);
        InstallCommand("Disassemble", "[ ] Run &Disassembler", disassembleCommand, disassembleEnabled, Key.NoKey);
        InstallChecked("Disassemble", disassembleIsChecked);
        InstallCommand("AutoSave", "[ ] &Save on Build", autosaveCommand, autosaveEnabled, Key.NoKey);
        InstallChecked("AutoSave", autosaveIsChecked);
    }
    loadOptions()
    {
        if (File.Exists(OptionsPath))
        {
            <string, variant> dict;
            if (JSON.Read(OptionsPath, ref dict))
            {
                buildOptions = dict["buildoptions"];
            }
        }
    }
    saveOptions()
    {
        if ((OptionsPath).Length != 0)
        {
            File.Delete(OptionsPath);
            <string, variant> dict;
            dict["buildoptions"] = buildOptions;
            if (JSON.Write(OptionsPath, dict))
            {
            }
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
    AutoSave()
    {
        if (buildOptions["autoSaveOnBuild"] == "false")
        {
            buildOptions["autoSaveOnBuild"] = "true";
        }
        else
        {
            buildOptions["autoSaveOnBuild"] = "false";
        }
        saveOptions();
    }
    
}
