unit BuildOptions
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    
    // to load and save options
    uses "/Source/Compiler/JSON/JSON"
    
    <string, string> buildOptions;
    bool IsCheckedEnabled()      { return buildOptions["checkedBuild"]    != "false"; }
    bool IsOptimizeEnabled()     { return buildOptions["runOptimizer"]    != "false"; }
    bool IsGenerateIHexEnabled() { return buildOptions["generateIHex"] != "false"; }
    bool IsDisassembleEnabled()  { return buildOptions["runDisassembler"] != "false"; }
    bool IsAutoSaveEnabled()     { return buildOptions["autoSaveOnBuild"] != "false"; }
    
    Register()
    {
        buildOptions["checkedBuild"]    = "false";
        buildOptions["runOptimizer"]    = "false";
        buildOptions["generateIHex"]    = "false";
        buildOptions["runDisassembler"] = "false";
        buildOptions["autoSaveOnBuild"] = "false";
        loadOptions();
        
        Commands.CommandExecuteDelegate checkedCommand = BuildOptions.Checked;
        Commands.CommandEnabledDelegate checkedEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate checkedIsChecked = BuildOptions.IsCheckedEnabled;
        Commands.CommandExecuteDelegate optimizeCommand = BuildOptions.Optimized;
        Commands.CommandEnabledDelegate optimizeEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate optimizeIsChecked = BuildOptions.IsOptimizeEnabled;
        Commands.CommandExecuteDelegate ihexCommand = BuildOptions.GenerateIHex;
        Commands.CommandEnabledDelegate ihexEnabled = BuildOptions.AlwaysEnabled;
        Commands.CommandCheckedDelegate ihexIsChecked = BuildOptions.IsGenerateIHexEnabled;
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
        InstallCommand("GenerateIHex", "[ ] Generate &IHex", ihexCommand, ihexEnabled, Key.NoKey);
        InstallChecked("GenerateIHex", ihexIsChecked);
        InstallCommand("Disassemble", "[ ] Run &Disassembler", disassembleCommand, disassembleEnabled, Key.NoKey);
        InstallChecked("Disassemble", disassembleIsChecked);
        InstallCommand("AutoSave", "[ ] &Save on Build", autosaveCommand, autosaveEnabled, Key.NoKey);
        InstallChecked("AutoSave", autosaveIsChecked);
        
        
        
        
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
    GenerateIHex()
    {
        if (buildOptions["generateIHex"] == "false")
        {
            buildOptions["generateIHex"] = "true";
        }
        else
        {
            buildOptions["generateIHex"] = "false";
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
