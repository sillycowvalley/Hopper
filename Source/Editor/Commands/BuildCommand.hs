unit BuildCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    
    // for DefineExists to see which platform to CODEGEN for
    uses "/Source/Compiler/JSON/JSON"
    
    // built successfully this session?
    bool buildSuccess = false;
    
    // reset during compile : checks for H6502 in compilation target symbols
    bool target6502 = false;
    bool Target6502 
    { 
        get { return target6502; }
    }
    
    // set when updating the title bar text to orange, reset after compile
    bool wasModified = false;
    bool WasModified
    {
        get { return wasModified; }
        set { wasModified = value; }
    }
        
    Register()
    {
        Commands.CommandExecuteDelegate buildCommand = BuildCommand.Execute;
        Commands.CommandEnabledDelegate buildEnabled = BuildCommand.Enabled;
        Commands.CommandExecuteDelegate runCommand = BuildCommand.Run;
        Commands.CommandExecuteDelegate debugCommand = BuildCommand.Debug;
        Commands.CommandEnabledDelegate runEnabled = BuildCommand.CanRun;
        Commands.CommandEnabledDelegate debugEnabled = BuildCommand.CanDebug;
        
        Key key = (Key.F7);
        InstallCommand("Build", "&Build", buildCommand, buildEnabled, key);
        key = (Key.F5 | Key.Control);
        InstallCommand("Run", "&Start Without Debugger", runCommand, runEnabled, key);
        key = (Key.F5);
        InstallCommand("Debug", "Launch &Debugger", debugCommand, debugEnabled, key);
    }
    
    DisplayError(string message, uint error)
    {
        if (File.Exists("/Temp/Errors.txt"))
        {
            file errorFile = File.Open("/Temp/Errors.txt");
            string errorText = errorFile.ReadLine();
            if (errorText.Length > 0)
            {
                message = message + ": " + errorText;
            }
        }
        if (error != 0x0E)
        {
            message = "0x" + error.ToHexString(2) + " " + message;
        }
        Editor.SetStatusBarText(message);
    }
    
    
    CheckTarget(string symbolsPath)
    {
        target6502 = false;
        if (File.Exists(symbolsPath))
        {
            <string,variant> dict;
            if (JSON.Read(symbolsPath, ref dict))
            {
                foreach (var kv in dict)
                {
                    switch (kv.key)
                    {
                        case "symbols":
                        {
                            // preprocessor symbols
                            <string,string> pdValues = kv.value;
                            if (pdValues.Contains("H6502"))
                            {
                                target6502 = true;
                            }
                            break;
                        }
                    }
                } // kv
            }    
        }
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
                    Editor.SetStatusBarText("Build Cancelled");
                    break;
                }
            }
            buildSuccess = false;
            
            string binaryPath ="/Bin/PreProcess" + hexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No PreProcessor: '" + binaryPath + "'");
                break;
            }
            string sourcePath = Editor.GetProjectPath();
            string fileName = Path.GetFileName(sourcePath);
            string extension = Path.GetExtension(fileName);
            fileName = fileName.Replace(extension, "");
            
            string jsonPath = "/Debug/Obj/" + fileName + ".json";
            string codePath = "/Debug/Obj/" + fileName + ".code";
            string hexePath = "/Bin/" + fileName + hexeExtension;
            string hasmPath = "/Debug/Obj/" + fileName + hasmExtension;
            
            Editor.SetStatusBarText("Preprocessing '" + sourcePath + "' -> '" + jsonPath + "'");
            
            byte col = Editor.Left + 1;
            byte row = Editor.Top + Editor.Height - 1;
            
            <string> arguments;
            arguments.Append(sourcePath);
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            uint error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("Preprocessor", error);
                break;
            }
            
            binaryPath ="/Bin/Compile" + hexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No Compiler: '" + binaryPath + "'");
                break;
            }
            
            CheckTarget(jsonPath);
            Editor.SetStatusBarText("Compiling '" + jsonPath + "' -> '" + codePath + "'");
            
            arguments.Clear();
            arguments.Append(jsonPath);
            arguments.Append("-o"); // 'o'ptimized, not checked build (release)
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("Compile", error);
                break;
            }
            
            binaryPath ="/Bin/Optimize" + hexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No Optimize: '" + binaryPath + "'");
                break;
            }
            string target = "";
            if (target6502)
            {
                target = " for 6502";
            }
            Editor.SetStatusBarText("Optimizing Code '" + codePath + "' -> '" + codePath + "'" + target);
            
            arguments.Clear();
            arguments.Append(codePath);
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("Optimize", error);
                break;
            }
            
            binaryPath ="/Bin/CODEGEN" + hexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No CODEGEN: '" + binaryPath + "'");
                break;
            }
            Editor.SetStatusBarText("Generating Code '" + codePath + "' -> '" + hexePath + "'" + target);
            
            arguments.Clear();
            arguments.Append(codePath);
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            if (target6502)
            {
                arguments.Append("-ihex");
            }
            error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("CODEGEN", error);
                break;
            }
                        
            binaryPath ="/Bin/DASM" + hexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No DASM: '" + binaryPath + "'");
                break;
            }
            
            Editor.SetStatusBarText("Disassembling '" + hexePath + "' -> '" + hasmPath + "'");
            
            arguments.Clear();
            arguments.Append(hexePath);
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("DASM", error);
                break;
            }
            Editor.SetStatusBarText("Success '" + sourcePath + "' -> '" + hexePath + "'" + target);
            buildSuccess = true;
            wasModified  = false;
            break;   
        }
    }
    string GetBinaryPath()
    {
        string path;
        if (Enabled())
        {
            path = Editor.GetProjectPath();
            path = Path.GetFileName(path);
            string extension = Path.GetExtension(path);
            path = path.Replace(extension, hexeExtension);
            path = Path.Combine("/Bin", path);
        }
        return path;
    }
    Debug()
    {
        Screen.Clear();
        if (!Target6502)
        {
            Die(0x0B); // assume we only arrive here for H6502
        }
        <string> arguments;
        string path = Editor.GetProjectPath();
        string ihexPath = path.Replace(hexeExtension, ".hex");
        arguments.Append(path);
        uint error = System.Execute("Debug", arguments);
        Editor.DrawAll();
    }
    Run()
    {
        Screen.Clear();
        <string> arguments;
        string path = GetBinaryPath();
        if (Target6502) // target was checked during the successful build
        {
            string ihexPath = path.Replace(hexeExtension, ".hex");
            arguments.Append("-x"); // <ctrl><F5>
            arguments.Append("-l");
            arguments.Append(ihexPath);
            path = "hm";
        }
        uint error = System.Execute(path, arguments);
        Editor.DrawAll();
    }
    
    bool Enabled()
    {
        string path = Editor.GetProjectPath();
        string extension = Path.GetExtension(path);
        extension = extension.ToLower();
        return extension == ".hs";
    }
    
    // Conditions for when we need to rebuild:
    // - we can only run if we successfully built the project during this session
    // - if we modify a file, we need to rebuild (does not matter if we undo the modification)
    // Not ideal but Hopper does not have file timestamps and I figure it is better
    // to be strict than to unwittingly run/debug a stale build.
    
    bool CanRun()
    {
        string path = GetBinaryPath();
        bool canRun = File.Exists(path);
        if (canRun)
        {
            canRun = !wasModified && buildSuccess;
        }
        return canRun;
    }
    bool CanDebug()
    {
        string path = GetBinaryPath();
        bool canRun = File.Exists(path);
        if (canRun)
        {
            canRun = !wasModified && buildSuccess && Target6502;
        }
        return canRun;
    }
    
}

