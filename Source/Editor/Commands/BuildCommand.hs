unit BuildCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    
    // for DefineExists to see which platform to CODEGEN for
    uses "/Source/Compiler/JSON/JSON"
    
    uses "/Source/Compiler/Tokens/Dependencies"
    
    // reset during compile : 
    //   -  checks for H6502 in compilation target symbols using CheckTarget(..) after preprocess step
    // or, failing that, in GetBinaryPath()
    //   - checks if a ".hex" exists when a ".hexe" is not found
    
    bool target6502 = false;
    bool Target6502 
    { 
        get { return target6502; }
    }
    string GetBinaryPath() // used in Run(..), Debug(..), CanRun(..) and CanDebug(..)
    {
        string path;
        if (Enabled())
        {
            path = Editor.GetProjectPath();
            path = Path.GetFileName(path);
            string extension = Path.GetExtension(path);
            path = path.Replace(extension, hexeExtension);
            path = Path.Combine("/Bin", path);
            target6502 = false;
            string ihexPath = path.Replace(hexeExtension, ".hex");   
            if (File.Exists(ihexPath))
            {
                if (File.Exists(path)) 
                {
                    // both .hexe and .hex exist    
                    long hexeFileTime = File.GetTime(path);
                    long hexFileTime  = File.GetTime(ihexPath);
                    string hexeFileTimeHex = hexeFileTime.ToHexString(8);
                    string hexFileTimeHex  = hexFileTime.ToHexString(8);
                    if (hexFileTimeHex >= hexeFileTimeHex)
                    {
                        // .hex is younger than .hexe
                        OutputDebug("HEX 1 " + path + " " + ihexPath + " " + hexeFileTimeHex + " " + hexFileTimeHex);
                        path = ihexPath;
                        target6502 = true;
                    }
                    else
                    {
                        OutputDebug("HEXE 2 " + path + " " + ihexPath + " " + hexeFileTimeHex + " " + hexFileTimeHex);
                    }
                }
                else
                {
                    // only .hex exists
                    OutputDebug("HEX 2 " + path + " " + ihexPath);
                    path = ihexPath;
                    target6502 = true;
                }
            }
            else
            {
                OutputDebug("HEXE 1 " + path + " " + ihexPath);
            }
        }
        return path;
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
    
        
    Register()
    {
        Commands.CommandExecuteDelegate buildCommand = BuildCommand.Execute;
        Commands.CommandEnabledDelegate buildEnabled = BuildCommand.Enabled;
        
        Commands.CommandExecuteDelegate runCommand   = BuildCommand.Run;
        Commands.CommandEnabledDelegate runEnabled   = BuildCommand.CanRun;
        
        Commands.CommandExecuteDelegate debugCommand = BuildCommand.Debug;
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
    
    Execute()
    {
        loop
        {
            if (Editor.CanUndo())
            {
                if (BuildOptions.IsAutoSaveEnabled())
                {
                    Editor.Save();
                }
                else
                {
                    string result = Editor.OfferSave();
                    if (result == "Cancel")
                    {
                        Editor.SetStatusBarText("Build Cancelled");
                        break;
                    }
                }
            }
            
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
            uint error = Runtime.Execute(binaryPath, arguments);
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
            string target = "";
            if (Target6502)
            {
                target = " for 6502";
            }
            
            arguments.Clear();
            arguments.Append(jsonPath);
            string checkedBuild;
            if (BuildOptions.IsCheckedEnabled())
            {
                checkedBuild = " (checked build)";
            }
            else
            {
                arguments.Append("-o"); // 'o'ptimized, not checked build (release)
            }
            
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            
            Editor.SetStatusBarText("Compiling '" + jsonPath + "' -> '" + codePath + "'" + checkedBuild);
            error = Runtime.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("Compile", error);
                break;
            }
    
            if (BuildOptions.IsOptimizeEnabled())
            {        
                binaryPath ="/Bin/Optimize" + hexeExtension;
                if (!File.Exists(binaryPath))
                {
                    Editor.SetStatusBarText("No Optimize: '" + binaryPath + "'");
                    break;
                }
                Editor.SetStatusBarText("Optimizing Code '" + codePath + "' -> '" + codePath + "'" + target);
                
                arguments.Clear();
                arguments.Append(codePath);
                arguments.Append("-g");
                arguments.Append(col.ToString());
                arguments.Append(row.ToString());
                error = Runtime.Execute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError("Optimize", error);
                    break;
                }
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
            if (Target6502 || BuildOptions.IsGenerateIHexEnabled())
            {
                arguments.Append("-ihex");
            }
            if (BuildOptions.IsExtendedEnabled())
            {
                arguments.Append("-extended");
            }
            error = Runtime.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("CODEGEN", error);
                break;
            }
            
            if (BuildOptions.IsDisassembleEnabled())
            {       
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
                error = Runtime.Execute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError("DASM", error);
                    break;
                }
            }
            // debugger needs .hexe file, even for 6502
            //if (!Target6502)
            //{
            //    string ihexPath = hexePath.Replace(hexeExtension, ".hex");
            //    File.Delete(ihexPath);   // don't leave a stale .hex lying around (if we didn't just build it)
            //}
            Editor.SetStatusBarText("Success '" + sourcePath + "' -> '" + hexePath + "'" + target);
            break;   
        }
    }
    Debug()
    {
        Screen.Clear();
        if (!Target6502)
        {
            Die(0x0B); // assume we only arrive here for H6502
        }
        <string> arguments;
        string sourcePath = Editor.GetProjectPath(); 
        arguments.Append(sourcePath); // Debugger takes the .hs source path
        uint error = Runtime.Execute("Debug", arguments);
        Editor.DrawAll();
    }
    Run()
    {
        Screen.Clear();
        <string> arguments;
        string path = GetBinaryPath();
        if (Target6502) // target was checked during the successful build
        {
            arguments.Append("-x"); // <ctrl><F5>
            arguments.Append("-l");
            arguments.Append(path); // HopperMon takes IHex path
            path = "hm";
        }
        uint error = Runtime.Execute(path, arguments);
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
    // - if we have an unsaved modified file in the editor, then we need to build or save first
    // - if no file is modified in the editor, we need to build if the binary file is not younger 
    //   than all the source files of the project
    
    bool CanRun()
    {
        string path = GetBinaryPath();
        bool canRun = File.Exists(path);
        if (canRun)
        {
            canRun = !Editor.CanUndo();
            if (canRun)
            {
                canRun = Editor.IsYoungerThanSource(path);
            }
        }
        return canRun;
    }
    bool CanDebug()
    {
        string path = GetBinaryPath();
        bool canRun = File.Exists(path);
        if (canRun)
        {
            canRun = !Editor.CanUndo() && Target6502;
            if (canRun)
            {
                canRun = Editor.IsYoungerThanSource(path);
            }
        }
        return canRun;
    }
    
}

