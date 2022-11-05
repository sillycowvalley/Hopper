unit BuildCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    uses "/Source/Editor/Debugger"
    
    const string hexeExtension = ".hexe2";
    const string hasmExtension = ".hasm2";
        
    Register()
    {
        Commands.CommandExecuteDelegate buildCommand = BuildCommand.Execute;
        Commands.CommandEnabledDelegate buildEnabled = BuildCommand.Enabled;
        Commands.CommandExecuteDelegate runCommand = BuildCommand.Run;
        Commands.CommandEnabledDelegate runEnabled = BuildCommand.CanRun;
        
        Key key = (Key.F7);
        InstallCommand("Build", "&Build", buildCommand, buildEnabled, key);
        key = (Key.F5);
        InstallCommand("Run", "&Run", runCommand, runEnabled, key);
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
                string result = Editor.OfferSave();
                if (result == "Cancel")
                {
                    Editor.SetStatusBarText("Build Cancelled");
                    break;
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
            
            //File.Delete(jsonPath);
            //File.Delete(codePath);
            //File.Delete(hexePath);
            //File.Delete(hasmPath);
            
            Editor.SetStatusBarText("Preprocessing '" + sourcePath + "' -> '" + jsonPath + "'");
            
            <string> arguments;
            arguments.Append(sourcePath);
            arguments.Append("-g");
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
            
            Editor.SetStatusBarText("Compiling '" + jsonPath + "' -> '" + codePath + "'");
            
            arguments.Clear();
            arguments.Append(jsonPath);
            arguments.Append("-g");
            error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("Compile", error);
                break;
            }
            
            binaryPath ="/Bin/CODEGEN" + hexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No CODEGEN: '" + binaryPath + "'");
                break;
            }
            Editor.SetStatusBarText("Generating Code '" + codePath + "' -> '" + hexePath + "'");
            
            arguments.Clear();
            arguments.Append(codePath);
            arguments.Append("-g");
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
            error = System.Execute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("DASM", error);
                break;
            }
            Editor.SetStatusBarText("Success '" + sourcePath + "' -> '" + hexePath + "'");
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
    Run()
    {
        string path = GetBinaryPath();
        
        InitializeSymbols();
        <string> arguments;
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
    bool CanRun()
    {
        string path = GetBinaryPath();
        bool canRun = File.Exists(path);
        if (canRun)
        {
            canRun = !Editor.CanUndo();
        }
        return canRun;
    }
    
}

