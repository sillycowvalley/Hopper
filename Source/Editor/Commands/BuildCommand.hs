unit BuildCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    
    // for DefineExists to see which platform to CODEGEN for
    uses "/Source/Compiler/JSON/JSON"
    
    uses "/Source/Compiler/Tokens/Dependencies"
    
    // reset during compile : 
    //   -  checks for 'HOPPER_6502' or 'MCU' in compilation target symbols using CheckTarget(..) after preprocess step
    // or, failing that, in GetBinaryPath()
    //   - checks if a ".ihex" exists when a ".hexe" is not found
    
    bool isHopper;
    bool isAssembly;
    bool isZ80;
    bool generateIHex;
    bool launchIHex;
    
    CPUArchitecture cpuArchitecture; // for toolchain
    
    uint runtimeExecute(string path, <string> arguments)
    {
        if (false) // for CLI diagnostics
        {
            PrintLn();
            Print(path + " ");
            foreach (var arg in arguments)
            {
                Print(arg + " ");
            }
        }
        return Runtime.Execute(path, arguments);
    }
    bool GenerateIHex 
    { 
        get { return generateIHex; } set { generateIHex = value; }
    }
    bool LaunchIHex 
    { 
        get { return launchIHex; } set { launchIHex = value; }
    }
    string GetBinaryPath() // used in Run(..), Debug(..), CanRun(..) and CanDebug(..)
    {
        string path;
        if (Enabled())
        {
            path = Editor.ProjectPath;
            path = Path.GetFileName(path);
            string extension = Path.GetExtension(path);
            path = path.Replace(extension, HexeExtension);
            path = Path.Combine("/Bin", path);
            LaunchIHex = false;
            string ihexPath = path.Replace(HexeExtension, ".ihex");   
            if (File.Exists(ihexPath))
            {
                if (File.Exists(path)) 
                {
                    // both .hexe and .ihex exist    
                    long hexeFileTime = File.GetTimeStamp(path);
                    long hexFileTime  = File.GetTimeStamp(ihexPath);
                    string hexeFileTimeHex = hexeFileTime.ToHexString(8);
                    string hexFileTimeHex  = hexFileTime.ToHexString(8);
                    if (hexFileTimeHex >= hexeFileTimeHex)
                    {
                        // .ihex is younger than .hexe
                        path = ihexPath;
                        LaunchIHex = true;
                    }
                }
                else
                {
                    // only .ihex exists
                    path = ihexPath;
                    LaunchIHex = true;
                }
            }
        }
        return path;
    }
    
    CheckTarget(string symbolsPath)
    {
        GenerateIHex = false;
        if (File.Exists(symbolsPath))
        {
            <string,variant> dict;
            if (JSON.Read(symbolsPath, ref dict))
            {
                foreach (var kv in dict)
                {
                    if (kv.key == "symbols")
                    {
                        // preprocessor symbols
                        <string,string> pdValues = kv.value;
                        if (   pdValues.Contains("MCU")
                            || pdValues.Contains("IHEX")             // generate .ihex for local debugger
                            || pdValues.Contains("SERIAL_CONSOLE")
                            || pdValues.Contains("MINIMAL_RUNTIME")
                            || pdValues.Contains("HOPPER_6502")
                           )
                        {
                            if (!pdValues.Contains("LOCALDEBUGGER")) // special case for debugging portable runtime locally
                            {
                                GenerateIHex = true;
                            }
                        }
                        break;
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
            if (errorText.Length != 0)
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
            
            string binaryPath ="/Bin/PreProcess" + HexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No PreProcessor: '" + binaryPath + "'");
                break;
            }
            string sourcePath = Editor.ProjectPath;
            string fileName = Path.GetFileName(sourcePath);
            string extension = Path.GetExtension(fileName);
            fileName = fileName.Replace(extension, "");
            
            string jsonPath = "/Debug/Obj/" + fileName + ".sym";
            string codePath = "/Debug/Obj/" + fileName + ".code";
            string hexePath = "/Bin/" + fileName + HexeExtension;
            string ihexPath = hexePath.Replace(".hexe", ".ihex");
            string hasmPath = "/Debug/" + fileName + HasmExtension;
            
            string target = "";
            uint error;
            
            Editor.SetStatusBarText("Preprocessing '" + sourcePath + "' -> '" + jsonPath + "'");
            Source.DefinitionSymbolsLoaded = false; // reload after Preprocessing..
            
            byte col = Editor.Left + 1;
            byte row = Editor.Top + Editor.Height - 1;
            
            <string> arguments;
            arguments.Append(sourcePath);
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());

            isZ80 = false;            
            switch (Architecture)
            {
                case CPUArchitecture.W65C02:
                {
                    target = " for 65C02S"; 
                    cpuArchitecture = Architecture;
                }
                case CPUArchitecture.M6502:
                {
                    target = " for 6502";
                    cpuArchitecture = Architecture;
                }
                case CPUArchitecture.Z80:
                {
                    target = " for Z80";
                    cpuArchitecture = Architecture;
                    isZ80 = true;
                }
                default: // includes None
                {
                    cpuArchitecture = CPUArchitecture.Hopper; // a good starting assumption
                }
            }
            if (isZ80 || isAssembly)
            {
                ihexPath = hexePath.Replace(".hexe", ".hex");
            }
            
            if (isAssembly) // set by source being ".asm"
            {
                if ((cpuArchitecture != CPUArchitecture.W65C02) && (cpuArchitecture != CPUArchitecture.M6502))
                {
                    Editor.SetStatusBarText("#define CPU_6502 or CPU_65C02S for '.asm' projects");
                    break;
                }
                arguments.Append("-a");
            }
            else
            {
                if ((cpuArchitecture == CPUArchitecture.W65C02) || (cpuArchitecture == CPUArchitecture.M6502))
                {
                    Editor.SetStatusBarText("#define CPU_6502 and CPU_65C02S only valid for '.asm' projects");
                    break;
                }
            }
            
            error = runtimeExecute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError("Preprocessor", error);
                break;
            }
            if (isAssembly)
            {
                hasmPath = "/Debug/" + fileName + ".asm";
                binaryPath ="/Bin/Assemble" + HexeExtension;
                if (!File.Exists(binaryPath))
                {
                    Editor.SetStatusBarText("No Assembler: '" + binaryPath + "'");
                    break;
                }
                arguments.Clear();
                arguments.Append(jsonPath);
                
                arguments.Append("-g");
                arguments.Append(col.ToString());
                arguments.Append(row.ToString());
                
                Editor.SetStatusBarText("Assembling '" + jsonPath + "' -> '" + codePath);
                error = runtimeExecute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError("Assemble", error);
                    break;
                }
            }
            if (isHopper)
            {
                binaryPath ="/Bin/Compile" + HexeExtension;
                if (!File.Exists(binaryPath))
                {
                    Editor.SetStatusBarText("No Compiler: '" + binaryPath + "'");
                    break;
                }
                
                CheckTarget(jsonPath);
                if (Target6502)
                {
                    target = " for 6502";
                }
                if (TargetMCU)
                {
                    target = " for MCU";
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
                error = runtimeExecute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError("Compile", error);
                    break;
                }
            }
            if (BuildOptions.IsOptimizeEnabled() || isZ80)
            {   
                string optName = "Optimize";
                if (isAssembly)
                {
                    optName = "OptAsm";
                }
                     
                binaryPath ="/Bin/" + optName + HexeExtension;
                if (!File.Exists(binaryPath))
                {
                    Editor.SetStatusBarText("No " + optName + ": '" + binaryPath + "'");
                    break;
                }
                Editor.SetStatusBarText("Optimizing Code '" + codePath + "' -> '" + codePath + "'" + target);
                
                arguments.Clear();
                arguments.Append(codePath);
                arguments.Append("-g");
                arguments.Append(col.ToString());
                arguments.Append(row.ToString());
                error = runtimeExecute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError(optName, error);
                    break;
                }
            } // BuildOptions.IsOptimizeEnabled()
            
            string genName = "CODEGEN";
            string outputPath = hexePath;
            if (isAssembly)
            {
                genName = "ASMGEN";
                outputPath = ihexPath;
            }
            if (isZ80)
            {
                genName = "Z80GEN";
                outputPath = ihexPath;
            }
                    
            binaryPath ="/Bin/" + genName + HexeExtension;
            if (!File.Exists(binaryPath))
            {
                Editor.SetStatusBarText("No " + genName + ": '" + binaryPath + "'");
                break;
            }
            Editor.SetStatusBarText("Generating Code '" + codePath + "' -> '" + outputPath + "'" + target);
            
            arguments.Clear();
            arguments.Append(codePath);
            arguments.Append("-g");
            arguments.Append(col.ToString());
            arguments.Append(row.ToString());
            if (isHopper && GenerateIHex && !isZ80)
            {
                arguments.Append("-ihex");
            }
            error = runtimeExecute(binaryPath, arguments);
            if (error != 0)
            {
                DisplayError(genName, error);
                break;
            }
            
            if (isZ80 && BuildOptions.IsOptimizeEnabled()) // Z80 has 2nd optimization stage!
            {   
                string optName = "Z80Opt";
                     
                binaryPath ="/Bin/" + optName + HexeExtension;
                if (!File.Exists(binaryPath))
                {
                    Editor.SetStatusBarText("No " + optName + ": '" + binaryPath + "'");
                    break;
                }
                Editor.SetStatusBarText("Optimizing Code '" + outputPath + "' -> '" + outputPath + "'" + target);
                
                arguments.Clear();
                arguments.Append(outputPath);
                arguments.Append("-g");
                arguments.Append(col.ToString());
                arguments.Append(row.ToString());
                error = runtimeExecute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError(optName, error);
                    break;
                }
            } // BuildOptions.IsOptimizeEnabled()
            
            
            if (BuildOptions.IsDisassembleEnabled())
            { 
                string dasmName = "DASM";
                string dasmInput = hexePath;
                if (isAssembly)
                {
                    dasmName = "65DASM";
                    dasmInput = ihexPath;
                }
                if (isZ80)
                {
                    dasmName = "Z80DASM";
                    dasmInput = ihexPath;
                }
                       
                binaryPath ="/Bin/" + dasmName + HexeExtension;
                if (!File.Exists(binaryPath))
                {
                    Editor.SetStatusBarText("No " + dasmName + ": '" + binaryPath + "'");
                    break;
                }
                
                Editor.SetStatusBarText("Disassembling '" + dasmInput + "' -> '" + hasmPath + "'");
                
                arguments.Clear();
                arguments.Append(dasmInput);
                arguments.Append("-g");
                arguments.Append(col.ToString());
                arguments.Append(row.ToString());
                error = runtimeExecute(binaryPath, arguments);
                if (error != 0)
                {
                    DisplayError(dasmName, error);
                    break;
                }
            } // BuildOptions.IsDisassembleEnabled()
            
            if (GenerateIHex)
            {
                outputPath = ihexPath;
            }
            Editor.SetStatusBarText("Success '" + sourcePath + "' -> '" + outputPath + "'" + target);
        
            break;   
        }
    }
    Debug()
    {
        Screen.Clear();
        if (!LaunchIHex)
        {
            Die(0x0B); // assume we only arrive here for HOPPER_6502 and MCU
        }
        <string> arguments;
        string sourcePath = Editor.ProjectPath; 
        arguments.Append(sourcePath); // Debugger takes the .hs source path
        arguments.Append("-g"); // Interactive mode (not launched directly from command line)
        uint error = runtimeExecute("Debug", arguments);
        Editor.DrawAll();
    }
    Run()
    {
        Screen.Clear();
        <string> arguments;
        string path = GetBinaryPath(); // updates LaunchIHex
        if (LaunchIHex) // target was checked during the successful build
        {
            arguments.Append("-x"); // <ctrl><F5>
            arguments.Append("-l");
            arguments.Append(path); // HopperMon takes IHex path
            path = "hm";
        }
        uint error = runtimeExecute(path, arguments);
        Editor.DrawAll();
    }
    
    bool Enabled()
    {
        string path = Editor.ProjectPath;
        string extension = Path.GetExtension(path);
        extension  = extension.ToLower();
        isHopper   = (extension == ".hs");
        isAssembly = (extension == ".asm");
        return isHopper || isAssembly;
    }
    
    // Conditions for when we need to rebuild:
    // - if we have an unsaved modified file in the editor, then we need to build or save first
    // - if no file is modified in the editor, we need to build if the binary file is not younger 
    //   than all the source files of the project
    
    bool CanRun()
    {
        string path = GetBinaryPath(); // updates LaunchIHex
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
        string path = GetBinaryPath(); // updates LaunchIHex
        bool canRun = File.Exists(path);
        if (canRun)
        {
            canRun = !Editor.CanUndo() && LaunchIHex;
            if (canRun)
            {
                canRun = Editor.IsYoungerThanSource(path);
            }
        }
        return canRun;
    }
    
}

