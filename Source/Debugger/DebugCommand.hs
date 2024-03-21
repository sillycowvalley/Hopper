unit DebugCommand
{
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    
    uses "Output"
    uses "6502/Monitor"
    
    uses "ConsoleCapture"
    
    <string> breakpoints;
    <uint>   breakpointAddresses;
    
    bool serialConnectionLost;
    bool SerialConnectionLost { get { return serialConnectionLost; } }
    
    Register()
    {
        Commands.CommandExecuteDelegate runCommand = DebugCommand.Run;
        Commands.CommandEnabledDelegate runEnabled = DebugCommand.AlwaysCan;
        Commands.CommandExecuteDelegate debugCommand = DebugCommand.Debug;
        Commands.CommandEnabledDelegate debugEnabled = DebugCommand.AlwaysCan;
        Commands.CommandExecuteDelegate reloadCommand = DebugCommand.Reload;
        Commands.CommandEnabledDelegate reloadEnabled = DebugCommand.AlwaysCan;
        
        Commands.CommandExecuteDelegate stepOverCommand = DebugCommand.StepOver;
        Commands.CommandEnabledDelegate stepOverEnabled = DebugCommand.AlwaysCan;
        Commands.CommandExecuteDelegate stepIntoCommand = DebugCommand.StepInto;
        Commands.CommandEnabledDelegate stepIntoEnabled = DebugCommand.AlwaysCan;
        
        Commands.CommandExecuteDelegate profileCommand = DebugCommand.Profile;
        Commands.CommandEnabledDelegate profileEnabled = DebugCommand.AlwaysCan;
        Commands.CommandExecuteDelegate memoryCommand = DebugCommand.Memory;
        Commands.CommandEnabledDelegate memoryEnabled = DebugCommand.AlwaysCan;
        
        Commands.CommandExecuteDelegate breakCommand = DebugCommand.Break;
        Commands.CommandEnabledDelegate breakEnabled = DebugCommand.CanBreak;
        
        Commands.CommandExecuteDelegate toggleBreakpointCommand = DebugCommand.ToggleBreakpoint;
        Commands.CommandEnabledDelegate toggleBreakpointEnabled = DebugCommand.AlwaysCan;
        
        Commands.CommandExecuteDelegate deleteBreakpointsCommand = DebugCommand.DeleteBreakpoints;
        Commands.CommandEnabledDelegate deleteBreakpointsEnabled = DebugCommand.BreakpointsExist;
        
        Key key;
        key = (Key.F5 | Key.Control);
        InstallCommand("Run", "&Start Without Debugging", runCommand, runEnabled, key);
        key = (Key.F5);
        InstallCommand("Debug", "Start With &Debugging", debugCommand, debugEnabled, key);
        key = (Key.F5 | Key.Control | Key.Shift);
        InstallCommand("Reload", "&Reload", reloadCommand, reloadEnabled, key);
        key = (Key.F10);
        InstallCommand("StepOver", "Step &Over", stepOverCommand, stepOverEnabled, key);
        key = (Key.F11);
        InstallCommand("StepInto", "Step &Into", stepIntoCommand, stepIntoEnabled, key);
        key = (Key.F2);
        InstallCommand("Profile", "Run in &Profiler", profileCommand, profileEnabled, key);
        key = (Key.F2 | Key.Alt);
        InstallCommand("Memory", "Dump &Memory", memoryCommand, memoryEnabled, key);
        key = (Key.ControlC);
        InstallCommand("Break", "&Break", breakCommand, breakEnabled, key);
        
        key = (Key.F9);
        InstallCommand("ToggleBreakpoint", "To&ggle Breakpoint", toggleBreakpointCommand, toggleBreakpointEnabled, key);
        key = (Key.F9 | Key.Control | Key.Shift);
        InstallCommand("DeleteAllBreakpoints", "Delete &All Breakpoints", deleteBreakpointsCommand, deleteBreakpointsEnabled, key);
    }
    
    setBreakpoints(<uint> breakpointAddresses)
    {
        Pages.LoadZeroPage(false); // for CODESTART
        if (ZeroPageContains("CODESTART"))
        {
            uint cs = GetZeroPage("CODESTART");
            
            <string> commands;
            commands.Append("BX"); // clear existing breakpoints
            for (uint i = 0; i < breakpointAddresses.Count; i++)
            {
                uint address = breakpointAddresses[i];
                if (address > 0)
                {
                    address = address + cs;
                    uint n = i + 1;
                    string command = "B" + n.ToHexString(1) + address.ToHexString(4);
                    commands.Append(command);
                }
            }
            Monitor.Command(commands, false, false);
        }
    }
    
    bool BreakpointsExist()
    {
        return breakpoints.Count != 0;
    }
    DeleteBreakpoints()
    {
        breakpoints.Clear();
        breakpointAddresses.Clear();
        Editor.Draw();
    }
    bool IsBreakpoint(string path, uint lineNumber)
    {
        string content = path + ":" + lineNumber.ToString();
        return breakpoints.Contains(content);
    }
    ToggleBreakpoint()
    {
        string currentPath = Editor.CurrentPath;
        currentPath = currentPath.ToLower();
        uint currentLine = Editor.GetCurrentLineNumber();
        
        Source.LoadSymbols(true);
        uint address = Code.GetLineAddress(currentPath, currentLine);
        if (address != 0)
        {
            string breakpoint = currentPath + ":" + currentLine.ToString();
            bool toggledOff = false;
            bool changed = false;
            for (uint index = 0; index < breakpoints.Count; index++)
            {
                if (breakpoints[index] == breakpoint)
                {
                    breakpoints.Remove(index);
                    breakpointAddresses.Remove(index);
                    toggledOff = true;
                    changed = true;
                    break;
                }
            }
            if (!toggledOff)
            {
                if (breakpoints.Count == 15)
                {
                    breakpoints.Remove(0);
                    breakpointAddresses.Remove(0);
                }
                breakpoints.Append(breakpoint);
                breakpointAddresses.Append(address);
                changed = true;
            }
            if (changed)
            {
                setBreakpoints(breakpointAddresses);
            }
            Editor.Draw();
        }
    }
    
    string GetBinaryPath()
    {
        string path;
        if (Enabled())
        {
            path = Editor.ProjectPath;
            path = Path.GetFileName(path);
            string extension = Path.GetExtension(path);
            path = path.Replace(extension, HexeExtension);
            path = Path.Combine("/Bin", path);
        }
        return path;
    }
    Break()
    {
        // placeholder: <ctrl><C> is sent to the device in Monitor.checkEchoRun(..)
    }
    bool CanBreak()
    {
        return false; // we never see this menu item when program is running
    }
    
    RefreshWatch()
    {
        if (Pages.IsPageLoaded(0))
        {
            // good chance we are running the debugger
            watchWindow();
        }
    }
    
    watchWindow()
    {
        Editor.SetStatusBarText("Updating stack view..");
        Pages.ClearPageData();
        Pages.LoadZeroPage(false); // for CSP and PC
        Output.Initialize();
        if (   ZeroPageContains("PC") 
            && ZeroPageContains("CSP") 
            && ZeroPageContains("CODESTART")
            // && IsPageLoaded(0x04)
           )
        {
            uint csp = GetZeroPage("CSP");
            if (csp > 0)
            {
                Output.CallStack(csp);
            }
        }
    }
    AttachDebugger()
    {
        debugCommand(char(0));        
    }
    
    debugCommand(char c)
    {
        Source.LoadSymbols(true);
        Editor.SetActiveLine(0, "", false);
        uint pc;
        if (c == char(0))
        {
            pc = Monitor.GetCurrentPC();
        }
        else
        {
            Editor.SetStatusBarText("Running in debugger..");
            if (!Monitor.RunCommand(c))
            {
                serialConnectionLost = true;
            }
            else
            {
                pc = ReturnToDebugger(c, ref serialConnectionLost);
            }
        }
        if (DebugOptions.IsCaptureConsoleMode)
        {
            ConsoleCapture.FlushLog();
        }
        if (pc != 0)
        {
            Editor.SetStatusBarText("");
            watchWindow();
            string sourceIndex = Code.GetClosestSourceIndex(pc);
            Output.GotoSourceIndex(sourceIndex, true);
            if (serialConnectionLost)
            {
                Editor.SetStatusBarText("Serial connection lost.");
            }
            else
            {
                Editor.SetStatusBarText("Waiting in debugger..");
            }
        }
        else
        {
            Editor.SetActiveLine(0, "", false);
            if (serialConnectionLost)
            {
                Editor.SetStatusBarText("Serial connection lost.");
            }
            else
            {
                Editor.SetStatusBarText("Program exited, session reset.");
            }
        }
    }
    
    
    Debug()
    {
        debugCommand('D');
    }
    Run()
    {
        Editor.SetStatusBarText("Running..");
        Editor.SetActiveLine(0, "", false);
        if (!Monitor.RunCommand("X"))
        {
            serialConnectionLost = true;
        }
        if (serialConnectionLost)
        {
            Editor.SetStatusBarText("Serial connection lost.");
        }
        else
        {
            Editor.SetStatusBarText("Program exited, session reset.");
        }
        if (DebugOptions.IsCaptureConsoleMode)
        {
            ConsoleCapture.FlushLog();
        }
    }
    StepOver()
    {
        debugCommand('O');
    }
    StepInto()
    {
        debugCommand('I');
    }
    bool AlwaysCan()
    {
        return true;
    }
    Reload()
    {
        // load the ihex to the HOPPER_6502
        Monitor.UploadHex(Monitor.CurrentHexPath);
        Output.Clear();
        ConsoleCapture.ClearLog();
    }
    Memory()
    {
        Editor.SetStatusBarText("Dumping memory stats..");
        if (Output.DumpMemory())
        {
            Editor.SetStatusBarText("Memory dump completed.");
        }
        else
        {
            Editor.SetStatusBarText("Memory dump failed.");
        }
    }
    Profile() // like manually pressing <F11> until either the end of the program run or until <ctrl><C> is pressed
    {
        Source.LoadSymbols(true);
        Editor.SetActiveLine(0, "", false);
        Editor.SetStatusBarText("Running in profiler..");
        watchWindow();   
        
        <uint,long> lineTimes;
        <uint,long> lineHits;
        uint ppc;
        uint minpc = 32000;
        uint maxpc = 0;
        uint ticks;
        long start;
        long elapsed;
        loop
        {
            ppc = Monitor.GetCurrentPC();
            if (ppc < minpc)
            {
                minpc = ppc;
            }
            if (ppc > maxpc)
            {
                maxpc = ppc;
            }
            start = Time.Millis;   
            if (!Monitor.RunCommand('I'))
            {
                serialConnectionLost = true;
            }
            uint pc;
            if (!serialConnectionLost)
            {
                pc = ReturnToDebugger('I', ref serialConnectionLost);
            }
            if (serialConnectionLost)
            {
                break;
            }
            
            elapsed = Time.Millis - start;
            if (!lineTimes.Contains(ppc))
            {
                lineTimes[ppc] = elapsed;
                lineHits[ppc]  = 1;
            }
            else
            {
                lineTimes[ppc] = lineTimes[ppc] + elapsed;
                lineHits[ppc]  = lineHits[ppc] + 1;
            }
            
            if (pc == 0)
            {
                break;
            }
            ticks++;
            if (ticks == 10240)
            {
                flushProfilerData(minpc, maxpc, lineTimes, lineHits);
                ticks = 0;
            }
        } // loop
        if (ticks != 0)
        {
            flushProfilerData(minpc, maxpc, lineTimes, lineHits);
        }
        
        watchWindow();
        Editor.SetActiveLine(0, "", false);
        if (serialConnectionLost)
        {
            Editor.SetStatusBarText("Serial connection lost.");
        }
        else
        {
            Editor.SetStatusBarText("Program exited, session reset.");
        }
    }    
    
    flushProfilerData(uint minpc, uint maxpc, <uint,long> lineTimes, <uint,long> lineHits)
    {
        if (DebugOptions.IsCaptureConsoleMode)
        {
            ConsoleCapture.AppendLineToLog("");
            ConsoleCapture.AppendLineToLog("Profiler Run:");
            for (uint pc = minpc; pc <= maxpc; pc++)
            {
                if (lineTimes.Contains(pc))
                {
                    long time = lineTimes[pc];
                    long hits = lineHits[pc];
                    float avg = 1.0 * time / hits;
                    string sourceIndex = Code.GetSourceIndex(pc);
                    ConsoleCapture.AppendLineToLog("0x" + pc.ToHexString(4) + "," + time.ToString()+ "," + hits.ToString()+ "," + avg.ToString() + "," + sourceIndex);
                }
            }
            ConsoleCapture.FlushLog();
        }
    }
    
}

