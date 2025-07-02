unit Emulator
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "Chips/ACIA"
    
    
    bool terminal6850;
    bool Terminal6850  { get { return terminal6850; }  set { terminal6850 = value; } }
    
    bool emulateAppleI;
    bool EmulateAppleI  { get { return emulateAppleI; }  set { emulateAppleI = value; } }
    
    
    bool ogMode;
    bool OGMode { get { return ogMode; } set { ogMode = value; } }
    
    bool showStringVariables;
    bool showWozVariables;
    
    bool Arguments(ref string filePath)
    {
        bool showHelp;
        
        <string> rawArgs = System.Arguments;
        <string> args;
        
        for (uint iArg = 0; iArg < rawArgs.Count; iArg++)
        {
            string arg = rawArgs[iArg];
            if (((arg.Length == 2) || (arg.Length == 3)) && (arg[0] == '-'))
            {
              arg = arg.ToLower();
              switch (arg)
              {
                  case "-t":
                  {
                      Terminal6850 = true;
                  }
                  case "-1":
                  {
                      EmulateAppleI = true;
                  }
                  case "-og":
                  {
                      OGMode = true;
                  }
                  default:
                  {
                      args.Clear();
                      break;
                  }
              }
            }
            else
            {
              args.Append(arg);
            }
        }
        if (args.Count > 1)
        {
            showHelp = true;
        }
        else if (args.Count == 1)
        {
            filePath = args[0];
        }
        if (filePath.Length == 0)
        {
            showHelp = true;
        }
        return showHelp;
    }
    
    bool FullPath(ref string filePath)
    {
        bool showHelp;
        loop
        {
            // check the file
            string fullPath;
            if (File.Exists(filePath))
            {
                break;
            }
            fullPath = Path.Combine(System.CurrentDirectory, filePath);
            if (File.Exists(fullPath))
            {
                filePath = fullPath;
                break;
            }
            string extension = Path.GetExtension(filePath);
            if (extension == ".")
            {
                string filePathExt = filePath + ".hs";
                if (File.Exists(filePathExt))
                {
                    filePath = filePathExt;
                    break;
                }
                string fullPathExt = Path.Combine(System.CurrentDirectory, filePathExt);
                if (File.Exists(fullPathExt))
                {
                    filePath = fullPathExt;
                    break;
                }
            }
            if (!File.Exists(fullPath))
            {
                showHelp = true;
            }
            break;
        } // loop
        return showHelp;
    }
    
    bool ExpandPaths(string filePath, ref string ihexPath, ref string symbolsPath)
    {
        bool showHelp;
        ihexPath = Path.GetFileName(filePath);
        string extension = Path.GetExtension(filePath);
        ihexPath = ihexPath.Replace(extension, ".hex");
        ihexPath = Path.Combine("/bin", ihexPath);
        
        symbolsPath = Path.GetFileName(filePath);
#ifdef Z80        
        symbolsPath = symbolsPath.Replace(extension, ".zcode");
#else
        symbolsPath = symbolsPath.Replace(extension, ".code");
#endif
        symbolsPath = Path.GetFileName(symbolsPath);
        symbolsPath = Path.Combine("/Debug/Obj", symbolsPath);
        if (!File.Exists(ihexPath))
        {
            showHelp = true;
        }                
        if (!File.Exists(symbolsPath))
        {
            showHelp = true;
        }
        return showHelp;
    }
    
    <uint,long> addressHits;
    DumpProfile()
    {
        file profileData = File.Create("/Temp/Profile.csv");
        foreach (var kv in addressHits)
        {
            uint address = kv.key;
            long hits    = kv.value;
            profileData.Append("0x" + address.ToHexString(4) + "," + hits.ToString() + Char.EOL);
        }
        profileData.Flush();
    }
    
    bool ValidateHexPage(ref string hexpage)
    {
        bool valid = false;
        hexpage = hexpage.ToUpper();
    
        uint returnValue = 0;
        if (UInt.TryParse("0x" + hexpage, ref returnValue))
        {
            if ((returnValue >= 0x00) && (returnValue <= 0xFF))
            {
                valid = true;
                if (hexpage.Length == 1)
                {
                    hexpage = "0" + hexpage;
                }
            }
        }
        return valid;
    }
    
    uint[16] breakpoints;
    ClearIfZeroBreakPoint(uint address)
    {
        if (breakpoints[0] == address)
        {
            breakpoints[0] = InvalidAddress;
        }
    }
    ClearBreakPoints()
    {
        for (uint i = 0; i < breakpoints.Count; i++)
        {
            breakpoints[i] = InvalidAddress;
        }
    }
    SetBreakPoint(uint breakpoint, uint address)
    {
        breakpoints[breakpoint] = address;  
    }
    bool IsBreakPoint(uint address)
    {
        for (uint i = 0; i < breakpoints.Count; i++)
        {
            if (breakpoints[i] == address) { return true; }
        }
        return false;
    }
    
    
    Run()
    {
        // run ignoring breakpoints
        loop
        {
            if (Emulator.BreakCheck()) { break; }
            uint pc = CPU.PC;
            if (pc == InvalidAddress) { break; }
            StepInto(true);
        }
    }
    Debug()
    {
        // run until breakpoint
        bool first = true;
        loop
        {
            if (Emulator.BreakCheck()) { break; }
            uint pc = CPU.PC;
            if (pc == InvalidAddress) { break; }
            if (!first && Emulator.IsBreakPoint(pc))
            {
                Emulator.ClearIfZeroBreakPoint(pc);
                break;
            }
#ifdef Z80
            OpCode instruction = GetInstruction(pc);
            if (instruction == OpCode.HALT)
            {
                break;
            }
#endif            
            StepInto(false);
            first = false;
        }
    }
    StepOver()
    {
        uint pc = CPU.PC;
        if (pc == InvalidAddress) { return; }
        OpCode instruction = GetInstruction(pc);
        if (instruction == GetJSRInstruction())
        {
            uint length = GetInstructionLength(instruction);
            Emulator.SetBreakPoint(0, pc+length);
            Debug();
        }
        else
        {
            StepInto(false);
        }
    }
    StepInto(bool ignoreBreakPoints)
    {
        if (Emulator.EmulateAppleI)
        {
            Emulator.ServiceAppleIO();
        }
        else if (ACIA.ServiceSerial())
        {
            RaiseIRQ();
        }
#ifdef PROFILING        
        uint pc = CPU.PC;
        if (!addressHits.Contains(pc))
        {
            addressHits[pc] = 0;
        }
        addressHits[pc] = addressHits[pc] + 1;
#endif

        CPU.Execute(ignoreBreakPoints);
    }
    
    Close()
    {
        if (!Emulator.EmulateAppleI)
        {
            ACIA.Close();
        }
    }
    
    DoReset()
    {
        if (!Emulator.EmulateAppleI)
        {
            ACIA.Initialize();
        }
        CPU.Reset();
    }
    
    bool haveKey;
    Key  peekKey;
    bool KeyAvailable()
    {
        return haveKey || Keyboard.IsAvailable;
    }
    Key PeekKey()
    {
        if (!haveKey)
        {
            peekKey = ReadKey();
            haveKey = true;
        }
        return peekKey;
    }
    Key GetKey()
    {
        if (haveKey)
        {
            haveKey = false;
            return peekKey;
        }
        return ReadKey(); 
    }
    
    bool haveAppleKey = false;
    char appleKey;
    ServiceAppleIO()
    {
        if (!haveAppleKey && Emulator.KeyAvailable())
        {
            Key key = Emulator.PeekKey();
            if (key != Key.ControlC)
            {
                _ = Emulator.GetKey(); // consume it
                char maker;
                appleKey = ToSerial(key, ref maker);
                appleKey = appleKey.ToUpper(); // Apple I was uppercase only
                if (appleKey == char(0x0D))
                {
                    appleKey = Char.EOL;
                }
                //Print(" " + (byte(appleKey)).ToHexString(2), Colour.Red, Colour.Black);
                haveAppleKey = true;
            }
        }
    }
    byte GetAppleKBD(uint address)
    {
        byte value;
        if (address == 0xD010) // KBD
        {
            value = byte(appleKey)  | 0x80;
            //Print(" " + value.ToHexString(2), Colour.Blue, Colour.Black);
            haveAppleKey = false;
        }
        else if (address == 0xD011) // KBDCR
        {
            value = haveAppleKey ? 0b10000000 : 0; // set bit 7 if there is a key available
        }
        return value;
    }
    SetAppleDSP(uint address, byte value)
    {
        if (address == 0xD012) // DSP
        {
            char ch = char(value & 0x7F);
            //Print(" " + value.ToHexString(2) +":", Colour.Ocean, Colour.Black);
            if (ch == Char.Escape)
            {
                // don't echo escape .. what else?
            }
            else if (ch == Char.EOL)
            {
                PrintLn();
            }
            else
            {
                Print(ch, Colour.Ocean, Colour.Black);
            }
        }
        else if (address == 0xD013) // DSPCR
        {
            // display is always ready
        }
    }
    
    bool BreakCheck()
    {
        if (KeyAvailable())
        {
            Key key = PeekKey();
            if (key == Key.ControlC)
            {
                _ = GetKey(); // consume it
                PrintLn("<ctrl><C>");
                return true;
            }
        }
        return false;
    }
    CommandLoop()
    {
        char currentCommand = ' ';
        string commandLine = "";
        bool refresh = true;
        bool firstRun = false;
        loop
        {
            if (refresh)
            {
                SetCursor(0, Screen.CursorY);
                string ln = ">" + commandLine;
                uint cursorX = ln.Length;
                ln = ln.Pad(' ', Screen.Columns-1);
                Print(ln);
                SetCursor(cursorX, Screen.CursorY);
                refresh = false;
            }
            
            Key key;
            if (firstRun)
            {
                key = Key.F5;
                firstRun = false;
            }
            else
            {
                key = Emulator.GetKey();
            }
            
            char ch = key.ToChar();
            ch = ch.ToUpper();
            uint clength = commandLine.Length;
            
            // shortcut keys
            bool doShortcut = false;
            if (key == (Key.Alt | Key.ModX))
            {
                commandLine = "Q";
                doShortcut = true;
            }
            else if (key == Key.F5)
            {
                commandLine = "D";
                doShortcut = true;
            }
            else if (key == (Key.Control | Key.F5))
            {
                commandLine = "X";
                doShortcut = true;
            }
            else if (key == Key.F11)
            {
                commandLine = "I";
                doShortcut = true;
            }
            else if (key == Key.F10)
            {
                commandLine = "O";
                doShortcut = true;
            }
            if (doShortcut && (commandLine.Length != 0))
            {
                currentCommand = commandLine[0];
                Print(commandLine);
                key = Key.Enter;
            }
            if (key == Key.Enter)
            {
                // execute commandLine
                if (currentCommand == 'Q') // exit monitor UI
                {
                    PrintLn();
                    break; 
                }
                else if (currentCommand == '?') // help
                {
                    Help();
                    refresh = true;
                }
                else if (currentCommand == 'Z') // zero page variables
                {
                    ShowZeroPage();
                    refresh = true;
                }
                else if (currentCommand == 'C') // show call stack
                {
#ifndef Z80                    
                    ShowCallStack();
                    refresh = true;
#endif
                }
                else if (currentCommand == 'H') // show Hopper heap
                {
                    GetRAMByteDelegate getRAMByte = GetMemory;
                    ShowHopperHeap(getRAMByte);
                    refresh = true;
                }
                else if (currentCommand == 'V') // show Hopper heap
                {
                    GetRAMByteDelegate getRAMByte = GetMemory;
#ifdef Z80                    
                    ShowHopperZ80Stack(getRAMByte);
#else
                    ShowHopperValueStack(getRAMByte);
#endif                    
                    refresh = true;
                }
                else if (currentCommand == 'M') // memory dump
                {
                    string hexpage = "";
                    if (commandLine.Length > 2)
                    {
                        if (commandLine == "M S")
                        {
                            showStringVariables = !showStringVariables;
                            refresh = true;
                        }
                        if (commandLine == "M W")
                        {
                            showWozVariables = !showWozVariables;
                            refresh = true;
                        }
                        else
                        {
                            hexpage = commandLine.Substring(2, commandLine.Length-2);
                            if (ValidateHexPage(ref hexpage))
                            {
                                MemoryDump(hexpage);
                                refresh = true;
                            }
                        }
                    }
                } // case 'M'
                              
                else if (currentCommand == 'B') // breakpoints
                {
                    if (commandLine == "B X")
                    {
                        Emulator.ClearBreakPoints();
                        refresh = true;
                    }
                    else if (commandLine.Length > 4)
                    {
                        string hex = "0x" + commandLine.Substring(2,1);
                        uint breakpoint;
                        if (UInt.TryParse(hex, ref breakpoint) && (breakpoint > 0))
                        {
                            hex = "0x" + commandLine.Substring(4);
                            uint breakAddress;
                            if (UInt.TryParse(hex, ref breakAddress) && (breakAddress > 0) && (breakAddress < 0xFFFF))
                            {
                                Emulator.SetBreakPoint(breakpoint, breakAddress);
                                refresh = true;
                            }
                        }
                    }
                } // case 'B'
            
                else if (currentCommand == 'X') // Execute (run with Warp)
                {
                    Emulator.Run();
                    refresh = true;
                }
                else if (currentCommand == 'D') // Debug (run with !Warp)
                {
                    Emulator.Debug();
                    uint pc = CPU.PC;
                    if (pc != InvalidAddress)
                    {
                        ShowRegisters(showStringVariables, showWozVariables);
                        ShowCurrentInstruction(3);
                    }
                    refresh = true;
                }
                else if (currentCommand == 'I') // Step (single / into / F11)
                {
                    Emulator.StepInto(false);
                    uint pc = CPU.PC;
                    if (pc != InvalidAddress)
                    {
                        ShowRegisters(showStringVariables, showWozVariables);
                        ShowCurrentInstruction(3);
                    }
                    refresh = true;
                }
                else if (currentCommand == 'O') // Step (next / over / F10)
                {
                    Emulator.StepOver();
                    uint pc = CPU.PC;
                    if (pc != InvalidAddress)
                    {
                        ShowRegisters(showStringVariables, showWozVariables);
                        ShowCurrentInstruction(3);
                    }
                    refresh = true;
                }
                else if (currentCommand == 'S') // Source
                {
                    ShowCurrentInstruction(15);
                    refresh = true;
                }
                else if (currentCommand == 'W') // Warm Restart (keep program, reset data)
                {
                    CPU.Reset();
                    refresh = true;
                }
                else if (currentCommand == 'R') // Registers
                {
                    ShowRegisters(showStringVariables, showWozVariables);
                    refresh = true;
                }
                else if (currentCommand == 'U') // dUmp profile data
                {
                    Emulator.DumpProfile();
                    refresh = true;
                }
                if (refresh)
                {
                    commandLine = "";
                    currentCommand = ' ';
                    PrintLn();
                }
            } // if (key == Key.Enter)
            
            
            else if (key == Key.Escape)
            {
                // cancel commandline
                commandLine = "";
                refresh = true;
            }
            else if (key == Key.ControlC)
            {
                // cancel commandline
                commandLine = "";
                //Monitor.EmptyCommand();
                PrintLn("<ctrl><C>");
                refresh = true;
            }
            else if (key == Key.Backspace)
            {
                // back up one
                if (commandLine.Length != 0)
                {
                    commandLine = commandLine.Substring(0, commandLine.Length-1);
                    SetCursor(0, Screen.CursorY);
                    refresh = true;
                }
            }
            else
            { 
                // alphanumeric
                if (clength < Screen.Columns-1)
                {
                    if (clength == 0)
                    {
                        // first character must be command key
#ifdef Z80
                        if (String.Contains("?BDHIMOQRSUVWXZ", ch))
                        {
                            currentCommand = ch;
                        }
                        else
                        {
                            continue;
                        }
#else                        
                        if (String.Contains("?BCDHIMOQRSUVWXZ", ch))
                        {
                            currentCommand = ch;
                        }
                        else
                        {
                            continue;
                        }
#endif
                    } // clength == 0
                    else if (clength == 1)
                    {
                        if (ch != ' ') // 2nd character must be ' '
                        {
                            continue;
                        }
                        if (currentCommand == 'L')
                        {
                            // has arguments
                        }
                        else if (currentCommand == 'M')
                        {
                            // has arguments
                        }
                        else if (currentCommand == 'F')
                        {
                            // has arguments
                        }
                        else if (currentCommand == 'B')
                        {
                            // has arguments
                        }
                        else
                        {
                            continue; // no arguments
                        }
                    } // clength == 1
                    else
                    {   // clength > 1
                        // arguments
                        if (currentCommand == 'L')
                        {
                            // L <ihex path>
                        }
                        else if (currentCommand == 'T')
                        {
                            // T <local file path> <remote folder>
                        }
                        else if (currentCommand == 'M')
                        {
                            // M n or nn (MSB hex for page)
                        }
                        else if (currentCommand == 'F')
                        {
                            // F n or nn (MSB hex for page)
                        }
                        else if (currentCommand == 'B')
                        {
                            // B X or B n nnnn
                        }
                        else
                        {
                            continue; // should never get here
                        }
                    } // clength > 1
                    
                    if (ch != char(0))
                    {
                        commandLine = commandLine + ch;
                        refresh = true;
                    }
                }  // if (commandLine.Length < Screen.Columns-1)
            } // alphanumeric
            
        } // loop
 
    }
  
    
}
