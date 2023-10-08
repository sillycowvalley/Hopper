unit Monitor
{
    uses "/Source/System/IO"
    uses "/Source/Debugger/Source"
    uses "/Source/Debugger/6502/Pages"
    
    // Zero Page FLAGS:
    const byte isTraceOn       = 0x01;
    const byte isWarpSpeed     = 0x02;
    const byte isCheckedBuild  = 0x04;
    const byte is8BitStack     = 0x08;
    const byte isProfileBuild  = 0x10;
    const byte isBreakpointSet = 0x20;
    
    bool collectOutput;
    string serialOutput;
    string lastHexPath;
    
    uint entryPC;
    bool entryPCIsSet;
    
    string GetSerialOutput()
    {
        return serialOutput;
    }
    ClearSerialOutput()
    {
        serialOutput = "";
    }
    
    bool checkEcho(bool waitForSlash)
    {
        bool success = true;
        char prev = ' ';
        loop
        {
            if (!Serial.IsAvailable)
            {
                if (!waitForSlash)
                {
                    break;
                }
                continue;
            }
            char c = Serial.ReadChar();
            if (c == char(0x0D))
            {
                if (collectOutput)
                {
                    Build(ref serialOutput, c);
                }
                else
                {
                    Output.Print(c);
                }
            }
            else
            {
                if (waitForSlash && (c == '\\'))
                {
                    success = (prev != '!');
                    break;
                }
                if (collectOutput)
                {
                    Build(ref serialOutput, c);
                }
                else
                {
                    Output.Print(c);
                }
            }
            prev = c;
        }
        return success;
    }
    
    checkEchoRun()
    {
        string keyboardBuffer;
        bool waitingForPrompt = false;
        loop
        {
            if (!Serial.IsAvailable)
            {
                if ((keyboardBuffer.Length != 0) && !waitingForPrompt)
                {
                    char ch = keyboardBuffer[0];
                    keyboardBuffer = keyboardBuffer.Substring(1);
                    if (ch != char(0x0A)) // only do the 0x0D's
                    {
                        Serial.WriteChar(ch);
                        Delay(10);
                        if (ch == char (0x0D))
                        {
                            waitingForPrompt = true;
                        }
                    }
                }
                else if (Keyboard.IsAvailable)
                {
                    Key key = Keyboard.ReadKey();
#ifdef DEBUGGER
                    // convert <right><click> in console area to <ctrl><V>
                    if ((key == Key.ClickRight) && Keyboard.ClickUp)
                    {
                        if (Output.ConsoleHitTest(Keyboard.ClickX, Keyboard.ClickY))
                        {
                            key = Key.ControlV;
                        }
                    }
#endif                                    
                    if ((key == Key.ControlV) && Clipboard.HasText)
                    {
                        string clipboardText = Clipboard.GetText();
                        keyboardBuffer = keyboardBuffer + clipboardText;
                        continue;
                    }
                    char ch = IO.TransformKey(key);
                    if (ch != char(0x00))
                    {
                        Serial.WriteChar(ch);
                    }
                }
                continue;
            }
            char c = Serial.ReadChar();
            if (c == char(0x0D))
            {
                Output.Print(c);
            }
            else
            {
                if (c == '\\')
                {
                    break;
                }
                if (c == char(0x0C)) // form feed
                {
                    Output.Clear();
                }
                else
                {
                    Output.Print(c);
                    if (c == '>')
                    {
                        waitingForPrompt = false;
                        Delay(100);
                    }
                }
            }
        }
    }
    
    sendCommand(string commandLine)
    {
        Serial.WriteChar(char(0x1B)); // to break VM if it is running
        if (checkEcho(true))  { }
        if (commandLine.Length > 0)
        {
            foreach (var ch in commandLine)
            {
                Serial.WriteChar(ch);
            }
            Serial.WriteChar(char(0x0D));
            if (checkEcho(true)) { }
        }
    }
    Command(string commandLine, bool collect, bool hasData)
    {
        <string> commandLines;
        commandLines.Append(commandLine);
        Command(commandLines, collect, hasData);
    }
    Command(<string> commandLines, bool collect, bool hasData)
    {
        if (collect)
        {
            collectOutput = true;
            serialOutput = "";
        }
        foreach (var commandLine in commandLines)
        {
            sendCommand(commandLine);
            if (hasData)
            {
                if (checkEcho(true)) { }
            }
        }
        if (collect)
        {
            collectOutput = false;     
        }
    }
    RunCommand(char ch)
    {
        RunCommand(ch.ToString());
    }
    RunCommand(string commandLine)
    {
        sendCommand(commandLine);
        checkEchoRun(); // special version for execute
    }
    EmptyCommand()
    {
        sendCommand(""); // empty : for <ctrl><C>?
    }
    uint ReturnToDebugger(char currentCommand)
    {
        //OutputDebug("ReturnToDebugger()");
        uint pc = GetCurrentPC();
        uint entry = GetEntryPC();
        bool showSource = true;
        loop
        {
            string sourceIndex = Code.GetSourceIndex(pc);
            if (sourceIndex.Length > 0)
            {
                break; // stop when we arrive at the next source line
            }
            else if (pc == entry)
            {
                // if there is no source line (global initialization)
                showSource = false;
                // after execute so we must have finished and restarted
                break; 
            }
            // keep stepping ..
            if (currentCommand == 'I')
            {
                Monitor.RunCommand("I");
            }
            else // 'D' or 'O'
            {
                Monitor.RunCommand("O");
            }
            pc = GetCurrentPC();
        }
        if (showSource)
        {
            return pc;
        }
        return 0;
    }
    
    
    UploadHex(string ihexPath)
    {
        entryPCIsSet = false;  // only changes when we upload a new program
        Pages.ClearPageData(); // just to be sure
        
        lastHexPath = "";
        sendCommand("L"); // waits for \ confirmation  
        file iFile = File.Open(ihexPath);
#ifdef DEBUGGER
        Editor.SetStatusBarText("Uploading '" + ihexPath + "' ..");
#endif    
        collectOutput = true; // just to toss it away    
        while (iFile.IsValid())
        {
            string ln = iFile.ReadLine();
            foreach (var c in ln)
            {
                Serial.WriteChar(c); 
                if (checkEcho(false)) { }
            }
            Serial.WriteChar(char(0x0D));
            if (checkEcho(false)) { }
#ifdef DEBUGGER            
            Parser.ProgressTick(".");
#endif
        }
        Serial.WriteChar('*'); // arbitrary terminator to get a \ back
        if (checkEcho(true)) // waits for \ confirmation    
        {
#ifdef DEBUGGER
            Editor.SetStatusBarText("Successfully uploaded '" + ihexPath + "'");
#else
            Output.Print("  Successfully uploaded '" + ihexPath + "'");
#endif
            lastHexPath = ihexPath;
        }
        else
        {
#ifdef DEBUGGER
            Editor.SetStatusBarText("Failed to upload '" + ihexPath + "'");
#else            
            Output.Print("  Failed to upload '" + ihexPath + "'");
#endif
        }
        ClearSerialOutput(); // toss it
        Source.ClearSymbols();
#ifdef DEBUGGER        
        DebugCommand.DeleteBreakpoints();
#endif
    }
    string GetCurrentHexPath()
    {
        return lastHexPath;
    }
    
    string GetHopperInfo()
    {
        LoadZeroPage(false);
        uint hopperFlags = 0;
        if (ZeroPageContains("FLAGS"))
        {
            hopperFlags = GetZeroPage("FLAGS");
        }
        string info;
        if (0 != hopperFlags & is8BitStack)
        {
            info = info + "8 bit SP and BP";
        }
        else
        {
            info = info + "16 bit SP and BP";
        }
        if (0 != hopperFlags & isWarpSpeed)
        {
            info = info + ", Warp speed (no <ctrl><C>)";
        }
        if (0 != hopperFlags & isCheckedBuild)
        {
            info = info + ", Checked Build";
        }
        if (0 != hopperFlags & isProfileBuild)
        {
            info = info + ", Profile Build";
        }
        return info;
    }     
    
    uint GetCurrentPC()
    {
        uint pc;
        if (!ZeroPageContains("CODESTART"))
        {
            LoadZeroPage(false);
        }
        Monitor.Command("P", true, true);
        string serialOutput = Monitor.GetSerialOutput();
        serialOutput = "0x" + serialOutput.Substring(1);
        //Print(" " + serialOutput, MatrixRed, Black);
        uint pc;
        if (UInt.TryParse(serialOutput, ref pc))
        {
            pc = pc - (GetZeroPage("CODESTART") << 8);
        }
        return pc;
    }
    uint GetEntryPC()
    {
        if (!entryPCIsSet)
        {
            if (!ZeroPageContains("CODESTART"))
            {
                LoadZeroPage(false);
            }
            byte csPage = byte(GetZeroPage("CODESTART"));
            LoadPageData(csPage);
            uint address = (csPage << 8);
            entryPC = GetPageWord(address+4);
            entryPCIsSet = true;
        }
        return entryPC;
    }
}
