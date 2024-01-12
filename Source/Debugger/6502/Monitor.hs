unit Monitor
{
    uses "/Source/System/IO"
    uses "/Source/Debugger/Source"
    uses "/Source/Debugger/6502/Pages"
    
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
#ifdef CAPTURESERIAL

    file captureFile;
    
    InitializeCapture()
    {
        string capturePath = "/Debug/SerialCapture.log";
        File.Delete(capturePath);
        captureFile = File.Create(capturePath);
    }
    SerialWriteChar(char ch)
    {
        Serial.WriteChar(ch);
        byte b = byte(ch);
        string logLine = "Desktop:" + b.ToHexString(2);
        if (b > 31)
        {
            logLine = logLine + " " + ch;
        }
        captureFile.Append(logLine + char(0x0A));
        captureFile.Flush();
    }
    char SerialReadChar()
    {
        char ch = Serial.ReadChar();
        
        byte b = byte(ch);
        string logLine ="Device: " + b.ToHexString(2);
        if (b > 31)
        {
            logLine = logLine + " " + ch;
        }
        captureFile.Append(logLine + char(0x0A));
        captureFile.Flush();
        
        return ch;
    }
#else    
    SerialWriteChar(char ch)
    {
        Serial.WriteChar(ch);
    }
    char SerialReadChar()
    {
        char ch = Serial.ReadChar();
        return ch;
    }
#endif

    WaitForDeviceReady()
    {
        return;
#ifdef CAPTURESERIAL        
        captureFile.Append("WaitForDeviceReady" + char(0x0A));
        captureFile.Flush();        
#endif
        char c;
        loop
        {
            SerialWriteChar(char(0x1B)); // to break VM if it is running
            while (Serial.IsAvailable)
            {
                c = SerialReadChar();
                if (c == '\\')
                {
                    break;
                }
            }
            if (c == '\\')
            {
                break;
            }
            Delay(100);
            if (IsDebugger)
            {
                Parser.ProgressTick(".");
            }
            else
            {
                Print('.');
            }
        }
#ifdef CAPTURESERIAL                
        captureFile.Append("Done" + char(0x0A));
        captureFile.Flush();        
#endif
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
            char c = SerialReadChar();
            if ((c == char(0x0D)) || (c == char(0x0A)))
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
                        SerialWriteChar(ch);
                        Delay(3);
                        if (ch == char (0x0D))
                        {
                            waitingForPrompt = true;
                        }
                    }
                }
                else if (Keyboard.IsAvailable)
                {
                    Key key = Keyboard.ReadKey();
                    if (IsDebugger)
                    {
                        // convert <right><click> in console area to <ctrl><V>
                        if ((key == Key.ClickRight) && Keyboard.ClickUp)
                        {
                            if (Output.ConsoleHitTest(Keyboard.ClickX, Keyboard.ClickY))
                            {
                                key = Key.ControlV;
                            }
                        }
                    }
                      
                    if ((key == Key.ControlV) && Clipboard.HasText)
                    {
                        string clipboardText = Clipboard.GetText();
                        keyboardBuffer = keyboardBuffer + clipboardText;
                        continue;
                    }
                    char ch = IO.TransformKey(key);
                    if (ch != char(0x00))
                    {
                        SerialWriteChar(ch);
                    }
                }
                continue;
            }
            char c = SerialReadChar();
            if ((c == char(0x0D)) || (c == char(0x0A)))
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
                        Delay(10);
                    }
                }
            }
        }
    }
    
    sendCommand(string commandLine)
    {
        SerialWriteChar(char(0x1B)); // to break VM if it is running
        if (checkEcho(true))  { }
        if (commandLine.Length > 0)
        {
            foreach (var ch in commandLine)
            {
                SerialWriteChar(ch);
            }
            SerialWriteChar(char(0x0D));
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
    
    UploadFile(string localPath, string remoteFolder)
    {
        Pages.ClearPageData(); // just to be sure
        loop
        {
            if (!remoteFolder.StartsWith('/'))
            {
                remoteFolder = '/' + remoteFolder;
            }
            sendCommand("T"); // waits for \ confirmation
            
            // destination name
            string filePath = Path.Combine(remoteFolder, Path.GetFileName(localPath));
            foreach (var ch in filePath)
            {
                SerialWriteChar(ch); 
            }
            SerialWriteChar(char(0x0D));
            if (!checkEcho(true)) // waits for \ confirmation    
            {
                Output.Print("  Failed to upload '" + filePath + "'");
                break;
            }
            
            // destination folder
            foreach (var ch in remoteFolder)
            {
                SerialWriteChar(ch); 
            }
            SerialWriteChar(char(0x0D));
            if (!checkEcho(true)) // waits for \ confirmation    
            {
                Output.Print("  Failed to upload '" + filePath + "'");
                break;
            }
            
            // transfer size in bytes
            long size = File.GetSize(localPath);
            string str = size.ToHexString(4);
            foreach (var c in str)
            {
                SerialWriteChar(c); 
            }
            
            if (checkEcho(true)) { } // waits for \ confirmation    
            
            file dFile = File.Open(localPath);
            collectOutput = true; // just to toss it away   
            while (size != 0)
            {
                byte b = dFile.Read();
                string str = b.ToHexString(2);
                foreach (var c in str)
                {
                    SerialWriteChar(c); 
                }
                size--;
                if (size % 256 == 0)
                {
                    Delay(1); // breathing room for serial comms
                }
            }
            
            if (checkEcho(true)) // waits for \ confirmation    
            {
                Output.Print("  Uploaded to '" + filePath + "'");
            }
            break;
        } // loop
        ClearSerialOutput(); // toss it
        collectOutput = false;   
    }
    
    UploadHex(string ihexPath)
    {
        entryPCIsSet = false;  // only changes when we upload a new program
        Pages.ClearPageData(); // just to be sure
        
        lastHexPath = "";
        sendCommand("L"); // waits for \ confirmation  
        file iFile = File.Open(ihexPath);
        if (IsDebugger)
        {
            Editor.SetStatusBarText("Uploading '" + ihexPath + "' ..");
        }

        collectOutput = true; // just to toss it away    
        while (iFile.IsValid())
        {
            string ln = iFile.ReadLine();
            foreach (var c in ln)
            {
                SerialWriteChar(c); 
                if (checkEcho(false)) { }
            }
            SerialWriteChar(char(0x0D));
            if (checkEcho(false)) { }
            if (IsDebugger)
            {
                Parser.ProgressTick(".");
            }
        }
        SerialWriteChar('*'); // arbitrary terminator to get a \ back
        if (checkEcho(true)) // waits for \ confirmation    
        {
            if (IsDebugger)
            {
                Editor.SetStatusBarText("Successfully uploaded '" + ihexPath + "'");
            }
            else
            {
                Output.Print("  Successfully uploaded '" + ihexPath + "'");
            }
            lastHexPath = ihexPath;
        }
        else
        {
            if (IsDebugger)
            {
                Editor.SetStatusBarText("Failed to upload '" + ihexPath + "'");
            }
            else
            {
                Output.Print("  Failed to upload '" + ihexPath + "'");
            }
        }
        ClearSerialOutput(); // toss it
        Source.ClearSymbols();
        if (IsDebugger)
        {
            DeleteBreakpoints();
        }
        collectOutput = false;
    }
    string GetCurrentHexPath()
    {
        return lastHexPath;
    }
    
    HopperFlags hopperFlags;
    
    bool IsMCU { get { return ( HopperFlags.MCUPlatform                               == (hopperFlags & HopperFlags.MCUPlatform)); } }
    bool IsLV  { get { return ((HopperFlags.MCUPlatform | HopperFlags.StackSlot32Bit) == hopperFlags & (HopperFlags.MCUPlatform | HopperFlags.StackSlot32Bit) ) } }
    
    string GetHopperInfo()
    {
        LoadZeroPage(false);
        
        if (ZeroPageContains("FLAGS"))
        {
            hopperFlags = HopperFlags(GetZeroPage("FLAGS"));
        }
        string info;
        if (HopperFlags.SP8Bit == hopperFlags & HopperFlags.SP8Bit)
        {
            info = info + "8 bit SP and BP";
        }
        else
        {
            info = info + "16 bit SP and BP";
        }
        if (HopperFlags.WarpSpeed == (hopperFlags & (HopperFlags.MCUPlatform | HopperFlags.WarpSpeed)))
        {
            info = info + ", Warp speed (no <ctrl><C>)";
        }
        if (HopperFlags.CheckedBuild == (hopperFlags & HopperFlags.CheckedBuild))
        {
            info = info + ", Checked Build";
        }
        if (HopperFlags.ProfileBuild == (hopperFlags & HopperFlags.ProfileBuild))
        {
            info = info + ", Profile Build";
        }
        if (HopperFlags.MCUPlatform == (hopperFlags & HopperFlags.MCUPlatform))
        {
            info = info + ", MCU";
            if (HopperFlags.StackSlot32Bit == (hopperFlags & HopperFlags.StackSlot32Bit))
            {
                info = info + ", Long Values";
            }
        }
        else
        {
            info = info + ", 6502";
        }
        return info;
    }     
    
    uint GetCurrentPC()
    {
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
