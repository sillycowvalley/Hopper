unit Monitor
{
    uses "/Source/System/IO"
    uses "/Source/Debugger/Source"
    
    uses "Pages"
    
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
    
    SendBreak()
    {
        // send a <ctrl><C> in case there is a program running
        //OutputDebug("Before 0x03");
        Serial.WriteChar(char(0x03));
        uint waitCount;
        loop
        {
            if (waitCount >= 100) { break; }
            if (Serial.IsAvailable)
            {
                // Typically: 
                //    "\nBREAK/" if process was running
                //    "/" if we aleady stopped in the debugger
                // The waiting is in case we were in a long running system call
                // when the <ctrl><C> arrived. I Delay(..) of >= 5000 would defeat this.
                while (Serial.IsAvailable)
                {
                    char c = SerialReadChar(); 
                }
                break;
            }
            Delay(50);
            waitCount++;   
            if (waitCount % 10 == 0) { Print("."); }       
        }
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
    SerialWriteChars(char maker, char ch)
    {
        Serial.WriteChar(maker);
        Serial.WriteChar(ch);
    }
    char SerialReadChar()
    {
        return Serial.ReadChar();
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
                if (!Serial.IsValid())
                {
                    success = false;
                    break; // lost the connection?
                }
                continue;
            }
            char c = SerialReadChar();
            if ((c == char(0x0D)) || (c == char(0x0A)))
            {
                if (collectOutput)
                {
                    String.Build(ref serialOutput, c);
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
                    String.Build(ref serialOutput, c);
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
    
    monitorCommand(char command)
    {
        switch (command)
        {
            case 'D': //     process command like Runtime.DateTime
            {
                string dateTime = Time.Date + " " + Time.Time;
                foreach (var ch in dateTime)
                {
                    SerialWriteChar(ch);
                }
            }
        }
        SerialWriteChar(char(0x0D));
    }
    
    bool checkEchoRun(bool makersAllowed)
    {
        bool success = true;
        string keyboardBuffer;
        bool waitingForPrompt = false;
        Screen.ShowCursor = true;
        loop
        {
            if (!Serial.IsAvailable)
            {
                // write to the remote device?
                if (!Serial.IsValid())
                {
                    success = false;
                    break; // lost the connection?
                }
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
                    char maker;
                    char ch = Keyboard.ToSerial(key, ref maker);
                    if (ch != char(0x00))
                    {
                        if (maker != char(0x00))
                        {
                            if (makersAllowed)
                            {
                                SerialWriteChars(maker, ch);
                            }
                        }
                        else
                        {
                            SerialWriteChar(ch);
                        }
                    }
                }
                continue;
            } // !Serial.IsAvailable
            
            if (Serial.IsAvailable)
            {
                char c;
                Screen.ShowCursor = false;
                while (Serial.IsAvailable)
                {
                    // read from the remote device
                    c = SerialReadChar();
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
                        else if (c == char(0x07)) // bell
                        {
                            // get next character
                            c = SerialReadChar();
                            monitorCommand(c);
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
                } // while (Serial.IsAvailable)
                Screen.ShowCursor = true;
                if (c == '\\')
                {
                    break;
                }
            } // if (Serial.IsAvailable)
        } // loop
        Screen.ShowCursor = false;
        return success;
    }
    
    sendCommand(string commandLine)
    {
        SerialWriteChar(char(0x1B)); // to break VM if it is running
        _ = checkEcho(true);
        if (commandLine.Length != 0)
        {
            foreach (var ch in commandLine)
            {
                SerialWriteChar(ch);
            }
            SerialWriteChar(char(0x0D));
            _ = checkEcho(true);
        }
    }
    Command(string commandLine, bool collect, bool hasData)
    {
        <string> commandLines;
        commandLines.Append(commandLine);
        Command(commandLines, collect, hasData);
        
        if (collect && hasData && commandLine.StartsWith('F'))
        {
            serialOutput = serialOutput.Replace(".", "00");
            if (serialOutput.EndsWith('+'))
            {
                serialOutput = serialOutput.Substring(0, serialOutput.Length-1);
                serialOutput = serialOutput.Pad('0', 512);
            }
            string expanded;
            for (uint i=0; i < 16; i++)
            {
                String.Build(ref expanded, serialOutput.Substring(0, 32));
                String.Build(ref expanded, char(0x0D));
                serialOutput = serialOutput.Substring(32);
            }
            serialOutput =expanded;
        }
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
                _ = checkEcho(true);
            }
        }
        if (collect)
        {
            collectOutput = false;     
        }
    }
    bool RunCommand(char ch)
    {
        return RunCommand(ch.ToString());
    }
    bool RunCommand(string commandLine)
    {
        bool makersAllowed = commandLine == "X";
        sendCommand(commandLine);
        return checkEchoRun(makersAllowed); // special version for execute
    }
    uint ReturnToDebugger(char currentCommand, ref bool serialConnectionLost)
    {
        uint pc = GetCurrentPC();
        bool showSource = true;
        serialConnectionLost = false;
        loop
        {
            string sourceIndex = Code.GetSourceIndex(pc);
            if (sourceIndex.Length != 0)
            {
                break; // stop when we arrive at the next source line
            }
            else if (pc == 0)
            {
                // if there is no source line (global initialization)
                showSource = false;
                // after execute so we must have finished and restarted
                break; 
            }
            // keep stepping ..
            if (currentCommand == 'I')
            {
                if (!Monitor.RunCommand("I"))
                {
                    serialConnectionLost = true;
                    break;
                }
            }
            else // 'D' or 'O'
            {
                if (!Monitor.RunCommand("O"))
                {
                    serialConnectionLost = true;
                    break;
                }
            }
            pc = GetCurrentPC();
        } // loop
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
            
            _ = checkEcho(true); // waits for \ confirmation    
            
            long transfered;
            file dFile = File.Open(localPath);
            collectOutput = true; // just to toss it away   
            while (size != 0)
            {
                byte b = dFile.Read();
                str = b.ToHexString(2);
                foreach (var c in str)
                {
                    SerialWriteChar(c); 
                }
                size--;
                if (size % 256 == 0)
                {
                    Delay(1); // breathing room for serial comms
                }
                transfered++;
            }
            
            if (checkEcho(true)) // waits for \ confirmation    
            {
                Output.Print("  Uploaded to '" + filePath + "' (" + transfered.ToString() + " bytes)");
            }
            break;
        } // loop
        ClearSerialOutput(); // toss it
        collectOutput = false;   
    }
    
    bool FindCurrentHex(uint remoteCRC)
    {
        directory dir = Directory.Open("/Bin");
        if (dir.IsValid())
        {
            uint count = dir.GetFileCount();
            for (uint i = 0; i < count; i ++)
            {
                string filePath = dir.GetFile(i);
                string extension = Path.GetExtension(filePath).ToLower();
                if (extension == ".ihex")
                {
                    uint localCRC = File.CRC16(filePath);
                    if (localCRC == remoteCRC)
                    {
                        CurrentHexPath = filePath;
                        return true;
                    }
                }
            }
        }
        return false;
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
        uint crc = File.CRC16(ihexPath);
        SerialWriteChar(Byte.ToHex(byte((crc >> 4) & 0xF))); 
        _ = checkEcho(false);
        SerialWriteChar(Byte.ToHex(byte(crc & 0xF))); 
        _ = checkEcho(false);
        SerialWriteChar(Byte.ToHex(byte(crc >> 12))); 
        _ = checkEcho(false);
        SerialWriteChar(Byte.ToHex(byte((crc >> 8) & 0xF))); 
        _ = checkEcho(false);
        SerialWriteChar(char(0x0D));
        _ = checkEcho(false);

        collectOutput = true; // just to toss it away    
        while (iFile.IsValid())
        {
            string ln = iFile.ReadLine();
            foreach (var c in ln)
            {
                SerialWriteChar(c); 
                _ = checkEcho(false);
            }
            SerialWriteChar(char(0x0D));
            _ = checkEcho(false);
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
        Pages.LoadZeroPage(true); // update IsLoaded reliably
        collectOutput = false;
    }
    string CurrentHexPath { get { return lastHexPath; } set {lastHexPath = value; } }
    
    HopperFlags hopperFlags;
    
    bool IsMCU { get { return ( HopperFlags.MCUPlatform                               == (hopperFlags & HopperFlags.MCUPlatform)); } }
    
    string GetHopperInfo()
    {
        LoadZeroPage(false);
        
        if (ZeroPageContains("FLAGS"))
        {
            hopperFlags = HopperFlags(GetZeroPage("FLAGS"));
        }
        string info;
        info = info + "8 bit SP and BP";
        if (HopperFlags.WarpSpeed == (hopperFlags & (HopperFlags.MCUPlatform | HopperFlags.WarpSpeed)))
        {
            info = info + ", Warp speed (no <ctrl><C>)";
        }
        if (HopperFlags.CheckedBuild == (hopperFlags & HopperFlags.CheckedBuild))
        {
            info = info + ", Checked Build";
        }
        if (HopperFlags.MCUPlatform == (hopperFlags & HopperFlags.MCUPlatform))
        {
            info = info + ", MCU";
        }
        else
        {
            info = info + ", 6502";
        }
        if (HopperFlags.BreakpointsSet == (hopperFlags & HopperFlags.BreakpointsSet))
        {
            info = info + ", Breakpoint/s exist";
        }
        if (IsLoaded)
        {
            info = info + ", program loaded";
        }
        
        return info;
    }     
    
    uint GetCurrentPC()
    {
        Monitor.Command("P", true, true);
        string serialOutput = Monitor.GetSerialOutput();
        while ((serialOutput.Length >= 5) && ((serialOutput[0] == char(0x0A)) || (serialOutput[0] == char(0x0D))))
        {
            serialOutput = serialOutput.Substring(1);
        }
        uint pc;
        _ = UInt.TryParse("0x" + serialOutput, ref pc);
        return pc;
    }
    uint GetCurrentCRC()
    {
        Monitor.Command("K", true, true);
        string serialOutput = Monitor.GetSerialOutput();
        while ((serialOutput.Length >= 5) && ((serialOutput[0] == char(0x0A)) || (serialOutput[0] == char(0x0D))))
        {
            serialOutput = serialOutput.Substring(1);
        }
        //OutputDebug("GetCurrentCRC: " + serialOutput);
        uint crc;
        if (UInt.TryParse("0x" + serialOutput, ref crc))
        {
        }
        return crc;
    }
    
    bool Connect(uint comPort)
    {
        bool success;
        loop
        {
            if (comPort == 4242)
            {
                // use the serial port with the highest number
                <string> ports = Serial.Ports;
                if (ports.Count != 0)
                {
                    string name = ports[ports.Count-1];
                    name = name.Replace("COM", "");
                    if (UInt.TryParse(name, ref comPort))
                    {
                    }
                }
            }
            if (comPort != 4242)
            {
                Serial.Connect(comPort);
                if (Serial.IsValid())
                {
                    success = true;
                    break; // success
                }
            }
            string message = "Failed to connect to " + "COM" + comPort.ToString() + ".";
            if (comPort == 4242)
            {
                message = "No COM ports found.";
            }
            PrintLn(message, Colour.MatrixRed, Colour.Black);
            if (!IsInteractive)
            {
                // invoked from command line
                break; 
            }
            Key k;
            Print("  <enter> to retry, <esc> to exit.", Colour.MatrixRed, Colour.Black);
            loop
            {
                k = ReadKey();
                if ((k == Key.Escape) || (k == Key.Enter)) { break; }
            }
            PrintLn();
            if (k == Key.Escape) { break; }
        } // loop
        return success;
    }
}
