program Runtime
{
#define SERIAL_CONSOLE // Source/System/IO uses serial only (for MCU's etc)
#define RUNTIME        // keyboard buffer to make IO.IsBreak() work cannot be a 'string'
 
//#define CHECKED      // mainly stack checks, range checks and division by zero
//#define MEMORYLEAKS

//#define LOCALDEBUGGER  // for debugging portable runtime locally

    uses "Emulation/Minimal" // minimal use of actual 'system' APIs
    uses "Emulation/Memory"
    
    uses "Platform/OpCodes"
    uses "Platform/SysCalls"
    uses "Platform/Types"
    
    uses "Platform/GC"
    uses "Platform/Array"
    uses "Platform/Dictionary"
    uses "Platform/Directory"
    uses "Platform/File"
    uses "Platform/Float"
    uses "Platform/Int"
    uses "Platform/List"
    uses "Platform/Long"
    uses "Platform/Pair"
    uses "Platform/Char"
    uses "Platform/Byte"
    uses "Platform/String"
    uses "Platform/UInt"
    uses "Platform/Variant"
    
    uses "Platform/External"
    uses "Platform/Instructions"
    uses "Platform/Library"
    
    uses "HopperVM"
    
    const byte enter  = 0x0D;
    const byte escape = 0x1B;
    const byte slash  = 0x5C;
    
    const uint codeMemoryStart = 0x0000; // code memory magically exists from 0x0000 to 0xFFFF
    
    bool loaded = false;
    uint currentCRC;
    
    // Zero Page FLAGS:
    flags HopperFlags
    {
      //TraceOn        = 0x01,
        WarpSpeed      = 0x02, // on 6502, built without checks for <Ctrl><C>
      //StackSlot32Bit = 0x02, // on MCUs, 'float' and 'long' are value types
        CheckedBuild   = 0x04,
      //SP8Bit         = 0x08,
      //ProfileBuild   = 0x10,
        BreakpointsSet = 0x20,
      //SingleStep     = 0x40,
        MCUPlatform    = 0x80,
    }
    
    byte FromHex(char ch)
    {
        switch (ch)
        {
            case '0': { return 0x00; }
            case '1': { return 0x01; }
            case '2': { return 0x02; }
            case '3': { return 0x03; }
            case '4': { return 0x04; }
            case '5': { return 0x05; }
            case '6': { return 0x06; }
            case '7': { return 0x07; }
            case '8': { return 0x08; }
            case '9': { return 0x09; }
            case 'a': case 'A': { return 0x0A; }
            case 'b': case 'B': { return 0x0B; }
            case 'c': case 'C': { return 0x0C; }
            case 'd': case 'D': { return 0x0D; }
            case 'e': case 'E': { return 0x0E; }
            case 'f': case 'F': { return 0x0F; }
        }
        return 0;
    }
    bool TryReadByte(ref byte data)
    {
        char c0 = IO.Read();
        char c1 = IO.Read();
        byte msn = FromHex(c0);
        byte lsn = FromHex(c1);
        data =  (msn << 4) + lsn;
        IO.Write(c0);
        IO.Write(c1);
        return true;
    }
    
    bool LoadHexe(uint path, uint startAddress, ref uint loadedAddress, ref uint codeLength, bool doCRC)
    {
        bool success;
        loadedAddress = startAddress;
        uint address = 0;
        if (HRFile.Exists(path))
        {
            success = ReadAllCodeBytes(path, loadedAddress, ref codeLength);
            if (doCRC)
            {
                uint crcpath = HopperVM.GetAppName(true);
                uint crcFile = HRFile.Open(path);
                byte crc0 = HRFile.Read(crcFile);
                byte crc1 = HRFile.Read(crcFile);
                currentCRC = crc0 + (crc1 << 8);
                GC.Release(crcFile);
                GC.Release(crcpath);
            }
        }
        return success;
    }
    
    bool LoadIHex(ref uint loadedAddress, ref uint codeLength)
    {
        bool success = true;
        loadedAddress = codeMemoryStart;
        
        
        codeLength = 0;
        loop
        {
            char colon = IO.Read();
            if (colon != ':') { success = false; break; }
            IO.Write(':');
            
            byte byteCount;
            if (!TryReadByte(ref byteCount)) { success = false; break; }
            
            byte lsb;
            byte msb;
            if (!TryReadByte(ref msb)) { success = false; break; }
            if (!TryReadByte(ref lsb)) { success = false; break; }
            uint recordAddress = lsb + (msb << 8);
            
            byte recordType;
            if (!TryReadByte(ref recordType)) { success = false; break; }
            
            switch (recordType)
            {
                case 0x00: // data
                {
                    for (uint c=0; c < byteCount; c++)
                    {
                        byte dataByte;
                        if (!TryReadByte(ref dataByte)) { success = false; break; }
                        WriteCodeByte(codeMemoryStart + recordAddress, dataByte);
                        codeLength++;
                        recordAddress++;
                    }
                    byte checkSum;
                    if (!TryReadByte(ref checkSum)) { success = false; break; }
                    
                    char eol = IO.Read();
                    if ((eol != char(0x0D)) && (eol != char(0x0A))) // either will do
                    { 
                        success = false; break;
                    }
                    IO.WriteLn();
                    continue; // next record
                }
                case 0x01:
                {
                    break; // EOF
                }
                default:
                {
                    success = false; break;
                }
            }
            break;
        } // loop
        
        return success;
    }
    
#ifdef LOCALDEBUGGER
    print(char c)
    {
        Desktop.Print(c, Colour.MatrixRed, Colour.Black);
    }
    
    printDigit(uint uthis)
    {
        uint digit = uthis % 10;
        char c = HRByte.ToDigit(byte(digit));
        uthis = uthis / 10;
        if (uthis != 0)
        {
            printDigit(uthis);
        }
        print(c);
    }
    
    printHex(byte b)
    {
        byte msn = ((b >> 4) & 0xF);
        print(ToHex(msn));
        byte lsn = b & 0xF;
        print(ToHex(lsn));
    }
    printHex(uint u)
    {
        byte msb = byte(u >> 8);
        printHex(msb);
        byte lsb = byte(u & 0xFF);
        printHex(lsb);
    }
    
#endif
    ErrorDump(uint number)
    {
#ifdef LOCALDEBUGGER
        print('D');print('A');print('N');print('G');print('!');printDigit(number);print(' ');print('P');print('C');print(':');printHex(PC);
#endif
        IO.Write('D');IO.Write('A');IO.Write('N');IO.Write('G');IO.Write('!');
        IO.WriteUInt(number);
    }
    
    
    bool TryReadSerialByte(ref byte data)
    {
        char c0 = Serial.ReadChar();
        char c1 = Serial.ReadChar();
        byte msn = FromHex(c0);
        byte lsn = FromHex(c1);
        data =  (msn << 4) + lsn;
        return true;
    }
    
    bool SerialLoadIHex(ref uint loadedAddress, ref uint codeLength)
    {
        bool success = true;
        loadedAddress = codeMemoryStart;
        
        currentCRC = 0;
        byte crc0;
        byte crc1;
        if (!TryReadSerialByte(ref crc0)) { success = false; }
        if (success)
        {
            if(!TryReadSerialByte(ref crc1)) { success = false; }
        }
        if (success)
        {
            char eol = Serial.ReadChar();
            if ((eol != char(0x0D)) && (eol != char(0x0A))) // either will do
            { 
                success = false;
            }
            else
            {
                currentCRC = crc0 + (crc1 << 8);
            }
        }
        
        uint codeLimit = External.GetSegmentPages() << 8;
        
        codeLength = 0;
        loop
        {
            if (!success) { break; }
            char colon = Serial.ReadChar();
            if (colon != ':') { success = false; break; }
            
            byte byteCount;
            if (!TryReadSerialByte(ref byteCount)) { success = false; break; }
            
            byte lsb;
            byte msb;
            if (!TryReadSerialByte(ref msb)) { success = false; break; }
            if (!TryReadSerialByte(ref lsb)) { success = false; break; }
            uint recordAddress = lsb + (msb << 8);
            
            byte recordType;
            if (!TryReadSerialByte(ref recordType)) { success = false; break; }
            
            switch (recordType)
            {
                case 0x00: // data
                {
                    for (uint c=0; c < byteCount; c++)
                    {
                        byte dataByte;
                        if (!TryReadSerialByte(ref dataByte))             { success = false; break; }
                        if (codeMemoryStart + recordAddress >= codeLimit) { success = false; break; }
                        WriteCodeByte(codeMemoryStart + recordAddress, dataByte);
                        codeLength++;
                        recordAddress++;
                    }
                    byte checkSum;
                    if (!TryReadSerialByte(ref checkSum)) { success = false; break; }
                    
                    char eol = Serial.ReadChar();
                    if ((eol != char(0x0D)) && (eol != char(0x0A))) // either will do
                    { 
                        success = false; break;
                    }
                    continue; // next record
                }
                case 0x01:
                {
                    byte checkSum;
                    if (!TryReadSerialByte(ref checkSum)) { success = false; break; }
                    break; // EOF
                }
                default:
                {
                    success = false; break;
                }
            }
            break;
        } // loop
        return success;
    }
    
    WaitForEnter()
    {
        loop
        {
            char ch = Serial.ReadChar();
            if (ch == char(enter))
            {
                break;
            }       
        } // loop
        Serial.WriteChar(char(slash)); // '\' response : acknowledge <enter> received
    }
    
    Out4Hex(uint value)
    {
        byte b = byte(value >> 12);
        Serial.WriteChar(HRByte.ToHex(b));
        b = byte((value >> 8) & 0x0F); 
        Serial.WriteChar(HRByte.ToHex(b));
        b = byte((value >> 4) & 0x0F); 
        Serial.WriteChar(HRByte.ToHex(b));
        b = byte(value & 0x0F); 
        Serial.WriteChar(HRByte.ToHex(b));
    }
    
    Out2Hex(byte value)
    {
        byte b = byte((value >> 4) & 0x0F); 
        Serial.WriteChar(ToHex(b));
        b = byte(value & 0x0F); 
        Serial.WriteChar(HRByte.ToHex(b));
    }
    
    out4Hex(ref uint pageBuffer, uint value)
    {
        byte b = byte(value >> 12);
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        b = byte((value >> 8) & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        b = byte((value >> 4) & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        b = byte(value & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
    }
    
    out2Hex(ref uint pageBuffer, byte value)
    {
        byte b = byte((value >> 4) & 0x0F); 
        HRString.BuildChar(ref pageBuffer, ToHex(b));
        b = byte(value & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
    }
    
    DumpPage(byte iPage, bool includeAddresses)
    {
        // 6502 zero page simulation
        
        uint pageBuffer = HRString.New();
        
        uint rowAddress = (iPage << 8);
        for (byte row = 0; row < 16; row++)
        {
            HRString.BuildChar(ref pageBuffer, char(enter)); // next line
            if (includeAddresses)
            {
                out4Hex(ref pageBuffer, rowAddress);
                HRString.BuildChar(ref pageBuffer, ' ');
                rowAddress = rowAddress + 16;
            }
            if (iPage == 0)
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte data;
                    byte address = col + (row << 4);
                    switch (address)
                    {
                        case 0xB1:
                        {
                            data = byte(HopperVM.PC >> 8);
                        }
                        case 0xB0:
                        {
                            data = byte(HopperVM.PC & 0xFF);
                        }
                        case 0xB2: // SP
                        {
                            data = HopperVM.SP;
                        }
                        case 0xB6:
                        {
                            data = HopperVM.BP;
                        }
                        case 0xB8:
                        {
                            data = HopperVM.CSP;
                        }
                        case 0xBB: // flags
                        {
                            data = byte(HopperFlags.MCUPlatform); // 0x08 would imply 8 bit SP
                            if (HopperVM.BreakpointExists)
                            {
                                data = data | byte(HopperFlags.BreakpointsSet);
                            }
#ifdef CHECKED
                            data = data | byte(HopperFlags.CheckedBuild);
#endif                          
                        }
                        
                        case 0xE9:
                        {
                            data = byte(Memory.FreeList >> 8);
                        }
                        case 0xE8:
                        {
                            data = byte(Memory.FreeList & 0xFF);
                        }
                        case 0xEB:// HeapSize MSB
                        {
                            data = byte(Memory.HeapSize >> 8);
                        }
                        case 0xEA: // HeapStart MSB
                        {
                            data = byte(Memory.HeapStart >> 8);
                        }
                        case 0xCA: // CodeStart MSB
                        {
                            data = 0;
                        }
                        default:
                        {
                            if ((address >= 0x50) && (address <= 0x5F))
                            {
                                data = byte(GetBreakpoint(address - 0x50) & 0xFF);
                            }
                            else if ((address >= 0x60) && (address <= 0x6F))
                            {
                                data = byte(GetBreakpoint(address - 0x60) >> 8);
                            }
                            else
                            {
                                data = 0;
                            }
                        }
                    }
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, data);
                }
            } // zero page
            
            else if (iPage == 3) // call stack LSB
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4);
                    uint stackData = HopperVM.GetCS(address);
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, byte(stackData & 0xFF));
                }
            } // call stack
            
            else if (iPage == 4) // call stack MSB
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col*2 + (row << 4);
                    uint stackData = HopperVM.GetCS(address);
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, byte(stackData >> 8));
                }
            } // call stack
            
            else if (iPage == 5) // type stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4); // 0..255
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, byte(htype));
                }
            } // type stack
            
            else if (iPage == 6) // value stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4);
                    
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, byte(stackData & 0xFF));
                }
            } // value stack
            
            else if (iPage == 7) // value stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4);
                    
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, byte(stackData >> 8));
                }
            } // value stack
            
            else if (iPage > 7) // heap
            {
                for (byte col = 0; col < 16; col++)
                {
                    uint address = col + (row << 4); // 0..127
                    address = address + (iPage << 8);
                    
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, Memory.ReadByte(address));
                }
            } // heap
            
            else // anything else (like 01 and 02 for example)
            {
                for (byte col = 0; col < 16; col++)
                {
                    if (includeAddresses)
                    {
                        HRString.BuildChar(ref pageBuffer, ' '); 
                        if (col == 8)
                        {
                            HRString.BuildChar(ref pageBuffer, ' '); 
                        }
                    }
                    out2Hex(ref pageBuffer, 0x00);
                }
            } // heap
        } // for (row = 0; r < 16; row++)
        HRString.BuildChar(ref pageBuffer, char(enter)); // next line
        External.SerialWriteString(pageBuffer);
        GC.Release(pageBuffer);
    }
    
    Hopper()
    {
#ifdef LOCALDEBUGGER
        // 4242 is a magic number that means "0, but as server (not client)"
        // "COM0" is our fake interprocess COM port on Windows (named pipe)
        Serial.Connect(4242); 
#endif    
        loaded = false;
        HopperVM.Restart();
        bool refresh = true;
        
#ifndef LOCALDEBUGGER        
        if (External.LoadAuto)
        {
            uint loadedAddress;
            uint codeLength;
            uint startAddress = codeMemoryStart;
            
            // load 'auto.hexe' if it exists
            uint autoPath = HopperVM.GetAppName(false);
            if (Runtime.LoadHexe(autoPath, startAddress, ref loadedAddress, ref codeLength, true))
            {
                HopperVM.Initialize(loadedAddress, codeLength);
                HopperVM.Restart();
                loaded = true;
                if (HopperVM.InlinedExecuteWarp(false))
                {
                    HopperVM.Restart();
                }
            }
            GC.Release(autoPath);
        }
#endif

        Serial.WriteChar(char(slash)); // ready
        loop
        {
            char ch;
            if (Serial.IsAvailable)
            {
                ch = Serial.ReadChar();
            }
            if (ch == char(0x03)) // <ctrl><C> from Debugger but we were not running 
            {
                Serial.WriteChar(char(slash));
            }
            else if (ch == char(escape)) // <esc> from Debugger
            {
                Serial.WriteChar(char(slash)); // '\' response -> ready for command
                ch = Serial.ReadChar(); // single letter command
                switch (ch)
                {
                    case 'F': // fast memory page dump
                    {
                        byte msn = FromHex(Serial.ReadChar());
                        byte lsn = FromHex(Serial.ReadChar());
                        WaitForEnter();
                        
                        byte iPage = (msn << 4) + lsn;
                        DumpPage(iPage, false);
                        Serial.WriteChar(char(slash)); // confirm data
                    }
                    case 'B':
                    {
                        char arg = Serial.ReadChar();
                        if (arg == 'X')
                        {
                            HopperVM.ClearBreakpoints(false);
                        }
                        else
                        {
                            byte n  = FromHex(arg);
                            byte a3 = FromHex(Serial.ReadChar());
                            byte a2 = FromHex(Serial.ReadChar());
                            byte a1 = FromHex(Serial.ReadChar());
                            byte a0 = FromHex(Serial.ReadChar());
                            uint address = (a3 << 12) + (a2 << 8) + (a1 << 4) + a0;
                            HopperVM.SetBreakpoint(n, address);   
                        }
                        WaitForEnter();
                    }
                    case 'P': // get PC
                    {
                        WaitForEnter();
                        
                        uint serialBuffer = HRString.New();
                        HRString.BuildChar(ref serialBuffer, char(enter));
                        out4Hex(ref serialBuffer, HopperVM.PC);
                        HRString.BuildChar(ref serialBuffer, char(slash)); // confirm data
                        External.SerialWriteString(serialBuffer);
                        GC.Release(serialBuffer);
                    }
                    case 'K': // get CRC
                    {
                        WaitForEnter();
                        
                        uint serialBuffer = HRString.New();
                        HRString.BuildChar(ref serialBuffer, char(enter));
                        out4Hex(ref serialBuffer, currentCRC);
                        HRString.BuildChar(ref serialBuffer, char(slash)); // confirm data
                        External.SerialWriteString(serialBuffer);
                        GC.Release(serialBuffer);
                    }
                    case 'E': // enter Boot Select mode
                    {
                        WaitForEnter();
                        External.MCUReboot(true);
                    }
                    case 'T':
                    {
                        WaitForEnter();
                        
                        // read name characters till 0x0D
                        uint destinationName = HRString.New();
                        loop
                        {
                            char rc = Serial.ReadChar();
                            if (rc == char(enter))
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationName, rc);
                        }
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar(char(slash));
                        
                        // read path characters till 0x0D
                        uint destinationFolder = HRString.New();
                        loop
                        {
                            char rc = Serial.ReadChar();
                            if (rc == char(enter))
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationFolder, rc);
                        }
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar(char(slash));
                        
                        HRDirectory.Create(destinationFolder);
                        
                        char h3 = Serial.ReadChar();
                        char h2 = Serial.ReadChar();
                        char h1 = Serial.ReadChar();
                        char h0 = Serial.ReadChar();
                        
                        uint size = (FromHex(h3) << 12) + (FromHex(h2) << 8) + (FromHex(h1) << 4) + FromHex(h0);
                        
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar(char(slash));
                        
                        uint fh = HRFile.Create(destinationName);
                        
                        // read file hex nibbles
                        while (size != 0)
                        {
                            char n1 = Serial.ReadChar();
                            char n0 = Serial.ReadChar();
                            byte b = (FromHex(n1) << 4) + FromHex(n0);
                            HRFile.Append(fh, b);
                            size--;
                        }
                        HRFile.Flush(fh);
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar(char(slash));
                        
                    }
                    case 'L':
                    {
                        WaitForEnter();
                        
                        uint loadedAddress = 0;
                        uint codeLength;
                    
                        loaded = SerialLoadIHex(ref loadedAddress, ref codeLength);
                        Serial.WriteChar(char(enter));
                        if (loaded)
                        {
                            HopperVM.Initialize(loadedAddress, codeLength);
                            HopperVM.Restart();
                            
                            uint serialBuffer = HRString.New();
                            
                            // loadCodeLength
                            out4Hex(ref serialBuffer, codeLength);
                            HRString.BuildChar(ref serialBuffer, ' ');
                            
                            // heapStart
                            out4Hex(ref serialBuffer, Memory.HeapStart);
                            HRString.BuildChar(ref serialBuffer, ' ');
                            
                            // heapSize
                            out4Hex(ref serialBuffer, Memory.HeapSize);
                            HRString.BuildChar(ref serialBuffer, ' ');
                            
                            External.SerialWriteString(serialBuffer);
                            GC.Release(serialBuffer);
                        }
                        
                        // '*' success
                        loop
                        {
                            ch = Serial.ReadChar();
                            if ((ch == '!') || (ch == '*'))
                            {
                                break;
                            }
                        }
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar(loaded ? '*' : '!');
                        
                        Serial.WriteChar(char(slash)); // confirm the data
#ifndef LOCALDEBUGGER                        
                        if (loaded)
                        {
                            FlashProgram(loadedAddress, codeLength, currentCRC);
                        }
#endif
                    } // L
                    default:
                    {
                        if (loaded)
                        {
                            switch (ch)
                            {
                                case 'O': // Step Over | <F10>
                                {
                                    WaitForEnter();
                                    bool restart;
                                    uint pc = HopperVM.PC;
                                    OpCode opCode = OpCode(ReadProgramByte(pc));
                                    
                                    if ((opCode == OpCode.CALL) || (opCode == OpCode.CALLI))
                                    {
                                        
                                        // set breakpoint[0] to PC+3
                                        HopperVM.SetBreakpoint(0, pc+3);
                                        restart = HopperVM.Execute();
                                    }
                                    else if (opCode == OpCode.CALLB)
                                    {
                                        // set breakpoint[0] to PC+2
                                        HopperVM.SetBreakpoint(0, pc+2);
                                        restart = HopperVM.Execute();
                                    }
                                    else
                                    {
                                        // use single step (set bit in HopperFlags)
                                        restart = HopperVM.ExecuteStepTo();
                                    }
                                    if (restart)
                                    {
                                        HopperVM.Restart();
                                    }
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'I': // Step Into | <F11>
                                {
                                    WaitForEnter();
                                    if (HopperVM.ExecuteStepTo())
                                    {
                                        HopperVM.Restart();
                                    }
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'D': // Debug
                                {
                                    WaitForEnter();
                                    if (HopperVM.Execute())
                                    {
                                        HopperVM.Restart();
                                    }
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'X': // Execute
                                {
                                    WaitForEnter();
                                    if (HopperVM.InlinedExecuteWarp(false))
                                    {
                                        HopperVM.Restart();
                                    }
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'W': // Warm restart
                                {
                                    WaitForEnter();
                                    HopperVM.Restart();
                                }
                            } // switch
                        } // loaded
                    } // default
                } // switch
            } // <esc> from Debugger
        } // loop
        HopperVM.Release();
    }
}

