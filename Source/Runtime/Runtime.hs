program Runtime
{
    //#define EXPERIMENTAL 
     
#define SERIAL_CONSOLE // Source/System/IO uses serial only (for MCU's etc)
#define RUNTIME        // keyboard buffer to make IO.IsBreak() work cannot be a 'string'

 
//#define CHECKED      // mainly stack checks, range checks and division by zero
//#define MEMORYLEAKS

//#define LOCALDEBUGGER  // for debugging portable runtime locally
#define CPU_Z80

// Minimal Runtime is the value-type only, minimal SysCalls version that we translate to Z80
#ifdef CPU_Z80
    #define NO_JIX_INSTRUCTIONS
    #define CDECL
    #define VALUE_TYPE_RUNTIME        // what we are building
    #define MINIMAL_RUNTIME_SYSCALLS  // limited set of SysCalls available to the user
#endif

#ifndef VALUE_TYPE_RUNTIME
    #define INCLUDE_FILESYSTEM
    #define INCLUDE_FLOATS
    #define INCLUDE_LONGS
    #define INCLUDE_LIBRARY
    #define INCLUDE_WIFI
#endif

#ifndef CPU_Z80
    #define INCLUDE_LISTS
    #define INCLUDE_DICTIONARIES
#endif

    uses "/Source/Debugger/6502/ZeroPage"

    uses "Emulation/Minimal" // minimal use of actual 'system' APIs
    uses "Emulation/Memory"
    
    uses "Platform/OpCodes"
    uses "Platform/SysCalls"
    uses "Platform/Types"

    uses "Platform/GC"
    uses "Platform/Array"
#ifdef INCLUDE_DICTIONARIES    
    uses "Platform/Dictionary"
#endif    
#ifndef VALUE_TYPE_RUNTIME
    uses "Platform/Directory"
    uses "Platform/File"
    uses "Platform/Float"
    uses "Platform/Long"
#endif    
    uses "Platform/Int"
    uses "Platform/List"
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
    
    const byte enter  = 0x0A;
    const byte escape = 0x1B;
    const byte slash  = 0x5C;
    
#ifdef CPU_Z80
    const uint codeMemoryStart = 0x8000; // assuming 32K of RAM starting here for now
#else    
    const uint codeMemoryStart = 0x0000; // code memory magically exists from 0x0000 to 0xFFFF
#endif

    bool loaded = false;
    uint currentCRC;
    
       
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
        print(HRByte.ToHex(msn));
        byte lsn = b & 0xF;
        print(HRByte.ToHex(lsn));
    }
    printHex(uint u)
    {
        byte msb = byte(u >> 8);
        printHex(msb);
        byte lsb = byte(u & 0xFF);
        printHex(lsb);
    }
    
#endif    
    
    char SerialReadChar()
    {
        char c = Serial.ReadChar();
#ifdef LOCALDEBUGGER        
        if (c > ' ')
        {
            Desktop.Print(c, Colour.MatrixBlue, Colour.Black);
        }
        else
        {
            byte b = byte(c);
            byte msn = ((b >> 4) & 0xF);
            byte lsn = b & 0xF;
            Desktop.Print(' ', Colour.MatrixBlue, Colour.Black);
            Desktop.Print(HRByte.ToHex(msn), Colour.MatrixBlue, Colour.Black);
            Desktop.Print(HRByte.ToHex(lsn), Colour.MatrixBlue, Colour.Black);
            Desktop.Print(' ', Colour.MatrixBlue, Colour.Black);
        }
#endif        
        return c;
    }
    SerialWriteChar(char c)
    {
#ifdef LOCALDEBUGGER        
        if (c > ' ')
        {
            Desktop.Print(c, Colour.MatrixRed, Colour.Black);
        }
        else
        {
            byte b = byte(c);
            byte msn = ((b >> 4) & 0xF);
            byte lsn = b & 0xF;
            Desktop.Print(' ', Colour.MatrixRed, Colour.Black);
            Desktop.Print(HRByte.ToHex(msn), Colour.MatrixRed, Colour.Black);
            Desktop.Print(HRByte.ToHex(lsn), Colour.MatrixRed, Colour.Black);
            Desktop.Print(' ', Colour.MatrixRed, Colour.Black);
        }
#endif
        Serial.WriteChar(c);
    }
    
    bool TryReadByte(ref byte data)
    {
        char c0 = IO.Read();
        char c1 = IO.Read();
        byte msn = HRChar.FromHex(c0);
        byte lsn = HRChar.FromHex(c1);
        data =  (msn << 4) + lsn;
        IO.Write(c0);
        IO.Write(c1);
        return true;
    }
#ifdef INCLUDE_FILESYSTEM    
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
#endif
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
                    if (eol != Char.EOL)
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
        char c0;
        char c1;
        byte msn;
        byte lsn;
        c0 = SerialReadChar();
        c1 = SerialReadChar();
        msn = HRChar.FromHex(c0);
        lsn = HRChar.FromHex(c1);
        data =  (msn << 4) + lsn;
        return true;
    }
    
    bool SerialLoadIHex(ref uint loadedAddress, ref uint codeLength)
    {
        byte crc0;
        byte crc1;
        byte checkSum;
        byte lsb;
        byte msb;
        char eol;    
        uint codeLimit;
        uint c;
        char colon;
        byte byteCount;
        byte dataByte;
        byte recordType;
        uint recordAddress;
        bool success = true;
        
        loadedAddress = codeMemoryStart;
        
        currentCRC = 0;
        if (!TryReadSerialByte(ref crc0)) { success = false; }
        if (success)
        {
            if(!TryReadSerialByte(ref crc1)) { success = false; }
        }
        if (success)
        {
            eol = SerialReadChar();
            if (eol != Char.EOL)
            { 
                success = false;
            }
            else
            {
                currentCRC = crc0 + (crc1 << 8);
            }
        }
        
        codeLimit = External.GetSegmentPages() << 8;
        
        codeLength = 0;
        loop
        {
            if (!success) { break; }
            colon = SerialReadChar();
            if (colon != ':') { success = false; break; }
            
            
            if (!TryReadSerialByte(ref byteCount)) { success = false; break; }
            
            if (!TryReadSerialByte(ref msb)) { success = false; break; }
            if (!TryReadSerialByte(ref lsb)) { success = false; break; }
            recordAddress = lsb + (msb << 8);
            
            if (!TryReadSerialByte(ref recordType)) { success = false; break; }
            
            if (recordType == 0x00)
            {
                // data
                for (c=0; c < byteCount; c++)
                {
                    if (!TryReadSerialByte(ref dataByte))             { success = false; break; }
                    if (codeMemoryStart + recordAddress >= codeLimit) { success = false; break; }
                    WriteCodeByte(codeMemoryStart + recordAddress, dataByte);
                    codeLength++;
                    recordAddress++;
                }
                if (!TryReadSerialByte(ref checkSum)) { success = false; break; }
                
                eol = SerialReadChar();
                if (eol != Char.EOL)
                { 
                    success = false; break;
                }
                continue; // next record
            }
            else if (recordType == 0x01)
            {
                // EOF
                if (!TryReadSerialByte(ref checkSum)) 
                { 
                    success = false; 
                }
            }
            else
            {
                success = false;
            }
            break;
        } // loop
        return success;
    }
    
    WaitForEnter()
    {
        char ch;
        loop
        {
            ch = SerialReadChar();
            if (ch == char(enter))
            {
                break;
            }       
        } // loop
        SerialWriteChar(char(slash)); // '\' response : acknowledge <enter> received
    }
    
    Out4Hex(uint value)
    {
        byte b;
        b = byte(value >> 12);
        SerialWriteChar(HRByte.ToHex(b));
        b = byte((value >> 8) & 0x0F); 
        SerialWriteChar(HRByte.ToHex(b));
        b = byte((value >> 4) & 0x0F); 
        SerialWriteChar(HRByte.ToHex(b));
        b = byte(value & 0x0F); 
        SerialWriteChar(HRByte.ToHex(b));
    }
    
    Out2Hex(byte value)
    {
        byte b;
        b = byte((value >> 4) & 0x0F); 
        SerialWriteChar(ToHex(b));
        b = byte(value & 0x0F); 
        SerialWriteChar(HRByte.ToHex(b));
    }
    
    out4Hex(ref uint pageBuffer, uint value)
    {
        byte b;
        b = byte(value >> 12);
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        b = byte((value >> 8) & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        b = byte((value >> 4) & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        b = byte(value & 0x0F); 
        HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
    }
    
    out2HexOrDot(ref uint pageBuffer, byte value)
    {
        byte b;
        if (value == 0)
        {
            HRString.BuildChar(ref pageBuffer, '.'); // '.' means '00'
        }
        else
        {
            b = byte((value >> 4) & 0x0F); 
            HRString.BuildChar(ref pageBuffer, ToHex(b));
            b = byte(value & 0x0F); 
            HRString.BuildChar(ref pageBuffer, HRByte.ToHex(b));
        }
    }
    
    DumpPage(byte iPage)
    {
        uint pageBuffer;
        uint rowAddress;
        byte row;
        pageBuffer = HRString.New();
        rowAddress = (iPage << 8);
        for (row = 0; row < 16; row++)
        {
            if (iPage == 0)
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte data;
                    byte address = col + (row << 4);
                    switch (address)
                    {
                        case ZPCL:
                        {
                            data = byte(HopperVM.PC & 0xFF);
                        }
                        case ZPCH:
                        {
                            data = byte(HopperVM.PC >> 8);
                        }
                        case ZCODESTARTL:
                        case ZCODESTARTH:
                        {
                            data = 0; // CODESTART
                        }
                        case ZSP: // SP
                        {
                            data = HopperVM.SP;
                        }
                        case ZBP:
                        {
                            data = HopperVM.BP;
                        }
                        case ZCSP:
                        {
                            data = HopperVM.CSP;
                        }
                        case ZCNP:
                        {
                            data = HopperVM.CNP ? 1 : 0;
                        }
                        
                        case ZFLAGS: // flags
                        {
                            HopperFlags flgs = HopperFlags.MCUPlatform;
                            if (HopperVM.BreakpointExists)
                            {
                                flgs = flgs | HopperFlags.BreakpointsSet;
                            }
                            if (loaded)
                            {
                                flgs = flgs | HopperFlags.ProgramLoaded;
                            }
#ifdef CHECKED
                            flgs = flgs | HopperFlags.CheckedBuild;
#endif                        
                            data = byte(flgs);  
                        }
                        
                        case ZFREELISTL:
                        {
                            data = byte(Memory.FreeList & 0xFF);
                        }
                        case ZFREELISTH:
                        {
                            data = byte(Memory.FreeList >> 8);
                        }
                        case ZHEAPSTART: // HeapStart MSB
                        {
                            data = byte(Memory.HeapStart >> 8);
                        }
                        case ZHEAPSIZE:// HeapSize MSB
                        {
                            data = byte(Memory.HeapSize >> 8);
                        }
                        
                        default:
                        {
                            if ((address >= ZBRKL) && (address <= ZBRKL+0x0F))
                            {
                                data = byte(GetBreakpoint(address - ZBRKL) & 0xFF);
                            }
                            else if ((address >= ZBRKH) && (address <= ZBRKH+0x0F))
                            {
                                data = byte(GetBreakpoint(address - ZBRKH) >> 8);
                            }
                            else
                            {
                                data = 0;
                            }
                        }
                    }
                    out2HexOrDot(ref pageBuffer, data);
                }
            } // zero page
            
            else if (iPage == 3) // call stack LSB
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4); // 0..255
                    uint stackData = HopperVM.GetCS(address);
                    out2HexOrDot(ref pageBuffer, byte(stackData & 0xFF));
                }
            } // call stack
            
            else if (iPage == 4) // call stack MSB
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4); // 0..255
                    uint stackData = HopperVM.GetCS(address);
                    out2HexOrDot(ref pageBuffer, byte(stackData >> 8));
                }
            } // call stack
            
            else if (iPage == 5) // type stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4); // 0..255
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    out2HexOrDot(ref pageBuffer, byte(htype));
                }
            } // type stack
            
            else if (iPage == 6) // value stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4); // 0..255
                    uint stackData = HopperVM.Get(address);
                    out2HexOrDot(ref pageBuffer, byte(stackData & 0xFF));
                }
            } // value stack
            
            else if (iPage == 7) // value stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    byte address = col + (row << 4); // 0..255
                    uint stackData = HopperVM.Get(address);
                    out2HexOrDot(ref pageBuffer, byte(stackData >> 8));
                }
            } // value stack
            
            else if (iPage > 7) // heap
            {
                for (byte col = 0; col < 16; col++)
                {
                    uint address = col + (row << 4); // 0..255
                    address = address + (iPage << 8);
                    out2HexOrDot(ref pageBuffer, Memory.ReadByte(address));
                }
            } // heap
            
            else // anything else (like 01 and 02 for example)
            {
                for (byte col = 0; col < 16; col++)
                {
                    out2HexOrDot(ref pageBuffer, 0x00);
                }
            } // heap
        } // for (row = 0; r < 16; row++)
        
        int length = int(HRString.GetLength(pageBuffer));
        int iLast = -1;
        int i = length-1;
        loop
        {
            char ch = HRString.GetChar(pageBuffer, uint(i));
            if ((ch != '0') && (ch != '.'))
            {
                iLast = i;
                break;
            }
            if (i == 0) { break; }
            i--;
        }
        if (iLast != length-1)
        {
            // smallest possible is length-2
            iLast++;
            HRString.SetChar(pageBuffer, uint(iLast), '+');
            HRString.SetLength(pageBuffer, uint(iLast+1));
        }
        
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
        
#if !defined(LOCALDEBUGGER) && defined(INCLUDE_FILESYSTEM)
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

        SerialWriteChar(char(slash)); // ready
        loop
        {
            char ch;
            if (Serial.IsAvailable)
            {
                ch = SerialReadChar();
            }
            if (ch == char(0x03)) // <ctrl><C> from Debugger but we were not running 
            {
                SerialWriteChar(char(slash));
            }
            else if (ch == char(escape)) // <esc> from Debugger
            {
                SerialWriteChar(char(slash)); // '\' response -> ready for command
                ch = SerialReadChar(); // single letter command
                switch (ch)
                {
                    case 'F': // fast memory page dump
                    {
                        byte msn = HRChar.FromHex(SerialReadChar());
                        byte lsn = HRChar.FromHex(SerialReadChar());
                        WaitForEnter();
                        
                        byte iPage = (msn << 4) + lsn;
                        DumpPage(iPage);
                        SerialWriteChar(char(slash)); // confirm data
                    }
                    case 'B':
                    {
                        char arg = SerialReadChar();
                        if (arg == 'X')
                        {
                            HopperVM.ClearBreakpoints(false);
                        }
                        else
                        {
                            byte n  = HRChar.FromHex(arg);
                            byte a3 = HRChar.FromHex(SerialReadChar());
                            byte a2 = HRChar.FromHex(SerialReadChar());
                            byte a1 = HRChar.FromHex(SerialReadChar());
                            byte a0 = HRChar.FromHex(SerialReadChar());
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
#ifndef VALUE_TYPE_RUNTIME
                        WaitForEnter();
                        External.MCUReboot(true);
#else
                        Error = 0x0A;
#endif                        
                    }
                    case 'T':
                    {
#ifdef INCLUDE_FILESYSTEM
                        WaitForEnter();
                        
                        // read name characters till 0x0A
                        uint destinationName = HRString.New();
                        loop
                        {
                            char rc = SerialReadChar();
                            if (rc == char(enter))
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationName, rc);
                        }
                        SerialWriteChar(char(enter));
                        SerialWriteChar(char(slash));
                        
                        // read path characters till 0x0A
                        uint destinationFolder = HRString.New();
                        loop
                        {
                            char rc = SerialReadChar();
                            if (rc == char(enter))
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationFolder, rc);
                        }
                        SerialWriteChar(char(enter));
                        SerialWriteChar(char(slash));
                        
                        HRDirectory.Create(destinationFolder);
                        
                        char h3 = SerialReadChar();
                        char h2 = SerialReadChar();
                        char h1 = SerialReadChar();
                        char h0 = SerialReadChar();
                        
                        uint size = (HRChar.FromHex(h3) << 12) + (HRChar.FromHex(h2) << 8) + (HRChar.FromHex(h1) << 4) + HRChar.FromHex(h0);
                        
                        SerialWriteChar(char(enter));
                        SerialWriteChar(char(slash));
                        
                        uint fh = HRFile.Create(destinationName);
                        
                        // read file hex nibbles
                        while (size != 0)
                        {
                            char n1 = SerialReadChar();
                            char n0 = SerialReadChar();
                            byte b = (HRChar.FromHex(n1) << 4) + HRChar.FromHex(n0);
                            HRFile.Append(fh, b);
                            size--;
                        }
                        HRFile.Flush(fh);
                        SerialWriteChar(char(enter));
                        SerialWriteChar(char(slash));
#else
                        Error = 0x0A;
#endif                        
                    }
                    case 'L':
                    {
                        WaitForEnter();
                        
                        uint loadedAddress = 0;
                        uint codeLength;
                    
                        loaded = SerialLoadIHex(ref loadedAddress, ref codeLength);
                        SerialWriteChar(char(enter));
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
                            ch = SerialReadChar();
                            if ((ch == '!') || (ch == '*'))
                            {
                                break;
                            }
                        }
                        SerialWriteChar(char(enter));
                        SerialWriteChar(loaded ? '*' : '!');
                        
                        SerialWriteChar(char(slash)); // confirm the data
#if !defined(LOCALDEBUGGER) && defined(INCLUDE_FILESYSTEM)
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
                                    SerialWriteChar(char(slash)); // confirm handing back control
                                }
                                case 'I': // Step Into | <F11>
                                {
                                    WaitForEnter();
                                    if (HopperVM.ExecuteStepTo())
                                    {
                                        HopperVM.Restart();
                                    }
                                    SerialWriteChar(char(slash)); // confirm handing back control
                                }
                                case 'D': // Debug
                                {
                                    WaitForEnter();
                                    if (HopperVM.Execute())
                                    {
                                        HopperVM.Restart();
                                    }
                                    SerialWriteChar(char(slash)); // confirm handing back control
                                }
                                case 'X': // Execute
                                {
                                    WaitForEnter();
                                    if (HopperVM.InlinedExecuteWarp(false))
                                    {
                                        HopperVM.Restart();
                                    }
                                    SerialWriteChar(char(slash)); // confirm handing back control
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

