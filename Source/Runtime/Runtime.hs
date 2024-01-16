program Runtime
{
//#define CHECKED     // mainly stack checks, range checks and division by zero
//#define MEMORYLEAKS

#define RUNTIME       // workaround special clipboard buffer for testing on Windows (that works without String)
#define SERIAL_CONSOLE // Source/System/IO uses serial only (for MCU's etc)

    uses "/Source/Runtime/Emulation/Minimal" // minimal use of actual 'system' APIs
    uses "/Source/Runtime/Emulation/Memory"
    
    uses "/Source/Runtime/Platform/OpCodes"
    uses "/Source/Runtime/Platform/SysCalls"
    uses "/Source/Runtime/Platform/Types"
    
    uses "/Source/Runtime/Platform/GC"
    uses "/Source/Runtime/Platform/Array"
    uses "/Source/Runtime/Platform/Dictionary"
    uses "/Source/Runtime/Platform/Directory"
    uses "/Source/Runtime/Platform/File"
    uses "/Source/Runtime/Platform/Float"
    uses "/Source/Runtime/Platform/Int"
    uses "/Source/Runtime/Platform/List"
    uses "/Source/Runtime/Platform/Long"
    uses "/Source/Runtime/Platform/Pair"
    uses "/Source/Runtime/Platform/Char"
    uses "/Source/Runtime/Platform/String"
    uses "/Source/Runtime/Platform/UInt"
    uses "/Source/Runtime/Platform/Variant"
    
    uses "/Source/Runtime/Platform/External"
    uses "/Source/Runtime/Platform/Instructions"
    uses "/Source/Runtime/Platform/Library"
    
    uses "/Source/Runtime/HopperVM"
    
    // Zero Page FLAGS:
    flags HopperFlags
    {
        TraceOn        = 0x01,
        WarpSpeed      = 0x02, // on 6502, built without checks for <Ctrl><C>
        StackSlot32Bit = 0x02, // on MCUs, 'float' and 'long' are value types
        CheckedBuild   = 0x04,
        SP8Bit         = 0x08,
        ProfileBuild   = 0x10,
        BreakpointsSet = 0x20,
        SingleStep     = 0x40,
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
    
    bool loaded = false;
    const uint codeMemoryStart = 0x0000; // code memory magically exists from 0x0000 to 0xFFFF
    
    bool LoadAuto(ref uint loadedAddress, ref uint codeLength)
    {
        bool success;
        
        loadedAddress = codeMemoryStart;
        uint address = 0;
        
        uint path = HopperVM.GetAppName();
        if (HRFile.Exists(path))
        {
            success = ReadAllCodeBytes(path, loadedAddress, ref codeLength);
        }
        
        GC.Release(path);
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
    
    ErrorDump(uint number)
    {
        IO.Write('D');IO.Write('A');IO.Write('N');IO.Write('G');IO.Write('!');
        IO.WriteUInt(number);
    }
    
    Windows()
    {
        loaded = false;
        HopperVM.Restart();
        bool refresh = true;
        loop
        {
            if (refresh)
            {
                IO.WriteLn();
                IO.Write('>');
                refresh = false;
            }
            char c = IO.Read().ToUpper();
            if (c != char(0x0D))
            {
                IO.Write(c);
            }
            
            switch (c.ToUpper())
            {
                case 'Q':
                {
                    break; // exit
                }
                case 'L':
                {
                    IO.WriteLn();
                    IO.Write('P');IO.Write('a');IO.Write('s');IO.Write('t');IO.Write('e');IO.Write(' ');
                    IO.Write('I');IO.Write('H');IO.Write('e');IO.Write('x');IO.Write(':');
                    IO.WriteLn();
                    
                    // load mode
                    uint loadedAddress;
                    uint codeLength;
                    
                    loaded = LoadIHex(ref loadedAddress, ref codeLength);
                    
                    if (loaded) // success?
                    {
                        IO.WriteLn();
                        IO.Write('L');IO.Write('o');IO.Write('a');IO.Write('d');IO.Write('e');IO.Write('d');
                        IO.WriteLn();
                        HopperVM.Initialize(loadedAddress);
                        HopperVM.Restart();
                    }
                    else
                    {
                        IO.WriteLn();
                        IO.Write('F');IO.Write('a');IO.Write('i');IO.Write('l');IO.Write('e');IO.Write('d');
                        IO.WriteLn();
                    }
                    refresh = true;
                }
                default:
                {
                    if (loaded)
                    {
                        switch (c.ToUpper())
                        {
                            case 'D':
                            {
                                if (HopperVM.Execute())
                                {
                                    HopperVM.Restart();
                                }
                                if (Error == 0)
                                {
                                    HopperVM.DumpHeap(false, 0x22 + 0x0A + 0x0A + 0x10); // breakpoints, Array bit tables and CurrentDirectory
                                }
                                refresh = true;
                            }
                            case 'I':
                            {
                                if (HopperVM.ExecuteStepTo())
                                {
                                    HopperVM.Restart();
                                }
                                refresh = true;
                            }
                            case 'O':
                            {
                                uint pc = HopperVM.PC;
                                OpCode opCode = OpCode(ReadByte(pc));
                                bool restart;
                                if ((opCode == OpCode.CALLW) || (opCode == OpCode.CALLIW))
                                {
                                    
                                    // set breakpoint[0] to PC+3
                                    HopperVM.SetBreakpoint(0, pc+3);
                                    restart = HopperVM.Execute();
                                }
                                else if (opCode == OpCode.CALLB)
                                {
                                    // set breakpoint[0] to PC+2
                                    HopperVM.SetBreakpoint(0, pc+2);
                                    restart =HopperVM.Execute();
                                }
                                else if (opCode == OpCode.CALLREL)
                                {
                                    // set breakpoint[0] to PC+1
                                    HopperVM.SetBreakpoint(0, pc+1);
                                    restart =HopperVM.Execute();
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
                                refresh = true;
                            }
                            case 'V':
                            {
                                HopperVM.DumpStack(20);
                                refresh = true;
                            }
                            case 'H':
                            {
                                HopperVM.DumpHeap(true, 0);
                                refresh = true;
                            }
                            case 'X':
                            {
                                if (HopperVM.InlinedExecuteWarp(false))
                                {
                                    HopperVM.Restart();
                                }
                                refresh = true;
                            }
                            case 'W':
                            {
                                HopperVM.Restart();
                                refresh = true;
                            }
                            default:
                            {
                                if (c != char(0x0D))
                                {
                                    IO.Write(char(0x08));
                                    IO.Write(' ');
                                    IO.Write(char(0x08));
                                }
                            }       
                        }
                    }
                    else if (c != char(0x0D))
                    {
                        IO.Write(char(0x08));
                        IO.Write(' ');
                        IO.Write(char(0x08));
                    }
                }
            }
        } // loop
        HopperVM.Release();
    }
    
    const byte enter  = 0x0D;
    const byte escape = 0x1B;
    const byte slash  = 0x5C;
    
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
        
        uint codeLimit = External.GetSegmentPages() << 8;
        
        codeLength = 0;
        loop
        {
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
        Serial.WriteChar(ToHex(b));
        b = byte((value >> 8) & 0x0F); 
        Serial.WriteChar(ToHex(b));
        b = byte((value >> 4) & 0x0F); 
        Serial.WriteChar(ToHex(b));
        b = byte(value & 0x0F); 
        Serial.WriteChar(ToHex(b));
    }
    
    Out2Hex(byte value)
    {
        byte b = byte((value >> 4) & 0x0F); 
        Serial.WriteChar(ToHex(b));
        b = byte(value & 0x0F); 
        Serial.WriteChar(ToHex(b));
    }
    
    DumpPage(byte iPage, bool includeAddresses)
    {
        // 6502 zero page simulation
        uint rowAddress = (iPage << 8);
        for (byte row = 0; row < 16; row++)
        {
            Serial.WriteChar(char(enter)); // next line
            if (includeAddresses)
            {
                Out4Hex(rowAddress);
                Serial.WriteChar(' ');
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
                        case 0xB3: // SPH
                        {
                            data = byte((HopperVM.SP + 0x0600) >> 8);
                        }
                        case 0xB2: // SPL
                        {
                            data = byte((HopperVM.SP + 0x0600) & 0xFF);
                        }
                        case 0xB5: // TSPH
                        {
                            data = byte(((HopperVM.SP/2) + 0x0500) >> 8);
                        }
                        case 0xB4: // TSPL
                        {
                            data = byte(((HopperVM.SP/2) + 0x0500) & 0xFF);
                        }
                        case 0xB7:
                        {
                            data = byte((HopperVM.BP + 0x0600) >> 8);
                        }
                        case 0xB6:
                        {
                            data = byte((HopperVM.BP + 0x0600) & 0xFF);
                        }
                        case 0xB9:
                        {
                            data = byte((HopperVM.CSP + 0x0400) >> 8);
                        }
                        case 0xB8:
                        {
                            data = byte((HopperVM.CSP + 0x0400) & 0xFF);
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
                        Serial.WriteChar(' ');
                        if (col == 8)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(data);
                }
            } // zero page
            
            else if (iPage == 4) // call stack
            {
                for (byte col = 0; col < 8; col++)
                {
                    uint address = col*2 + (row << 4);
                    uint stackData = HopperVM.GetCS(address);
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                        if (col == 4)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(byte(stackData & 0xFF));
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                    }
                    Out2Hex(byte(stackData >> 8));
                    
                }
            } // call stack
            
            else if (iPage == 5) // type stack
            {
                for (byte col = 0; col < 16; col++)
                {
                    uint address = col + (row << 4); // 0..255
                    address = address * 2; // 0..510
                    
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                        if (col == 8)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(byte(htype));
                }
            } // type stack
            
            else if (iPage == 6) // value stack
            {
                for (byte col = 0; col < 8; col++)
                {
                    uint address = col*2 + (row << 4);
                    
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                        if (col == 4)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(byte(stackData & 0xFF));
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                    }
                    Out2Hex(byte(stackData >> 8));
                }
            } // value stack
            
            else if (iPage == 7) // value stack
            {
                for (byte col = 0; col < 8; col++)
                {
                    uint address = col*2 + (row << 4);
                    address = address + 256;
                    
                    Type htype;
                    uint stackData = HopperVM.Get(address, ref htype);
                    
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                        if (col == 4)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(byte(stackData & 0xFF));
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                    }
                    Out2Hex(byte(stackData >> 8));   
                }
            } // value stack
            
            else if (iPage > 7) // heap
            {
                for (byte col = 0; col < 16; col++)
                {
                    uint address = col + (row << 4); // 0..127
                    address = address + (256 * iPage);
                    
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                        if (col == 8)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(Memory.ReadByte(address));
                }
            } // heap
            
            else // anything else (like 01 and 02 for example)
            {
                for (byte col = 0; col < 16; col++)
                {
                    if (includeAddresses)
                    {
                        Serial.WriteChar(' ');
                        if (col == 8)
                        {
                            Serial.WriteChar(' ');
                        }
                    }
                    Out2Hex(0x00);
                }
            } // heap
        } // for (row = 0; r < 16; row++)
        
        Serial.WriteChar(char(enter)); // next line
    }
    
    MCU()
    {
        loaded = false;
        HopperVM.Restart();
        bool refresh = true;
        
        // load 'auto.hexe' if it exists
        uint loadedAddress;
        uint codeLength;
        if (External.LoadAuto && Runtime.LoadAuto(ref loadedAddress, ref codeLength))
        {
            HopperVM.Initialize(loadedAddress, codeLength);
            HopperVM.Restart();
            loaded = true;
            if (HopperVM.InlinedExecuteWarp(false))
            {
                HopperVM.Restart();
            }
        }

        Serial.WriteChar(char(slash)); // ready
        loop
        {
            char ch;
            if (Serial.IsAvailable)
            {
                ch = Serial.ReadChar();
            }
            if (ch == char(escape)) // <esc> from Debugger
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
                    case 'M': // memory page dump
                    {
                        byte msn = FromHex(Serial.ReadChar());
                        byte lsn = FromHex(Serial.ReadChar());
                        WaitForEnter();
                        
                        byte iPage = (msn << 4) + lsn;
                        DumpPage(iPage, true);
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
                        
                        Serial.WriteChar(char(enter));
                        Out4Hex(HopperVM.PC);
                        Serial.WriteChar(char(slash)); // confirm data
                    }
                    case 'R': // get Registers
                    {
                        WaitForEnter();
                        
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar('P');
                        Serial.WriteChar('C');
                        Serial.WriteChar('=');
                        Out4Hex(HopperVM.PC);
                        Serial.WriteChar(' ');
                        
                        Serial.WriteChar('C');
                        Serial.WriteChar('S');
                        Serial.WriteChar('P');
                        Serial.WriteChar('=');
                        Out2Hex(byte(HopperVM.CSP));
                        Serial.WriteChar(' ');
                        
                        Serial.WriteChar('S');
                        Serial.WriteChar('P');
                        Serial.WriteChar('=');
                        Out4Hex(HopperVM.SP + 0x0600);
                        Serial.WriteChar(' ');
                        
                        Serial.WriteChar('T');
                        Serial.WriteChar('S');
                        Serial.WriteChar('P');
                        Serial.WriteChar('=');
                        Out4Hex(HopperVM.SP + 0x0500);
                        Serial.WriteChar(' ');
                        
                        Serial.WriteChar('B');
                        Serial.WriteChar('P');
                        Serial.WriteChar('=');
                        Out4Hex(HopperVM.BP + 0x0600);
                        
                        Serial.WriteChar(char(slash)); // confirm data
                    }
                    case 'T':
                    {
                        WaitForEnter();
                        
                        // read name characters till 0x0D
                        uint destinationName = HRString.New();
                        loop
                        {
                            char ch = Serial.ReadChar();
                            if (ch == char(enter))
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationName, ch);
                        }
                        Serial.WriteChar(char(enter));
                        Serial.WriteChar(char(slash));
                        
                        // read path characters till 0x0D
                        uint destinationFolder = HRString.New();
                        loop
                        {
                            char ch = Serial.ReadChar();
                            if (ch == char(enter))
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationFolder, ch);
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
                        
                        loadedAddress = 0;
                        uint codeLength;
                    
                        loaded = SerialLoadIHex(ref loadedAddress, ref codeLength);
                        Serial.WriteChar(char(enter));
                        if (loaded)
                        {
                            HopperVM.Initialize(loadedAddress, codeLength);
                            HopperVM.Restart();
                            
                            // codeLength
                            Out4Hex(codeLength);
                            Serial.WriteChar(' ');
                            
                            // heapStart
                            Out4Hex(Memory.HeapStart);
                            Serial.WriteChar(' ');
                            
                            // heapSize
                            Out4Hex(Memory.HeapSize);
                            Serial.WriteChar(' ');
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
                        
                        if (loaded)
                        {
                            FlashProgram(loadedAddress, codeLength);
                        }
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
                                    OpCode opCode = OpCode(ReadCodeByte(pc));
                                    
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
                                case 'V':
                                {
                                    WaitForEnter();
                                    HopperVM.DumpStack(20);
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'H':
                                {
                                    WaitForEnter();
                                    HopperVM.DumpHeap(true, 0);
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                            } // switch
                        } // loaded
                    } // default
                } // switch
            } // <esc> from Debugger
        } // loop
        HopperVM.Release();
    }
    {
#ifdef SERIAL_CONSOLE
        MCU();
#else        
        Windows();
#endif
    }
    
}
