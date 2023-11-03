program Runtime
{
#define PORTABLE      // use .hs runtime methods where they exist (rather than 'system')
//#define CHECKED     // mainly stack checks, range checks and division by zero

#define RUNTIME       // workaround special clipboard buffer for testing on Windows (that works without String)
#define SERIALCONSOLE // Source/System/IO uses serial only (for MCU's etc)

    uses "/Source/Runtime/Emulation/Minimal" // minimal use of actual 'system' APIs
    uses "/Source/Runtime/Emulation/Memory"
    
    uses "/Source/Runtime/Platform/OpCodes"
    uses "/Source/Runtime/Platform/SysCalls"
    uses "/Source/Runtime/Platform/Types"
    
    uses "/Source/Runtime/Platform/GC"
    uses "/Source/Runtime/Platform/Array"
    uses "/Source/Runtime/Platform/Dictionary"
    uses "/Source/Runtime/Platform/Float"
    uses "/Source/Runtime/Platform/Int"
    uses "/Source/Runtime/Platform/List"
    uses "/Source/Runtime/Platform/Long"
    uses "/Source/Runtime/Platform/Pair"
    uses "/Source/Runtime/Platform/String"
    uses "/Source/Runtime/Platform/UInt"
    uses "/Source/Runtime/Platform/Variant"
    
    uses "/Source/Runtime/Platform/External"
    
    uses "/Source/Runtime/HopperVM"
    
    // Zero Page FLAGS:
    flags HopperFlags
    {
        TraceOn        = 0x01,
        WarpSpeed      = 0x02,
        CheckedBuild   = 0x04,
        Stack8Bit      = 0x08,
        ProfileBuild   = 0x10,
        BreakpointsSet = 0x20,
        MPUPlatform    = 0x40,
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
    const uint codeStart = 0x0000; // memory magically exists from 0x0000 to 0xFFFF
    
    bool LoadIHex(ref uint loadedAddress, ref uint codeLength)
    {
        bool success = true;
        loadedAddress = codeStart;
        
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
                        Memory.WriteByte(codeStart + recordAddress, dataByte);
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
        IO.Write('F');IO.Write('U');IO.Write('C');IO.Write('K');IO.Write('!');
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
                        HopperVM.Initialize(loadedAddress, codeLength);
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
                                HopperVM.Execute();
                                if (Error == 0)
                                {
                                    HopperVM.DumpHeap(false, 0x22 + 0x0A + 0x0A); // breakpoints and Array bit tables
                                }
                                refresh = true;
                            }
                            case 'I':
                            {
                                HopperVM.ExecuteStepTo();
                                refresh = true;
                            }
                            case 'O':
                            {
                                uint pc = HopperVM.PC;
                                OpCode opCode = OpCode(ReadByte(pc));
                                
                                if (opCode == OpCode.CALLW)
                                {
                                    
                                    // set breakpoint[0] to PC+3
                                    HopperVM.SetBreakpoint(0, pc+3);
                                    HopperVM.Execute();
                                }
                                else if (opCode == OpCode.CALLB)
                                {
                                    // set breakpoint[0] to PC+2
                                    HopperVM.SetBreakpoint(0, pc+2);
                                    HopperVM.Execute();
                                }
                                else if (opCode == OpCode.CALLREL)
                                {
                                    // set breakpoint[0] to PC+1
                                    HopperVM.SetBreakpoint(0, pc+1);
                                    HopperVM.Execute();
                                }
                                else
                                {
                                    // use single step (set bit in HopperFlags)
                                    HopperVM.ExecuteStepTo();
                                }
                                refresh = true;
                            }
                            case 'V':
                            {
                                HopperVM.DumpStack();
                                refresh = true;
                            }
                            case 'H':
                            {
                                HopperVM.DumpHeap(true, 0);
                                refresh = true;
                            }
                            case 'X':
                            {
                                HopperVM.ExecuteWarp();
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
        loadedAddress = codeStart;
        
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
                        if (!TryReadSerialByte(ref dataByte)) { success = false; break; }
                        Memory.WriteByte(codeStart + recordAddress, dataByte);
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
                            data = byte(HopperFlags.MPUPlatform); // 0x08 would imply 8 bit SP
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
        Serial.WriteChar(char(slash)); // ready
        loop
        {
            char ch = Serial.ReadChar();
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
                    case 'L':
                    {
                        WaitForEnter();
                        
                        uint loadedAddress;
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
                                    
                                    uint pc = HopperVM.PC;
                                    OpCode opCode = OpCode(ReadByte(pc));
                                    
                                    if (opCode == OpCode.CALLW)
                                    {
                                        
                                        // set breakpoint[0] to PC+3
                                        HopperVM.SetBreakpoint(0, pc+3);
                                        HopperVM.Execute();
                                    }
                                    else if (opCode == OpCode.CALLB)
                                    {
                                        // set breakpoint[0] to PC+2
                                        HopperVM.SetBreakpoint(0, pc+2);
                                        HopperVM.Execute();
                                    }
                                    else
                                    {
                                        // use single step (set bit in HopperFlags)
                                        HopperVM.ExecuteStepTo();
                                    }
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'I': // Step Into | <F11>
                                {
                                    WaitForEnter();
                                    HopperVM.ExecuteStepTo();
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'D': // Debug
                                {
                                    WaitForEnter();
                                    HopperVM.Execute();
                                    Serial.WriteChar(char(slash)); // confirm handing back control
                                }
                                case 'X': // Execute
                                {
                                    WaitForEnter();
                                    HopperVM.ExecuteWarp();
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
            }
        } // loop
        HopperVM.Release();
    }
    {
#ifdef SERIALCONSOLE
        MCU();
#else        
        Windows();
#endif
    }
    
}
