program R6502
{
    //#define EXPERIMENTAL
    //#define CHECKED
    
    // This cannot be in '/Bin/Options/Configuration.options':
    #define CPU_65C02S  // Rockwell and WDC
    //#define CPU_6502  // MOS
    
    // For all other configuration options, use '/Bin/Options/Configuration.options' now

#if defined(CPU_65C02S) && !defined(CHECKED) && !defined(FASTINTS) && !defined(INLINE_EXPANSIONS)
    #define ROM_8K // 240 bytes overrun with I2C but without FASTINTS
#endif
    
#if defined(CPU_6502)   && !defined(JIX_INSTRUCTIONS) && !defined(CHECKED) && !defined(FASTINTS) && !defined(INLINE_EXPANSIONS)
    #define ROM_8K // 31 bytes remaining without JIX_INSTRUCTIONS or FASTINTS
#endif

#if defined(FAST_6502_RUNTIME)
    #define ROM_32K
#endif
#if !defined(ROM_8K) && !defined(ROM_16K) && !defined(ROM_32K)
    #define ROM_16K
#endif
    
    // HopperMon commands to support:
    //
    // General:
    //   Fxx    - memory dump
    //   L      - load ihex to program memory
    //
    // VM:
    //   BX     - clear breakpoints
    //   Byxxxx - set breakpoint
    //   P      - fetch PC
    //
    //   X      - execute (run ignoring breakpoints)
    //   D      - debug (run respecting breakpoints)
    //   I      - step into
    //   O      - step over
    //   W      - warm restart

    uses "6502/MemoryMap"
    uses "6502/ZeroPage"
    uses "6502/Breakpoints"
    uses "6502/Stacks"
    uses "6502/Memory"
    uses "6502/GC"
    
    uses "6502/Utilities"
    uses "6502/Serial"
    uses "6502/Diagnostics"
    
    uses "6502/LibCalls"
    uses "6502/SysCalls"
    uses "6502/Instructions"
    
    uses "6502/Devices/SerialEEPROM"
    
    IRQ()
    {
        Serial.ISR();
#ifdef W65C22_VIA
        W65C22.ISR();
#endif  
    }
    NMI()
    {
        INC ZP.SerialBreakFlag // hardware <ctrl><C>
    }
    
    breakpointCommand()
    {
        Serial.WaitForChar();
        CMP #'X'
        if (Z)
        {
            Breakpoints.Clear(); // munts X
        }
        else
        {
            // set a breakpoint
            
            Utilities.MakeNibble();
            TAX
            
            Serial.HexIn();
            STA ZP.BRKH, X
            
            Serial.HexIn();
            STA ZP.BRKL, X
            
#ifdef CPU_65C02S            
            SMB5 ZP.FLAGS
#else
            LDA ZP.FLAGS
            ORA # 0b00100000
            STA ZP.FLAGS
#endif
        }
        Utilities.WaitForEnter();
    }
    
    memoryCommand()
    {
        Serial.HexIn();             
        // A now contains the page number for the memory dump
        PHA

        Utilities.WaitForEnter();
        
        // clear U0..U3 to make zero page transfer faster (optional)
#ifdef CPU_65C02S 
        STZ ZP.U0    
        STZ ZP.U1    
        STZ ZP.U2    
        STZ ZP.U3  
#else        
        LDA # 0  
        STA ZP.U0    
        STA ZP.U1    
        STA ZP.U2    
        STA ZP.U3  
#endif        
        PLA
        Diagnostics.PageMemory();
        
        Utilities.SendSlash();  // confirm the data
    }
    crcCommand()
    {
        Utilities.WaitForEnter();  // consume <enter>
        LDA # Enter
        Serial.WriteChar();
        
        loop
        {
#ifdef I2C            
            LDA ZP.PLUGNPLAY
            AND # 0b00000010
            if (NZ) // EEPROM?
            {
                // BeginTx
                LDA # (I2C.SerialEEPROMAddress << 1)
                STA ZP.OutB
                I2C.Start();
                LDA #0
                STA ZP.OutB
                I2C.ByteOut(); // EEPROM address MSB (0)
                LDA #0
                STA ZP.OutB
                I2C.ByteOut(); // EEPROM address LSB (0)
                // EndTx
                I2C.Stop();
                
                // read first 3 bytes from EEPROM:
                LDA # 3
                STA ZP.TOPL
                LDA # I2C.SerialEEPROMAddress
                STA NEXTL
                RequestFromTOPNEXT(); // NEXTL has I2C adddress, TOPL has number of bytes to return, TOPL returns number of bytes read
                // assume success
                LDX ZP.I2CInReadPtr
                LDA Address.I2CInBuffer, X
                if (NZ)
                {
                    // program is loaded in EEPROM
                    INX
                    INX
                    LDA Address.I2CInBuffer, X
                    Serial.HexOut();
                    DEX
                    LDA Address.I2CInBuffer, X
                    Serial.HexOut();
                    break;
                }
            }
#endif
            // return zero CRC
            LDA #0
            Serial.HexOut();
            Serial.HexOut();
            break;
        }
        Utilities.SendSlash();     // confirm the data
    }
    pcCommand()
    {
        Utilities.WaitForEnter();  // consume <enter>
        
        LDA # Enter
        Serial.WriteChar();
        LDA PCH
        Serial.HexOut();
        LDA PCL
        Serial.HexOut();
        
        Utilities.SendSlash();      // confirm the data
    }
    loadRecord()
    {
        Serial.WaitForChar();
        CMP # ':'
        if (Z)
        {
            loop
            {
                Serial.HexIn();      
                STA ZP.ACCH 
                // ACCH contains record length
          
                Serial.HexIn();
                STA ZP.IDXH
                Serial.HexIn();      
                STA ZP.IDXL 
                
                CLC
                LDA ZP.IDXL
                ADC #(HopperData & 0xFF)
                STA ZP.IDXL
                LDA ZP.IDXH
                ADC #(HopperData >> 8)
                STA ZP.IDXH
                // IDX contains the destination address
           
                Serial.HexIn(); // A contains record type   
                switch (A)
                {
                    case 0x00: // data record
                    {
                        // load the data
                        LDY # 0
                        LDX ZP.ACCH // record length
                        loop
                        {
                            //CPX # 0 - redundant CPX
                            if (Z) { break; }
                            
                            Serial.HexIn(); // A contains data byte
                            STA [ZP.IDX], Y
                            
                            INC ZP.IDYL
                            if (Z)
                            {
                                INC ZP.IDYH
                            }
                            
                            INY
                            DEX
                        }
                        
                        // ignore data checksum and EOL for now
                        Serial.WaitForChar();
                        Serial.WaitForChar();
                        Serial.WaitForChar();
                        
                        LDA # 0x00 // not '!' or '*'
                    }
                    case 0x01: // EOF record
                    {
                        // ignore EOF checksum and EOL
                        Serial.WaitForChar(); // 'F'
                        Serial.WaitForChar(); // 'F'
                        
                        LDA # '*' // EOF success terminator
                    }
                    default: // what is this record type? fail ->
                    {
                        break;                   
                    }
                } // switch
                RTS
            } // loop
        }
        Serial.EmptyTheBuffer(); // discard input after the error, munts A
        LDA # '!'                // failure terminator
    }
    
    loadCommand()
    {
        Utilities.WaitForEnter();     // consume <enter>
        
#ifdef I2C        
        LDA ZP.PLUGNPLAY
        AND # 0b00000010
        if (NZ) // EEPROM?
        {
            // BeginTx
            LDA # (I2C.SerialEEPROMAddress << 1)
            STA ZP.OutB
            I2C.Start();
            LDA # 0
            STA ZP.OutB
            I2C.ByteOut(); // EEPROM address MSB (0)
            LDA # 0
            STA ZP.OutB
            I2C.ByteOut(); // EEPROM address LSB (0)
            
            // byte upfront reserved for 'size in pages' (zero means failure)
            LDA # 0
            STA ZP.OutB
            I2C.ByteOut();
        }
#endif
        
        // CRC: 4 hex characters and <enter>
        Serial.HexIn();
#ifdef I2C
        STA ZP.OutB
        LDA ZP.PLUGNPLAY
        AND # 0b00000010
        if (NZ) // EEPROM?
        {
            I2C.ByteOut(); // CRC byte
        }
#endif
        Serial.HexIn();
#ifdef I2C
        STA ZP.OutB
        LDA ZP.PLUGNPLAY
        AND # 0b00000010
        if (NZ) // EEPROM?
        {
            I2C.ByteOut(); // CRC byte
            I2C.Stop();         // header loaded
            
            // delay 5ms after Stop() for EEPROM
            LDA # 5
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.DelayTOP();
        }
#endif
        Serial.WaitForChar(); // <enter>
        
        // Y is zero now
        STY IDYH
        STY IDYL
        loop
        {
            loadRecord();
            CMP # '*' // success terminator
            if (Z) { break; }
            CMP # '!' // failure terminator
            if (Z) { break; }
        }
        PHA
        CMP # '*' // success terminator
        if (Z) 
        {
            Serial.WaitForChar(); // Enter
            Serial.WaitForChar(); // Enter
            
            Serial.WaitForChar(); // arbitrary '*' terminator from client
                        
            // PROGSIZE: program size in pages (rounded up to the nearest page)
            LDA ZP.IDYH
            STA ZP.PROGSIZE
            LDA ZP.IDYL
            if (NZ)
            {
                INC ZP.PROGSIZE
            }
            
            hopperInit();                // good defaults for HopperVM

#ifdef CPU_65C02S
            SMB0 ZP.FLAGS                // program is loaded
#else
            LDA ZP.FLAGS
            ORA # 0b00000001             // program is loaded
            STA ZP.FLAGS
#endif
      
            LDA # Enter
            Serial.WriteChar();
            
            LDA ZP.IDYH
            Serial.HexOut();
            LDA ZP.IDYL
            Serial.HexOut();
            LDA #' '
            Serial.WriteChar();
            
            LDA ZP.HEAPSTART
            Serial.HexOut();
            LDA #0
            Serial.HexOut();
            LDA #' '
            Serial.WriteChar();
            
            LDA ZP.HEAPSIZE
            Serial.HexOut();
            LDA #0
            Serial.HexOut();
            LDA #' '
            Serial.WriteChar();
                 
            LDA # Enter
            Serial.WriteChar();
        }
        else
        {
#ifdef CPU_65C02S
            RMB0 ZP.FLAGS                // failed to load program
#else            
            LDA ZP.FLAGS
            AND # 0b11111110
            STA ZP.FLAGS
#endif            
            LDA #0
            STA ZP.PROGSIZE
        }
        
        PLA // restore terminator   
        Serial.WriteChar();         // success or failure ('*' or '!')?
        Utilities.SendSlash();      // confirm the data
        
#ifdef I2C
        LDA ZP.PLUGNPLAY
        AND # 0b00000010
        if (NZ) // EEPROM?
        {
            // delay 10ms after Stop() for EEPROM
            LDA # 10
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.DelayTOP();
            
            // BeginTx
            LDA # (I2C.SerialEEPROMAddress << 1)
            STA ZP.OutB
            I2C.Start();
            LDA # 0
            STA ZP.OutB
            I2C.ByteOut(); // EEPROM address MSB (0)
            LDA # 0
            STA ZP.OutB
            I2C.ByteOut(); // EEPROM address LSB (0)
            
            LDA ZP.PROGSIZE  // zero size means failed to load
            STA ZP.OutB
            I2C.ByteOut();
            I2C.Stop();
            
            // delay 10ms after Stop() for EEPROM
            LDA # 10
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.DelayTOP();
            
            LDA ZP.PROGSIZE  // zero size means failed to load
            if (NZ)
            {
                // program was successfully loaded
                SaveToEEPROM();
            }
        }
#endif
    }
       
    warmRestart()
    {
        Memory.InitializeHeapSize(); // sets HEAPSTART and HEAPSIZE based on size of program loaded
        Stacks.Initialize();
        
#ifdef CPU_65C02S
        STZ ZP.CNP
        
        // hopperInitPC -> 0x0000
        STZ ZP.PCL
        STZ ZP.PCH
        
        RMB6 ZP.FLAGS // reset ProgramExited
#else
        LDA #0
        STA ZP.CNP
        
        // hopperInitPC -> 0x0000
        STA ZP.PCL
        STA ZP.PCH
        
        LDA ZP.FLAGS
        AND # 0b10111111
        STA ZP.FLAGS
#endif
    }
    
    
    hopperInit()
    {
        Memory.InitializeHeapSize(); // sets HEAPSTART and HEAPSIZE based on size of program loaded
        Stacks.Initialize();
        
        // CODESTART = EntryPoint + HopperData
        LDA #(HopperData & 0xFF)
        CLC
        ADC (HopperData+4)
        STA ZP.CODESTARTL
        LDA #(HopperData >> 8)
        ADC (HopperData+5)
        STA ZP.CODESTARTH
        
#ifdef CPU_65C02S 
        STZ ZP.CNP
        STZ ZP.PCL
        STZ ZP.PCH
        STZ ZP.FLAGS  // resets ProgramExited
  #ifdef CHECKED
        SMB2 ZP.FLAGS // this is a checked build
  #endif        
        SMB3 ZP.FLAGS // 8 bit SP and BP        
#else        
        LDA #0
        STA ZP.CNP
        STA ZP.PCL
        STA ZP.PCH
  #ifdef CHECKED
        ORA # 0b00000100  // this is a checked build
  #endif
        ORA # 0b00001000  // 8 bit SP and BP
        STA ZP.FLAGS      // resets ProgramExited
#endif                                
    }
   
    resetVector()
    {
        // zeroes mean faster debug protocol
        
        // clear the Zero Page
#ifndef ZEROPAGE_IO  
        LDA #0
        LDX #0
        loop
        {
            STA 0x00, X
            DEX
            if (Z) { break; }
        }
#else
        LDX #0
        loop
        {
            CPX # ZP.ACIADATA // don't write to ACIA data register (on Zero Page right now)
            if (NZ) 
            {
#ifdef CPU_65C02S
                STZ 0x00, X
#else
                LDA #0
                STA 0x00, X
#endif
            }
            DEX
            if (Z) { break; }
        }  
#endif        
        LDA #0
        STA IDXL
        LDA # (SerialInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // clear the serial buffer
        
        Serial.Initialize(); // munts A
        
#ifdef W65C22_VIA
        W65C22.Initialize();
#endif             

        LDA #0
        STA IDXL
        LDA # (I2CInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // clear the I2C buffer
        
        // scan for I2C devices
#ifdef CPU_65C02S        
        STZ ZP.PLUGNPLAY
    #ifdef I2C        
        LDA # I2C.SSD1306Address // SSD1306 OLED
        I2C.Scan();
        if (Z)
        {
            SMB0 ZP.PLUGNPLAY
        }
        
        LDA # I2C.SerialEEPROMAddress // EEPROM?
        I2C.Scan();
        if (Z)
        {
            SMB1 ZP.PLUGNPLAY
        }
    #endif        
#else
        LDA # 0
        STA ZP.PLUGNPLAY
    #ifdef I2C        
        LDA # I2C.SSD1306Address // SSD1306 OLED
        I2C.Scan();
        if (Z)
        {
            LDA ZP.PLUGNPLAY
            ORA # 0b00000001
            STA ZP.PLUGNPLAY
        }
        
        LDA # I2C.SerialEEPROMAddress // EEPROM?
        I2C.Scan();
        if (Z)
        {
            LDA ZP.PLUGNPLAY
            ORA # 0b00000010
            STA ZP.PLUGNPLAY
        }
    #endif
#endif        
           
        hopperInit();
    }
    
    runCommand()
    {
        // Bit 1 set - run ignoring breakpoints
        LDA ZP.FLAGS
        ORA # 0b00000010
        STA ZP.FLAGS

        loop
        {
            LDA ZP.SerialBreakFlag
            if (NZ) 
            { 
#ifdef CPU_65C02S
                STZ ZP.SerialBreakFlag
#else                
                LDA #0
                STA ZP.SerialBreakFlag
#endif
                break; 
            }
#ifdef CPU_65C02S
            if (BBS6, ZP.FLAGS) // is ProgramExited set?
            {
                break;
            }
#else
            BIT ZP.FLAGS
            if (V) // is ProgramExited set?
            {
                break;
            }
#endif
            Instruction.Execute();
        }
        
        // Bit 1 clear - run until breakpoint
#ifdef CPU_65C02S            
        RMB1 ZP.FLAGS
#else
        LDA ZP.FLAGS
        AND # 0b11111101
        STA ZP.FLAGS
#endif                                           
    }
    debugCommand()
    {
        // Bit 1 clear - run until breakpoint
#ifdef CPU_65C02S            
        RMB1 ZP.FLAGS
#else
        LDA ZP.FLAGS
        AND # 0b11111101
        STA ZP.FLAGS
#endif                                           
        
        // for first instruction, don't check for breakpoint (in case we are already stopped on one)
        stepintoCommand();
        
        loop
        {
            /*
            LDA # ' '
            Serial.WriteChar();
            LDA ZP.PCH
            Serial.HexOut();
            LDA ZP.PCL
            Serial.HexOut();
            */
            LDA ZP.SerialBreakFlag
            if (NZ) 
            { 
#ifdef CPU_65C02S
                STZ ZP.SerialBreakFlag
#else                
                LDA #0
                STA ZP.SerialBreakFlag
#endif
                break; 
            }
#ifdef CPU_65C02S
            if (BBS6, ZP.FLAGS) // is ProgramExited set?
            {
                break;
            }
#else
            BIT ZP.FLAGS
            if (V) // is ProgramExited set?
            {
                break;
            }
#endif
            // munts A and X, 
            //   if hit, Z set and breakpoint in X
            Breakpoints.IsPCBreakpoint(); 
            if (Z)
            {
                // if breakpoint '0', clear it
                CPX # 0
                if (Z)
                {
                    Breakpoints.ClearX(); // munts A
                }
                break;
            }
            stepintoCommand();
#ifndef HAS_SERIAL_ISR            
            IsAvailable(); // used to poll serial port
#endif                
        }
    }
    stepoverCommand()
    {
#ifdef CPU_65C02S
        if (BBS6, ZP.FLAGS) // is ProgramExited set?
        {
            return;
        }
#else
        BIT ZP.FLAGS
        if (V) // is ProgramExited set?
        {
            return;
        }
#endif
        
        // is the instruction about to be executed a CALL/JSR?
        Instruction.IsCurrentCALL(); // munts X
        if (Z)
        {
            // set breakpoint '0' on next instruction
            
            // ACC = PC + CODESTART + instruction length
            Instruction.GetNextAddress(); // munts X
            
            // machine address - CODESTART = Hopper address
            SEC
            LDA ZP.ACCL
            SBC ZP.CODESTARTL
            STA ZP.BRKL
            LDA ZP.ACCH
            SBC ZP.CODESTARTH
            STA ZP.BRKH
            
#ifdef CPU_65C02S
            SMB5 ZP.FLAGS
#else
            LDA ZP.FLAGS
            ORA # 0b00100000
            STA ZP.FLAGS
#endif            
            
            // run to breakpoint
            debugCommand();
        }
        else
        {
            stepintoCommand();
        }
    }
    stepintoCommand()
    {
        Instruction.Execute();   
    }
    checkRestart()
    {
#ifdef CPU_65C02S
        if (BBS6, ZP.FLAGS) // is ProgramExited set?
        {
            warmRestart();
        }
#else
        BIT ZP.FLAGS
        if (V) // is ProgramExited set?
        {
            warmRestart();
        }
#endif
    }
    
    
    loadAuto()
    {
        // is the User button held low?
        
  #if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
       if (BBR1, ZP.PORTA)
       {
           return;
       }
  #else
       LDA ZP.PORTA
       AND # 0b00000010
       if (Z)
       {
           return;
       }
  #endif        

        // BeginTx
        LDA # (I2C.SerialEEPROMAddress << 1)
        STA ZP.OutB
        I2C.Start();
        LDA #0
        STA ZP.OutB
        I2C.ByteOut(); // EEPROM address MSB (0)
        LDA #0
        STA ZP.OutB
        I2C.ByteOut(); // EEPROM address LSB (0)
        // EndTx
        I2C.Stop();
        
        // read first 3 bytes from EEPROM:
        LDA # 3
        STA ZP.TOPL
        LDA # I2C.SerialEEPROMAddress
        STA NEXTL
        RequestFromTOPNEXT(); // NEXTL has I2C adddress, TOPL has number of bytes to return, TOPL returns number of bytes read
        // assume success
        LDX ZP.I2CInReadPtr
        LDA Address.I2CInBuffer, X
        if (NZ)
        {
            STA ZP.PROGSIZE // in 256 byte pages
            LoadFromEEPROM();
            hopperInit(); // initialized heap based on program loaded, initializes stacks, sets up the entrypoint ready to run
            
#ifdef CPU_65C02S
            SMB0 ZP.FLAGS                // program is loaded
#else
            LDA ZP.FLAGS
            ORA # 0b00000001             // program is loaded
            STA ZP.FLAGS
#endif            
            
            runCommand();
            checkRestart();
        }
    }
    
    Hopper()
    {
        resetVector();
        
#ifdef I2C
    #ifdef CPU_65C02S
        if (BBS1, ZP.PLUGNPLAY) // EEPROM?
        {
            loadAuto();
        }
    #else
        LDA ZP.PLUGNPLAY
        AND # 0b00000010
        if (NZ) // EEPROM?
        {
            loadAuto();
        }
    #endif        
#endif
        Utilities.SendSlash(); // ready
        
        loop
        {
            LDA ZP.SerialBreakFlag
            if (NZ) 
            { 
                // <ctrl><C> from Debugger but we were not running
#ifdef CPU_65C02S
                STZ ZP.SerialBreakFlag
#else
                LDA #0
                STA ZP.SerialBreakFlag
#endif
                Utilities.SendSlash();
                continue;
            }
            IsAvailable(); // munts A
            if (NZ)
            {
                Serial.WaitForChar();
            }
            else
            {
                LDA #0
            }
            CMP #0x03 // <ctrl><C> from Debugger but we were not running 
            if (Z)
            {
                Utilities.SendSlash();
                continue;
            }
            CMP #Escape
            if (Z)
            {
                Utilities.SendSlash(); // '\' response -> ready for command
                Serial.WaitForChar();
                
                switch (A)
                {
                    case 'F':
                    {
                        memoryCommand();
                        continue;
                    }
                    case 'K':
                    {
                        crcCommand();
                        continue;
                    }
                    case 'P':
                    {
                        pcCommand();
                        continue;
                    }
                    case 'B':
                    {
                        breakpointCommand();
                        continue;
                    }
                    case 'L':
                    {
                        loadCommand();
                        continue;
                    }
                }
#ifdef CPU_65C02S                
                if (BBR0, ZP.FLAGS)
                {
                    continue; // no program is loaded
                }
#else
                PHA
                LDA ZP.FLAGS
                AND #0b00000001
                if (Z)
                {
                    PLA
                    continue; // no program is loaded
                }
                PLA
#endif
                
                // Execution commands:
                switch (A)
                {
                    case 'W':
                    {
                        Utilities.WaitForEnter();    // consume <enter>
                        warmRestart();
                        continue;
                    }
                    case 'X':
                    {
                        // <ctrl><F5>
                        Utilities.WaitForEnter();    // consume <enter>
                        runCommand();
                    }
                    case 'D':
                    {
                        // <F5>
                        Utilities.WaitForEnter();    // consume <enter>
                        debugCommand();
                    }
                    case 'O':
                    {
                        // <F10>
                        Utilities.WaitForEnter();    // consume <enter>
                        stepoverCommand();
                    }
                    case 'I':
                    {
                        // <F11>
                        Utilities.WaitForEnter();    // consume <enter>
                        stepintoCommand();
                    }
                    default:
                    {
                        continue;
                    }
                }
                checkRestart();
                Utilities.SendSlash();   // confirm handing back control
            }
        } // loop
    }
} 
