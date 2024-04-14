program R6502
{
    //#define CHECKED
    #define PACKED_INSTRUCTIONS
    #define FASTINTS
    
    //#define CPU_65C02S  // Rockwell and WDC
    #define CPU_6502  // MOS

        
#if defined(CPU_65C02S) && !defined(FASTINTS) && !defined(CHECKED)
    #define ROM_8K
#else 
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
    
    uses "6502/SysCalls"
    uses "6502/Instructions"
    
    IRQ()
    {
        Serial.ISR();
    }
    NMI()
    {
        INC ZP.SerialBreakFlag // hardware <ctrl><C>
        RTI
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
        
        // clear U0..U3 to make zero page transfer faster
        
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
        LDA #0
        Serial.HexOut();
        Serial.HexOut();
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
                            
                            Utilities.IncIDY();
                            
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
        Serial.EmptyTheBuffer(); // discard input after the error
        LDA # '!'                // failure terminator
    }
    
    loadCommand()
    {
        Utilities.WaitForEnter();     // consume <enter>
        
        // ignore CRC for now
        Serial.WaitForChar();
        Serial.WaitForChar();
        Serial.WaitForChar();
        Serial.WaitForChar();
        Serial.WaitForChar(); // <enter>
        
        LDA #0
        STA IDYH
        STA IDYL
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

            LDA ZP.FLAGS
            ORA # 0b00000001             // program is loaded
            STA ZP.FLAGS
      
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
            LDA ZP.FLAGS
            AND # 0b11111110
            STA ZP.FLAGS
            
            LDA #0
            STA ZP.PROGSIZE
        }
        PLA // restore terminator   
        Serial.WriteChar();         // success or failure ('*' or '!')?
        Utilities.SendSlash();      // confirm the data
    }
    
    warmRestart()
    {
        Memory.InitializeHeapSize(); // sets HEAPSTART and HEAPSIZE based on size of program loaded
        Stacks.Init();
        
#ifdef CPU_65C02S        
        STZ ZP.CNP
        
        // hopperInitPC -> 0x0000
        STZ ZP.PCL
        STZ ZP.PCH
#else
        LDA #0
        STA ZP.CNP
        
        // hopperInitPC -> 0x0000
        STA ZP.PCL
        STA ZP.PCH
#endif
    }
    
    
    hopperInit()
    {
        Memory.InitializeHeapSize(); // sets HEAPSTART and HEAPSIZE based on size of program loaded
        Stacks.Init();
        
        // CODESTART = EntryPoint + HopperData
        LDA #(HopperData & 0xFF)
        CLC
        ADC (HopperData+4)
        STA ZP.CODESTARTL
        LDA #(HopperData >> 8)
        ADC (HopperData+5)
        STA ZP.CODESTARTH
        
        LDA #0
        STA ZP.CNP
        STA ZP.PCL
        STA ZP.PCH
        
#ifdef CPU_65C02S
        STZ ZP.FLAGS
  #ifdef CHECKED
        SMB2 ZP.FLAGS // this is a checked build
  #endif        
        SMB3 ZP.FLAGS // 8 bit SP and BP
#else
  #ifdef CHECKED
        ORA # 0b00000100  // this is a checked build
  #endif
        ORA # 0b00001000  // 8 bit SP and BP
        STA ZP.FLAGS
#endif                                
    }
   
    resetVector()
    {
        // zeroes mean faster debug protocol
        
        // clear the Zero Page
        LDX #0
        loop
        {
            CPX # ACIADATA // don't write to ACIA data register (on Zero Page right now)
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
        LDA #0
        STA IDXL
        LDA # (SerialInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // clear the serial buffer
        
        Serial.Initialize();
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
                LDA #0
                STA ZP.SerialBreakFlag
                break; 
            }
            LDA # (InvalidAddress & 0xFF) // assume that MSB and LSB of InvalidAddress are the same
            CMP PCH
            if (Z)
            {
                CMP PCL
                if (Z)
                {
                    break; // end of program run
                }
            }
            stepintoCommand();
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
            
            LDA # (InvalidAddress & 0xFF) // assume that MSB and LSB of InvalidAddress are the same
            CMP PCH
            if (Z)
            {
                CMP PCL
                if (Z)
                {
                    break; // end of program run
                }
            }
            
            // munts A and X, 
            //   if hit, Z set and breakpoint in X
            Breakpoints.IsPCBreakpoint(); 
            if (Z)
            {
                // if breakpoint '0', clear it
                CPX # 0
                if (Z)
                {
                    Breakpoints.ClearX();
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
        LDA # (InvalidAddress & 0xFF) // assume that MSB and LSB of InvalidAddress are the same
        CMP PCH
        if (Z)
        {
            CMP PCL
            if (Z)
            {
                return; // end of program run
            }
        }
        
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
        LDA # (InvalidAddress & 0xFF) // assume that MSB and LSB of InvalidAddress are the same
        CMP PCH
        if (Z)
        {
            CMP PCL
            if (Z)
            {
                warmRestart();
            }
        }
    }
    
    Hopper()
    {
        resetVector();
        
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
                    }
                    case 'X':
                    {
                        // <ctrl><F5>
                        Utilities.WaitForEnter();    // consume <enter>
                        runCommand();
                        checkRestart();
                        Utilities.SendSlash();   // confirm handing back control
                    }
                    case 'D':
                    {
                        // <F5>
                        Utilities.WaitForEnter();    // consume <enter>
                        debugCommand();
                        checkRestart();
                        Utilities.SendSlash();   // confirm handing back control
                    }
                    case 'O':
                    {
                        // <F10>
                        Utilities.WaitForEnter();    // consume <enter>
                        stepoverCommand();
                        checkRestart();
                        Utilities.SendSlash();   // confirm handing back control
                    }
                    case 'I':
                    {
                        // <F11>
                        Utilities.WaitForEnter();    // consume <enter>
                        stepintoCommand();
                        checkRestart();
                        Utilities.SendSlash();   // confirm handing back control
                    }
                }
            }
        } // loop
    }
} 
