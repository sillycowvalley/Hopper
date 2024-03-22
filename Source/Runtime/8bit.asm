program Runtime
{
 
#define CHECKED

    // mapping of Z80 -> 6502
    // https://litwr2.github.io/8080-8085-z80-8088-6502/z80-6502.html
    
    #define ROM_8K
    
    #define CPU_65C02  // Rockwell and WDC

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

#ifdef CPU_65C02    
    uses "6502/MemoryMap"
    uses "6502/ZeroPage"
    uses "6502/Breakpoints"
    uses "6502/Stacks"
    uses "6502/Memory"
    
#endif
    uses "Common/Utilities"
    uses "Common/Serial"
    uses "Common/Diagnostic"
    uses "Common/Instructions"
    
#ifdef CPU_65C02
    IRQ()
    
    {
        BBR7 ZP.ACIASTATUS, +6 // interrupt request by 6850
        BBR0 ZP.ACIASTATUS, +3 // RDRF : receive data register full
        
        Serial.ISR();
    }
    NMI()
    {
        INC ZP.SerialBreakFlag // hardware <ctrl><C>
        RTI
    }
#endif
     
    breakpointCommand()
    {
        Serial.WaitForChar();
        CMP #'X'
        if (Z)
        {
            Breakpoints.Clear();
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
            
            SMB5 ZP.FLAGS
        }
        Utilities.WaitForEnter();
    }
    
    memoryCommand()
    {
        Serial.HexIn();             
        // A now contains the page number for the memory dump
        PHA
        
        Utilities.WaitForEnter();
        
        PLA
        Diagnostic.PageMemory();
        
        Utilities.SendSlash();  // confirm the data
    }
    crcCommand()
    {
        Utilities.WaitForEnter();  // consume <enter>
        LDA #0
        Serial.WriteChar();
        Serial.WriteChar();
        Serial.WriteChar();
        Serial.WriteChar();
        Utilities.SendSlash();     // confirm the data
    }
    pcCommand()
    {
        Utilities.WaitForEnter();  // consume <enter>
        
        LDA # 0x0D
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
                            CPX # 0
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
                        Serial.WaitForChar();
                        Serial.WaitForChar();
                        Serial.WaitForChar();
                        
                        LDA # 0x0D
                        Serial.WriteChar();
                        LDA ZP.IDYH
                        Serial.HexOut();
                        LDA ZP.IDYL
                        Serial.HexOut();
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
        
        STZ IDYH
        STZ IDYL
        loop
        {
            loadRecord();
            CMP # '*' // success terminator
            if (Z) { break; }
            CMP # '!' // failure terminator
            if (Z) { break; }
        }
        CMP # '*' // success terminator
        if (Z) 
        {
            hopperInit();                // good defaults for HopperVM
            
            SMB0 ZP.FLAGS                // program is loaded
            
            LDA # 0x0D
            Serial.WriteChar();
            LDA # '*'               // restore success terminator
        }
        Serial.WriteChar();         // success or failure ('*' or '!')?
        Utilities.SendSlash();      // confirm the data
    }
    
    warmRestart()
    {
        Memory.InitializeHeapSize(); // sets HEAPSTART and HEAPSIZE based on size of program loaded
        Stacks.Init();
        STZ ZP.CNP
        
        // hopperInitPC -> 0x0000
        STZ ZP.PCL
        STZ ZP.PCH
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
        
        STZ ZP.CNP
        STZ ZP.FLAGS
#ifdef CHECKED
        SMB2 ZP.FLAGS // this is a checked build
#endif        
        SMB3 ZP.FLAGS // 8 bit SP and BP
        
        // hopperInitPC -> 0x0000
        STZ ZP.PCL
        STZ ZP.PCH
    }
   
    resetVector()
    {
        STZ IDXL
        LDA # 0
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // clear the Zero Page
        
        STZ IDXL
        LDA # (SerialInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // clear the serial buffer
        
        // arbitrary default heap until we load a program
        LDA #0x60
        STA ZP.HEAPSTART
        LDA #0x20
        STA ZP.HEAPSIZE
        
        Serial.Initialize();
        hopperInit();        
    }
    
    runCommand()
    {
        // run ignoring breakpoints
        loop
        {
            LDA ZP.SerialBreakFlag
            if (NZ) 
            { 
                STZ ZP.SerialBreakFlag
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
        
    }
    debugCommand()
    {
        // run until breakpoint
        
        // for first instruction, don't check for breakpoint (in case we are already stopped on one)
        stepintoCommand();
        
        loop
        {
            LDA ZP.SerialBreakFlag
            if (NZ) 
            { 
                STZ ZP.SerialBreakFlag
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
        
        // is the next instruction a CALL/JSR?
        Instruction.IsNextCALL();
        if (Z)
        {
            // set breakpoint '0' on next instruction
            
            // ACC = PC + CODESTART + instruction length
            GetNextAddress();
            
            // machine address - CODESTART = Hopper address
            SEC
            LDA ZP.ACCL
            SBC ZP.CODESTARTL
            STA ZP.BRKL
            LDA ZP.ACCH
            SBC ZP.CODESTARTH
            STA ZP.BRKH
            
            SMB5 ZP.FLAGS
            
            // run to breakpoint
            debugCommand();
        }
        stepintoCommand();
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
                STZ ZP.SerialBreakFlag
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
            //CMP #0x03 // <ctrl><C> from Debugger but we were not running 
            //if (Z)
            //{
            //    Utilities.SendSlash();
            //    continue;
            //}
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
                
                BBS0 ZP.FLAGS, +3 
                continue; // no program is loaded
                
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
