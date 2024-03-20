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
    
#ifdef CPU_65C02
    IRQ()
    
    {
        BBR7 ZP.ACIASTATUS, +6 // interrupt request by 6850
        BBR0 ZP.ACIASTATUS, +3 // RDRF : receive data register full
        
        Serial.ISR();
    }
    NMI()
    {
        //INC ZP.SerialBreakFlag // hardware <ctrl><C>
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
            
            Serial.WaitForChar();
            Utilities.MakeNibble();
            ASL A ASL A ASL A ASL A
            AND #0xF0
            STA ZP.ACCL
            Serial.WaitForChar();
            Utilities.MakeNibble();
            ORA ZP.ACCL
            STA ZP.BRKH, X
            
            Serial.WaitForChar();
            Utilities.MakeNibble();
            ASL A ASL A ASL A ASL A
            AND #0xF0
            STA ZP.ACCL
            Serial.WaitForChar();
            Utilities.MakeNibble();
            ORA ZP.ACCL
            STA ZP.BRKL, X
            
            SMB5 ZP.FLAGS
        }
        Serial.WaitForChar();
        Utilities.SendSlash();
    }
    
    memoryCommand()
    {
        Serial.WaitForChar();   // consume hex character
        Utilities.MakeNibble();
        ASL A ASL A ASL A ASL A
        AND #0xF0
        STA ZP.ACCL
        
        Serial.WaitForChar();   // consume hex character
        Utilities.MakeNibble();
        ORA ZP.ACCL             // A now contains the page number for the memory dump
        PHA
        
        Utilities.WaitForEnter();
        
        PLA
        Diagnostic.PageMemory();
        
        Utilities.SendSlash();  // confirm the data
    }
    crcCommand()
    {
        Serial.WaitForChar();    // consume <enter>
        LDA #0
        Serial.WriteChar();
        Serial.WriteChar();
        Serial.WriteChar();
        Serial.WriteChar();
        Utilities.SendSlash();      // confirm the data
    }
    pcCommand()
    {
        Serial.WaitForChar();    // consume <enter>
        Utilities.SendSlash();   // confirm the command
        
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
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ASL A ASL A ASL A ASL A
                AND # 0xF0
                STA ZP.ACCL
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ORA ZP.ACCL      
                STA ZP.ACCL 
                // ACCL contains record length
          
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ASL A ASL A ASL A ASL A
                AND #0xF0
                STA ZP.IDXH
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ORA ZP.IDXH
                STA ZP.IDXH
          
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ASL A ASL A ASL A ASL A
                AND # 0xF0
                STA ZP.IDXL
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ORA ZP.IDXL      
                STA ZP.IDXL 
                
                CLC
                LDA ZP.IDXL
                ADC #(HopperData & 0xFF)
                STA ZP.IDXL
                LDA ZP.IDXH
                ADC #(HopperData >> 8)
                STA ZP.IDXH
                // IDX contains the destination address
           
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ASL A ASL A ASL A ASL A
                AND # 0xF0
                STA ZP.ACCH
                Serial.WaitForChar();
                Utilities.MakeNibble();
                ORA ZP.ACCH // A contains record type    
                
                CMP # 0x01 // EOF record
                if (NZ)
                {
                    CMP # 0x00 // data record
                    if (NZ) { break; } // what's this? fail ->
                    
                    // load the data
                    LDY # 0
                    LDX ZP.ACCL
                    loop
                    {
                        CPX # 0
                        if (Z) { break; }
                        
                        Serial.WaitForChar();
                        Utilities.MakeNibble();
                        ASL A ASL A ASL A ASL A
                        AND # 0xF0
                        STA ZP.ACCL
                        Serial.WaitForChar();
                        Utilities.MakeNibble();
                        ORA ZP.ACCL // A contains data byte
                        STA [ZP.IDX], Y
                        
                        Utilities.IncIDY();
                        
                        INY
                        DEX
                    }
                    // ignore data checksum and EOL for now : TODO
                    Serial.WaitForChar();
                    Serial.WaitForChar();
                    Serial.WaitForChar();
                    LDA # 0x00 // not '!' or '*'
                }
                else
                {
                    // ignore EOF checksum and EOL : TODO
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
                RTS
            } // loop
        }
        Serial.EmptyTheBuffer(); // discard input after the error
        LDA # '!'                // failure terminator
    }
    
    loadCommand()
    {
        Serial.WaitForChar();    // consume <enter>
        Utilities.SendSlash();   // confirm the command
        
        // ignore CRC for now: TODO
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
            Memory.InitializeHeapSize();
            hopperInit();           // good defaults for HopperVM
            LDA # 0x0D
            Serial.WriteChar();
            LDA # '*'               // restore success terminator
        }
        Serial.WriteChar();         // success or failure ('*' or '!')?
        Utilities.SendSlash();      // confirm the data
    }
    
    hopperInitPC()
    {
        LDA #(HopperData & 0xFF)
        CLC
        ADC (HopperData+4)
        STA ZP.PCL
        LDA #(HopperData >> 8)
        ADC (HopperData+5)
        STA ZP.PCH
    }
    hopperInit()
    {
        Stacks.Init();
        LDA #(HopperData >> 8)
        STA ZP.CODESTART
        STZ ZP.COPYNEXTPOP
        STZ ZP.FLAGS
#ifdef CHECKED
        SMB2 ZP.FLAGS // this is a checked build
#endif        
        SMB3 ZP.FLAGS // 8 bit SP and BP
        hopperInitPC();
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
        
        // good default heap until we load a program? TODO : should depend on RAM
        LDA #0x60
        STA ZP.HEAPSTART
        LDA #0x20
        STA ZP.HEAPSIZE
        
        Serial.Initialize();
        hopperInit();        
    }
    Hopper()
    {
        resetVector();
        
        Utilities.SendSlash(); // ready
        loop
        {
            Serial.WaitForChar();
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
                CMP #'F'
                if (Z)
                {
                    memoryCommand();
                    continue;
                }
                CMP #'K'
                if (Z)
                {
                    crcCommand();
                    continue;
                }
                CMP #'P'
                if (Z)
                {
                    pcCommand();
                    continue;
                }
                CMP #'B'
                if (Z)
                {
                    breakpointCommand();
                    continue;
                }
                CMP #'L'
                if (Z)
                {
                    loadCommand();
                    continue;
                }
            }
        } // loop
    }
} 
