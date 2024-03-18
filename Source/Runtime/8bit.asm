program Runtime
{
    // mapping of Z80 -> 6502
    // https://litwr2.github.io/8080-8085-z80-8088-6502/z80-6502.html
    
    #define ROM_8K
    
    #define CPU_65C02  // Rockwell and WDC

#ifdef CPU_65C02    
    uses "6502/MemoryMap"
    uses "6502/ZeroPage"
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
    //   X      - execute (run ignoring breakpoints)
    //   D      - debug (run respecting breakpoints)
    //   I      - step into
    //   O      - step over
    //   W      - warm restart
    
    
    
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
        Utilities.WaitForEnter();   // consume hex character
        LDA #0
        Serial.WriteChar();
        Serial.WriteChar();
        Serial.WriteChar();
        Serial.WriteChar();
        Utilities.SendSlash();  // confirm the data
    }
   
    Hopper()
    {
        Serial.Initialize();
        
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
            }
        } // loop
    }
} 
