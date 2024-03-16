
program Test
{
    // mapping of Z80 -> 6502
    // https://litwr2.github.io/8080-8085-z80-8088-6502/z80-6502.html
    
    //#define ROM_32K
    //#define ROM_16K
    #define ROM_8K
    //#define ROM_4K
    
    #define CPU_65C02  // Rockwell and WDC
    //#define CPU_6502 // original MOS subset
    //#define CPU_Z80
    
    uses "Serial"
    uses "Command"
    
    IRQ()
    {
        BBR7 ACIASTATUS, +6
        BBS0 ACIASTATUS, +3  
        
        Serial.ISR();
    }
    NMI()
    {
        INC SerialBreakFlag // hardware <ctrl><C>
    }
    
    Hopper()
    {
        Serial.Initialize();
        loop
        {
            Serial.IsAvailable();
            if (NZ)        
            {
                ReadChar();
                WriteChar(); // echo
                
                loop
                {
                    CMP #'R'
                    if (Z)
                    {
                        Command.Registers();
                        break;
                    }
                    
                    CMP #'M'
                    if (Z)
                    {
                        Command.MemoryDump();
                        break;
                    }
                    break;
                } // loop
                
                continue;
            }
            
            LDA SerialBreakFlag // 3
            if (NZ)
            {
                // it was non-zero so <ctrl><C>, consume it
                SEI
                DEC SerialBreakFlag
                CLI
                break;
            }
        } // loop
    }
} 
