unit BASICSysCalls
{
    uses "./Definitions/Messages"
   
    // System call IDs for SYSCALL opcode
    // Bit layout: Bit 7-3: Function ID, Bit 2: Return Value, Bit 1-0: Argument Count
    enum SysCallType
    {
        // System functions (ID 1-2)
        PrintValue   = (0b00001 << 3) | (0 << 2) | 0b01,  // ID=1, void,     1 arg  = 0x09
        PrintChar    = (0b00010 << 3) | (0 << 2) | 0b01,  // ID=2, void,     1 arg  = 0x11
        
        // Built-in functions (ID 3-9)
        Abs          = (0b00011 << 3) | (1 << 2) | 0b01,  // ID=3, returns,  1 arg  = 0x1D
        Rnd          = (0b00100 << 3) | (1 << 2) | 0b01,  // ID=4, returns,  1 arg  = 0x25
        Millis       = (0b00101 << 3) | (1 << 2) | 0b00,  // ID=5, returns,  0 args = 0x2C  
        Seconds      = (0b00110 << 3) | (1 << 2) | 0b00,  // ID=6, returns,  0 args = 0x34
        Delay        = (0b00111 << 3) | (0 << 2) | 0b01,  // ID=7, void,     1 arg  = 0x39
        Peek         = (0b01000 << 3) | (1 << 2) | 0b01,  // ID=8, returns,  1 arg  = 0x45
        Poke         = (0b01001 << 3) | (0 << 2) | 0b10,  // ID=9, void,     2 args = 0x4A
        
        // Hardware I/O functions (ID 10-13)
        PinMode      = (0b01010 << 3) | (0 << 2) | 0b10,  // ID=10, void,    2 args = 0x52
        Read         = (0b01011 << 3) | (1 << 2) | 0b01,  // ID=11, returns, 1 arg  = 0x5D
        Write        = (0b01100 << 3) | (0 << 2) | 0b10,  // ID=12, void,    2 args = 0x62
        
        // Character/String functions (ID 13-15)
        Chr          = (0b01101 << 3) | (1 << 2) | 0b01,  // ID=13, returns, 1 arg  = 0x6D
        Asc          = (0b01110 << 3) | (1 << 2) | 0b01,  // ID=14, returns, 1 arg  = 0x75
        Len          = (0b01111 << 3) | (1 << 2) | 0b01,  // ID=15, returns, 1 arg  = 0x7D
    }
   
    // SYSCALL formatting for DASM:
#ifdef DEBUG    
    ToString()
    {
        loop
        {
            // Input: A = SYSCALL ID
            // Output: Prints syscall name and details
            TAX
            
            // Map syscall ID to Token value for keyword printing
            switch (X)
            {
                case SysCallType.PrintValue: // PRINTVALUE
                { 
                    LDA #(Messages.PrintValue % 256)
                    STA ZP.STRL
                    LDA #(Messages.PrintValue / 256)
                    STA ZP.STRH
                    Print.String();
                    LDA #0                    
                }
                case SysCallType.PrintChar: // PRINTCHAR
                {
                    LDA #(PrintChar % 256)
                    STA ZP.STRL
                    LDA #(PrintChar / 256)
                    STA ZP.STRH
                    Print.String();
                    LDA #0
                } 
                case SysCallType.Abs:     { LDA #Token.ABS     }  // ABS
                case SysCallType.Rnd:     { LDA #Token.RND     }  // RND
                case SysCallType.Millis:  { LDA #Token.MILLIS  }  // MILLIS
                case SysCallType.Seconds: { LDA #Token.SECONDS }  // SECONDS
                case SysCallType.Delay:   { LDA #Token.DELAY   }  // DELAY
                case SysCallType.Peek:    { LDA #Token.PEEK    }  // PEEK
                case SysCallType.Poke:    { LDA #Token.POKE    }  // POKE
                case SysCallType.PinMode: { LDA #Token.PINMODE }  // PINMODE
                case SysCallType.Read:    { LDA #Token.READ    }  // READ
                case SysCallType.Write:   { LDA #Token.WRITE   }  // WRITE
                case SysCallType.Chr:     { LDA #Token.CHR     }  // CHR
                case SysCallType.Asc:     { LDA #Token.ASC     }  // ASC
                case SysCallType.Len:     { LDA #Token.LEN     }  // LEN
                default:                  { Error.InternalError(); BIT ZP.EmulatorPCL LDA #0 }  // Unknown
            }
            if (NZ)
            {
                Tokens.PrintKeyword(); 
            }
            
            // Show argument count and return type in parentheses
            LDA #'(' COut();
            TXA
            AND #0x03  // Extract argument count
            if (NZ)
            {
                switch (X)
                {
                    case SysCallType.PrintValue:
                        { LDA #Token.VAR         }
                    case SysCallType.Abs:
                        { LDA #Token.LONG        }
                    case SysCallType.Delay:
                    case SysCallType.Peek:
                        { LDA #Token.LONG        }
                    case SysCallType.Poke:
                        { LDA #Token.LONG       Tokens.PrintKeyword(); LDA #',' COut(); Space(); LDA #Token.LONG }
                    case SysCallType.PinMode:
                        { LDA #Token.LONG       Tokens.PrintKeyword(); LDA #',' COut(); Space(); LDA #Token.LONG }
                    case SysCallType.Read:
                    case SysCallType.Chr:
                        { LDA #Token.LONG        }
                    case SysCallType.PrintChar:
                    case SysCallType.Asc:
                        { LDA #Token.CHAR        }
                    case SysCallType.Len:
                        { LDA #Token.STRING      }
                    case SysCallType.Write:
                        { LDA #Token.LONG       Tokens.PrintKeyword(); LDA #',' COut(); Space(); LDA #Token.BIT }
                }
                Tokens.PrintKeyword();
            }
            LDA #')' COut();
            
            TXA
            AND #0x04  // Check return bit
            if (NZ)
            {
                Space(); LDA #'-' COut(); LDA #'>' COut(); Space();
                switch (X)
                {
                    case SysCallType.Abs:     
                    case SysCallType.Rnd:
                    case SysCallType.Millis:
                    case SysCallType.Seconds:
                    case SysCallType.Len:
                    case SysCallType.Peek:
                    case SysCallType.Asc:
                        { LDA #Token.LONG  }
                            
                    case SysCallType.Read:
                        { LDA #Token.BIT   }
                    case SysCallType.Chr:
                        { LDA #Token.CHAR  }
                }
                Tokens.PrintKeyword(); 
            }
            break;
        } // single exit
    }
#endif    
   
   // Execute SYSCALL opcode - system call with flags-based dispatch
   const string executeSysCallTrace = "SYSCALL // System call";
   ExecuteSysCall()
   {
#ifdef TRACE
       LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL  LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH  Trace.MethodEntry();
#endif
       loop
       {

           FetchOperandByte();  // A = SYSCALL ID
           States.CanContinue();
           if (NC) { break; }
           
           TAY  // Preserve full SYSCALL ID in Y
           
#ifdef TRACE
           //TYA Debug.HOut(); Debug.NL(); 
           //CMP # SysCallType.Delay
           //if (Z)
           //{
           //   DumpStack();
           //}
#endif           
           // Handle arguments based on count (bits 1-0)
           TYA
           AND #0b00000011   // A = argument count
           switch (A)
           {
               case 0: { /* No arguments to pop */ }
               case 1: 
               { 
                   Long.PopTop();  // Arg in ZP.TOP*, munts X
               }
               case 2: 
               { 
                   PHY
                   Long.PopTopNext(); // First arg in ZP.NEXT, Second arg in ZP.TOP*, munts X and Y
                   PLY
                   if (NC) { break; }
               }
               case 3: 
               { 
#ifdef DEBUG
                   // Handle 3-argument functions (future expansion)
                   TODO(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
#endif
               }
           }
           
           switch (Y) // full SYSCALL ID
           {
               case SysCallType.PrintValue:    // ID = 1
               {
                   CLC // no quotes
                   BASICTypes.PrintValue();  // Uses ZP.TOP*, CLC = no quotes
                   SEC // all good
               }
               case SysCallType.PrintChar:     // ID = 2
               {
                   // Character to print is in ZP.TOP (1 argument)
                   LDA ZP.TOPL
                   Serial.WriteChar();
               }
               case SysCallType.Abs:           // ID = 3
               {
                   // ABS function - compute absolute value
                   // Input: ZP.TOP* contains value and type
                   // Output: ZP.TOP* contains absolute value
                   TODO(); BIT ZP.EmulatorPCL // TODO LONG
                   
               }
               case SysCallType.Rnd:           // ID = 4
               {
                   // RND function - random number generation
                   // Input: ZP.TOP* contains max value
                   // Output: ZP.TOP* contains random number 0 to max-1
                   
                   // TODO: Replace with actual implementation
                   TODO(); BIT ZP.EmulatorPCL
               }
               case SysCallType.Millis:        // ID = 5
               {
                   LDA ZP.TICK3 STA ZP.TOP3  // reading TICK3 makes a snapshot of all 4 registers on the emulator
                   LDA ZP.TICK2 STA ZP.TOP2
                   LDA ZP.TICK1 STA ZP.TOP1
                   LDA ZP.TICK0 STA ZP.TOP0
                   LDA #BASICType.LONG STA ZP.TOPT
               }
               case SysCallType.Seconds:       // ID = 6
               {
                   // SECONDS function - get elapsed seconds
                   PHY
                   Tools.Seconds();              
                   PLY
                   LDA #BASICType.LONG STA ZP.TOPT
               }
               case SysCallType.Delay:         // ID = 7
               {
                   // DELAY function - delay in milliseconds
                   Time.DelayTOP();            // Uses ZP.TOP*
               }
               case SysCallType.Peek:          // ID = 8
               {
                   // PEEK function - read memory byte
                   // Input: ZP.TOP* contains address
                   // Output: ZP.TOP* contains byte value
                   
                   LDA # BASICType.WORD
                   BASICTypes.Coerce();
                   if (NC) { break; }
                   
                   // Read byte from memory address
                   LDA [ZP.TOP]
                   STA ZP.TOP0
                   STZ ZP.TOP1
                   STZ ZP.TOP2
                   STZ ZP.TOP3
                   LDA #BASICType.LONG
                   STA ZP.TOPT
               }
               case SysCallType.Poke:          // ID = 9
               {
                   // POKE function - write memory byte
                   // Input: ZP.NEXT* contains address, ZP.TOP* contains value
                   
                   // TODO TYPE DEMOTION
                   
                   // Validate address is WORD or INT type
                   LDA ZP.NEXTT
                   CMP # BASICType.LONG
                   if (NZ)
                   {
                       Error.TypeMismatch(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                       break;
                   }
                   LDA ZP.NEXT2
                   ORA ZP.NEXT3
                   if (NZ)
                   {
                       Error.RangeError(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                       break;
                   }
                   
                   LDA ZP.TOPT
                   CMP # BASICType.LONG
                   if (NZ)
                   {
                       Error.TypeMismatch(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                       break;
                   }
                   LDA ZP.TOP1
                   ORA ZP.TOP2
                   ORA ZP.TOP3
                   if (NZ)
                   {
                       Error.RangeError(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                       break;
                   }
                   
                   // Write byte to memory address
                   LDA ZP.TOP0     // Get value to write
                   STA [ZP.NEXT]   // Write to address
               }
               
                case SysCallType.PinMode:  // ID = 10
                {
                    // PINMODE function - configure pin direction
                    // Input: ZP.NEXT* = pin number, ZP.TOP* = mode
                    
                    // TODO TYPE DEMOTION
                    
                    LDA ZP.NEXTT
                    CMP # BASICType.LONG
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    // Validate pin number (0-15)
                    LDA ZP.NEXT1
                    ORA ZP.NEXT2
                    ORA ZP.NEXT3
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    LDA ZP.NEXT0
                    AND #0xF0
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    LDA ZP.TOPT
                    CMP # BASICType.LONG
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    // Validate mode (0 or 1)
                    LDA ZP.TOP1
                    ORA ZP.TOP2
                    ORA ZP.TOP3
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    LDA ZP.TOP0
                    AND #0xFE
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    // Call GPIO.PinMode
                    LDA ZP.NEXT0    // Pin number
                    LDX ZP.TOP0     // Mode
                    GPIO.PinMode();
                }
                
                case SysCallType.Read:  // ID = 11
                {
                    // READ function - read digital input
                    // Input: ZP.TOP* = pin number
                    // Output: ZP.TOP* = pin value (0 or 1)
                    
                    // TODO TYPE DEMOTION
                    
                    LDA ZP.TOPT
                    CMP # BASICType.LONG
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    // Validate pin number (0-15)
                    LDA ZP.TOP1
                    ORA ZP.TOP2
                    ORA ZP.TOP3
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    LDA ZP.TOP0
                    AND #0xF0
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    // Call GPIO.PinRead
                    GPIO.PinRead();  // Result in A
                    STA ZP.TOP0
                    STZ ZP.TOP1
                    LDA #BASICType.BIT
                    STA ZP.TOPT
                }
                
                case SysCallType.Write:  // ID = 12
                {
                    // WRITE function - write digital output
                    // Input: ZP.NEXT* = pin number, ZP.TOP* = value
                    
                    // TODO TYPE DEMOTION
                    
                    LDA ZP.NEXTT
                    CMP # BASICType.LONG
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    LDA ZP.NEXT1
                    ORA ZP.NEXT2
                    ORA ZP.NEXT3
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    LDA ZP.NEXT0
                    AND #0xF0
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    // Validate value (0 or 1 for digital)
                    LDA ZP.TOPT
                    CMP # BASICType.BIT
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    // Call GPIO.PinWrite
                    LDA ZP.NEXT0    // Pin number
                    LDX ZP.TOP0     // Value
                    GPIO.PinWrite();
                }
                
                case SysCallType.Chr:           // ID = 13
                {
                    // CHR function - convert numeric to CHAR
                    // Input: ZP.TOP* contains numeric value (BYTE/WORD/INT)
                    // Output: ZP.TOP* contains CHAR value
                    
                    LDA # BASICType.BYTE
                    STA ZP.ACCT
                    BASICTypes.Coerce();
                    
                    // Value is valid, convert to CHAR
                    // ZP.TOPL already contains the byte value
                    STZ ZP.TOPH  // Clear high byte
                    LDA #BASICType.CHAR
                    STA ZP.TOPT
                }
                
                case SysCallType.Asc:           // ID = 14
                {
                    // ASC function - convert CHAR to BYTE
                    // Input: ZP.TOP* contains CHAR value
                    // Output: ZP.TOP* contains BYTE value
                    
                    // Validate input is CHAR type
                    LDA ZP.TOPT
                    CMP #BASICType.CHAR
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                    
                    // Convert CHAR to BYTE (value stays the same)
                    // ZP.TOPL already contains the ASCII value
                    
                    STZ ZP.TOP1
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                    LDA #BASICType.LONG
                    STA ZP.TOPT
                }
                
                case SysCallType.Len:           // ID = 15
                {
                    // LEN function - get string length
                    // Input: ZP.TOP* contains STRING pointer
                    // Output: ZP.TOP* contains length as WORD
                    // Validate input is STRING type
                    LDA ZP.TOPT
                    AND # BASICType.TYPEMASK
                    CMP # BASICType.STRING
                    if (Z)
                    {
                        PHY
                        
                        LDA ZP.TOPL
                        STA ZP.STRL
                        LDA ZP.TOPH
                        STA ZP.STRH
                        String.Length();
                        STY ZP.TOP0
                        STZ ZP.TOP1
                        
                        PLY
                    }
                    else
                    {
                        // Check if it's an array type
                        if (BBS5, ZP.TOPT) // Bit 5 - ARRAY
                        {
                            // Array handling
                            LDA ZP.TOPL
                            STA ZP.IDXL
                            LDA ZP.TOPH
                            STA ZP.IDXH
                            BASICArray.GetCount();  // Returns in ZP.ACC
                            // Move to TOP and set type
                            LDA ZP.ACCL
                            STA ZP.TOP0
                            LDA ZP.ACCH
                            STA ZP.TOP1
                        }
                        else
                        {
                            Error.TypeMismatch(); BIT ZP.EmulatorPCL
                            States.SetFailure();
                            break;
                        }
                    }
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                    LDA #BASICType.LONG
                    STA ZP.TOPT
               }
               
               default:
               {
#ifdef DEBUG
                   TODO(); BIT ZP.EmulatorPCL // unknown SysCall
                   States.SetFailure();
                   break;
#endif
               }
           }
           
           // Handle return value (bit 2)
           TYA              // Restore full SYSCALL ID
           AND # 0b00000100 // Test return value bit
           if (NZ) 
           {
               // type in ZP.TOPT
               Long.PushTopStrict(); // Push return value from ZP.TOP0..ZP.TOP3
               if (NC) { break; }
               
           }
           SEC
           States.SetSuccess();
           break;
       } // loop exit
#ifdef TRACE
       LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
}
