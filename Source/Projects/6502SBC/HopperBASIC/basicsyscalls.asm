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
        Input        = (0b00100 << 3) | (1 << 2) | 0b00,  // ID=4, returns,  0 args = 0x24
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
        Rnd          = (0b10000 << 3) | (1 << 2) | 0b01,  // ID=16, returns, 1 arg  = 0x85
        
        // I2C functions (ID 17-22)
        I2CFind      = (0b10001 << 3) | (1 << 2) | 0b01,  // ID=17, returns, 1 arg  = 0x8D
        I2CBegin     = (0b10010 << 3) | (0 << 2) | 0b01,  // ID=18, void,    1 arg  = 0x91
        I2CPut       = (0b10011 << 3) | (0 << 2) | 0b01,  // ID=19, void,    1 arg  = 0x99
        I2CEnd       = (0b10100 << 3) | (1 << 2) | 0b00,  // ID=20, returns, 0 args = 0xA4
        I2CGet       = (0b10101 << 3) | (1 << 2) | 0b10,  // ID=21, returns, 2 args = 0xAE
        I2CNext      = (0b10110 << 3) | (1 << 2) | 0b00,  // ID=22, returns, 0 args = 0xB4
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
                case SysCallType.Input:   { LDA #Token.INPUT   }  // INPUT
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
                
                case SysCallType.I2CFind: { LDA #Token.I2CFIND  }
                case SysCallType.I2CBegin:{ LDA #Token.I2CBEGIN }
                case SysCallType.I2CPut:  { LDA #Token.I2CPUT   }
                case SysCallType.I2CEnd:  { LDA #Token.I2CEND   }
                case SysCallType.I2CGet:  { LDA #Token.I2CGET   }
                case SysCallType.I2CNext: { LDA #Token.I2CNEXT  }
                
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
                    case SysCallType.Rnd:
                    case SysCallType.Delay:
                    case SysCallType.Peek:
                    case SysCallType.Chr:
                    
                    case SysCallType.I2CFind:
                    case SysCallType.I2CBegin:
                    case SysCallType.I2CPut:
                        { LDA #Token.LONG        }
                            
                    case SysCallType.Poke:
                    case SysCallType.PinMode:
                    case SysCallType.I2CGet:
                        { LDA #Token.LONG       Tokens.PrintKeyword(); LDA #',' COut(); Space(); LDA #Token.LONG }
                    case SysCallType.Write:
                        { LDA #Token.LONG       Tokens.PrintKeyword(); LDA #',' COut(); Space(); LDA #Token.BIT }
                    case SysCallType.Read:
                    case SysCallType.PrintChar:
                    case SysCallType.Asc:
                        { LDA #Token.CHAR        }
                    case SysCallType.Len:
                        { LDA #Token.STRING      }
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
                    case SysCallType.Input:
                    case SysCallType.Seconds:
                    case SysCallType.Len:
                    case SysCallType.Peek:
                    case SysCallType.Asc:
                    case SysCallType.I2CGet:
                    case SysCallType.I2CNext:
                        { LDA #Token.LONG  }
                            
                    case SysCallType.I2CFind:
                    case SysCallType.I2CEnd:
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

    // Update 16-bit random seed using simplified XORshift algorithm
    // Input: ZP.RANDOMSEEDL/H = current seed
    // Output: ZP.RANDOMSEEDL/H = new seed (never zero)
    // Munts: A, X
    updateRandomSeed()
    {
        // Handle seed = 0 case
        LDA ZP.RANDOMSEEDL
        ORA ZP.RANDOMSEEDH
        if (Z)
        {
            LDA #0x01  // Use 1 as default seed if zero
            STA ZP.RANDOMSEEDL
        }
        
        // Save original for the XOR step
        LDA ZP.RANDOMSEEDL
        STA ZP.ACCL
        LDA ZP.RANDOMSEEDH
        STA ZP.ACCH
        
        // Multiply by 17: seed = seed * 16 + seed
        ASL ZP.RANDOMSEEDL  // *2
        ROL ZP.RANDOMSEEDH
        ASL ZP.RANDOMSEEDL  // *4
        ROL ZP.RANDOMSEEDH
        ASL ZP.RANDOMSEEDL  // *8
        ROL ZP.RANDOMSEEDH
        ASL ZP.RANDOMSEEDL  // *16
        ROL ZP.RANDOMSEEDH
        
        // Add original: seed*16 + seed = seed*17
        CLC
        LDA ZP.RANDOMSEEDL
        ADC ZP.ACCL
        STA ZP.RANDOMSEEDL
        LDA ZP.RANDOMSEEDH
        ADC ZP.ACCH
        STA ZP.RANDOMSEEDH
        
        // XOR with (original >> 1)
        LSR ZP.ACCH
        ROR ZP.ACCL
        
        LDA ZP.RANDOMSEEDL
        EOR ZP.ACCL
        STA ZP.RANDOMSEEDL
        LDA ZP.RANDOMSEEDH
        EOR ZP.ACCH
        STA ZP.RANDOMSEEDH
        
        // Add 1
        INC ZP.RANDOMSEEDL
        if (Z)
        {
            INC ZP.RANDOMSEEDH
        }
    }

    // Generate random number using XORshift PRNG
    // Input: ZP.TOP* = max value (LONG)
    // Output: ZP.TOP* = random number 1-max (LONG)
    // Modifies: ZP.ACC, ZP.NEXT, ZP.RESULT, ZP.RANDOMSEED*
    executeRnd()
    {
        PHY
        loop // single exit
        {
            // Check for valid range (max > 0)
            Long.ZeroCheckTop();
            if (Z)  // max == 0
            {
                // Return 0 for invalid range
                CLC  // Error condition
                break;
            }
            
            // Update the random seed
            updateRandomSeed();
            
            // Convert 16-bit seed to 32-bit for modulo operation
            LDA ZP.RANDOMSEEDL
            STA ZP.NEXT0
            LDA ZP.RANDOMSEEDH
            STA ZP.NEXT1
            STZ ZP.NEXT2
            STZ ZP.NEXT3
            
            // Calculate result = (seed % max) + 1
            LDX #1 // Mod
            Long.DivMod();  // RESULT = NEXT % TOP
            
            // Add 1 for range 1-max
            INC ZP.RESULT0
            if (Z)
            {
                INC ZP.RESULT1
                if (Z)
                {
                    INC ZP.RESULT2
                    if (Z)
                    {
                        INC ZP.RESULT3
                    }
                }
            }
            
            // Return result in ZP.TOP*
            LDA ZP.RESULT0
            STA ZP.TOP0
            LDA ZP.RESULT1
            STA ZP.TOP1
            LDA ZP.RESULT2
            STA ZP.TOP2
            LDA ZP.RESULT3
            STA ZP.TOP3
            
            SEC // Success
            break;
        }
        PLY
    }
   
   
    validateTopBYTE()
    {
        LDA # BASICType.BYTE
        STA ZP.ACCT
        BASICTypes.Coerce();
    }
    validatePinNumber()
    {
        LDA # BASICType.BYTE
        STA ZP.ACCT
        BASICTypes.CoerceNext();
        if (C)
        {
            LDA ZP.NEXT0
            AND #0xF0
            if (NZ)
            {
                Error.RangeError(); BIT ZP.EmulatorPCL // -> NC
            }
        }
    }
    
    pushLongExit()
    {
        PushTopStrictLONG(); // LONG -> TOPT
        CheckErrorAndSetFailure();
    }
   
   // Execute SYSCALL opcode - system call with flags-based dispatch
   const string executeSysCallTrace = "SYSCALL // System call";
   ExecuteSysCall()
   {
#ifdef TRACE
       LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL  LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH  Trace.MethodEntry();
#endif
       loop
       {

           FetchOperandByte();  // A = SYSCALL ID, never fails
           STA ZP.CURRENTSYSCALL
           AND #0b00000011   // A = argument count
           switch (A)
           {
               case 0: { /* No arguments to pop */ }
               case 1: 
               { 
                   Long.PopTop();                 }
               case 2: 
               { 
                   // don't use PopTopNext() - it requires both types to be LONG if one is LONG
                   Long.PopTop();  // second arg in ZP.TOP*, munts X
                   Long.PopNext(); // first arg in ZP.TOP*, munts X
                   if (NC) { break; }
               }
               case 3: 
               { 
#ifdef DEBUG
                   // Handle 3-argument functions (future expansion)
                   TODO(); BIT ZP.EmulatorPCL // DEBUG
                   break;
#endif
               }
           }
           
           LDY ZP.CURRENTSYSCALL
           switch (Y) // full SYSCALL ID
           {
               case SysCallType.Millis:        // ID = 5
               {
                   LDA ZP.TICK3 STA ZP.TOP3  // reading TICK3 makes a snapshot of all 4 registers on the emulator
                   LDA ZP.TICK2 STA ZP.TOP2
                   LDA ZP.TICK1 STA ZP.TOP1
                   LDA ZP.TICK0 STA ZP.TOP0
                   pushLongExit();
                   return;
               }
               case SysCallType.Seconds:       // ID = 6
               {
                   // SECONDS function - get elapsed seconds
                   Tools.Seconds();              
                   pushLongExit();
                   return;
               }
                case SysCallType.Rnd:
                {
                   // RND function - compute absolute value
                   // Input: ZP.TOP* contains maxvalue
                   // Output: ZP.TOP* contains random number in range [1..maxvalue]
                   executeRnd();
                   if (NC)
                   {
                       Error.RangeError(); BIT ZP.EmulatorPCL // maxvalue cannot be zero
                       break;
                   }
                   pushLongExit();
                   return;
                }
               
               case SysCallType.Abs:           // ID = 3
               {
                   // ABS function - compute absolute value
                   // Input: ZP.TOP* contains value and type  
                   // Output: ZP.TOP* contains absolute value
                   if (BBR3, ZP.TOPT) // Bit 3 - Long
                   {
                       Error.TypeMismatch(); BIT ZP.EmulatorPCL
                       break;
                   }
                  
                   // Check if negative (test MSB of TOP3)
                   LDA ZP.TOP3
                   if (MI)
                   {
                       // Negative, so negate it
                       Long.NegateLongTOP();
                   }
                   pushLongExit();
                   return;
               }
               case SysCallType.Peek:          // ID = 8
               {
                   // PEEK function - read memory byte
                   // Input: ZP.TOP* contains address
                   // Output: ZP.TOP* contains byte value
                   
                   LDA # BASICType.WORD
                   STA ZP.ACCT
                   BASICTypes.Coerce(); // WORD
                   if (NC) { break; }
                   
                   // Read byte from memory address
                   LDA [ZP.TOP]
                   STA ZP.TOP0
                   STZ ZP.TOP1
                   pushLongExit();
                   return;
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
                        break;
                    }
                    
                    // Convert CHAR to BYTE (value stays the same)
                    // ZP.TOPL already contains the ASCII value
                    
                    Long.ZeroTop3();
                    pushLongExit();
                    return;
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
                        // Check for null pointer
                        LDA ZP.TOP0
                        ORA ZP.TOP1
                        if (Z)
                        {
                            // Null pointer - return length 0
                            STZ ZP.TOP0
                        }
                        else
                        {
                            LDY #0
                            loop
                            {
                                LDA [ZP.TOP], Y
                                if (Z) { break; }
                                INY
#ifdef DEBUG
                                if (Z) // wrapped around from 0xFF to 0x00
                                {
                                    LDA # 0x04  Debug.Crash(); // runaway SysCallType.Len calculation
                                }
#endif
                            } // loop
                        }
                        STY ZP.TOP0
                        STZ ZP.TOP1
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
                            break;
                        }
                    }
                    STZ ZP.TOP2
                    STZ ZP.TOP3 // x2
                    pushLongExit();
                    return;
               }
               
                
                case SysCallType.PrintValue:    // ID = 1
                {
                    CLC // no quotes
                    BASICTypes.PrintValue();  // Uses ZP.TOP*, CLC = no quotes
                }
                case SysCallType.PrintChar:     // ID = 2
                {
                   // Character to print is in ZP.TOP (1 argument)
                   LDA ZP.TOPT
                   CMP # BASICType.CHAR
                   if (NZ)
                   {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        break;
                   }
                   
                   LDA ZP.TOPL
                   Serial.WriteChar();
                }
                
                case SysCallType.Input:          // ID = 16
                {
                    // INPUT function - read user input and parse as literal
                    // Input: No arguments (uses previous PRINT as prompt)
                    // Output: ZP.TOP* contains parsed value (LONG)
                    loop
                    {
                        // Read line using existing infrastructure
                        Tokenizer.ReadLine();    // Returns length in A, input in BasicInputBuffer
                        
                        STZ ZP.TOP0
                        LDA # BASICType.BYTE
                        STA ZP.TOPT
                                
                        // Check for empty input - return CHAR(0)
                        LDA ZP.BasicInputLength
                        if (NZ)
                        {
                            // Tokenize the single input line
                            LDX #0
                            STX ZP.OpCodeTemp  // Replace mode = 0
                            Tokenizer.TokenizeLineWithMode();
                            CheckError();
                            if (C) 
                            { 
                                // Get the first token
                                Tokenizer.NextTokenCheck();
                                if (C) 
                                { 
                                    // is it a NUMBER?
                                    LDA ZP.CurrentToken
                                    CMP #Token.NUMBER
                                    if (Z)
                                    {
                                        // Parse as LONG literal
                                        Tokenizer.GetTokenNumber();    // Parse into ZP.TOP*, sets ZP.TOPT
                                        CheckError();
                                        if (C)
                                        {
                                            break; // return the number (BYTE|INT|WORD|LONG)
                                        }
                                    }
                                }
                            }
                            Error.ClearError();
                            // Well, tokenizing a NUMBER didn't work so ..
                            // just return ASCII for the first character:
                            LDA Address.BasicInputBuffer
                            STA ZP.TOP0
                        }
                        break;
                    } // single exit
                    BASICTypes.Promote(); // -> LONG 
                    if (NC) { break; }
                }
               
               
               case SysCallType.Poke:          // ID = 9
               {
                   // POKE function - write memory byte
                   // Input: ZP.NEXT* contains address, ZP.TOP* contains value
                   
                   LDA # BASICType.WORD
                   STA ZP.ACCT
                   BASICTypes.CoerceNext(); // WORD
                   if (NC) { break; }

                   validateTopBYTE();
                   if (NC) { break; }
                   
                   // Write byte to memory address
                   LDA ZP.TOP0     // Get value to write
                   STA [ZP.NEXT]   // Write to address
               }
               
                case SysCallType.PinMode:  // ID = 10
                {
                    // PINMODE function - configure pin direction
                    // Input: ZP.NEXT* = pin number, ZP.TOP* = mode
                    
                    validatePinNumber();
                    if (NC) { break; }
                    
                    validateTopBYTE();
                    if (NC) { break; }
                    
                    LDA ZP.TOP0
                    AND #0xFE
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
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
                    
                    validateTopBYTE();
                    if (NC) { break; }
                    
                    LDA ZP.TOP0
                    AND #0xF0
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    
                    // Call GPIO.PinRead
                    LDA ZP.TOP0
                    GPIO.PinRead();  // Result in A
                    STA ZP.TOP0
                    //STZ ZP.TOP1 - already 0 from Coerce above
                    LDA #BASICType.BIT
                    STA ZP.TOPT
                }
                
                case SysCallType.Write:  // ID = 12
                {
                    // WRITE function - write digital output
                    // Input: ZP.NEXT* = pin number, ZP.TOP* = value
            
                    validatePinNumber();
                    if (NC) { break; }
                    
                    // Validate value (0 or 1 for digital)
                    LDA ZP.TOPT
                    CMP # BASICType.BIT
                    if (NZ)
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        break;
                    }
                    
                    // Call GPIO.PinWrite
                    LDA ZP.NEXT0    // Pin number
                    LDX ZP.TOP0     // Value
                    GPIO.PinWrite();
                }
                
                
               
                
                case SysCallType.I2CFind:  // ID = 17
                {
                    // I2CFIND(addr) - Test if device responds
                    // Input: ZP.TOP* = I2C address (LONG)
                    // Output: ZP.TOP* = BIT (TRUE/FALSE)
                    
                    validateTopBYTE();
                    if (NC) { break; }
                    
                    // Validate address (0-127)
                    if (BBS7, ZP.TOP0)  // Bit 7 set = > 127
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    LDA ZP.TOP0
                    I2C.Scan();  // A = address, returns ZP.LastAck, preserves Y
                    LDA ZP.LastAck
                    EOR #1       // Invert: ACK (0) becomes TRUE (1)
                    STA ZP.TOP0
                    LDA #BASICType.BIT
                    STA ZP.TOPT
                }
                
                case SysCallType.I2CBegin:  // ID = 18
                {
                    // I2CBEGIN(addr) - Start write transaction
                    // Input: ZP.TOP* = I2C address (LONG)
                    
                    validateTopBYTE();
                    if (NC) { break; }
                    
                    // Validate address (0-127)
                    if (BBS7, ZP.TOP0)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    // I2C address
                    LDA ZP.TOP0
                    ASL
                    STA ZP.OutB
                    // BeginTx
                    I2C.Start();
                }
                
                case SysCallType.I2CPut:  // ID = 19  
                {
                    // I2CPUT(byte) - Send byte in transaction
                    // Input: ZP.TOP* = byte value (LONG)
                    
                    validateTopBYTE();
                    if (NC) { break; }
                    // write byte to I2C
                    LDA ZP.TOP0
                    STA ZP.OutB
                    I2C.ByteOut();
                }
                case SysCallType.I2CEnd:  // ID = 20
                {
                    // I2CEND() - End transaction
                    // Output: ZP.TOP* = BIT (TRUE if ACKed)
                    I2C.Stop();
                    LDA ZP.LastAck
                    STA ZP.TOP0
                    LDA #BASICType.BIT
                    STA ZP.TOPT
                }
                case SysCallType.I2CGet:  // ID = 21
                {
                    // I2CGET(addr, count) - Read bytes from device
                    // Input: ZP.NEXT* = I2C address, ZP.TOP* = byte count
                    // Output: ZP.TOP* = bytes actually read (LONG)
                    
                    // Validate and convert address
                    LDA #BASICType.BYTE
                    STA ZP.ACCT
                    BASICTypes.CoerceNext(); // BYTE
                    if (NC) { break; }
                    
                    if (BBS7, ZP.NEXT0)  // Address > 127
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    
                    // Validate and convert count
                    validateTopBYTE();
                    if (NC) { break; }
                    
                    // number of bytes to read is in ZP.TOPL
                    LDA ZP.NEXTL // I2C address -> A
                    RequestFromTOPA(); // A has I2C adddress, TOPL has number of bytes to return, TOPL returns number of bytes read
                    
                    // Convert result to LONG
                    Long.ZeroTop3();
                    pushLongExit();
                    return;
                }
                case SysCallType.I2CNext:  // ID = 22
                {
                    // I2CNEXT() - Get next byte from buffer
                    // Output: ZP.TOP* = byte value (LONG)
                    
                    STZ ZP.TOP0
                    
                    LDA ZP.I2CInReadPtr
                    CMP ZP.I2CInWritePtr
                    if (NZ) // ReadPtr != WritePtr means we have more data available in the I2CInBuffer
                    {
                        LDX ZP.I2CInReadPtr
                        LDA Address.I2CInBuffer, X
                        STA ZP.TOPL
                        INC ZP.I2CInReadPtr
                    }
                    Long.ZeroTop3();
                    pushLongExit();
                    return;
                }
                
               
                case SysCallType.Delay:         // ID = 7
                {
                    // DELAY function - delay in milliseconds
                    if (BBR3, ZP.TOPT) // Bit 3 - Long
                    {
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        break;
                    }
                    Time.DelayTOP();            // Uses ZP.TOP*
                }
               
                case SysCallType.Chr:           // ID = 13
                {
                    // CHR function - convert numeric to CHAR
                    // Input: ZP.TOP* contains numeric value (BYTE/WORD/INT)
                    // Output: ZP.TOP* contains CHAR value
                    validateTopBYTE();
                    if (NC) { break; }
                    
                    // Value is valid, convert to CHAR
                    // ZP.TOP0 already contains the byte value
                    // ZP.TOP1-3 are clear (thanks to Coerce() above)
                    LDA #BASICType.CHAR
                    STA ZP.TOPT
                }
                
                
               
               default:
               {
#ifdef DEBUG
                   TODO(); BIT ZP.EmulatorPCL // unknown SysCall, DEBUG
                   break;
#endif
               }
           } // switch

           // Handle return value (bit 2)
           LDA ZP.CURRENTSYSCALL
           AND # 0b00000100 // Test return value bit
           if (NZ) 
           {
               // type in ZP.TOPT
               Long.PushTop(); // Push return value from ZP.TOP0..ZP.TOP3
               if (NC) { break; }
           }
           SEC
           States.SetSuccess();
           break;
       } // loop exit
       CheckErrorAndSetFailure();
       
#ifdef TRACE
       LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
}
