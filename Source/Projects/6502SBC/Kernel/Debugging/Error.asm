unit Error // ErrorID.asm
{
    
    // Error word IDs - using bits 6-5 for table selection, bits 4-0 for word index
    // Bit 7 = 0 (distinguishes from keywords which have bit 7 = 1)
    
    enum ErrorWord
    {
        // Table 0 (bits 6-5 = 00): Common error words (0x00-0x1F)
        ERROR      = 0x00,
        OUT        = 0x04,
        OF         = 0x05,
        MEMORY     = 0x06,
        WILL       = 0x07,
        ERASE      = 0x08,
        ALL        = 0x09,
        AVAILABLE  = 0x0A,
        YN         = 0x0B, // "Y/N"
        //COMMAND    = 0x0C,
        HOPPER     = 0x0D,
        BIOS       = 0x0E,
        
        NOT        = 0x10,
        
        FORMAT     = 0x11,
        MEM        = 0x12,
        DIR        = 0x13,
        CLS        = 0x14,
        HEX        = 0x15,
        DEL        = 0x16,
        EXIT       = 0x17,
        
        FILE       = 0x19,
        FOUND      = 0x1A,
        TOO        = 0x1D,
        LONG       = 0x1E,
        FILES      = 0x1F,
        
        // Table 1 (bits 6-5 = 01): Expression/parsing words (0x20-0x3F)
        EXPECTED   = 0x21,
        HEAP       = 0x31,
        CORRUPT    = 0x32,
        NUMERIC    = 0x33,
        OVERFLOW   = 0x34,
        
        READY      = 0x35,
        FOR        = 0x36,
        
        
        // Table 2 (bits 6-5 = 10): Additional words (0x40-0x5F)
        FILENAME   = 0x41,
        DIRECTORY  = 0x42,
        FULL       = 0x43,
        EEPROM     = 0x44,
        ILLEGAL    = 0x56,
        
        DIVISION   = 0x57,
        BY         = 0x58,
        ZERO       = 0x59,
        
        INVALID    = 0x5A,
        SYSTEM     = 0x5B,
        CALL       = 0x5C,
        
        BYTES      = 0x5D,
        USED       = 0x5E,
        OVERWRITE  = 0x5F,
    }
    
    // Table 0: Common error words (0x00-0x1F)
    const byte[] errorWordsTable0 = {
        5,  ErrorWord.ERROR,      'E', 'R', 'R', 'O', 'R',
        3,  ErrorWord.OUT,        'O', 'U', 'T',
        2,  ErrorWord.OF,         'O', 'F',
        6,  ErrorWord.MEMORY,     'M', 'E', 'M', 'O', 'R', 'Y',
        3,  ErrorWord.NOT,        'N', 'O', 'T',
        4,  ErrorWord.FILE,       'F', 'I', 'L', 'E',
        5,  ErrorWord.FILES,      'F', 'I', 'L', 'E', 'S',
        5,  ErrorWord.FOUND,      'F', 'O', 'U', 'N', 'D',
        3,  ErrorWord.TOO,        'T', 'O', 'O',
        4,  ErrorWord.LONG,       'L', 'O', 'N', 'G',
        
        // Command keywords (must be in table 0)
        6,  ErrorWord.FORMAT,     'F', 'O', 'R', 'M', 'A', 'T',
        3,  ErrorWord.MEM,        'M', 'E', 'M',
        3,  ErrorWord.DIR,        'D', 'I', 'R',
        3,  ErrorWord.CLS,        'C', 'L', 'S',
        3,  ErrorWord.DEL,        'D', 'E', 'L',
        3,  ErrorWord.HEX,        'H', 'E', 'X',
        4,  ErrorWord.EXIT,       'E', 'X', 'I', 'T',
        
        // Message keywords
        4,  ErrorWord.WILL,       'W', 'I', 'L', 'L',
        5,  ErrorWord.ERASE,      'E', 'R', 'A', 'S', 'E',
        3,  ErrorWord.ALL,        'A', 'L', 'L',
        9,  ErrorWord.AVAILABLE,  'A', 'V', 'A', 'I', 'L', 'A', 'B', 'L', 'E',
        3,  ErrorWord.YN,         'Y', '/', 'N',
        //7,  ErrorWord.COMMAND,    'C', 'O', 'M', 'M', 'A', 'N', 'D',
        6,  ErrorWord.HOPPER,     'H', 'O', 'P', 'P', 'E', 'R',
        4,  ErrorWord.BIOS,       'B', 'I', 'O', 'S',
        
        0  // End marker
    };
    
    // Table 1: Expression/parsing words (0x20-0x3F)
    const byte[] errorWordsTable1 = {
        8,  ErrorWord.EXPECTED,   'E', 'X', 'P', 'E', 'C', 'T', 'E', 'D',
        4,  ErrorWord.HEAP,       'H', 'E', 'A', 'P',
        7,  ErrorWord.CORRUPT,    'C', 'O', 'R', 'R', 'U', 'P', 'T',
        7,  ErrorWord.NUMERIC,    'N', 'U', 'M', 'E', 'R', 'I', 'C',
        8,  ErrorWord.OVERFLOW,   'O', 'V', 'E', 'R', 'F', 'L', 'O', 'W',
        5,  ErrorWord.READY,      'R', 'E', 'A', 'D', 'Y',
        3,  ErrorWord.FOR,        'F', 'O', 'R',
        0  // End marker
    };
    
    // Table 2: Additional words (0x40-0x5F)
    const byte[] errorWordsTable2 = {
        8,  ErrorWord.FILENAME,   'F', 'I', 'L', 'E', 'N', 'A', 'M', 'E',
        9,  ErrorWord.DIRECTORY,  'D', 'I', 'R', 'E', 'C', 'T', 'O', 'R', 'Y',
        4,  ErrorWord.FULL,       'F', 'U', 'L', 'L',
        6,  ErrorWord.EEPROM,     'E', 'E', 'P', 'R', 'O', 'M',
        7,  ErrorWord.ILLEGAL,    'I', 'L', 'L', 'E', 'G', 'A', 'L',
        8,  ErrorWord.DIVISION,   'D', 'I', 'V', 'I', 'S', 'I', 'O', 'N',
        2,  ErrorWord.BY,         'B', 'Y',
        4,  ErrorWord.ZERO,       'Z', 'E', 'R', 'O',
        7,  ErrorWord.INVALID,    'I', 'N', 'V', 'A', 'L', 'I', 'D',
        6,  ErrorWord.SYSTEM,     'S', 'Y', 'S', 'T', 'E', 'M',
        4,  ErrorWord.CALL,       'C', 'A', 'L', 'L',
        5,  ErrorWord.BYTES,      'B', 'Y', 'T', 'E', 'S',
        4,  ErrorWord.BYTES,      'U', 'S', 'E', 'D',
        9,  ErrorWord.OVERWRITE,  'O', 'V', 'E', 'R', 'W', 'R', 'I', 'T', 'E',
        0  // End marker
    };
    
    enum ErrorID
    {
        InvalidSystemCall   = 0x01,
        
        // File system errors
        FileNotFound        = 0x10,
        FilenameExpected    = 0x11,
        FilenameTooLong     = 0x12,
        IllegalFilename     = 0x13,
        DirectoryFull       = 0x14,
        EEPROMFull          = 0x15,
        EEPROMError         = 0x16,
        
        // Memory errors
        OutOfMemory         = 0x20,
        HeapCorrupt         = 0x21,
        
        // Mathematical errors
        DivisionByZero      = 0x30,
        NumericOverflow     = 0x31,
        
        // Messages:
        Files               = 0x32,
        BytesUsedLabel      = 0x33,
        BytesLabel          = 0x34,
        FormatWarning       = 0x35,
        MemoryMessage       = 0x36,
        BytesMessage        = 0x37,
        YesNo               = 0x38,
        SystemReady         = 0x39,
        //InvalidCommand      = 0x3A,
        EEPROMLabel         = 0x3B,
        OverwriteWarning    = 0x3C,
        ReadyForHEX         = 0x3D,
        HEXDone             = 0x3E,
        
    }
    
    const byte[] errorMessages0 = {
        3, ErrorID.FileNotFound,      ErrorWord.FILE, ErrorWord.NOT, ErrorWord.FOUND,
        2, ErrorID.FilenameExpected,  ErrorWord.FILENAME, ErrorWord.EXPECTED,
        3, ErrorID.FilenameTooLong,   ErrorWord.FILENAME, ErrorWord.TOO, ErrorWord.LONG,
        2, ErrorID.IllegalFilename,   ErrorWord.ILLEGAL, ErrorWord.FILENAME,
        2, ErrorID.DirectoryFull,     ErrorWord.DIRECTORY, ErrorWord.FULL,
        2, ErrorID.EEPROMFull,        ErrorWord.EEPROM, ErrorWord.FULL,
        2, ErrorID.EEPROMError,       ErrorWord.EEPROM, ErrorWord.ERROR,
        3, ErrorID.OutOfMemory,       ErrorWord.OUT, ErrorWord.OF, ErrorWord.MEMORY,
        2, ErrorID.HeapCorrupt,       ErrorWord.HEAP, ErrorWord.CORRUPT,
        3, ErrorID.DivisionByZero,    ErrorWord.DIVISION, ErrorWord.BY, ErrorWord.ZERO,
        2, ErrorID.NumericOverflow,   ErrorWord.NUMERIC, ErrorWord.OVERFLOW,
        3, ErrorID.InvalidSystemCall, ErrorWord.INVALID, ErrorWord.SYSTEM, ErrorWord.CALL,
        
        1, ErrorID.Files,             ErrorWord.FILES,
        2, ErrorID.BytesUsedLabel,    ErrorWord.BYTES, ErrorWord.USED,
        1, ErrorID.BytesLabel,        ErrorWord.BYTES,
        1, ErrorID.EEPROMLabel,       ErrorWord.EEPROM, 
        
        5, ErrorID.FormatWarning,     ErrorWord.FORMAT, ErrorWord.WILL, ErrorWord.ERASE, ErrorWord.ALL, ErrorWord.FILES,
        1, ErrorID.MemoryMessage,     ErrorWord.MEMORY,
        2, ErrorID.BytesMessage,      ErrorWord.BYTES, ErrorWord.AVAILABLE,
        1, ErrorID.YesNo,             ErrorWord.YN,
        //2, ErrorID.InvalidCommand,    ErrorWord.INVALID, ErrorWord.COMMAND,
        2, ErrorID.SystemReady,       ErrorWord.HOPPER, ErrorWord.BIOS,
        1, ErrorID.OverwriteWarning,  ErrorWord.OVERWRITE,
        3, ErrorID.ReadyForHEX,       ErrorWord.READY, ErrorWord.FOR, ErrorWord.HEX,
        1, ErrorID.HEXDone,           ErrorWord.HEX,
        
        0  // End marker
    };
    
    // Find keyword in errorWordsTable0
    // Input: None (assumes command starts at LineBuffer[0])
    // Output: A = ErrorWord token if found, 0 if not found
    // Munts: X, Y, ZP.ACC
    FindKeyword()
    {
        LDA #(errorWordsTable0 % 256)
        STA ZP.IDXL
        LDA #(errorWordsTable0 / 256)
        STA ZP.IDXH
        
        LDY #0  // Start at beginning of keyword table
        loop
        {
            LDA [ZP.IDX], Y     // Get length of this keyword
            if (Z) { break; }   // End of table - not found
            
            STA ZP.ACCL         // Save keyword length
            INY
            LDA [ZP.IDX], Y     // Get token value
            STA ZP.ACCH         // Save token value
            INY
            
            // Compare characters
            LDX #0  // Character index in our identifier
            loop
            {
                LDA Address.LineBuffer, X   // Get char from our identifier
                if (Z) // Hit null delimiter?
                {
                    // Check if we've matched the full keyword length
                    CPX ZP.ACCL
                    if (Z)
                    {
                        LDA ZP.ACCH  // Return token value - exact match!
                        return;
                    }
                    break; // Length mismatch
                }
                
                // Check if we've exceeded keyword length
                CPX ZP.ACCL
                if (Z) { break; }       // Our identifier is longer than keyword
                
                CMP [ZP.IDX], Y         // Compare with expected character
                if (NZ) { break; }      // Mismatch
                
                INX
                INY
            } // loop
            
            // Mismatch - skip to next keyword
            loop
            {
                CPX ZP.ACCL         // Have we reached the end of keyword?
                if (Z) { break; }   // Yes, Y now points to start of next keyword
                INX                 // Move to next character position  
                INY                 // Advance Y to next character
            }
        } // loop
        LDA #0  // Not found
    }
    
    
    // Helper method to search error word table and print word
    // Input: X = target word ID, ZP.IDX = table address
    // Output: Word printed to serial if found, C if found, NC if not
    printKeywordFromTable()
    {
        STX ZP.TEMP
        
        LDY #0  // Index into table
        loop
        {
            LDA [ZP.IDX], Y     // Get length of this word
            if (Z) 
            { 
                CLC
                break; 
            }   // End of table - not found
            
            PHA                    // Save length on stack
            INY
            LDA [ZP.IDX], Y        // Get word ID 
            CMP ZP.TEMP            // Compare with target
            if (Z)
            {
                // Found it! Print the word
                INY  // Move to first character
                PLX                // Pull length into X register

                loop
                {
                    CPX #0
                    if (Z) { break; }
                    
                    LDA [ZP.IDX], Y  // Access character 
                    Serial.WriteChar();
                    INY
                    DEX
                }
                
                SEC
                break;  // Done printing
            }
            
            // Skip to next word: advance Y by word length + 1 (for word ID byte)
            INY                   // Skip the word ID byte first
            PLA                   // Get length back from stack
            TAX                   // Transfer to X for counting
            loop
            {
                CPX #0
                if (Z) { break; }
                INY
                DEX
            }
        }// single exit
    }

    // Print error word corresponding to word ID
    // Input: A = word ID (0x00-0x7F for error words)
    // Output: Word printed to serial, C set if found, NC if not found
    PrintWord()
    {
        PHY
        PHX
        
        TAX
        AND #0x60   // Extract bits 6-5 (table selector)
        switch (A)
        {
            case 0x00:  // Table 0 (0x00-0x1F)
            {
                LDA #(errorWordsTable0 % 256)
                STA ZP.IDXL
                LDA #(errorWordsTable0 / 256)
            }
            case 0x20:  // Table 1 (0x20-0x3F)
            {
                LDA #(errorWordsTable1 % 256)
                STA ZP.IDXL
                LDA #(errorWordsTable1 / 256)
            }
            case 0x40:  // Table 2 (0x40-0x5F)
            {
                LDA #(errorWordsTable2 % 256)
                STA ZP.IDXL
                LDA #(errorWordsTable2 / 256)
            }
            default:
            {
                BRK // internal error - table not implemented
            }
        }   
        STA ZP.IDXH
        printKeywordFromTable(); // Input: X = target word ID, ZP.IDX = table address, munts X, Y
        
        PLX PLY
    }
    
    flags MessageExtras
    {
        None,
        PrefixSpace  = 0b00000001, // Print a space before the message
        SuffixSpace  = 0b00000010, // Print a space after the message
        SuffixColon  = 0b00000100, // Print a ':' after the message
        SuffixQuest  = 0b00001000, // Print a '?' after the message
        SuffixComma  = 0b00010000, // Print a ',' after the message
        SuffixPeriod = 0b00100000, // Print a '.' after the message
        PrefixQuest  = 0b01000000, // Print a '?' before the message
        InParens     = 0b10000000  // Print message within '(' .. ')'
    }
    
    // Inputs: ZP.IDY  = table
    //         ZP.ACCL = Error.ID
    // Output: C if found, NC if not
    //         Y - location of first word ID
    //         X - word count
    // Munts X, Y, A
    findMessage()
    {
        LDY #0  // Start at beginning of table
        loop
        {
            LDA [ZP.IDY], Y    // Get word count for this message
            if (Z) { CLC break; }  // End of table - not found
            
            TAX                // X = word count
            INY
            LDA [ZP.IDY], Y    // Get error ID
            CMP ZP.ACCL        // Compare with target
            if (Z)
            {
                // Found it!
                INY  // Move to first word ID
                SEC
                break;
            }
            
            // Skip to next message: advance Y by word count + 1 (for error ID)
            INY  // Skip the error ID byte first
            loop
            {
                CPX #0
                if (Z) { break; }
                INY
                DEX
            }
        }
    }
    
    // same as Message() but followed by '\n'
    MessageNL()
    {
        Message();
        Print.NewLine();
    }
    
#ifdef UNIVERSAL
    // Input A = error ID, X = MessageExtras
    Message()
    {
        STA ZP.ACCL  // Store target error ID
        TXA PHA TYA PHA
        
        STX ZP.ACCH  // Store MessageExtras
        
        LDA ZP.ACCH
        AND # 0b00000001
        if (NZ) // PrefixSpace
        {
            Print.Space();
        }
        BIT ZP.ACCH
        if (V) // PrefixQuest
        {
            LDA #'?' 
            Print.Char();
        }
        if (MI) // InParens
        {
            LDA #'(' 
            Print.Char();
        }
        
        LDA #(errorMessages0 % 256)
        STA ZP.IDYL
        LDA #(errorMessages0 / 256)
        STA ZP.IDYH
        findMessage();// munts A, X, Y
        if (NC)
        {
            BRK // internal error - message not found
        }
        
        // Y - location of first word ID
        // X - word count
        loop
        {
            CPX #0
            if (Z) { break; }
            
            LDA [ZP.IDY], Y    // Get word ID
            PrintWord(); // word in A
            INY
            DEX
            
            CPX #0
            if (Z) { break; }  // Don't add space after last word
            
            Print.Space();
        }
        
        BIT ZP.ACCH
        if (MI) // InParens
        {
            LDA #')' 
            Print.Char();
        }
        LDA ZP.ACCH
        AND #0b00000100
        if (NZ) // SuffixColon
        {
            LDA #':' 
            Print.Char();
        }
        LDA ZP.ACCH
        AND #0b00001000
        if (NZ) // SuffixQuest
        {
            LDA #'?' 
            Print.Char();
        }
        LDA ZP.ACCH
        AND #0b00010000
        if (NZ) // SuffixComma
        {
            LDA #',' 
            Print.Char();
        }
        LDA ZP.ACCH
        AND #0b00100000
        if (NZ) // SuffixPeriod
        {
            LDA #'.' 
            Print.Char();
        }
        LDA ZP.ACCH
        AND #0b00000010
        if (NZ) // SuffixSpace
        {
            Print.Space();
        }
        
        PLA TAY PLA TAX
    }
#else    
    
    // Input A = error ID, X = MessageExtras
    Message()
    {
        PHX 
        PHY
        
        STA ZP.ACCL  // Store target error ID
        STX ZP.ACCH  // Store MessageExtras
        

        if (BBS0, ZP.ACCH) // PrefixSpace
        {
            Print.Space();
        }
        if (BBS6, ZP.ACCH) // PrefixQuest
        {
            LDA #'?' 
            Print.Char();
        }
        if (BBS7, ZP.ACCH) // InParens
        {
            LDA #'(' 
            Print.Char();
        }
        
        LDA #(errorMessages0 % 256)
        STA ZP.IDYL
        LDA #(errorMessages0 / 256)
        STA ZP.IDYH
        findMessage();// munts A, X, Y
        if (NC)
        {
            BRK // internal error - message not found
        }
        
        // Y - location of first word ID
        // X - word count
        loop
        {
            CPX #0
            if (Z) { break; }
            
            LDA [ZP.IDY], Y    // Get word ID
            PrintWord(); // word in A
            INY
            DEX
            
            CPX #0
            if (Z) { break; }  // Don't add space after last word
            
            Print.Space();
        }
        
        if (BBS7, ZP.ACCH) // InParens
        {
            LDA #')' 
            Print.Char();
        }
        if (BBS2, ZP.ACCH) // SuffixColon
        {
            LDA #':' 
            Print.Char();
        }
        if (BBS3, ZP.ACCH) // SuffixQuest
        {
            LDA #'?' 
            Print.Char();
        }
        if (BBS4, ZP.ACCH) // SuffixComma
        {
            LDA #',' 
            Print.Char();
        }
        if (BBS5, ZP.ACCH) // SuffixPeriod
        {
            LDA #'.' 
            Print.Char();
        }
        if (BBS1, ZP.ACCH) // SuffixSpace
        {
            Print.Space();
        }
      
        PLY
        PLX
    }
#endif
    
    commonError()
    {
        STA ZP.LastError
        CLC
    }
    
    InvalidSystemCall()
    {
        LDA #ErrorID.InvalidSystemCall
        commonError();
    }
    
    // Mathematical errors
    DivisionByZero() inline
    {
        LDA #ErrorID.DivisionByZero
        commonError();
    }
    
    NumericOverflow() inline
    {
        LDA #ErrorID.NumericOverflow
        commonError();
    }
    
    // File system errors
    FileNotFound() 
    { 
        LDA #ErrorID.FileNotFound
        commonError();        
    }
    
    FilenameExpected()
    {
        LDA #ErrorID.FilenameExpected
        commonError();
    }
    
    FilenameTooLong() inline
    {
        LDA #ErrorID.FilenameTooLong
        commonError();
    }
    
    IllegalFilename() inline
    {
        LDA #ErrorID.IllegalFilename
        commonError();
    }
    
    DirectoryFull() inline
    {
        LDA #ErrorID.DirectoryFull
        commonError();
    }
    
    EEPROMFull()
    {
        LDA #ErrorID.EEPROMFull
        commonError();
    }
    
    EEPROMError() inline
    {
        LDA #ErrorID.EEPROMError
        commonError();
    }
    
    // Memory errors
    OutOfMemory() inline
    { 
        LDA #ErrorID.OutOfMemory
        commonError();
    }
    
    HeapCorruptError()
    {
        LDA #ErrorID.HeapCorrupt
        commonError();
    }
    
    // Clear error state
    // Input: None
    // Output: ZP.LastError cleared (set to 0)
    ClearError()
    {
        STZ ZP.LastError
    }
    
    // Check if error has occurred
    // Input: None
    // Output: C set if no error, NC if error occurred
    // Modifies: Processor flags only
    CheckError()
    {
        LDA ZP.LastError
        if (Z)
        {
            SEC  // No error
        }
        else
        {
            CLC  // Error occurred
        }
    }
    
    // Check for error and print it if found
    // Input: None
    // Output: C set if no error, NC if error was printed
    //         Error cleared after printing
    // Modifies: ZP.LastError (cleared if error was printed)
    CheckAndPrint()
    {
        PHX
        loop
        {
            CheckError();
            if (C) 
            { 
                break;  // No error
            }
            
            // Error occurred - print it
            LDA ZP.LastError
            LDX #(MessageExtras.PrefixSpace | MessageExtras.PrefixQuest)
            MessageNL();
            
            ClearError();  // Clear error state after printing
            CLC  // Error was found and printed
            break;
        } // single exit
        PLX
    }
}
