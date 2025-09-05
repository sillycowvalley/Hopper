unit Error // ErrorID.asm
{
    
    // Error word IDs - using bits 6-5 for table selection, bits 4-0 for word index
    // Bit 7 = 0 (distinguishes from keywords which have bit 7 = 1)
    
    enum ErrorWord
    {
        // Table 0 (bits 6-5 = 00): Common error words (0x00-0x1F)
        ERROR      = 0x00,  // "ERROR"
        OUT        = 0x04,  // "OUT"
        OF         = 0x05,  // "OF"
        MEMORY     = 0x06,  // "MEMORY"
        NOT        = 0x10,  // "NOT"
        FILE       = 0x19,  // "FILE"
        FOUND      = 0x1A,  // "FOUND"
        TOO        = 0x1D,  // "TOO"
        LONG       = 0x1E,  // "LONG"
        FILES      = 0x1F,  // "FILES"
        
        // Table 1 (bits 6-5 = 01): Expression/parsing words (0x20-0x3F)
        EXPECTED   = 0x21,  // "EXPECTED"
        HEAP       = 0x31,  // "HEAP"
        CORRUPT    = 0x32,  // "CORRUPT"
        NUMERIC    = 0x33,  // "NUMERIC"
        OVERFLOW   = 0x34,  // "OVERFLOW"
        
        // Table 2 (bits 6-5 = 10): Additional words (0x40-0x5F)
        FILENAME   = 0x41,  // "FILENAME"
        DIRECTORY  = 0x42,  // "DIRECTORY"
        FULL       = 0x43,  // "FULL"
        EEPROM     = 0x44,  // "EEPROM"
        ILLEGAL    = 0x56,  // "ILLEGAL"
        
        DIVISION   = 0x57,  // "DIVISION"
        BY         = 0x58,  // "BY"  
        ZERO       = 0x59,  // "ZERO"
        
        INVALID    = 0x5A,  // "INVALID"
        SYSTEM     = 0x5B,  // "SYSTEM"  
        CALL       = 0x5C,  // "CALL"
        
        BYTES      = 0x5D,  // "BYTES"
        USED       = 0x5E,  // "USED"
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
        0  // End marker
    };
    
    // Table 1: Expression/parsing words (0x20-0x3F)
    const byte[] errorWordsTable1 = {
        8,  ErrorWord.EXPECTED,   'E', 'X', 'P', 'E', 'C', 'T', 'E', 'D',
        4,  ErrorWord.HEAP,       'H', 'E', 'A', 'P',
        7,  ErrorWord.CORRUPT,    'C', 'O', 'R', 'R', 'U', 'P', 'T',
        7,  ErrorWord.NUMERIC,    'N', 'U', 'M', 'E', 'R', 'I', 'C',
        8,  ErrorWord.OVERFLOW,   'O', 'V', 'E', 'R', 'F', 'L', 'O', 'W',
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
        
        3, ErrorID.Files,             ErrorWord.FILES,
        2, ErrorID.BytesUsedLabel,    ErrorWord.BYTES, ErrorWord.USED,
        1, ErrorID.BytesLabel,        ErrorWord.BYTES,
        0  // End marker
    };
    
    // Helper method to search error word table and print word
    // Input: X = target word ID, ZP.IDX = table address
    // Output: Word printed to serial if found, C if found, NC if not
    printKeywordFromTable()
    {
        STZ ZP.TEMP
        
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
        
        PLX
        PLY
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
    DivisionByZero()
    {
        LDA #ErrorID.DivisionByZero
        commonError();
    }
    
    NumericOverflow()
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
    
    FilenameTooLong()
    {
        LDA #ErrorID.FilenameTooLong
        commonError();
    }
    
    IllegalFilename()
    {
        LDA #ErrorID.IllegalFilename
        commonError();
    }
    
    DirectoryFull()
    {
        LDA #ErrorID.DirectoryFull
        commonError();
    }
    
    EEPROMFull()
    {
        LDA #ErrorID.EEPROMFull
        commonError();
    }
    
    EEPROMError()
    {
        LDA #ErrorID.EEPROMError
        commonError();
    }
    
    // Memory errors
    OutOfMemory() 
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
