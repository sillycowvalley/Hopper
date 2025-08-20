program EEPROMTest
{
    
    #define ROM_32K
    #define CPU_65C02S
    #define HOPPER_BASIC
    
    #define HASEEPROM
    
    #define DEBUG
    
    uses "./Definitions/ZeroPage"
    uses "./Definitions/Limits"
    uses "./Definitions/MemoryMap"
    uses "./Definitions/BASICTypes"
    uses "./Definitions/Tokens"
    uses "./Definitions/States"
    
    uses "./Debugging/Error"
    uses "./Debugging/Debug"
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Parallel"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/I2C"
    uses "/Source/Runtime/6502/Time"
    uses "/Source/Runtime/6502/Memory"
    
    uses "./Files/EEPROM"
    uses "./Files/File"
    
    uses "./Objects/Table"
    uses "./Objects/Objects"
    uses "./Objects/String"
    uses "./Objects/Char"
    
    uses "./Utilities/Print"
    uses "./Utilities/Tools"
    uses "./Objects/Long"
    
    
     // String constants for test output
    const string msgStarting         = "EEPROM Test Program Starting...\n\n";
    const string msgInitializing     = "Initializing EEPROM...\n";
    const string msgInitializeFailed = "Initialize() failed\n";
    const string msgDetecting        = "Detecting EEPROM...";
    const string msgDetected         = " DETECTED\n";
    const string msgNotFound         = " NOT FOUND\n";
    const string msgErrorNoEEPROM    = "ERROR: No EEPROM detected. Test aborted.\n";
    const string msgSize             = "EEPROM Size: ";
    const string msgKBytes           = "K bytes\n\n";
    const string msgUnknown          = "Unknown\n\n";
    const string msgComplete         = "\n=== EEPROM Test Complete ===\n";
    
    // Interrupt handlers
    IRQ()
    {
        Serial.ISR();
        Parallel.ISR();
    }
    
    NMI()
    {
        // Hardware break - could be used for BASIC BREAK functionality
        SMB0 ZP.SerialBreakFlag
    }
    
    Initialize()
    {
        // Clear Zero Page
        LDX #0
        loop
        {
            CPX # ZP.ACIADATA // don't write to ACIA data register
            if (NZ) 
            {
                STZ 0x00, X
            }
            DEX
            if (Z) { break; }
        } 
        
        // Clear serial buffer
        LDA # (SerialInBuffer >> 8)
        Memory.ClearPage();
        
        Serial.Initialize();
        Parallel.Initialize();
        
    }
    
    const string TestFileOne   = "FIRSTFILE";
    const string TestFileTwo   = "SECONDFILE";
    const string TestFileThree = "THIRDFILE";
    
    // 242 including '\0;
    const string TestDataOne = "00000 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. END";
    
    // 11 including '\0;
    //const string TestDataOne = "0123456789";
    
    
    // 223 including '\0;
    const string TestDataTwo = "@@@@@ Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. END";
    
    // 27 including '\0'
    //const string TestDataTwo = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    
    
    LoadTestDataOne()
    {
        LDA # (TestDataOne / 256)
        STA ZP.STRH
        LDA # (TestDataOne % 256)
        STA ZP.STRL
        String.Length(); // -> Y
//Debug.NL(); Print.String();

        // Set up copy parameters
        LDA ZP.STRH
        STA ZP.FSOURCEADDRESSH
        LDA ZP.STRL
        STA ZP.FSOURCEADDRESSL
        
        LDA #(FunctionOpCodeBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        LDA #(FunctionOpCodeBuffer % 256)
        STA ZP.FDESTINATIONADDRESSL
        
        STY ZP.FLENGTHL
        INC ZP.FLENGTHL // '\0' terminator
        STZ ZP.FLENGTHH
        
        LDA ZP.FLENGTHL
        PHA
        LDA ZP.FLENGTHH
        PHA
            
        // Copy string including null terminator
        Memory.Copy();
        
        
        LDA #(FunctionOpCodeBuffer / 256)
        STA File.SectorSourceH
        LDA #(FunctionOpCodeBuffer % 256)
        STA File.SectorSourceL
        
        PLA
        STA File.TransferLengthH
        PLA
        STA File.TransferLengthL
        
        LDA File.TransferLengthH 
        STA ZP.TOPH
        LDA File.TransferLengthL
        STA ZP.TOPL
        LDA # BASICType.WORD
        STA ZP.TOPT
        
//Debug.NL(); LDA #'1' COut(); LDA #':' COut(); Space(); Print.Decimal();
        
    }
    
    LoadTestDataTwo()
    {
        LDA # (TestDataTwo / 256)
        STA ZP.STRH
        LDA # (TestDataTwo % 256)
        STA ZP.STRL
        String.Length(); // -> Y
//Debug.NL(); Print.String();
        
        // set destination once
        LDA #(FunctionOpCodeBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        
        LDA #(FunctionOpCodeBuffer % 256)
        STA ZP.FDESTINATIONADDRESSL
        
        // use source once:         
        LDA ZP.STRH
        STA ZP.FSOURCEADDRESSH
        LDA ZP.STRL
        STA ZP.FSOURCEADDRESSL
        
        STY ZP.FLENGTHL
        INC ZP.FLENGTHL // '\0' terminator
        STZ ZP.FLENGTHH
        LDA ZP.FLENGTHL
        PHA // Y + 1
        PHA // Y + 1
        
        // Copy string including null terminator
        Memory.Copy();
        
        // use source again:
        LDA ZP.STRH
        STA ZP.FSOURCEADDRESSH
        LDA ZP.STRL
        STA ZP.FSOURCEADDRESSL
        
        PLA // Y + 1 (already includes '\0')
        STA ZP.FLENGTHL
        STZ ZP.FLENGTHH
        
        LDA ZP.FLENGTHL
        PHA
        LDA ZP.FLENGTHH
        PHA
            
        // Copy string including null terminator
        Memory.Copy();
        
        PLA
        STA File.TransferLengthH
        PLA
        STA File.TransferLengthL

        PLA // Y + 1
        CLC
        ADC File.TransferLengthL
        STA File.TransferLengthL
        LDA File.TransferLengthH
        ADC #0
        STA File.TransferLengthH
        
        LDA #(FunctionOpCodeBuffer / 256)
        STA File.SectorSourceH
        LDA #(FunctionOpCodeBuffer % 256)
        STA File.SectorSourceL
        
        LDA File.TransferLengthH 
        STA ZP.TOPH
        LDA File.TransferLengthL
        STA ZP.TOPL
        LDA # BASICType.WORD
        STA ZP.TOPT
        
//Debug.NL(); LDA #'2' COut(); LDA #':' COut(); Space(); Print.Decimal();        

    }
    
    AddFileOne()
    {
        
        LDA #(TestFileOne / 256)
        STA ZP.STRH
        LDA #(TestFileOne % 256)
        STA ZP.STRL
Debug.NL(); Print.String();        
        
        File.StartSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        LoadTestDataOne();
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        LoadTestDataOne();
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        EndSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
    }
    
    AddFileTwo()
    {
        LDA #(TestFileTwo / 256)
        STA ZP.STRH
        LDA #(TestFileTwo % 256)
        STA ZP.STRL
Debug.NL(); Print.String();

        File.StartSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        LoadTestDataOne();
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        EndSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
    }
    
    AddFileThree()
    {
        LDA #(TestFileThree / 256)
        STA ZP.STRH
        LDA #(TestFileThree % 256)
        STA ZP.STRL
Debug.NL(); Print.String();

        File.StartSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        LoadTestDataTwo();
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        
        LoadTestDataOne();
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        
        LoadTestDataTwo();
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        
        EndSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
    }
    LoadAndDisplay()
    {
        
        
        
Debug.NL(); Print.String();
Debug.NL();         
        File.StartLoad();
        if (C)
        {
            loop
            {
                File.NextStream();
                if (NC) 
                { 
                    States.IsFailure();
                    if (C)
                    {
                        Error.CheckAndPrint();
                    }
                    break;
                }
                
                // Set up pointer to returned data
                LDA SectorSourceL
                STA ZP.IDXL
                LDA SectorSourceH  
                STA ZP.IDXH
                
                // Use TransferLength as countdown (we're allowed to munt it)
                loop
                {
                    // Check if done (16-bit zero test)
                    LDA TransferLengthL
                    ORA TransferLengthH
                    if (Z) { break; }
                    
                    // Print character
                    LDY #0
                    LDA [ZP.IDX], Y
                    Debug.Printable();
                    
                    // Increment pointer
                    INC ZP.IDXL
                    if (Z) { INC ZP.IDXH }
                    
                    // Decrement count (16-bit)
                    LDA TransferLengthL
                    if (Z) 
                    {
                        DEC TransferLengthH 
                    }
                    DEC TransferLengthL
                }     
            } // loop
        }
        else
        {
            Error.CheckAndPrint();
        }
    }
    
    // Main entry point
    Hopper()
    {
        
        Initialize();
        
        
        
        LDA #(msgStarting % 256)
        STA ZP.STRL
        LDA #(msgStarting / 256)
        STA ZP.STRH
        Print.String();
        
        // Initialize EEPROM system
        LDA #(msgInitializing % 256)
        STA ZP.STRL
        LDA #(msgInitializing / 256)
        STA ZP.STRH
        Print.String();
        
        EEPROM.Initialize();
        if (NC)
        {
            LDA #(msgInitializeFailed % 256)
            STA ZP.STRL
            LDA #(msgInitializeFailed / 256)
            STA ZP.STRH
            Print.String();
        }
        
        // Test EEPROM detection
        LDA #(msgDetecting % 256)
        STA ZP.STRL
        LDA #(msgDetecting / 256)
        STA ZP.STRH
        Print.String();
        
        EEPROM.Detect();
        if (C)  // Set C (detected)
        {
            LDA #(msgDetected % 256)
            STA ZP.STRL
            LDA #(msgDetected / 256)
            STA ZP.STRH
            Print.String();
        }
        else    // Set NC (not detected)
        {
            LDA #(msgNotFound % 256)
            STA ZP.STRL
            LDA #(msgNotFound / 256)
            STA ZP.STRH
            Print.String();
            
            LDA #(msgErrorNoEEPROM % 256)
            STA ZP.STRL
            LDA #(msgErrorNoEEPROM / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Get and display EEPROM size
        LDA #(msgSize % 256)
        STA ZP.STRL
        LDA #(msgSize / 256)
        STA ZP.STRH
        Print.String();
        
        EEPROM.GetSize();
        if (C)  // Set C (size retrieved)
        {
            Serial.HexOut();
            LDA #(msgKBytes % 256)
            STA ZP.STRL
            LDA #(msgKBytes / 256)
            STA ZP.STRH
            Print.String();
        }
        else    // Set NC (size unknown)
        {
            LDA #(msgUnknown % 256)
            STA ZP.STRL
            LDA #(msgUnknown / 256)
            STA ZP.STRH
            Print.String();
        }
        /*
        DirectoryList();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        return;
        */
        
        File.Format();
        if (NC)
        {
Print.NewLine(); LDA #'F' Print.Char(); LDA #'!' Print.Char(); 
        }
        
        
        AddFileOne();
        AddFileTwo();
        AddFileThree();
        
        DirectoryList();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        LDA #1
        DumpDriveState();
        
        /*
        LDA #(TestFileTwo / 256)
        STA ZP.STRH
        LDA #(TestFileTwo % 256)
        STA ZP.STRL
Debug.NL(); Print.String();        
        DeleteFile();
        if (NC)
        {
            Error.CheckAndPrint();
            
            LDA #1
            DumpDriveState();
            loop {  }   
        }
        
        LDA #(TestFileThree / 256)
        STA ZP.STRH
        LDA #(TestFileThree % 256)
        STA ZP.STRL
Debug.NL(); Print.String();        
        DeleteFile();
        if (NC)
        {
            Error.CheckAndPrint();
            
            LDA #1
            DumpDriveState();
            loop {  }   
        }
        
        AddFileThree();
        */
        
        DirectoryList();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        /*
        LDA #1
        DumpDriveState();
        
        LDA #(TestFileOne / 256)
        STA ZP.STRH
        LDA #(TestFileOne % 256)
        STA ZP.STRL
        LoadAndDisplay();
        
        LDA #(TestFileTwo / 256)
        STA ZP.STRH
        LDA #(TestFileTwo % 256)
        STA ZP.STRL
        LoadAndDisplay();

        
        LDA #(TestFileThree / 256)
        STA ZP.STRH
        LDA #(TestFileThree % 256)
        STA ZP.STRL
        LoadAndDisplay();
        
        LDA #(msgComplete % 256)
        STA ZP.STRL
        LDA #(msgComplete / 256)
        STA ZP.STRH
        Print.String();
        */
    }
}
