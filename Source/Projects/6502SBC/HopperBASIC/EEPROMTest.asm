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
    
    const string TestFileOne = "FILEONE.BAS";
    const string TestDataOne = 
"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.";
    
    const string TestFileTwo = "FILETWO.BAS";
    const string TestDataTwo = 
"Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum";
     
    
    LoadTestDataOne()
    {
        LDA # (TestDataOne / 256)
        STA ZP.STRH
        LDA # (TestDataOne % 256)
        STA ZP.STRL
        String.Length(); // -> Y

        PHY
           
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
            
        // Copy string including null terminator
        Memory.Copy();
        
        /*
        LDA #0x10
        DumpPage();       
        */
        PLY
    }
    
    LoadTestDataTwo()
    {
        LDA # (TestDataTwo / 256)
        STA ZP.STRH
        LDA # (TestDataTwo % 256)
        STA ZP.STRL
        String.Length(); // -> Y
        
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
            
        // Copy string including null terminator
        PHY
        Memory.Copy();
        PLY
        
        // use source again:
        LDA ZP.STRH
        STA ZP.FSOURCEADDRESSH
        LDA ZP.STRL
        STA ZP.FSOURCEADDRESSL
        
        STY ZP.FLENGTHL
        INC ZP.FLENGTHL // '\0' terminator
        STZ ZP.FLENGTHH
            
        // Copy string including null terminator
        Memory.Copy();
        
        /*
        LDA #0x10
        DumpPage();
        LDA #0x11
        DumpPage();
        */
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
        
        
        
        File.Format();
        if (NC)
        {
Print.NewLine(); LDA #'F' Print.Char(); LDA #'!' Print.Char(); 
        }
//LDA #1 // load
//File.DumpDriveState();
        /*
        if (NC)
        {
            Error.CheckAndPrint();
        }
        */
        
        //LoadTestDataOne();
        //LoadTestDataTwo();
        
        LDA #(TestFileOne / 256)
        STA ZP.STRH
        LDA #(TestFileOne % 256)
        STA ZP.STRL
//Print.NewLine(); Print.String();  LDA #':' Print.Char();      
//Print.NewLine(); 
//Print.NewLine(); 
        
        File.StartSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }

        LoadTestDataOne();
        
        LDA #(FunctionOpCodeBuffer / 256)
        STA File.SectorSourceH
        LDA #(FunctionOpCodeBuffer % 256)
        STA File.SectorSourceL

        STY File.TransferLengthL
        STZ File.TransferLengthH
        File.AppendStream();
        if (NC)
        {
            Error.CheckAndPrint();
        }

        LoadTestDataOne();
        
        LDA #(FunctionOpCodeBuffer / 256)
        STA File.SectorSourceH
        LDA #(FunctionOpCodeBuffer % 256)
        STA File.SectorSourceL
        
        STY File.TransferLengthL
        STZ File.TransferLengthH
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

        
//LDA #1 // reload after file one
//File.DumpDriveState();
//File.DumpFileState();        
        
        LDA #(TestFileTwo / 256)
        STA ZP.STRH
        LDA #(TestFileTwo % 256)
        STA ZP.STRL
//Print.NewLine(); Print.String();  LDA #':' Print.Char();      
//Print.NewLine(); 
//Print.NewLine(); 
        
        File.StartSave();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        LoadTestDataOne();
        
        LDA #(FunctionOpCodeBuffer / 256)
        STA File.SectorSourceH
        LDA #(FunctionOpCodeBuffer % 256)
        STA File.SectorSourceL
        
        STY File.TransferLengthL
        STZ File.TransferLengthH
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

//LDA #1 // reload after file two
//File.DumpDriveState();
//File.DumpFileState();


/*
Print.NewLine();        
LDA #(FATBuffer / 256)
Debug.DumpPage();
Print.NewLine();        

Print.NewLine();        
LDA #(DirectoryBuffer / 256)
Debug.DumpPage();
Print.NewLine();        
*/

        DirectoryList();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        
        LDA #(msgComplete % 256)
        STA ZP.STRL
        LDA #(msgComplete / 256)
        STA ZP.STRH
        Print.String();
        
        loop { }
    }
}
