program MemoryCheck
{
    
    //#define CPU_65C02S
    #define CPU_65C02
    #define ROM_16K
    #define MEMORY_CHECK
    
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Parallel"
    uses "/Source/Runtime/6502/Utilities"
    
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "/Source/Runtime/6502/I2C"
    
    IRQ()
    {
        Serial.ISR();
        Parallel.ISR();
    }
    NMI()
    {
    }
    
    const byte zpSlots = 0x90; // above Hopper VM
    const byte Indent = zpSlots+0;
    
    const string EchoMessage         = "\n\nHopper Echo:\n";
    const string SerialInitialized   = "\nSerial Initialized";
    const string ParallelInitialized = "\nParallel Initialized";
    const string ClearPage           = "\nClear Page  0x";
    const string ClearPages          = "\nClear Pages 0x";
    const string Cleared             = ", Cleared";
    const string InitializeHeap      = "\nInitializing Heap:";
    const string InitializeStack     = "\nInitializing Stack:";
    const string HeapSize            = "\nHeapSize    0x";
    const string HeapStart           = "\nHeapStart   0x";
    const string FreeList            = "\nFreeList    0x";
    const string CodeStart           = "\nCodeStart     0x";
    
    const string ProbingRAM          = "\nProbing RAM Extent:";
    const string ProbingSuccess56K   = "\nFound 56K";
    const string ProbingSuccess48K   = "\nFound 48K";
    const string ProbingSuccess32K   = "\nFound 32K";
    const string ProbingSuccess16K   = "\nFound 16K";
    const string ProbingFailed       = "\nFailed (<16K?)";
    const string ProbingTest         = "\nTesting Address 0x";
    const string ProbingRW           = "-> read and write success (RAM)";
    const string ProbingRO           = "-> ROM (or nothing)";
    
    const string I2CInitialize       = "\nInitializing I2C:";
    const string I2CScan             = "\nScanning for device 0x";
    const string I2CScanFound        = ", Found!";
    const string I2CScanNotFound     = ", Not found";
    
    const string Leaving             = "\nLeaving hopperInit()";
    const string Leaving2            = "\nLeaving resetVector()";
    
    PrintCount() // prints X in (..)
    {
        PHA
        LDA #' '
        Serial.WriteChar();
        LDA #'('
        Serial.WriteChar();
        LDA #'0'
        Serial.WriteChar();
        LDA #'x'
        Serial.WriteChar();
        
        TXA
        Serial.HexOut();
        
        LDA #')'
        Serial.WriteChar();
        
        PLA
    }
    PrintIndentACC()
    {
        PHA
        PHX
        
        LDA [ACC]
        CMP #0x0A
        if (Z)
        {
            Serial.WriteChar();
            IncACC();
        }
        LDX Indent
        loop
        {
            if (Z) { break; }
            LDA #' '
            Serial.WriteChar();
            Serial.WriteChar();
            DEX
        }
        
        PrintACC();
        
        PLX
        PLA
    }
    PrintACC()
    {
        PHA
        PHX
                   
        loop
        {
            LDA [ACC]
            if (Z) { break; }
            Serial.WriteChar();
            IncACC();
        }
        PLX
        PLA
    }
    
    hopperInit()
    {
        LDA # (InitializeHeap / 256) STA ACCH LDA # (InitializeHeap % 256) STA ACCL PrintACC();
        INC Indent
        
        Memory.InitializeHeapSize(); // sets HEAPSTART and HEAPSIZE based on size of program loaded
        
        LDA # (HeapStart / 256) STA ACCH LDA # (HeapStart % 256) STA ACCL PrintIndentACC();
        
        LDA HEAPSTART
        Serial.HexOut();
        LDA #0
        Serial.HexOut();
        
        LDA # (HeapSize / 256) STA ACCH LDA # (HeapSize % 256) STA ACCL PrintIndentACC();
        
        LDA HEAPSIZE
        Serial.HexOut();
        LDA #0
        Serial.HexOut();
        
        LDA # (FreeList / 256) STA ACCH LDA # (FreeList % 256) STA ACCL PrintIndentACC();
        
        LDA FREELISTH
        Serial.HexOut();
        LDA FREELISTL
        Serial.HexOut();
        
        DEC Indent
        
        LDA # (InitializeStack / 256) STA ACCH LDA # (InitializeStack % 256) STA ACCL PrintACC();
        INC Indent
        
        Stacks.Initialize();
        
        DEC Indent
        
        LDA # (CodeStart / 256) STA ACCH LDA # (CodeStart % 256) STA ACCL PrintACC();
        
        // CODESTART = EntryPoint + HopperData
        LDA #(HopperData & 0xFF)
        CLC
        ADC (HopperData+4)
        STA ZP.CODESTARTL
        
        LDA #(HopperData >> 8)
        ADC (HopperData+5)
        STA ZP.CODESTARTH
        
        Serial.HexOut();
        LDA ZP.CODESTARTL
        Serial.HexOut();
        
        STZ ZP.CNP
        STZ ZP.PCL
        STZ ZP.PCH
        
#ifdef  CPU_65C02S
        STZ ZP.FLAGS  // resets ProgramExited
        
        SMB3 ZP.FLAGS // 8 bit SP and BP
        SMB4 ZP.FLAGS // IsInDebugger    
#else
        LDA # 0b00011000
        STA ZP.FLAGS
#endif        
        
        LDA # (Leaving / 256) STA ACCH LDA # (Leaving % 256) STA ACCL PrintIndentACC();
    }
    
    resetVector()
    {
        // zeroes mean faster debug protocol
        
        // clear the Zero Page
        LDA # 0
        LDX # 0
        loop
        {
            STA 0x00, X
            DEX
            if (Z) { break; }
        }

        LDA # 0
        STA IDXL
        LDA # (SerialInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // clear the serial buffer
        
        Serial.Initialize(); // munts A
        
        LDA # (SerialInitialized / 256) STA ACCH LDA # (SerialInitialized % 256) STA ACCL PrintACC();
        
        Parallel.Initialize();
        
        LDA # (ParallelInitialized / 256) STA ACCH LDA # (ParallelInitialized % 256) STA ACCL PrintACC();
        
        LDA # (ClearPage / 256) STA ACCH LDA # (ClearPage % 256) STA ACCL PrintACC();
        
        LDA #0
        STA IDXL
        LDA # (I2CInBuffer >> 8)
        STA IDXH
        
        Serial.HexOut();
        
        LDX # 1
        Utilities.ClearPages(); // clear the I2C buffer
        
        LDA # (Cleared / 256) STA ACCH LDA # (Cleared % 256) STA ACCL PrintACC();
        
        // scan for I2C devices
        STZ ZP.PLUGNPLAY
    #ifdef I2C 
        LDA # (I2CInitialize / 256) STA ACCH LDA # (I2CInitialize % 256) STA ACCL PrintACC();
        INC Indent
        LDA # (I2CScan / 256) STA ACCH LDA # (I2CScan % 256) STA ACCL PrintIndentACC();
    
        LDA # I2C.SSD1306Address // SSD1306 OLED
        
        Serial.HexOut();
        
        I2C.Scan();
        if (Z)
        {
            LDA # (I2CScanFound / 256) STA ACCH LDA # (I2CScanFound % 256) STA ACCL PrintACC();
            SMB0 ZP.PLUGNPLAY
        }
        else
        {
            LDA # (I2CScanNotFound / 256) STA ACCH LDA # (I2CScanNotFound % 256) STA ACCL PrintACC();
        }
        
        
        LDX # I2C.SerialEEPROMAddress // EEPROM?
        loop
        {
            LDA # (I2CScan / 256) STA ACCH LDA # (I2CScan % 256) STA ACCL PrintIndentACC();
            
            TXA
            PHX
            
            Serial.HexOut();
            
            I2C.Scan();
            if (Z)
            {
                LDA # (I2CScanFound / 256) STA ACCH LDA # (I2CScanFound % 256) STA ACCL PrintACC();
                SMB1 ZP.PLUGNPLAY
            }
            else
            {
                LDA # (I2CScanNotFound / 256) STA ACCH LDA # (I2CScanNotFound % 256) STA ACCL PrintACC();
            }
            PLX
            INX
            CPX #0x58
            if (Z) { break; }
        }
        
        
        
        DEC Indent
    #endif  
           
        hopperInit();
        
        LDA # (Leaving2 / 256) STA ACCH LDA # (Leaving2 % 256) STA ACCL PrintIndentACC();
    }
    
    
    Hopper()
    {
        SEI
        
        STZ Indent
        
        resetVector();
        
        CLI
        
        LDA # (EchoMessage / 256) STA ACCH LDA # (EchoMessage % 256) STA ACCL PrintACC();
        
        loop
        {
            Serial.WaitForChar();
            Serial.WriteChar();
        }
    }
}
