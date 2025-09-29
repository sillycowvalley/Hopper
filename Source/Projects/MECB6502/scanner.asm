program I2CScanner
{
    #define CPU_65C02
    #define ROM_16K
    
    uses "/Source/Runtime/6502/MemoryMap"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    
    // Motorola 6821 PIA addresses
    const uint PORTA = 0xF010; // Peripheral A Data Register / DDRA
    const uint CRA   = 0xF011; // Control Register A
    const uint PORTB = 0xF012; // Peripheral B Data Register / DDRB  
    const uint CRB   = 0xF013; // Control Register B
    
    // I2C pin assignments on Port A
    const byte SCL_PIN = 0b01000000;  // PA6 = bit 6
    const byte SDA_PIN = 0b10000000;  // PA7 = bit 7
    const byte I2C_PINS = 0b11000000; // Both pins
    
    // I2C timing delays (adjust for your clock speed)
    const byte DELAY_COUNT = 10;
    
    // String constants
    const string msgTitle = "I2C Scanner for 6821 PIA\r\nPress any key to scan, Ctrl+C to exit\r\n\r\n\0";
    const string msgScanning = "Scanning I2C bus (addresses 0x08-0x77)...\r\n\0";
    const string msgFound = "Found device at address 0x\0";
    const string msgDone = "Scan complete.\r\nPress any key to scan again...\r\n\r\n\0";
    
    IRQ()
    {
        Serial.ISR();
    }
    NMI()
    {
    }
    
    // Initialize PIA for I2C operation
    InitializePIA()
    {
        // Access Data Direction Register A (clear CRA-2)
        LDA CRA
        AND #0b11111011  // Clear bit 2 to access DDRA
        STA CRA
        
        // Set PA6/PA7 as inputs initially (I2C idle state)
        // Internal pullups will pull lines high
        LDA DDRA
        AND #0b00111111  // Clear bits 6,7 = inputs
        STA DDRA
        
        // Access Peripheral Data Register A (set CRA-2)
        LDA CRA
        ORA #0b00000100  // Set bit 2 to access PORTA
        STA CRA
        
        // Ensure output data is 0 for when we switch to output
        LDA PORTA
        AND #0b00111111  // Clear PA6/PA7 data bits
        STA PORTA
    }
    
    // Short delay for I2C timing
    I2CDelay()
    {
        LDY #DELAY_COUNT
        loop
        {
            DEY
            if (Z) { break; }
        }
    }
    
    // Release SDA (make it input, pullup takes it high)
    ReleaseSDA()
    {
        LDA CRA
        AND #0b11111011  // Access DDRA
        STA CRA
        
        LDA DDRA
        AND #0b01111111  // PA7 = input
        STA DDRA
        
        LDA CRA
        ORA #0b00000100  // Back to PORTA
        STA CRA
    }
    
    // Drive SDA low (make it output, data already 0)
    DriveSDALow()
    {
        LDA CRA
        AND #0b11111011  // Access DDRA
        STA CRA
        
        LDA DDRA
        ORA #SDA_PIN     // PA7 = output
        STA DDRA
        
        LDA CRA
        ORA #0b00000100  // Back to PORTA
        STA CRA
    }
    
    // Release SCL (make it input, pullup takes it high)
    ReleaseSCL()
    {
        LDA CRA
        AND #0b11111011  // Access DDRA
        STA CRA
        
        LDA DDRA
        AND #0b10111111  // PA6 = input
        STA DDRA
        
        LDA CRA
        ORA #0b00000100  // Back to PORTA
        STA CRA
    }
    
    // Drive SCL low (make it output, data already 0)
    DriveSCLLow()
    {
        LDA CRA
        AND #0b11111011  // Access DDRA
        STA CRA
        
        LDA DDRA
        ORA #SCL_PIN     // PA6 = output
        STA DDRA
        
        LDA CRA
        ORA #0b00000100  // Back to PORTA
        STA CRA
    }
    
    // Read SDA state - Returns: A=0 if low, A=1 if high
    ReadSDA()
    {
        LDA PORTA
        AND #SDA_PIN
        if (Z)
        {
            LDA #0  // SDA is low
        }
        else
        {
            LDA #1  // SDA is high
        }
    }
    
    // I2C Start Condition: SDA goes low while SCL is high
    I2CStart()
    {
        ReleaseSDA();    // SDA high
        I2CDelay();
        ReleaseSCL();    // SCL high
        I2CDelay();
        DriveSDALow();   // SDA low while SCL high = START
        I2CDelay();
        DriveSCLLow();   // SCL low
        I2CDelay();
    }
    
    // I2C Stop Condition: SDA goes high while SCL is high
    I2CStop()
    {
        DriveSDALow();   // SDA low
        I2CDelay();
        ReleaseSCL();    // SCL high
        I2CDelay();
        ReleaseSDA();    // SDA high while SCL high = STOP
        I2CDelay();
    }
    
    // Write one byte to I2C bus
    // Input: A = byte to send
    // Output: C = 0 if ACK received, C = 1 if NACK
    I2CWriteByte()
    {
        STA ZP.ACCL      // Save byte to send
        LDX #8           // 8 bits to send
        
        loop
        {
            // Send MSB first
            ASL ZP.ACCL    // Shift MSB into carry
            if (C)
            {
                ReleaseSDA();  // Send 1
            }
            else
            {
                DriveSDALow(); // Send 0
            }
            
            I2CDelay();
            ReleaseSCL();    // Clock high
            I2CDelay();
            DriveSCLLow();   // Clock low
            I2CDelay();
            
            DEX
            if (Z) { break; }
        }
        
        // Read ACK bit
        ReleaseSDA();        // Release SDA for slave to control
        I2CDelay();
        ReleaseSCL();        // Clock high
        I2CDelay();
        
        ReadSDA();           // Read ACK bit
        STA ZP.ACCL          // Save ACK state
        
        DriveSCLLow();       // Clock low
        I2CDelay();
        
        LDA ZP.ACCL          // Return ACK state
        if (Z)
        {
            CLC              // ACK received (SDA was low)
        }
        else
        {
            SEC              // NACK received (SDA was high)
        }
    }
    
    // Print hex byte
    // Input: A = byte to print
    PrintHex()
    {
        STA ZP.ACCL
        
        // Print upper nibble
        LSR
        LSR
        LSR
        LSR
        CMP #10
        if (C)  // >= 10
        {
            CLC
            ADC #('A' - 10)
        }
        else
        {
            CLC
            ADC #'0'
        }
        Serial.WriteChar();
        
        // Print lower nibble
        LDA ZP.ACCL
        AND #0x0F
        CMP #10
        if (C)  // >= 10
        {
            CLC
            ADC #('A' - 10)
        }
        else
        {
            CLC
            ADC #'0'
        }
        Serial.WriteChar();
    }
    
    // Print string
    // Input: X = string address low, Y = string address high
    PrintString()
    {
        STX ZP.IDXL
        STY ZP.IDXH
        LDY #0
        
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) { break; }  // Null terminator
            Serial.WriteChar();
            INY
            if (Z)
            {
                INC ZP.IDXH    // Handle page boundary
            }
        }
    }
    
    // Scan for I2C devices
    ScanI2C()
    {
        LDX # (msgScanning % 256)
        LDY # (msgScanning / 256)
        PrintString();
        
        LDA #0x08        // Start at address 0x08 (skip reserved addresses)
        STA ZP.W0        // Current address
        
        loop
        {
            // Try to contact device at current address
            I2CStart();
            
            LDA ZP.W0
            ASL A           // Shift address left, add write bit (0)
            I2CWriteByte();
            
            I2CStop();
            
            if (NC)  // If ACK received
            {
                LDX # (msgFound % 256)
                LDY # (msgFound / 256)
                PrintString();
                
                LDA ZP.W0
                PrintHex();
                
                LDA #'\r'
                Serial.WriteChar();
                LDA #'\n'
                Serial.WriteChar();
            }
            
            INC ZP.W0
            LDA ZP.W0
            CMP #0x78       // Stop at 0x77 (skip reserved high addresses)
            if (Z)
            {
                break;
            }
        }
        
        LDX # (msgDone % 256)
        LDY # (msgDone / 256)
        PrintString();
    }
    
    // Main program entry point
    Hopper()
    {
        Serial.Initialize();
        
        LDX # ( msgTitle % 256)
        LDY # ( msgTitle / 256)
        PrintString();
        
        InitializePIA();
        ScanI2C();
        
        // Infinite loop
        loop
        {
            Serial.WaitForChar();  // Wait for any key to rescan
            CMP #0x03
            if (Z)
            {
                break;
            }
            
            ScanI2C();
        }
    }
}


