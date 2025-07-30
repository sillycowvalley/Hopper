// API Reference - Complete Implementations
// 
// All methods listed in this file are fully implemented and thoroughly tested.
// The actual source code has been omitted from this reference to reduce the 
// volume of code in the project knowledge base while maintaining API visibility.
//
// These are working implementations, not placeholder stubs - the term "stubs" 
// refers only to the abbreviated documentation format, not the implementation status.
//
// For debugging or modification needs, refer to the actual source files which
// contain the complete, tested implementations with full error handling and
// proper register preservation as documented in the API contracts below.

unit SerialDevice
{
    // API Status: Clean
    // Device-specific ACIA 6850 implementation
    // All public methods preserve caller state except for documented outputs
    
    // Motorola 6850
    
    const byte ControlRegister    = ZP.ACIACONTROL;
    const byte DataRegister       = ZP.ACIADATA;
    const byte StatusRegister     = ZP.ACIASTATUS;
    
}

unit Serial
{
    // API Status: Clean
    // Serial communication interface
    // All public methods preserve caller state except for documented outputs
    
    // Zero Page locations used by Serial:
    const byte InWritePointer        = ZP.SerialInWritePointer;
    const byte InReadPointer         = ZP.SerialInReadPointer;
    const byte BreakFlag             = ZP.SerialBreakFlag;
    const byte WorkSpaceHexIn        = ZP.W0;
    const byte WorkSpaceWaitForChar  = ZP.W1;
    
    // Location of the Serial input buffer (256 bytes)
    const uint InBuffer        = Address.SerialInBuffer;

    // Initialize or reset the serial ACIA firmware
    // Output: Serial buffers reset, device initialized
    // Modifies: InWritePointer, InReadPointer, BreakFlag (all zeroed)
    Initialize()
    
    // Handle serial interrupt service routine
    // Input: Called from interrupt context
    // Output: Delegates to SerialDevice.isr()
    ISR()
    
    // Empty the serial input buffer
    // Output: All buffered characters consumed and discarded
    EmptyTheBuffer()
    
    // Check if character is available in buffer
    // Output: Z flag clear if character available, Z set if buffer empty
    // Modifies: Processor flags only
    IsAvailable()
    
    // Wait for and consume next character from buffer
    // Output: A = next character from buffer (0x03 if break/Ctrl+C)
    // Modifies: A register, InReadPointer, BreakFlag
    WaitForChar()
       
    // Transmit character via serial
    // Input: A = character to transmit
    // Output: Character sent via serial device
    WriteChar()
    
    // Transmit byte as two hex characters
    // Input: A = byte value to output as hex
    // Output: Two hex characters transmitted (e.g., 0xFF outputs "FF")
    HexOut()
    
    // Read two hex characters from serial and convert to byte
    // Output: A = byte value from two hex characters
    // Modifies: A register, WorkSpaceHexIn
    HexIn()
}

unit Stacks
{
    // API Status: Clean
    // Stack management for value, type, and call stacks
    // All public methods preserve caller state except for documented outputs
    
    // Initialize all stacks
    // Output: SP, BP, CSP reset to zero, stack memory cleared
    // Modifies: ZP.SP, ZP.BP, ZP.CSP, stack memory pages
    Initialize()
    
    // Pop base pointer from call stack
    // Output: ZP.BP = popped value
    // Modifies: ZP.BP, ZP.CSP
    PopBP()
    
    // Push base pointer to call stack
    // Input: ZP.BP = value to push
    // Output: BP pushed to call stack
    // Modifies: ZP.CSP
    PushBP()
    
    // Pop program counter from call stack
    // Output: ZP.PC = popped 16-bit value
    // Modifies: ZP.PCL, ZP.PCH, ZP.CSP
    PopPC()
    
    // Push program counter to call stack
    // Input: ZP.PC = 16-bit value to push
    // Output: PC pushed to call stack
    // Modifies: ZP.CSP
    PushPC()
    
    // Pop typed value from value/type stacks
    // Output: ZP.TOP = popped value, ZP.TOPT = popped type
    // Modifies: ZP.TOPL, ZP.TOPH, ZP.TOPT, ZP.SP
    PopTop()
    
    // Push typed value to value/type stacks
    // Input: A = type, ZP.TOP = 16-bit value
    // Output: Value and type pushed to stacks
    // Modifies: ZP.SP
    PushTop()
    
    // Push IDX register with type to stacks
    // Input: A = type, ZP.IDX = 16-bit value
    // Output: IDX value and type pushed to stacks
    // Modifies: ZP.SP
    PushIDX()
    
    // Pop typed value to NEXT register
    // Output: ZP.NEXT = popped value, ZP.NEXTT = popped type
    // Modifies: ZP.NEXTL, ZP.NEXTH, ZP.NEXTT, ZP.SP
    PopNext()
    
    // Pop two typed values (TOP then NEXT)
    // Output: ZP.TOP and ZP.NEXT = popped values with types
    // Modifies: ZP.TOP*, ZP.NEXT*, ZP.SP
    PopTopNext()
    
    // Push NEXT register with type to stacks
    // Input: A = type, ZP.NEXT = 16-bit value
    // Output: NEXT value and type pushed to stacks
    // Modifies: ZP.SP
    PushNext()
    
    // Push ACC register to value/type stacks
    // Input: ZP.ACC = value, ZP.ACCT = type
    // Output: ACC value and type pushed to stacks
    // Modifies: ZP.SP
    PushACC()
    
    // Push X register as boolean value
    // Input: X = 0 or 1
    // Output: X value pushed as Types.Bool
    // Modifies: ZP.NEXT*, ZP.SP
    PushX()
    
    // Pop value to ACC register (type ignored)
    // Output: ZP.ACC = popped 16-bit value
    // Modifies: ZP.ACCL, ZP.ACCH, ZP.SP
    PopACC()
    
    // Pop value to IDX register
    // Output: ZP.IDX = popped 16-bit value
    // Modifies: ZP.IDXL, ZP.IDXH, ZP.SP
    PopIDX()
    
    // Pop value to IDY register
    // Output: ZP.IDY = popped 16-bit value
    // Modifies: ZP.IDYL, ZP.IDYH, ZP.SP
    PopIDY()
    
    // Pop single byte value
    // Output: A = popped byte (LSB only)
    // Modifies: A register, ZP.SP
    PopA()
}

unit Memory
{
    // API Status: Clean
    // Memory management and heap operations
    // All public methods preserve caller state except for documented outputs
    // Uses ZP.M* scratch space for memory management (internal operations only)
    
    // Initialize heap management system
    // Input: ZP.PROGSIZE = program size in pages
    // Output: Heap initialized with free list, memory cleared
    // Modifies: ZP.HEAPSTART, ZP.HEAPSIZE, ZP.FREELIST, heap memory
    InitializeHeapSize()
    
    // Allocate memory block
    // Input: ZP.ACC = requested size (16-bit) 
    // Output: ZP.IDX = allocated address (0x0000 if allocation failed)
    // Modifies: ZP.M* scratch space (internal to memory management operations)
    Allocate()
    
    // Free memory block
    // Input: ZP.IDX = address to free (must not be 0x0000)
    // Output: C set (success)
    // Modifies: ZP.M* scratch space (internal to memory management operations)
    Free()
    
    // Calculate total available free memory
    // Output: ZP.ACC = total free bytes available
    // Modifies: ZP.ACC, ZP.IDX (internal traversal)
    AvailableACC()
    
    // Get total available memory on stack
    // Output: Available memory pushed to stack as Types.UInt
    Available()
    
    // Calculate largest single allocatable block
    // Output: ZP.ACC = size of largest free block
    // Modifies: ZP.ACC, ZP.IDX, ZP.IDY (internal traversal)
    MaximumACC()
    
    // Get largest allocatable block size on stack
    // Output: Largest block size pushed to stack as Types.UInt
    Maximum()
}

unit I2C
{
    // API Status: Clean
    // I2C communication protocol implementation
    // All public methods preserve caller state except for documented outputs
    
    const byte SSD1306Address      = 0x3C;
    const byte SerialEEPROMAddress = 0x50;
    
    const byte SCL     = 0b00000001;    // DRB0 bitmask
    const byte SCL_INV = 0b11111110;    //   inverted for easy clear bit
    const byte SDA     = 0b00000010;    // DRB1 bitmask
    const byte SDA_INV = 0b11111101;    //   inverted for easy clear bit
      
    // Scan for I2C device at address
    // Input: A = I2C device address
    // Output: Z flag set if device found, ZP.LastAck contains ACK status
    Scan()
    
    // Begin I2C transmission to device
    // Input: Stack top = I2C device address
    // Output: I2C start condition sent, device address transmitted
    BeginTx()
    
    // Begin I2C read from device
    // Input: Stack top = I2C device address
    // Output: I2C start condition sent, device address with read bit transmitted
    BeginRx()
    
    // End I2C transmission
    // Output: I2C stop condition sent, ACK status pushed to stack as Types.Bool
    EndTx()
    
    // Generate I2C start condition
    // Output: I2C start condition on bus, device address transmitted
    // Modifies: I2C bus state, ZP.LastAck
    Start()
    
    // Generate I2C stop condition
    // Output: I2C stop condition on bus
    // Modifies: I2C bus state
    Stop()
        
    // Write byte to I2C bus
    // Input: Stack top = byte to send
    // Output: Byte transmitted via I2C
    Write()
    
    // Request bytes from I2C device
    // Input: Stack top = bytes to read (0..255), Stack next = I2C address
    // Output: Bytes read pushed to stack as Types.Byte count
    RequestFrom()
    
    // Request bytes using TOP/NEXT registers
    // Input: ZP.NEXTL = I2C address, ZP.TOPL = bytes to read
    // Output: ZP.TOPL = actual bytes read, data in I2C buffer
    // Modifies: ZP.I2CInWritePtr, ZP.I2CInReadPtr, I2C buffer
    RequestFromTOPNEXT()
    
    // Read byte from I2C buffer
    // Output: Next buffered byte pushed to stack as Types.Byte (0 if buffer empty)
    Read()
     
    // Transmit byte via I2C protocol
    // Input: ZP.OutB = byte to transmit
    // Output: Byte transmitted, ZP.LastAck = ACK/NACK status
    // Modifies: I2C bus state, ZP.LastAck, ZP.OutB (cleared)
    ByteOut()
}

unit IntMath
{
    // API Status: Clean
    // Integer mathematics operations
    // All public methods preserve caller state except for documented outputs
    
    // Shared multiplication core
    // Input: ZP.NEXT = multiplicand, ZP.TOP = multiplier
    // Output: ZP.TOP = product, ZP.TOPT = Types.UInt
    // Modifies: ZP.TOP, ZP.TOPT, internal scratch space
    MulShared()
    
    // Core division routine
    // Input: ZP.NEXT = dividend, ZP.TOP = divisor
    // Output: ZP.NEXT = quotient
    // Modifies: ZP.NEXT, internal scratch space
    UtilityDiv()
    
    // Division with remainder
    // Input: ZP.NEXT = dividend, ZP.TOP = divisor
    // Output: ZP.NEXT = quotient, ZP.ACC = remainder
    // Modifies: ZP.NEXT, ZP.ACC, internal scratch space
    DivMod()
    
    // Negate TOP register
    // Input: ZP.TOP = value
    // Output: ZP.TOP = -value (two's complement)
    // Modifies: ZP.TOP
    NegateTop()
    
    // Negate NEXT register
    // Input: ZP.NEXT = value  
    // Output: ZP.NEXT = -value (two's complement)
    // Modifies: ZP.NEXT
    NegateNext()
    
    // Unsigned multiplication
    // Input: Two values on stack
    // Output: Product pushed to stack as Types.UInt
    Mul()
    
    // Signed multiplication
    // Input: Two signed values on stack
    // Output: Signed product pushed to stack as Types.Int
    MulI()
    
    // Unsigned division
    // Input: Two values on stack (dividend, divisor)
    // Output: Quotient pushed to stack as Types.UInt
    Div()
    
    // Unsigned modulo
    // Input: Two values on stack (dividend, divisor)
    // Output: Remainder pushed to stack as Types.UInt
    Mod()
    
    // Signed division
    // Input: Two signed values on stack (dividend, divisor)
    // Output: Signed quotient pushed to stack as Types.Int
    DivI()
    
    // Signed modulo
    // Input: Two signed values on stack (dividend, divisor)
    // Output: Remainder pushed to stack as Types.Int (always positive)
    ModI()
    
    // Convert unsigned to signed integer
    // Input: UInt value on stack
    // Output: Same value pushed as Types.Int
    // Modifies: Stack type only
    UIntToInt()
}

unit Utilities
{
    // API Status: Clean
    // General utility functions
    // All public methods preserve caller state except for documented outputs
    
    const byte Slash  = 0x5C;
    const byte Escape = 0x1B;
    const byte Enter  = 0x0A;
    
    // Convert ASCII hex character to 4-bit value
    // Input: A = ASCII character ('0'-'9', 'A'-'F')
    // Output: A = 4-bit value (0x00-0x0F)
    // Modifies: A register only
    MakeNibble()
    
    // Send backslash character via serial
    // Output: '\' character transmitted
    SendSlash()
    
    // Wait for Enter key press
    // Output: Waits until Enter received, sends '\' acknowledgment
    WaitForEnter()
    
    // Clear multiple pages of memory
    // Input: ZP.IDX = start address, X = number of pages
    // Output: Memory cleared to zero
    // Modifies: A, X, Y, ZP.IDXH (ZP.IDXL preserved)
    ClearPages()
    
    // Increment IDX register (16-bit)
    // Input: ZP.IDX = current value
    // Output: ZP.IDX = incremented value
    // Modifies: ZP.IDX
    IncIDX()
    
    // Increment IDY register (16-bit)
    // Input: ZP.IDY = current value
    // Output: ZP.IDY = incremented value
    // Modifies: ZP.IDY
    IncIDY()
    
    // Decrement IDY register (16-bit)
    // Input: ZP.IDY = current value
    // Output: ZP.IDY = decremented value
    // Modifies: ZP.IDY
    DecIDY()
    
    // Increment PC register (16-bit)
    // Input: ZP.PC = current value
    // Output: ZP.PC = incremented value
    // Modifies: ZP.PC
    IncPC()
    
    // Increment ACC register (16-bit)
    // Input: ZP.ACC = current value
    // Output: ZP.ACC = incremented value
    // Modifies: ZP.ACC
    IncACC()
    
    // Decrement ACC register by 2
    // Input: ZP.ACC = current value
    // Output: ZP.ACC = value - 2
    // Modifies: ZP.ACC
    DecACCx2()
    
    // Increment source address register
    // Input: ZP.FSOURCEADDRESS = current address
    // Output: ZP.FSOURCEADDRESS = incremented address
    // Modifies: ZP.FSOURCEADDRESS
    IncSOURCEADDRESS()
    
    // Increment destination address register
    // Input: ZP.FDESTINATIONADDRESS = current address
    // Output: ZP.FDESTINATIONADDRESS = incremented address
    // Modifies: ZP.FDESTINATIONADDRESS
    IncDESTINATIONADDRESS()
    
    // Increment length register
    // Input: ZP.FLENGTH = current value
    // Output: ZP.FLENGTH = incremented value
    // Modifies: ZP.FLENGTH
    IncLENGTH()
    
    // Increment size register
    // Input: ZP.FSIZE = current value
    // Output: ZP.FSIZE = incremented value
    // Modifies: ZP.FSIZE
    IncSIZE()
    
    // Decrement source address register
    // Input: ZP.FSOURCEADDRESS = current address
    // Output: ZP.FSOURCEADDRESS = decremented address
    // Modifies: ZP.FSOURCEADDRESS
    DecSOURCEADDRESS()
    
    // Copy bytes from source to destination
    // Input: ZP.LCOUNT = byte count, ZP.FSOURCEADDRESS = source, ZP.FDESTINATIONADDRESS = destination
    // Output: Bytes copied from source to destination
    // Modifies: ZP.LCOUNT, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, A, Y
    CopyBytes()
}

unit SerialEEPROM
{
    // API Status: Clean
    // Serial EEPROM interface for program storage and retrieval
    // All public methods preserve caller state except for documented outputs
    // No accidental side effects or register corruption
    // Uses ZP.IDX/ZP.IDY for address management and ZP.TOP for timing control

    // Hardware configuration - EEPROM chip page size selection
    // Different EEPROM chips have different page sizes for write operations
#ifdef SERIAL128BYTEPAGES
    const byte serialPageSize = 128;  // Large page EEPROMs (24AA512, 24AA1026)
#endif    
#ifdef SERIAL64BYTEPAGES
    const byte serialPageSize = 64;   // Smaller page EEPROMs (24C256)
#endif    
    
    // Copy one EEPROM page (128 or 64 bytes) to RAM
    // Input: ZP.IDY = EEPROM source address (16-bit)
    //        ZP.IDX = RAM destination address (16-bit)
    // Output: One page copied from EEPROM to RAM
    //         ZP.IDY advanced by serialPageSize bytes
    //         ZP.IDX advanced by serialPageSize bytes
    // Modifies: ZP.OutB (I2C operations), ZP.TOP (timing delay), X (byte counter)
    //          ZP.NEXTL (I2C address), ZP.TOPL (byte count)
    copyProgramPage()
    
    // Load complete Hopper program from Serial EEPROM to RAM
    // Input: ZP.PROGSIZE = program size in 256-byte pages
    // Output: Program loaded from EEPROM to HopperData address
    //         ZP.IDX = final destination address (after last copied byte)
    //         ZP.IDY = final EEPROM address (after last read byte)
    // Modifies: ZP.IDX, ZP.IDY, Y (page counter), all registers used by copyProgramPage()
    LoadFromEEPROM()
    
    // Copy one RAM page (serialPageSize bytes) to EEPROM
    // Input: ZP.IDX = RAM source address (16-bit)
    //        ZP.IDY = EEPROM destination address (16-bit)
    // Output: One page copied from RAM to EEPROM
    //         ZP.IDX advanced by serialPageSize bytes
    //         ZP.IDY advanced by serialPageSize bytes
    // Modifies: ZP.OutB (I2C operations), ZP.TOP (timing delay), X (byte counter)
    //          A, X, Y (preserved via stack)
    copyPageToEEPROM()
    
    // Save complete Hopper program from RAM to Serial EEPROM
    // Input: ZP.PROGSIZE = program size in 256-byte pages
    // Output: Program saved from HopperData address to EEPROM
    //         ZP.IDX = final source address (after last copied byte)
    //         ZP.IDY = final EEPROM address (after last written byte)
    // Modifies: ZP.IDX, ZP.IDY, X (page counter), all registers used by copyPageToEEPROM()
    SaveToEEPROM()
    
    // Write single 256-byte page from RAM to EEPROM
    // Input: ZP.IDX = RAM source address (16-bit, page-aligned recommended)
    //        ZP.IDY = EEPROM destination address (16-bit, page-aligned recommended)
    // Output: 256 bytes copied from RAM to EEPROM
    //         ZP.IDX advanced by 256 bytes
    //         ZP.IDY advanced by 256 bytes
    // Modifies: All registers used by copyPageToEEPROM()
    WritePage()
    
    // Read single 256-byte page from EEPROM to RAM
    // Input: ZP.IDY = EEPROM source address (16-bit, page-aligned recommended)
    //        ZP.IDX = RAM destination address (16-bit, page-aligned recommended)
    // Output: 256 bytes copied from EEPROM to RAM
    //         ZP.IDY advanced by 256 bytes
    //         ZP.IDX advanced by 256 bytes
    // Modifies: All registers used by copyProgramPage()
    ReadPage()
}