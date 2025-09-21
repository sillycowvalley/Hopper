unit BIOSInterface
{
    const uint SerialInBuffer       = 0x0500;  // 256-byte buffer 0x0200-0x02FF
    const uint WorkSpace            = 0x0700;  // 256 byte general workspace
    const uint LineBuffer           = WorkSpace; // first 64 bytes (of the 256) for the command line parser
    const uint EntryPoint           = 0x0B00;
    
    // System Call IDs - shared enum for client compilation
    // Calling convention: X = SysCall ID, additional params vary by call
    // Returns: C set on success, clear on error (unless noted)
    
    enum SysCall
    {
        // Memory Management
        MemAllocate,      // In: ZP.ACC = size (16-bit) | Out: ZP.IDX = address, C = success | Allocates memory block
        MemFree,          // In: ZP.IDX = address | Out: C = success | Frees memory block  
        MemAvailable,     // In: None | Out: ZP.ACC = free bytes | Returns available memory
        MemMaximum,       // In: None | Out: ZP.ACC = largest block size | Returns largest contiguous block
        
        // File Operations (requires EEPROM)
        FileExists,       // In: ZP.STR = filename, A = DirWalkAction | Out: C = exists | Checks if file exists
        FileDelete,       // In: ZP.STR = filename | Out: C = success | Deletes file from EEPROM
        FileDir,          // In: None | Out: C = success (prints to serial) | Lists directory contents
        FileStartSave,    // In: ZP.STR = filename | Out: C = success | Opens file for writing
        FileAppendStream, // In: SectorSource = data ptr, TransferLength = bytes | Out: C = success | Writes data chunk
        FileEndSave,      // In: A = 0x80 (executable) or 0x00 (data) | Out: C = success | Closes and finalizes file
        FileStartLoad,    // In: ZP.STR = filename, A = DirWalkAction | Out: C = success | Opens file for reading
        FileNextStream,   // In: None | Out: C = data available, TransferLength = bytes, data in FileDataBuffer | Reads next chunk
        FileFormat,       // In: None | Out: C = success | Formats EEPROM filesystem
        
        // Serial I/O
        SerialWriteChar,  // In: A = character | Out: None | Writes character to serial port
        SerialWaitForChar,// In: None | Out: A = character | Blocks until character available
        SerialIsAvailable,// In: None | Out: C = available | Checks if serial input available
        
        IsBreak,          // In: None | Out: C = break detected | Checks for Ctrl+C/NMI break
        
        // Print/Console
        PrintString,      // In: ZP.STR = string pointer | Out: None | Prints null-terminated string
        PrintChar,        // In: A = character | Out: None | Prints single character
        PrintHex,         // In: A = byte value | Out: None | Prints byte as 2 hex digits
        PrintNewLine,     // In: None | Out: None | Prints newline character
        PrintSpace,       // In: None | Out: None | Prints single space
        PrintSpaces,      // In: Y = count | Out: None | Prints Y spaces
        
        // Timer Services
        TimeDelay,        // In: ZP.TOP = milliseconds (32-bit) | Out: None | Delays execution
        TimeMillis,       // In: None | Out: ZP.TOP = ms since boot (32-bit) | Returns millisecond counter
        TimeSeconds,      // In: None | Out: ZP.TOP = seconds (32-bit) | Returns seconds since boot
        
        // Long Math (32-bit integer operations)
        LongAdd,          // In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT = NEXT + TOP | 32-bit addition
        LongSub,          // In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT = NEXT - TOP | 32-bit subtraction
        LongMul,          // In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT = NEXT * TOP | 32-bit multiplication
        LongDiv,          // In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT = NEXT / TOP | 32-bit division
        LongMod,          // In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT = NEXT % TOP | 32-bit modulo
        LongPrint,        // In: ZP.TOP = value | Out: None (prints to serial) | Prints 32-bit decimal
        LongLT,           // In: ZP.NEXT, ZP.TOP | Out: C = (NEXT < TOP) | Less than comparison
        LongGT,           // In: ZP.NEXT, ZP.TOP | Out: C = (NEXT > TOP) | Greater than comparison
        LongEQ,           // In: ZP.NEXT, ZP.TOP | Out: C = (NEXT == TOP) | Equality comparison
        LongNE,           // In: ZP.NEXT, ZP.TOP | Out: C = (NEXT != TOP) | Not equal comparison
        LongLE,           // In: ZP.NEXT, ZP.TOP | Out: C = (NEXT <= TOP) | Less or equal comparison
        LongGE,           // In: ZP.NEXT, ZP.TOP | Out: C = (NEXT >= TOP) | Greater or equal comparison
        
        // GPIO (uses VIA 65C22 ports A and B)
        PinMode,          // In: A = pin (0-15), Y = mode (0=INPUT, 1=OUTPUT) | Out: None | Configure pin direction
        PinRead,          // In: A = pin (0-15) | Out: A = value (0/1), Z = LOW | Read digital pin state
        PinWrite,         // In: A = pin (0-15), Y = value (0/1) | Out: None | Write digital pin state
        
        // Optionals last:
        
        // Float Math (IEEE 754 single precision, requires HASFLOAT)
        FloatAdd,         // In: ZP.NEXT, ZP.TOP (IEEE floats) | Out: ZP.NEXT = NEXT + TOP | Float addition
        FloatSub,         // In: ZP.NEXT, ZP.TOP (IEEE floats) | Out: ZP.NEXT = NEXT - TOP | Float subtraction
        FloatMul,         // In: ZP.NEXT, ZP.TOP (IEEE floats) | Out: ZP.NEXT = NEXT * TOP | Float multiplication
        FloatDiv,         // In: ZP.NEXT, ZP.TOP (IEEE floats) | Out: ZP.NEXT = NEXT / TOP | Float division
        FloatToLong,      // In: ZP.NEXT (IEEE float) | Out: ZP.NEXT = (long)NEXT | Convert float to long
        FloatLT,          // In: ZP.NEXT, ZP.TOP (IEEE floats) | Out: C = (NEXT < TOP) | Float less than
        FloatEQ,          // In: ZP.NEXT, ZP.TOP (IEEE floats) | Out: C = (NEXT == TOP) | Float equality
        
         FOpen,            // In: STR=filename, NEXT=mode("w"/"r") | Out: TOP=FILE*/NULL
        FClose,           // In: NEXT=FILE* | Out: TOP=0/-1
        FGetC,            // In: NEXT=FILE* | Out: TOP=char(0-255)/-1
        FRead,            // In: IDX=buffer, IDY = element size, ACC= element count, NEXT=FILE* | Out: TOP=bytes read/-1 (not elements read)
        FPutC,            // In: ACC=char(0-255), NEXT=FILE* | Out: TOP=char written/-1
        FWrite,           // In: IDX=buffer, IDY=element size, ACC=element count, NEXT=FILE* | Out: TOP=bytes written/-1 (not elements written)
        
        // TODO:
        // - I2C
    }
}
