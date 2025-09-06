unit BIOSInterface
{
    // System Call IDs - shared enum for client compilation
    enum SysCall
    {
        // Memory Management
        MemAllocate,
        MemFree, 
        MemAvailable,
        MemMaximum,
        
        // File Operations
        FileExists,
        FileDelete,
        FileDir,
        FileStartSave,
        FileAppendStream,
        FileEndSave,
        FileStartLoad,
        FileNextStream,
        FileFormat,
        
        // Serial I/O
        SerialWriteChar,
        SerialWaitForChar,
        SerialIsAvailable,
        
        IsBreak, // NMI
        
        // Print/Console
        PrintString,
        PrintChar,
        PrintHex,
        PrintNewLine,
        PrintSpace,
        PrintSpaces,
        
        // Timer Services
        TimeDelay,
        TimeMillis,
        TimeSeconds,
        
        // Long Math
        LongAdd,
        LongSub,
        LongMul,
        LongDiv,
        LongMod,
        LongPrint,
        LongLT,
        LongGT,
        LongEQ,
        LongNE,
        LongLE,
        LongGE,
        
        // Float Math
        FloatAdd,
        FloatSub,
        FloatMul,
        FloatDiv,
        FloatToLong,
        FloatLT,
        FloatEQ,
        
        // GPIO
        PinMode,
        PinRead,
        PinWrite,
        
        // TODO:
        // - I2C
    }
}
