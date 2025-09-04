I'll search through the project knowledge to find all public functions (those starting with uppercase letters) in the Hopper Assembly code.I'll search through the project knowledge to compile a list of all public functions (those starting with uppercase letters).Based on my search through the project knowledge, here's a comprehensive list of all public functions (those starting with uppercase letters) in the Hopper Assembly codebase, organized by module:

## **Memory Management**

### Memory.asm
- `Initialize()` - Initialize heap management
- `Allocate()` - Allocate memory block
- `Free()` - Free memory block
- `Available()` - Get available free space
- `ClearPage()` - Clear a 256-byte page
- `ClearPages()` - Clear multiple pages
- `Copy()` - Copy memory block

### Allocate.asm
- `Allocate()` - Internal allocator (called by Memory)

### Free.asm
- `Free()` - Internal free (called by Memory)

## **File System**

### File.asm
- `ValidateFilename()` - Validate filename format
- `GetAvailable()` - Get available EEPROM space
- `Format()` - Format EEPROM filesystem
- `StartSave()` - Start file save operation
- `AppendStream()` - Write data chunk to file
- `EndSave()` - Finalize file save
- `Dir()` - List directory contents
- `Delete()` - Delete file from EEPROM
- `Exists()` - Check if file exists
- `StartLoad()` - Open file for reading
- `NextStream()` - Read next chunk from file
- `GetFileLength()` - Get file size
- `DumpDriveState()` - Debug dump of drive state (debug builds)
- `DumpFileState()` - Debug dump of file state (debug builds)

## **Storage**

### EEPROM.asm
- `Initialize()` - Initialize EEPROM
- `Detect()` - Test if EEPROM present
- `GetSize()` - Get EEPROM size
- `WritePage()` - Write 256-byte page to EEPROM
- `ReadPage()` - Read 256-byte page from EEPROM
- `RequestFromTOPA()` - Request bytes from EEPROM

## **I/O & Communication**

### Serial.asm
- `Initialize()` - Initialize serial port
- `EmptyTheBuffer()` - Clear serial buffer
- `ISR()` - Interrupt service routine
- `IsAvailable()` - Check for available data
- `WaitForChar()` - Wait and read character
- `WriteChar()` - Transmit character
- `HexOut()` - Output byte as hex

### I2C.asm
- `Scan()` - Scan for I2C device
- `BeginTx()` - Start I2C transmission
- `BeginRx()` - Start I2C reception
- `EndTx()` - End transmission
- `Start()` - I2C start condition
- `Stop()` - I2C stop condition
- `ByteOut()` - Send byte
- `ByteIn()` - Receive byte
- `RequestFromTOPA()` - Request data from I2C device

### Parallel.asm
- `Initialize()` - Initialize parallel port
- `ISR()` - Interrupt service routine

### VIA65C22.asm (friend of Parallel and Time)
- Functions are private (friend access only)

## **Types & Data Operations**

### Char.asm
- `IsDigit()` - Check if digit
- `IsAlpha()` - Check if letter
- `IsAlphaNumeric()` - Check if alphanumeric
- `IsHexDigit()` - Check if hex digit
- `IsLower()` - Check if lowercase
- `IsUpper()` - Check if uppercase
- `IsPrintable()` - Check if printable
- `ToLower()` - Convert to lowercase
- `ToUpper()` - Convert to uppercase

### String.asm
- `Length()` - Get string length
- `Compare()` - Compare two strings
- `ToUpperSTR()` - Convert string to uppercase

### Long.asm
- `New()` - Create new long
- `NewFromConstant()` - Create from constant
- `FromBytes()` - Create from bytes
- `GetBytes()` - Get bytes
- `PushNext()` - Push to stack
- `Print()` - Print long value
- `Add()` - Add longs
- `Sub()` - Subtract longs
- `Mul()` - Multiply longs
- `DivMod()` - Divide with remainder
- `EQ()` - Equality comparison
- `LT()` - Less than comparison
- `GT()` - Greater than comparison
- `LE()` - Less than or equal
- `GE()` - Greater than or equal
- `NegateLongTOP()` - Negate TOP value

### Float.asm
- `New()` - Create new float
- `NewFromConstant()` - Create from constant
- `FromBytes()` - Create from bytes
- `GetByte()` - Get byte value
- `Add()` - Add floats
- `Sub()` - Subtract floats
- `Mul()` - Multiply floats
- `Div()` - Divide floats
- `ToLong()` - Convert to long

## **Time & Timing**

### Time.asm
- `Delay()` - Delay in milliseconds
- `Millis()` - Get millisecond counter
- `Seconds()` - Get seconds

## **Printing & Output**

### Print.asm
- `String()` - Print string
- `Char()` - Print character
- `Hex()` - Print hex byte
- `NewLine()` - Print newline
- `Space()` - Print space
- `Spaces()` - Print multiple spaces
- `Decimal()` - Print decimal number

## **Utilities**

### Shared.asm
- `IncIDX()` - Increment IDX pointer
- `IncIDY()` - Increment IDY pointer
- `DecIDY()` - Decrement IDY pointer
- `IncACC()` - Increment ACC
- `DecACCx2()` - Decrement ACC by 2

## **Debugging**

### Debug.asm
- `COut()` - Character output (preserves state)
- `NL()` - Newline (preserves state)
- `Space()` - Space (preserves state)
- `Printable()` - Print if printable
- `HexOut()` - Hex output (preserves state)
- `DumpZeroPage()` - Dump zero page
- `DumpHeap()` - Dump heap contents
- `DumpMemory()` - Dump memory range
- `ValidateHeap()` - Validate heap integrity
- `Crash()` - Crash handler

## **Error Handling**

### Error.asm (inferred from usage)
- `FileNotFound()`
- `FilenameExpected()`
- `IllegalFilename()`
- `FilenameTooLong()`
- `DirectoryFull()`
- `EEPROMFull()`
- `EEPROMError()`
- `OutOfMemory()`
- `HeapCorruptError()`
- `ThrowError()`
- `Message()`
- `MessageNL()`

## **Main Program Entry**

### BIOS.asm (program unit)
- `IRQ()` - Interrupt request handler
- `NMI()` - Non-maskable interrupt handler
- `Initialize()` - System initialization
- `Hopper()` - Main entry point

This comprehensive list includes all public functions (starting with uppercase) found in the Hopper Assembly project. The functions are organized by their respective modules/units for easy reference.