# HopperBASIC File System External API Requirements
**Document Type: API Specification**

## Overview

This document specifies all external APIs required by the HopperBASIC file system. By implementing these utilities outside the file system, we can significantly reduce the complexity of the core file system code and leverage existing HopperBASIC infrastructure.

---

## Character Validation APIs

### **Character Type Checking**
```hopper
// Check if character is alphabetic (A-Z, a-z)
// Input: A = character to test
// Output: C set if alphabetic, NC if not
// Preserves: X, Y
IsAlpha()

// Check if character is numeric (0-9)  
// Input: A = character to test
// Output: C set if numeric, NC if not
// Preserves: X, Y
IsDigit()

// Check if character is alphanumeric (A-Z, a-z, 0-9)
// Input: A = character to test
// Output: C set if alphanumeric, NC if not
// Preserves: X, Y
IsAlphaNumeric()

// Check if character is valid for filename (alphanumeric + period)
// Input: A = character to test
// Output: C set if valid, NC if invalid
// Preserves: X, Y
IsValidFilenameChar()
```

### **Character Conversion**
```hopper
// Convert character to lowercase
// Input: A = character to convert
// Output: A = lowercase character (unchanged if not alphabetic)
// Preserves: X, Y, flags
ToLowercase()

// Convert character to uppercase  
// Input: A = character to convert
// Output: A = uppercase character (unchanged if not alphabetic)
// Preserves: X, Y, flags
ToUppercase()
```

---

## String Manipulation APIs

### **String Properties**
```hopper
// Calculate string length
// Input: ZP.IDXL/H = pointer to null-terminated string
// Output: ZP.ACCL/H = string length (16-bit)
// Preserves: X, Y
// Munts: A, ZP.ACC
StringLength()

// Validate string as filename (length 1-12, valid characters)
// Input: ZP.IDXL/H = pointer to null-terminated string
// Output: C set if valid filename, NC if invalid
//         ZP.ACCL = actual string length
// Preserves: X, Y
// Munts: A, ZP.ACC, ZP.TOP
ValidateFilename()
```

### **String Operations**
```hopper
// Copy null-terminated string
// Input: ZP.IDXL/H = source string pointer
//        ZP.IDYL/H = destination buffer pointer
// Output: C set if successful, NC if failed
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT
StringCopy()

// Compare two strings (case-insensitive, length-limited)
// Input: ZP.IDXL/H = string1 pointer
//        ZP.IDYL/H = string2 pointer  
//        ZP.ACCL = maximum length to compare
// Output: Z set if equal, NZ if different
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT
StringCompare()

// Clear memory block
// Input: ZP.IDXL/H = destination pointer
//        ZP.ACCL/H = number of bytes to clear
// Output: Memory cleared to zeros
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT
MemoryClear()

// Copy memory block
// Input: ZP.FSOURCEADDRESSL/H = source pointer
//        ZP.FDESTINATIONADDRESSL/H = destination pointer
//        ZP.FLENGTHL/H = number of bytes to copy
// Output: Memory copied
// Preserves: X, Y  
// Munts: A, ZP.TOP, ZP.NEXT
MemoryCopy()
```

---

## Memory Management APIs

### **Dynamic Allocation**
```hopper
// Allocate memory block
// Input: ZP.ACCL/H = number of bytes to allocate
// Output: ZP.IDXL/H = pointer to allocated memory (0x0000 if failed)
//         C set if successful, NC if failed
// Preserves: X, Y
// Munts: A, ZP.ACC, ZP.TOP, ZP.NEXT
Memory.Allocate()

// Free allocated memory block
// Input: ZP.IDXL/H = pointer to memory block to free
// Output: C set if successful, NC if failed  
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT, ZP.IDX
Memory.Free()

// Get available heap space
// Output: ZP.ACCL/H = bytes of free heap space
// Preserves: All registers
HeapAvailable()
```

---

## Serial I/O APIs

### **Character Output**
```hopper
// Write single character to serial output
// Input: A = character to write
// Output: Character sent to serial port
// Preserves: X, Y, A
// Munts: None
Serial.WriteChar()

// Write carriage return + line feed
// Output: CR+LF sent to serial port
// Preserves: All registers
Serial.NewLine()
```

### **String Output**
```hopper
// Print null-terminated string
// Input: ZP.TOPL/H = pointer to null-terminated string
// Output: String printed to serial
// Preserves: All registers
// Munts: None (internally saves/restores)
Tools.PrintStringTOP()

// Print null-terminated string  
// Input: ZP.NEXTL/H = pointer to null-terminated string
// Output: String printed to serial
// Preserves: All registers
// Munts: None (internally saves/restores)
Tools.PrintStringNEXT()

// Print null-terminated string
// Input: ZP.IDYL/H = pointer to null-terminated string  
// Output: String printed to serial
// Preserves: All registers
// Munts: None (internally saves/restores)
Tools.PrintStringIDY()
```

### **Numeric Output**
```hopper
// Print 16-bit value as decimal
// Input: ZP.TOPL/H = 16-bit value to print
// Output: Decimal representation printed to serial
// Preserves: All registers
// Munts: None (internally saves/restores)
Tools.PrintDecimalWord()

// Print byte value as decimal
// Input: A = byte value to print  
// Output: Decimal representation printed to serial
// Preserves: X, Y, A
// Munts: None
Tools.PrintDecimalByte()

// Print spaces for formatting
// Input: A = number of spaces to print
// Output: Spaces printed to serial
// Preserves: X, Y
// Munts: A
PrintSpaces()
```

---

## User Input APIs

### **Character Input**
```hopper
// Wait for and read single character from serial
// Output: A = character received
// Preserves: X, Y
// Munts: A
Serial.WaitForChar()

// Check if character is available (non-blocking)
// Output: Z set if no character available, NZ if character ready
//         A = character if available (undefined if Z set)
// Preserves: X, Y
Serial.IsAvailable()
```

### **User Prompts**
```hopper
// Display message and get Y/N confirmation
// Input: ZP.TOPL/H = pointer to prompt message
// Output: C set if user confirmed (Y/y), NC if cancelled (anything else)
// Preserves: X, Y
// Munts: A, ZP.ACC, ZP.TOP, ZP.NEXT
PromptYesNo()

// Wait for Y or N keypress (case insensitive)
// Output: A = 'Y' or 'N' (normalized to uppercase)
//         C set if Y, NC if N
// Preserves: X, Y
// Munts: A
WaitForYesNo()
```

---

## EEPROM I/O APIs

### **Page-Level Operations**
```hopper
// Read 256-byte page from EEPROM
// Input: ZP.FSOURCEADDRESSL/H = EEPROM address (page-aligned)
//        ZP.FDESTINATIONADDRESSL/H = buffer to receive data
// Output: C set if successful, NC if I2C error
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT, working registers
ReadEEPROMPage()

// Write 256-byte page to EEPROM
// Input: ZP.FDESTINATIONADDRESSL/H = EEPROM address (page-aligned)  
//        ZP.FSOURCEADDRESSL/H = buffer containing data to write
// Output: C set if successful, NC if I2C error
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT, working registers
WriteEEPROMPage()

// Verify EEPROM page matches buffer
// Input: ZP.FSOURCEADDRESSL/H = EEPROM address (page-aligned)
//        ZP.FDESTINATIONADDRESSL/H = buffer to compare against
// Output: C set if match, NC if different or I2C error
// Preserves: X, Y
// Munts: A, ZP.TOP, ZP.NEXT, working registers  
VerifyEEPROMPage()
```

### **I2C Status**
```hopper
// Test if EEPROM is present and responding
// Output: C set if EEPROM detected, NC if not present
// Preserves: All registers
// Munts: None (internally saves/restores)
EEPROMDetect()

// Get EEPROM capacity information
// Output: ZP.ACCL/H = total capacity in pages (typically 256)
// Preserves: X, Y
// Munts: A, ZP.ACC
EEPROMCapacity()
```

---

## Error Reporting APIs

### **Error Integration**
```hopper
// Set error code and display message
// Input: A = error code (from FileSystemErrors enum)
// Output: Error message displayed, ZP.LastError updated
// Preserves: X, Y
// Munts: A, ZP.LastError, ZP.TOP, ZP.NEXT
Error.FileSystemError()

// Set generic "not implemented" error
// Output: Error message displayed
// Preserves: X, Y  
// Munts: A, ZP.LastError
Error.TODO()

// Set out of memory error
// Output: Error message displayed
// Preserves: X, Y
// Munts: A, ZP.LastError  
Error.OutOfMemory()

// Check and print any pending error
// Output: Error printed if ZP.LastError != 0
// Preserves: All registers if no error
// Munts: Various if error present
Error.CheckAndPrint()
```

---

## Buffer Management APIs

### **Buffer Allocation**
```hopper
// Get pointer to primary file system buffer (256 bytes)
// Output: ZP.IDXL/H = pointer to FunctionOpCodeBuffer[0-255]
// Preserves: All registers
// Munts: ZP.IDX
GetPrimaryBuffer()

// Get pointer to secondary file system buffer (256 bytes)  
// Output: ZP.IDYL/H = pointer to FunctionOpCodeBuffer[256-511]
// Preserves: All registers except ZP.IDY
// Munts: ZP.IDY
GetSecondaryBuffer()

// Verify buffers are available (not in use by compiler)
// Output: C set if buffers available, NC if in use
// Preserves: All registers
BuffersAvailable()
```

---

## BASIC Integration APIs

### **Tokenizer Integration**
```hopper
// Get current program size in TokenizerBuffer
// Output: ZP.ACCL/H = program size in bytes
// Preserves: X, Y
// Munts: A, ZP.ACC
GetCurrentProgramSize()

// Get pointer to current program in TokenizerBuffer
// Output: ZP.IDXL/H = pointer to tokenized program
// Preserves: X, Y
// Munts: ZP.IDX
GetCurrentProgramPointer()

// Clear current program (equivalent to NEW command)
// Output: Program cleared, system reset to ready state
// Preserves: X, Y
// Munts: Various system state
ExecuteNewCommand()

// Load program from buffer into BASIC system
// Input: ZP.IDXL/H = pointer to tokenized program
//        ZP.ACCL/H = program size in bytes
// Output: C set if loaded successfully, NC if error
// Preserves: X, Y
// Munts: Various system state
LoadProgramFromBuffer()
```

### **State Management**
```hopper
// Check if BASIC is ready for file operations
// Output: C set if ready, NC if busy (compiling/executing)
// Preserves: All registers
BASICReady()

// Set BASIC to ready state after file operation
// Output: BASIC prompt displayed, system ready for input
// Preserves: All registers
BASICReadyPrompt()
```

---

## Implementation Priority

### **Phase 1: Essential Infrastructure**
```
1. Memory APIs (Memory.Allocate, Memory.Free) - CRITICAL
2. EEPROM APIs (ReadEEPROMPage, WriteEEPROMPage) - CRITICAL  
3. String APIs (StringLength, StringCopy, StringCompare) - CRITICAL
4. Character APIs (IsAlphaNumeric, ToLowercase, IsValidFilenameChar) - HIGH
5. Error APIs (Error.FileSystemError, Error.TODO) - HIGH
```

### **Phase 2: User Interface** 
```
6. Serial Output APIs (Serial.WriteChar, Tools.PrintStringTOP) - HIGH
7. Numeric Output APIs (Tools.PrintDecimalWord, PrintSpaces) - HIGH
8. User Input APIs (Serial.WaitForChar, PromptYesNo) - HIGH
9. Buffer Management APIs (GetPrimaryBuffer, GetSecondaryBuffer) - MEDIUM
```

### **Phase 3: BASIC Integration**
```
10. Tokenizer APIs (GetCurrentProgramSize, LoadProgramFromBuffer) - MEDIUM
11. State Management APIs (BASICReady, BASICReadyPrompt) - MEDIUM  
12. Advanced EEPROM APIs (VerifyEEPROMPage, EEPROMDetect) - LOW
```

---

## Benefits of External API Strategy

### **Reduced File System Complexity**
- **File system focuses only on file operations** - no string handling, I/O formatting, etc.
- **Smaller, more maintainable code** - each API has single responsibility
- **Easier testing** - can test each API independently
- **Better error isolation** - failures contained to specific API layers

### **Leverages Existing Infrastructure**
- **Reuses proven HopperBASIC code** - Memory, Serial, Tools units already exist
- **Consistent patterns** - same APIs used throughout HopperBASIC
- **No duplication** - don't reimplement functionality that already exists
- **Easier maintenance** - improvements to base APIs benefit file system automatically

### **Development Efficiency**
- **Parallel development** - different developers can work on different API sets
- **Incremental testing** - test each API set before building file system
- **Clear requirements** - well-defined interfaces reduce integration issues
- **Easier debugging** - can test file system with mock implementations

---

## Conclusion

This comprehensive API specification provides everything needed to implement a robust file system while minimizing the actual file system code. By implementing these external APIs first, the core file system becomes much simpler and more focused on its primary responsibility: managing files on EEPROM.

Many of these APIs likely already exist in HopperBASIC (Memory, Serial, Tools, Error) and just need to be verified or slightly extended. The remaining APIs are straightforward utility functions that can be implemented and tested independently.

This approach ensures the file system is built on a solid foundation of tested, reliable utilities rather than trying to implement everything within the file system itself.