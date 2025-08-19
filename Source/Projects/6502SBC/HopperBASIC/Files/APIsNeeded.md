# HopperBASIC File System External API Requirements
**Document Type: API Specification**

## Overview

This document specifies all external APIs required by the HopperBASIC file system. By leveraging existing HopperBASIC infrastructure and implementing focused file-specific utilities, we minimize the complexity of the core file system code while maintaining full integration with the BASIC runtime.

---

## Character Validation APIs - **✅ IMPLEMENTED**

### **Character Type Checking**
```hopper
// Check if character is alphabetic (A-Z, a-z)
// Input: A = character to test
// Output: C set if alphabetic, NC if not
// Preserves: A, X, Y (saves/restores A)
// Munts: Internal working state only
Char.IsAlpha()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Char.asm`

```hopper
// Check if character is numeric (0-9)  
// Input: A = character to test
// Output: C set if numeric, NC if not
// Preserves: A, X, Y (saves/restores A)
// Munts: Internal working state only
Char.IsDigit()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Char.asm`

```hopper
// Check if character is alphanumeric (A-Z, a-z, 0-9)
// Input: A = character to test
// Output: C set if alphanumeric, NC if not
// Preserves: A, X, Y (saves/restores A)
// Munts: Internal working state only
Char.IsAlphaNumeric()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Char.asm`

### **Filename Character Validation**
```hopper
// Check if character is valid for filename (alphanumeric + period)
// Input: A = character to test
// Output: C set if valid, NC if invalid
// Preserves: X, Y
// Munts: A only
File.IsValidFilenameChar()
```
**Status:** ✅ **IMPLEMENTED** - Available in `File.asm`, uses `Char.IsAlphaNumeric()` + period check

### **Bonus Character Methods (Available)**
```hopper
// Check if character is lowercase (a-z)
Char.IsLower()
// Check if character is hex digit (0-9, A-F, a-f)  
Char.IsHex()
```
**Status:** ✅ **BONUS** - Additional functionality available in `Char.asm`

---

## String Analysis APIs - **✅ IMPLEMENTED**

### **String Length**
```hopper
// Calculate string length
// Input: ZP.STR = pointer to null-terminated string
// Output: Y = string length
// Preserves: Input pointer (ZP.STR unchanged)
// Munts: A, X, Y
// Note: Returns 0 for null pointer
String.Length()
```
**Status:** ✅ **IMPLEMENTED** - Available in `String.asm`

### **String Comparison**
```hopper
// Compare two strings
// Input: ZP.STR = first string pointer
//        ZP.STR2 = second string pointer
// Output: C set if match, NC if different
// Preserves: Input pointers unchanged
// Munts: A, X, Y
String.Compare()
```
**Status:** ✅ **IMPLEMENTED** - Available in `String.asm`

### **Filename Validation**
```hopper
// Validate string as filename (length 1-12, valid characters)
// Input: ZP.STR = pointer to null-terminated uppercase string
// Output: C set if valid filename, NC if invalid
//         A = actual string length
// Preserves: X, Y, ZP.STR
// Munts: A only
File.ValidateFilename()
```
**Status:** ✅ **IMPLEMENTED** - Available in `File.asm`, uses `String.Length()` + `File.IsValidFilenameChar()`

---

## Memory Management APIs - **✅ IMPLEMENTED**

### **Allocation & Deallocation**
```hopper
// Allocate memory block
// Input: ZP.ACC = number of bytes to allocate
// Output: ZP.IDX = pointer to allocated memory (0x0000 if failed)
//         C set if successful, NC if failed
// Preserves: X, Y, processor status, ZP.ACC
// Munts: Internal ZP.M* scratch space only
Memory.Allocate()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Memory.asm`

```hopper
// Free allocated memory block
// Input: ZP.IDX = pointer to memory block to free
// Output: C set if successful, NC if failed  
// Preserves: X, Y, processor status
// Munts: Internal ZP.M* scratch space only
Memory.Free()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Memory.asm`

### **Memory Operations**
```hopper
// Clear memory block to zeros
// Input: ZP.FDESTINATIONADDRESS = destination pointer  
//        ZP.FLENGTH = number of bytes to zero (16-bit)
// Output: Memory zeroed at destination
// Preserves: Input parameters unchanged
// Munts: A, Y, internal working state
Memory.Clear()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Memory.asm`

```hopper
// Copy memory block
// Input: ZP.FSOURCEADDRESS = source pointer
//        ZP.FDESTINATIONADDRESS = destination pointer
//        ZP.FLENGTH = number of bytes to copy (16-bit)
// Output: Data copied from source to destination
// Preserves: Input parameters unchanged
// Munts: A, Y, internal working state
Memory.Copy()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Memory.asm`

---

## Character Output APIs - **✅ IMPLEMENTED**

### **Core Output Methods**
```hopper
// Write single character to serial output
// Input: A = character to write
// Output: Character sent to serial port
// Preserves: X, Y, A
// Munts: None
Serial.WriteChar()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Serial.asm`

### **String Output Methods**
```hopper
// Print null-terminated string
// Input: ZP.STR = pointer to null-terminated string
// Output: String printed to serial
// Preserves: Everything
// Munts: Flags, A, Y (internal working state)
Print.String()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Print.asm`

### **Numeric & Formatting Output**
```hopper
// Print 16-bit/32-bit decimal number
// Input: ZP.TOP = 16-bit number to print (0-65535)
//        ZP.LTOP0-3 = 32-bit signed LONG (if ZP.TOPT == BASICType.LONG)
//        ZP.TOPT = type (for signed/unsigned determination)
// Output: Decimal representation printed to serial
// Preserves: X, Y, A
// Munts: Flags
Print.Decimal()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Print.asm`

```hopper
// Write carriage return + line feed
// Output: '\n' sent to serial port
// Preserves: X, Y, A
// Munts: A, Flags
Print.PrintNewLine()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Print.asm`

```hopper
// Print spaces for formatting
// Input: X = number of spaces to print
// Output: Spaces printed to serial
// Preserves: Y
// Munts: A, flags, X (consumed as counter)
Print.Spaces()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Print.asm`

---

## EEPROM I/O APIs - **✅ IMPLEMENTED**

### **Initialization & Detection**
```hopper
// Initialize I2C and EEPROM detection
// Output: ZP.PLUGNPLAY with device status bits set (bit 1 = EEPROM present)
// Preserves: All registers except internal working registers
// Munts: A, working registers
EEPROM.Initialize()
```
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit

```hopper
// Test if EEPROM is present and responding
// Output: C set if EEPROM detected, NC if not present
// Preserves: All registers
// Munts: None (internally saves/restores)
EEPROM.Detect()
```
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit

```hopper
// Get EEPROM size in kilobytes
// Output: A = size in K (32, 64, or 128) and C set, or 0 and NC if no EEPROM
// Preserves: X, Y
// Munts: A only
EEPROM.GetSize()
```
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit

### **Page I/O Operations**
```hopper
// Read 256-byte page from EEPROM to RAM
// Input: ZP.IDY = EEPROM source address (16-bit, page-aligned recommended)
//        ZP.IDX = RAM destination address (16-bit, page-aligned recommended)
// Output: 256 bytes copied from EEPROM to RAM
//         ZP.IDY advanced by 256 bytes
//         ZP.IDX advanced by 256 bytes
// Preserves: X, Y  
// Munts: A, working registers
// Note: Handles EEPROM page size differences automatically (64/128/256 byte pages)
EEPROM.ReadPage()
```
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit

```hopper
// Write 256-byte page from RAM to EEPROM
// Input: ZP.IDX = RAM source address (16-bit, page-aligned recommended)
//        ZP.IDY = EEPROM destination address (16-bit, page-aligned recommended)
// Output: 256 bytes copied from RAM to EEPROM
//         ZP.IDX advanced by 256 bytes
//         ZP.IDY advanced by 256 bytes
// Preserves: X, Y
// Munts: A, working registers
// Note: Handles EEPROM page size differences automatically (64/128/256 byte pages)
EEPROM.WritePage()
```
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit

---

## Error Reporting APIs - **✅ IMPLEMENTED**

```hopper
// Check and proceed only if no error occurred
// Output: C set if no error, NC if error occurred
// Preserves: Processor flags only
Error.CheckError()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Error.asm`

```hopper
// Set "not implemented" error
// Output: Error message set
// Preserves: X, Y
// Munts: A, ZP.LastError  
Error.NotImplemented()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Error.asm`

```hopper
// Set out of memory error
// Output: Error message set
// Preserves: X, Y
// Munts: A, ZP.LastError  
Error.OutOfMemory()
```
**Status:** ✅ **IMPLEMENTED** - Available in `Error.asm`

---

## Program Serialization APIs
*For saving/loading tokenized BASIC programs*

### **Save Operations**
```hopper
// Start saving a program to EEPROM
// Input: ZP.STR = pointer to filename (null-terminated, uppercase, 1-12 chars)
// Output: C set if file created successfully, NC if error
//         File system ready to receive token streams
// Preserves: X, Y
// Munts: A, file system state
StartSave()
```
**Status:** ❌ **NEEDED** - File system core functionality

```hopper
// Append token stream to current save operation
// Input: ZP.FSOURCEADDRESS = pointer to token stream
//        ZP.FLENGTH = length of token stream in bytes
// Output: C set if data written successfully, NC if error
// Preserves: X, Y
// Munts: A, file system state
// Note: First token indicates object type (Token.FUNC, Token.BEGIN, Token.CONST, Token.VAR, etc.)
AppendStream()
```
**Status:** ❌ **NEEDED** - File system core functionality

```hopper
// Complete save operation and close file
// Output: C set if file saved successfully, NC if error
// Preserves: X, Y
// Munts: A, file system state
EndSave()
```
**Status:** ❌ **NEEDED** - File system core functionality

### **Load Operations**
```hopper
// Start loading a program from EEPROM
// Input: ZP.STR = pointer to filename (null-terminated, uppercase)
//        ZP.IDX = pointer to working buffer
//        ZP.FLENGTH = length of working buffer in bytes
// Output: C set if file opened successfully, NC if error (file not found)
//         File system ready to return token streams
// Preserves: X, Y
// Munts: A, file system state
StartLoad()
```
**Status:** ❌ **NEEDED** - File system core functionality

```hopper
// Get next token stream from current load operation
// Output: C set if stream available, NC if end of file
//         ZP.FSOURCEADDRESS = pointer to token stream (if C set)
//         ZP.FLENGTH = length of token stream in bytes (if C set)
//         First token indicates object type
// Preserves: X, Y
// Munts: A, file system state
// Note: Caller must process/copy stream before next call
NextStream()
```
**Status:** ❌ **NEEDED** - File system core functionality

### **File Management Operations**
```hopper
// List all files in directory
// Input: ZP.IDX = pointer to working buffer
//        ZP.FLENGTH = length of working buffer in bytes
// Output: File list printed to serial, C set if successful
// Preserves: X, Y
// Munts: A, working buffer
DirectoryList()
```
**Status:** ❌ **NEEDED** - File system core functionality

```hopper
// Delete a file
// Input: ZP.STR = pointer to filename (null-terminated, uppercase)
//        ZP.IDX = pointer to working buffer
//        ZP.FLENGTH = length of working buffer in bytes
// Output: C set if file deleted successfully, NC if error (file not found)
// Preserves: X, Y
// Munts: A, file system state, working buffer
DeleteFile()
```
**Status:** ❌ **NEEDED** - File system core functionality

```hopper
// Format EEPROM and initialize empty file system
// Input: ZP.IDX = pointer to working buffer
//        ZP.FLENGTH = length of working buffer in bytes
// Output: C set if format successful, NC if error
//         All existing files destroyed, file system reset
// Preserves: X, Y
// Munts: A, entire EEPROM contents, working buffer
// Note: User confirmation already handled by Console before calling
Format()
```
**Status:** ❌ **NEEDED** - File system core functionality

---

## Implementation Priority

### **Phase 1: Essential Infrastructure ✅ COMPLETE**
```
1. Memory APIs (Memory.Allocate, Memory.Free, Memory.Clear, Memory.Copy) - ✅ IMPLEMENTED
2. EEPROM APIs (EEPROM.Initialize, EEPROM.Detect, EEPROM.GetSize, EEPROM.ReadPage, EEPROM.WritePage) - ✅ IMPLEMENTED  
3. Character APIs (Char.IsAlpha, Char.IsDigit, Char.IsAlphaNumeric, File.IsValidFilenameChar) - ✅ IMPLEMENTED
4. String APIs (String.Length, String.Compare, File.ValidateFilename) - ✅ IMPLEMENTED
5. Output APIs (Serial.WriteChar, Print.String, Print.Decimal, Print.PrintNewLine, Print.Spaces) - ✅ IMPLEMENTED
6. Error APIs (Error.CheckError, Error.NotImplemented, Error.OutOfMemory) - ✅ IMPLEMENTED
```

### **Phase 2: File System Core - ❌ NEEDED** 
```
7. Save APIs (StartSave, AppendStream, EndSave) - ❌ NEEDED
8. Load APIs (StartLoad, NextStream) - ❌ NEEDED
9. Management APIs (DirectoryList, DeleteFile, Format) - ❌ NEEDED
```

---

## Benefits of External API Strategy

### **Reduced File System Complexity**
- **File system focuses only on file operations** - no string handling, I/O formatting, etc.
- **Smaller, more maintainable code** - each API has single responsibility
- **Easier testing** - can test each API independently
- **Better error isolation** - failures contained to specific API layers

### **Leverages Existing Infrastructure**
- **Reuses proven HopperBASIC code** - Memory, String, Char, Print units already exist
- **Consistent patterns** - same APIs used throughout HopperBASIC
- **No duplication** - don't reimplement functionality that already exists
- **Easier maintenance** - improvements to base APIs benefit file system automatically

### **Clean Integration**
- **Proper unit organization** - Each unit has focused responsibility
- **Immutable strings** - no string memory management complexity
- **Streaming serialization** - handles complex program structures elegantly
- **Proper ZP usage** - follows HopperBASIC conventions
- **Console separation** - user interaction handled by Console unit

---

## Summary: Final API Requirements

### **✅ Ready to Use (24 APIs)**
- **Character validation:** `Char.IsAlpha()`, `Char.IsDigit()`, `Char.IsAlphaNumeric()`, `Char.IsLower()`, `Char.IsHex()`, `File.IsValidFilenameChar()`
- **String operations:** `String.Length()`, `String.Compare()`, `File.ValidateFilename()`
- **Memory management:** `Memory.Allocate()`, `Memory.Free()`, `Memory.Clear()`, `Memory.Copy()`
- **Character output:** `Serial.WriteChar()`, `Print.String()`, `Print.Decimal()`, `Print.PrintNewLine()`, `Print.Spaces()`
- **EEPROM I/O:** `EEPROM.Initialize()`, `EEPROM.Detect()`, `EEPROM.GetSize()`, `EEPROM.ReadPage()`, `EEPROM.WritePage()`
- **Error handling:** `Error.CheckError()`, `Error.NotImplemented()`, `Error.OutOfMemory()`

### **❌ Need Implementation (6 APIs)**
**File System Core (6 APIs):**
- `StartSave()`, `AppendStream()`, `EndSave()`
- `StartLoad()`, `NextStream()`
- `DirectoryList()`, `DeleteFile()`, `Format()`

---

## Major Progress Update

**🎉 INFRASTRUCTURE COMPLETE:** All infrastructure APIs are properly organized and implemented!

**📈 Implementation Status:** **80% Complete** (24 of 30 total APIs implemented)

### **Key Achievements:**
- **✅ Proper unit organization** - Each unit has focused responsibility
- **✅ Complete memory management** - `Memory.*` unit handles allocation and memory operations
- **✅ Complete string operations** - `String.*` unit provides core string functionality  
- **✅ Complete character validation** - `Char.*` and `File.*` units handle all character needs
- **✅ Complete output formatting** - `Print.*` unit provides formatted output
- **✅ Complete EEPROM interface** - `EEPROM.*` unit ready for file operations
- **✅ Complete error handling** - `Error.*` integrated error reporting

### **Clean Architecture Benefits:**
- **Focused units** - Each unit has single responsibility
- **No API duplication** - Clean separation of concerns
- **Proper state preservation** - Each unit follows HopperBASIC conventions
- **Ready for file system** - All infrastructure dependencies satisfied

**🚀 Ready for File System Core:** Only 6 core file operations remain - the actual file system logic!