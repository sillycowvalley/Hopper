# HopperBASIC File System External API Requirements
**Document Type: API Specification**

## Overview

This document specifies all external APIs required by the HopperBASIC file system. By leveraging existing HopperBASIC infrastructure and implementing focused file-specific utilities, we minimize the complexity of the core file system code while maintaining full integration with the BASIC runtime.

---

## Character Validation APIs

### **Character Type Checking**
```hopper
// Check if character is alphabetic (A-Z, a-z)
// Input: A = character to test
// Output: C set if alphabetic, NC if not
// Preserves: X, Y
IsAlpha()
```
**Status:** ✅ **EXISTS** - Available in `tokenizer.asm`, already implemented

```hopper
// Check if character is numeric (0-9)  
// Input: A = character to test
// Output: C set if numeric, NC if not
// Preserves: X, Y
IsDigit()
```
**Status:** ✅ **EXISTS** - Available in `tokenizer.asm`, already implemented

```hopper
// Check if character is alphanumeric (A-Z, a-z, 0-9)
// Input: A = character to test
// Output: C set if alphanumeric, NC if not
// Preserves: X, Y
IsAlphaNumeric()
```
**Status:** ✅ **EXISTS** - Available in `tokenizer.asm`, already implemented

```hopper
// Check if character is valid for filename (alphanumeric + period)
// Input: A = character to test
// Output: C set if valid, NC if invalid
// Preserves: X, Y
IsValidFilenameChar()
```
**Status:** ❌ **NEEDED** - Could leverage existing `IsAlphaNumeric()` + period check

---

## String Analysis APIs
*Note: File system receives immutable uppercase string pointers, no string memory management*

```hopper
// Calculate string length
// Input: ZP.STR = pointer to null-terminated string
// Output: A = string length (max 255)
// Preserves: X, Y, ZP.STR
// Munts: A only
StringLength()
```
**Status:** ❌ **NEEDED** - Could leverage string iteration patterns from `Tools.PrintStringSTR()`

```hopper
// Validate string as filename (length 1-12, valid characters)
// Input: ZP.STR = pointer to null-terminated uppercase string
// Output: C set if valid filename, NC if invalid
//         A = actual string length
// Preserves: X, Y, ZP.STR
// Munts: A only
ValidateFilename()
```
**Status:** ❌ **NEEDED** - Could leverage existing `IsValidFilenameChar()` + `StringLength()`

```hopper
// Compare two strings (case-sensitive since both are uppercase)
// Input: ZP.STR = string1 pointer
//        ZP.IDX = string2 pointer  
//        A = maximum length to compare (typically 12 for filenames)
// Output: Z set if equal, NZ if different
// Preserves: X, Y, ZP.STR, ZP.IDX
// Munts: A only
StringCompare()
```
**Status:** ❌ **NEEDED** - Simple case-sensitive comparison

---

## Memory Management APIs

```hopper
// Allocate memory block
// Input: ZP.ACC = number of bytes to allocate
// Output: ZP.IDX = pointer to allocated memory (0x0000 if failed)
//         C set if successful, NC if failed
// Preserves: X, Y
// Munts: A, ZP.ACC
Memory.Allocate()
```
**Status:** ✅ **EXISTS** - Available in `Memory.asm`, fully implemented

```hopper
// Free allocated memory block
// Input: ZP.IDX = pointer to memory block to free
// Output: C set if successful, NC if failed  
// Preserves: X, Y
// Munts: A, ZP.IDX
Memory.Free()
```
**Status:** ✅ **EXISTS** - Available in `Memory.asm`, fully implemented

```hopper
// Clear memory block
// Input: ZP.IDX = destination pointer
//        ZP.ACC = number of bytes to clear
// Output: Memory cleared to zeros
// Preserves: X, Y
// Munts: A, destination buffer
MemoryClear()
```
**Status:** ❌ **NEEDED** - Simple byte-filling loop

```hopper
// Copy memory block
// Input: ZP.FSOURCEADDRESS = source pointer
//        ZP.IDX = destination pointer
//        ZP.FLENGTH = number of bytes to copy
// Output: Memory copied
// Preserves: X, Y  
// Munts: A, destination buffer
MemoryCopy()
```
**Status:** ❌ **NEEDED** - Basic memory copy loop

---

## Character Output APIs
*For directory listings and file system messages*

```hopper
// Write single character to serial output
// Input: A = character to write
// Output: Character sent to serial port
// Preserves: X, Y, A
// Munts: None
Serial.WriteChar()
```
**Status:** ✅ **EXISTS** - Available in `Serial.asm`, fully implemented

```hopper
// Print null-terminated string
// Input: ZP.STR = pointer to null-terminated string
// Output: String printed to serial
// Preserves: All registers
// Munts: None (internally saves/restores)
Tools.PrintStringSTR()
```
**Status:** ✅ **EXISTS** - Available in `Tools.asm`, fully implemented

```hopper
// Print 16-bit value as decimal
// Input: ZP.TOP = 16-bit value to print
//        ZP.TOPT = type (for signed/unsigned determination)
// Output: Decimal representation printed to serial
// Preserves: All registers
// Munts: None (internally saves/restores)
Tools.PrintDecimal()
```
**Status:** ✅ **EXISTS** - Available in `Tools.asm`, fully implemented

```hopper
// Write carriage return + line feed
// Output: CR+LF sent to serial port
// Preserves: All registers
Tools.NL()
```
**Status:** ✅ **EXISTS** - Available in `Tools.asm`, fully implemented

```hopper
// Print spaces for formatting
// Input: A = number of spaces to print
// Output: Spaces printed to serial
// Preserves: X, Y
// Munts: A
PrintSpaces()
```
**Status:** ❌ **NEEDED** - Simple loop calling `Serial.WriteChar()`

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
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit, handles all page sizes

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
**Status:** ✅ **IMPLEMENTED** - Available in EEPROM unit, handles all page sizes

### **Legacy Compatibility Note**
The original API specification called for:
- `ReadPage()` and `WritePage()` without unit qualification
- `VerifyEEPROMPage()` and `EEPROMDetect()` utility methods

**✅ Updated Interface:** The EEPROM unit provides a cleaner, more comprehensive interface:
- **Qualified calls** (`EEPROM.ReadPage()`, `EEPROM.WritePage()`) follow HopperBASIC conventions
- **Automatic page size handling** eliminates complexity from file system code
- **Integrated detection** through `Initialize()`, `Detect()`, and `GetSize()` methods
- **Plug-and-play support** via `ZP.PLUGNPLAY` bit flags

---

## Error Reporting APIs

```hopper
// Check and proceed only if no error occurred
// Output: C set if no error, NC if error occurred
// Preserves: Processor flags only
Error.CheckError()
```
**Status:** ✅ **EXISTS** - Available in `Error.asm`, fully implemented

```hopper
// Set "not implemented" error
// Output: Error message set
// Preserves: X, Y
// Munts: A, ZP.LastError  
Error.NotImplemented()
```
**Status:** ✅ **EXISTS** - Available in `Error.asm`, fully implemented

```hopper
// Set out of memory error
// Output: Error message set
// Preserves: X, Y
// Munts: A, ZP.LastError  
Error.OutOfMemory()
```
**Status:** ✅ **EXISTS** - Available in `Error.asm`, fully implemented

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

### **Phase 1: Essential Infrastructure ✅ LARGELY COMPLETE**
```
1. Memory APIs (Memory.Allocate, Memory.Free) - ✅ AVAILABLE
2. EEPROM APIs (EEPROM.Initialize, EEPROM.Detect, EEPROM.GetSize, EEPROM.ReadPage, EEPROM.WritePage) - ✅ IMPLEMENTED  
3. Character APIs (IsAlpha, IsDigit, IsAlphaNumeric) - ✅ AVAILABLE
4. Output APIs (Serial.WriteChar, Tools.PrintStringSTR, Tools.PrintDecimal, Tools.NL) - ✅ AVAILABLE
5. Error APIs (Error.CheckError, Error.NotImplemented, Error.OutOfMemory) - ✅ AVAILABLE
```

### **Phase 2: Simple Utilities - ❌ NEEDED**
```
6. String APIs (StringLength, StringCompare, ValidateFilename) - ❌ NEEDED
7. Character validation (IsValidFilenameChar) - ❌ NEEDED
8. Memory utilities (MemoryClear, MemoryCopy) - ❌ NEEDED
9. Output utilities (PrintSpaces) - ❌ NEEDED
```

### **Phase 3: File System Core - ❌ NEEDED** 
```
10. Save APIs (StartSave, AppendStream, EndSave) - ❌ NEEDED
11. Load APIs (StartLoad, NextStream) - ❌ NEEDED
12. Management APIs (DirectoryList, DeleteFile, Format) - ❌ NEEDED
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

### **Clean Integration**
- **Immutable strings** - no string memory management complexity
- **Streaming serialization** - handles complex program structures elegantly
- **Proper ZP usage** - follows HopperBASIC conventions
- **Console separation** - user interaction handled by Console unit

---

## Summary: Final API Requirements

### **✅ Ready to Use (17 APIs)**
- **Character validation:** `IsAlpha()`, `IsDigit()`, `IsAlphaNumeric()`
- **Memory management:** `Memory.Allocate()`, `Memory.Free()`
- **Character output:** `Serial.WriteChar()`, `Tools.PrintStringSTR()`, `Tools.PrintDecimal()`, `Tools.NL()`
- **EEPROM I/O:** `EEPROM.Initialize()`, `EEPROM.Detect()`, `EEPROM.GetSize()`, `EEPROM.ReadPage()`, `EEPROM.WritePage()`
- **Error handling:** `Error.CheckError()`, `Error.NotImplemented()`, `Error.OutOfMemory()`

### **❌ Need Implementation (12 APIs)**
**Simple Utilities (7 APIs):**
- `IsValidFilenameChar()`, `StringLength()`, `ValidateFilename()`, `StringCompare()`
- `MemoryClear()`, `MemoryCopy()`, `PrintSpaces()`

**File System Core (5 APIs):**
- `StartSave()`, `AppendStream()`, `EndSave()`
- `StartLoad()`, `NextStream()`
- `DirectoryList()`, `DeleteFile()`, `Format()`

---

## Major Progress Update

**✅ EEPROM APIs Complete:** The EEPROM unit provides a comprehensive, production-ready interface that handles:
- **Device detection and initialization** with plug-and-play support
- **Automatic page size handling** (supports 64-byte, 128-byte, and 256-byte EEPROM pages)
- **Clean ZP.IDX/ZP.IDY interface** following HopperBASIC conventions
- **Size detection** for different EEPROM capacities (32K, 64K, 128K)

**📈 Implementation Status:** **58% Complete** (17 of 29 total APIs implemented)

The file system now has all the infrastructure APIs needed and can focus on implementing the simple utilities and core file operations. The EEPROM implementation eliminates significant complexity from the file system layer.