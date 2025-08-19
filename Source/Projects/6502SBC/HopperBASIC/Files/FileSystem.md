# HopperBASIC File System Architecture
**Document Type: System Architecture Specification**

## Overview

This document specifies the low-level file system architecture for HopperBASIC's EEPROM storage. The system provides **storage engine functionality only** - all user interface, command parsing, and BASIC integration is handled by existing HopperBASIC infrastructure.

---

## Design Philosophy

### **Storage Engine Focus**
- **Pure storage mechanics** - no user interface or command parsing
- **Simple FAT-based allocation** using 256-byte sectors
- **Fixed single directory** (no subdirectories)
- **Stream-based I/O** for efficient program save/load

### **Integration Strategy**
- **Reuse existing APIs** for string handling, memory management, I/O
- **Leverage existing error handling** through HopperBASIC Error unit
- **Use existing ZP conventions** for parameter passing
- **Clean separation** between storage and user interface

---

## On-Disk Structure

### **EEPROM Memory Map (256 sectors × 256 bytes = 64KB)**
```
┌─────────────────────────────────────────────────────────────┐
│ Sector 0: File Allocation Table (FAT)                       │
├─────────────────────────────────────────────────────────────┤
│ Sector 1: Root Directory (16 files × 16 bytes = 256 bytes)  │
├─────────────────────────────────────────────────────────────┤
│ Sector 2: File Data                                         │
│ Sector 3: File Data                                         │
│ ...                                                         │
│ Sector 255: File Data                                       │
└─────────────────────────────────────────────────────────────┘
```

### **Sector 0: File Allocation Table (FAT)**
```
Byte Layout (256 bytes, one per sector):
┌──────┬─────────────────────────────────────────────────────┐
│ 0    │ Reserved (always 1 - marks sector 0 as system)      │
│ 1    │ Reserved (always 1 - marks sector 1 as system)      │
│ 2    │ Sector 2 status: 0=free, 1=end-of-chain, 2-255=next│
│ 3    │ Sector 3 status: 0=free, 1=end-of-chain, 2-255=next│
│ ...  │ ...                                                 │
│ 255  │ Sector 255 status: 0=free, 1=end-of-chain, 2-255=next│
└──────┴─────────────────────────────────────────────────────┘

FAT Values:
  0     = Free sector (available for allocation)
  1     = End of chain marker (last sector in file)
  2-255 = Next sector number in chain
```

### **Sector 1: Root Directory**
```
Directory Layout (256 bytes):
┌─────────────────────────────────────────────────────────────┐
│ File Entry 0  (bytes 0-15)   - 16 bytes                     │
│ File Entry 1  (bytes 16-31)  - 16 bytes                     │
│ File Entry 2  (bytes 32-47)  - 16 bytes                     │
│ ...                                                         │
│ File Entry 15 (bytes 240-255) - 16 bytes                    │
└─────────────────────────────────────────────────────────────┘

Total: 16 file entries (fixed maximum)
```

### **File Entry Structure (16 bytes)**
```
File Entry Layout:
┌────────┬─────────────────────────────────────────────────────┐
│ Offset │ Description                                         │
├────────┼─────────────────────────────────────────────────────┤
│ 0-1    │ File Length (16-bit LSB first)                      │
│        │ 0x0000 = empty/deleted entry                        │
│        │ 0x0001-0xFFFF = file size in bytes                  │
├────────┼─────────────────────────────────────────────────────┤
│ 2      │ Start Sector Number                                 │
│        │ 0 = invalid (used with length=0 for empty)          │
│        │ 2-255 = first sector of file data                   │
├────────┼─────────────────────────────────────────────────────┤
│ 3-15   │ Filename (up to 13 characters)                      │
│        │ Valid chars: A-Z, 0-9, . (ASCII 0x30-0x39,         │
│        │              0x41-0x5A, 0x2E)                       │
│        │ High bit set on LAST character                      │
│        │ Unused bytes after last char = undefined            │
└────────┴─────────────────────────────────────────────────────┘

Empty Entry Detection:
  - fileLength == 0x0000 indicates unused entry slot
  - startSector == 0 confirms empty entry
```

### **File Data Chain Example**
```
File: "HELLO.BAS" (450 bytes)
Directory Entry: startSector = 15, fileLength = 450

FAT Chain:
  FAT[15] = 23    ← Points to next sector
  FAT[23] = 1     ← End of chain marker

Data Storage:
  Sector 15: Bytes 0-255 of file data
  Sector 23: Bytes 256-449 of file data + padding
```

---

## Zero Page Usage

### **File System Operations (Reusing Existing Allocations)**
```
ZP.FSOURCEADDRESS (0x5F-0x60):     Source buffer/sector address
  - Sector addresses for EEPROM operations
  - Source buffer addresses for data transfers
  
ZP.FDESTINATIONADDRESS (0x61-0x62): Destination buffer/sector address  
  - Target buffer addresses for EEPROM reads
  - Destination addresses for data copies
  
ZP.FLENGTH (0x63-0x64):            Transfer length/byte counts
  - Data transfer lengths
  - File sizes and remaining bytes

ZP.LCURRENT (0x65-0x66):   Current sector/directory entry pointer
  - Current sector number during chain following
  - Current file entry offset during directory operations
  
ZP.LHEAD (0x67-0x68):      File start sector/directory position
  - File start sector number
  - Directory entry base address
  
ZP.LPREVIOUS (0x6B-0x6C):  Previous sector in chain
  - For chain manipulation and deallocation
  - Directory entry management
  
ZP.LNEXT (0x6D-0x6E):      Next sector in chain
  - Next sector number in file chain
  - Chain traversal state
```

### **File System Status (Existing Allocations)**
```
ZP.FSIGN (0x69):           File operation flags/status
  - Bit 0: Read/Write mode flag
  - Bit 1: File open flag
  - Bit 2: End of file flag
  - Bit 3: Operation success flag
  
ZP.LHEADX (0x6A):          File handle/operation workspace
  - Current file handle index (0-15)
  - Temporary calculation workspace
```

---

## Public API Specification

### **Save Operations**
```hopper
// Create new file for writing (or overwrite existing)
// Input: ZP.STR = pointer to filename (uppercase, null-terminated)
// Output: C set if successful, NC if error (disk full, invalid name)
// Preserves: X, Y
// Munts: A, file system state
StartSave()

// Write data chunk to current save file
// Input: ZP.FSOURCEADDRESS = pointer to data
//        ZP.FLENGTH = number of bytes to write
// Output: C set if successful, NC if error (disk full)
// Preserves: X, Y
// Munts: A, file system state
AppendStream()

// Close and finalize current save file
// Output: C set if successful, NC if error
// Preserves: X, Y  
// Munts: A, file system state
EndSave()
```

### **Load Operations**
```hopper
// Open file for reading
// Input: ZP.STR = pointer to filename (uppercase, null-terminated)
// Output: C set if successful, NC if error (file not found)
//         File ready for reading via NextStream()
// Preserves: X, Y
// Munts: A, file system state
StartLoad()

// Read next chunk of data from current load file
// Output: C set if data available, NC if end of file
//         ZP.FSOURCEADDRESS = pointer to data buffer (if C set)
//         ZP.FLENGTH = number of bytes available (if C set)
// Preserves: X, Y
// Munts: A, file system state
// Note: Caller must process data before next call
NextStream()
```

### **File Management Operations**
```hopper
// List all files to serial output
// Output: C set if successful, file list printed to serial
// Preserves: X, Y
// Munts: A
DirectoryList()

// Delete specified file
// Input: ZP.STR = pointer to filename (uppercase, null-terminated)
// Output: C set if successful, NC if error (file not found)
// Preserves: X, Y
// Munts: A, file system state
DeleteFile()

// Initialize empty file system
// Output: C set if successful, NC if error
//         All existing files destroyed, file system reset
// Preserves: X, Y
// Munts: A, entire EEPROM contents
Format()
```

---

## Core Storage Operations

### **FAT Management**
```hopper
// Internal operations for sector allocation
sectorAllocate() → sectorNumber     // Find free sector
sectorFree(sectorNumber)            // Mark sector as free
chainFollow(sectorNumber) → nextSector // Get next in chain
chainAppend(lastSector, newSector)  // Extend file chain
chainDeallocate(startSector)        // Free entire chain
```

### **Directory Management**
```hopper
// Internal operations for file entries
findFileEntry(filename) → entryOffset   // Locate file in directory
findFreeEntry() → entryOffset           // Find unused directory slot
createFileEntry(filename, startSector, length) // Add new file
removeFileEntry(entryOffset)             // Clear directory entry
validateFilename(filename) → valid       // Check filename format
```

### **Sector I/O**
```hopper
// Internal operations for EEPROM access
readSector(sectorNum, buffer)       // Read 256-byte sector
writeSector(sectorNum, buffer)      // Write 256-byte sector
readFAT()                          // Load FAT into memory
writeFAT()                         // Save FAT to EEPROM
readDirectory()                    // Load directory into memory
writeDirectory()                   // Save directory to EEPROM
```

---

## File System Capacity and Performance

### **Storage Limits**
```
Total EEPROM: 256 sectors × 256 bytes = 65,536 bytes (64KB)
System overhead: 2 sectors (FAT + directory) = 512 bytes
Available for files: 254 sectors = 65,024 bytes

Maximum files: 16 (limited by directory size)
Maximum file size: 65,024 bytes (all data sectors)
Filename length: 1-13 characters (A-Z, 0-9, .)

Typical BASIC program: 1-10KB
Expected usage: 10-15 programs easily fit
```

### **Allocation Strategy**
```
File allocation: First-fit algorithm
- Scan FAT[2..255] for first free sector (FAT[i] == 0)
- Allocate sectors on-demand as file grows
- No pre-allocation or clustering

File deallocation: Chain following
- Start from file's startSector
- Follow FAT chain marking each sector free
- No defragmentation needed (all sectors equal)

Directory management: Linear scan
- 16 entries maximum, linear search acceptable
- File creation/deletion compacts directory
```

### **Buffer Strategy**
```
Working buffers: Use existing FunctionOpCodeBuffer (512 bytes)
- Buffer A (0-255): Primary sector buffer for file I/O
- Buffer B (256-511): FAT/directory operations

No additional memory allocation required
All operations use existing HopperBASIC buffer infrastructure
```

---

## Error Integration

### **File System Error Conditions**
```hopper
// Error conditions reported via existing Error unit
DISK_FULL        // No free sectors available
FILE_NOT_FOUND   // Requested file does not exist
INVALID_FILENAME // Filename format invalid
DIRECTORY_FULL   // No free directory entries (16 files max)
EEPROM_ERROR     // I2C communication failure
CORRUPTED_FS     // File system corruption detected
```

### **Error Reporting Pattern**
```hopper
// All file system functions use existing error infrastructure
fileOperation()
{
    // ... operation logic ...
    if (errorCondition)
    {
        // Set appropriate error code in ZP.LastError
        Error.SetSpecificError();
        CLC  // Signal failure to caller
        return;
    }
    SEC  // Signal success
}
```

---

## Implementation Dependencies

### **Required External APIs (All Available)**
```
EEPROM Operations:
✅ EEPROM.ReadPage()   - Read 256-byte sector
✅ EEPROM.WritePage()  - Write 256-byte sector

Memory Operations:
✅ Memory.Clear()      - Zero memory blocks
✅ Memory.Copy()       - Copy memory blocks

String Operations:
✅ String.Compare()    - Compare filenames
✅ String.Length()     - Get filename length

Character Operations:
✅ Char.IsAlpha()      - Validate filename chars
✅ Char.IsDigit()      - Validate filename chars

Output Operations:
✅ Print.String()      - Directory listing output
✅ Print.Decimal()     - File size output
✅ Serial.WriteChar()  - Character output

Error Operations:
✅ Error.CheckError()  - Error status checking
```

### **Buffer Requirements**
```
Primary Buffer: FunctionOpCodeBuffer[0-255]
- Sector read/write operations
- File data streaming
- FAT operations

Secondary Buffer: FunctionOpCodeBuffer[256-511]  
- Directory operations
- Multi-sector operations
- Temporary workspace

No additional memory allocation required
```

---

## Conclusion

This streamlined file system provides robust EEPROM storage with a clean API that integrates seamlessly with existing HopperBASIC infrastructure. The design focuses purely on storage mechanics, leaving all user interface and language integration to the existing Console and Error units.

Key benefits:
- **Simple FAT-based allocation** with predictable performance
- **Fixed 16-file directory** eliminates complex directory management
- **Stream-based I/O** efficiently handles variable-size BASIC programs
- **Zero additional memory overhead** through buffer reuse
- **Clean API separation** between storage engine and user interface
- **Robust error handling** via existing HopperBASIC error infrastructure

The file system provides exactly what HopperBASIC needs: reliable program storage and retrieval with a simple, focused API that handles the low-level details while integrating cleanly with the existing runtime system.