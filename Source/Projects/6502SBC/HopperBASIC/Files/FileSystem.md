# HopperBASIC File System Architecture
**Document Type: System Architecture Specification**

## Overview

This document specifies the low-level file system architecture for HopperBASIC's EEPROM storage. The system provides **storage engine functionality only** - all user interface, command parsing, and BASIC integration is handled by existing HopperBASIC infrastructure.

---

## Usage Examples

### **File Save Operation State Flow**
```hopper
StartSave("HELLO.BAS"):
- Load FAT into FATBuffer (cached for entire operation)
- Find free directory entry: CurrentFileEntry = 3
- Allocate first sector: FileStartSector = 45, CurrentFileSector = 45
- Initialize state: FilePosition = 0, SectorPosition = 0
- Set flags: FileSystemFlags = 0b00000011 (save mode + file open)

AppendStream(data, 100 bytes):
- Copy data to FileDataBuffer[SectorPosition..SectorPosition+100]
- Update counters: SectorPosition += 100, FilePosition += 100
- No sector write needed yet (buffer not full)

AppendStream(data, 200 bytes):
- Copy 156 bytes to fill FileDataBuffer[100..255]
- Write FileDataBuffer to CurrentFileSector (45)
- Allocate new sector from FATBuffer: NextFileSector = 78
- Link sectors: FATBuffer[45] = 78, FATDirty = 1
- Copy remaining 44 bytes to FileDataBuffer[0..43]
- Update state: CurrentFileSector = 78, SectorPosition = 44

EndSave():
- Write final FileDataBuffer to CurrentFileSector if dirty
- Mark final sector: FATBuffer[78] = 1 (end of chain)
- Update directory entry: startSector=45, fileLength=344
- Flush cached data: write FATBuffer to sector 0, directory to sector 1
- Clear state: FileSystemFlags = 0, FATDirty = 0, DirectoryDirty = 0
```

### **File Load Operation State Flow**
```hopper
StartLoad("HELLO.BAS"):
- Load FAT into FATBuffer (cached for entire operation)
- Search directory: CurrentFileEntry = 3, FileStartSector = 45
- Initialize state: CurrentFileSector = 45, FileBytesRemaining = 344
- Set flags: FileSystemFlags = 0b00000010 (load mode + file open)
- Read first sector into FileDataBuffer

NextStream() - First call:
- Return pointer: FileDataBuffer + SectorPosition (0)
- Return length: min(256, FileBytesRemaining) = 256
- Update state: SectorPosition = 256, FileBytesRemaining = 88

NextStream() - Second call:
- Follow chain: NextFileSector = FATBuffer[45] = 78
- Read sector 78 into FileDataBuffer, CurrentFileSector = 78
- Return pointer: FileDataBuffer + 0
- Return length: min(256, FileBytesRemaining) = 88
- Update state: FileBytesRemaining = 0, FileSystemFlags |= 0b00000100 (EOF)

NextStream() - Third call:
- Check FileBytesRemaining = 0: return NC (end of file)
```

### **Directory Operations**
```hopper
DirectoryList():
- Read directory sector into WorkingSectorBuffer
- For each 16-byte entry in WorkingSectorBuffer[0..255]:
  - Check fileLength != 0 (entry in use)
  - Parse filename: scan bytes until high bit found
  - Print filename and fileLength to serial
- No FAT operations needed, single sector read

DeleteFile("HELLO.BAS"):
- Read directory into WorkingSectorBuffer, find CurrentFileEntry = 3
- Load FAT into FATBuffer
- Follow chain starting from FileStartSector = 45:
  - FATBuffer[45] = 78 → FATBuffer[78] = 1 (end)
  - Mark free: FATBuffer[45] = 0, FATBuffer[78] = 0
- Clear directory entry: WorkingSectorBuffer[48..63] = zeros
- Write updates: FATBuffer to sector 0, WorkingSectorBuffer to sector 1
```

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

### **Core File State (Reusing ZP.F/L Allocations)**
```
const byte CurrentFileEntry     = ZP.LCURRENT;      // (0x65) Directory entry index (0-15)
const byte CurrentFileSector    = ZP.LCURRENTH;     // (0x66) Current sector number in file

const byte FilePosition         = ZP.LHEAD;         // (0x67-0x68) Current byte position in file (16-bit)
const byte FilePositionL        = ZP.LHEADL;        // (0x67)
const byte FilePositionH        = ZP.LHEADH;        // (0x68)

const byte FileBytesRemaining   = ZP.LPREVIOUS;     // (0x6B-0x6C) Bytes left to read/write (16-bit)
const byte FileBytesRemainingL  = ZP.LPREVIOUSL;    // (0x6B)
const byte FileBytesRemainingH  = ZP.LPREVIOUSH;    // (0x6C)

const byte SectorPosition       = ZP.LHEADX;        // (0x6A) Byte position within current sector (0-255)
const byte NextFileSector       = ZP.LNEXTL;        // (0x6D) Next sector in chain (from FAT)
const byte FileStartSector      = ZP.LNEXTH;        // (0x6E) First sector of current file

const byte FileSystemFlags      = ZP.FSIGN;         // (0x69) Operation mode and status flags
// Bit 0: Operation mode (0=load, 1=save)
// Bit 1: File is open flag
// Bit 2: End of file reached
// Bit 3: FAT modified (needs flush)

const byte SectorSourceL        = ZP.FSOURCEADDRESSL;      // (0x5F) Source address for sector ops  
const byte SectorSourceH        = ZP.FSOURCEADDRESSH;     // (0x60)
const byte SectorDestL          = ZP.FDESTINATIONADDRESSL; // (0x61) Destination address for sector ops
const byte SectorDestH          = ZP.FDESTINATIONADDRESSH;// (0x62)
const byte TransferLength       = ZP.FLENGTHL;             // (0x63-0x64) Bytes to transfer
const byte TransferLengthL      = ZP.FLENGTHL;             // (0x63)
const byte TransferLengthH      = ZP.FLENGTHH;             // (0x64)
```

### **Extended File System State (Using ZP.M Allocations)**
```
const byte DirectoryDirty       = ZP.M1;           // Directory needs write-back (0=clean, 1=dirty)
const byte FATDirty            = ZP.M2;           // FAT needs write-back (0=clean, 1=dirty)
const byte LastAllocatedSector  = ZP.M3;           // Last sector allocated (optimization hint)

const byte StreamBytesAvailable = ZP.M4;           // Bytes available in current NextStream() call
const byte StreamMode          = ZP.M5;           // 0=none, 1=save, 2=load
const byte PendingSectorWrite  = ZP.M6;           // Sector number waiting to be written (0=none)

const byte FreeSectorCount     = ZP.M7;           // Cache of free sectors (0=unknown, recalculate)
const byte OpenFileCount       = ZP.M8;           // Number of files in directory (0-16)

const byte TempSector          = ZP.M9;           // Temporary sector number for operations
const byte TempByte1           = ZP.M10;          // General temporary storage
const byte TempByte2           = ZP.M11;          // General temporary storage

// Future expansion (5 slots available)
const byte Reserved1           = ZP.M12;          // Available for future features
const byte Reserved2           = ZP.M13;          // Available for future features  
const byte Reserved3           = ZP.M14;          // Available for future features
const byte Reserved4           = ZP.M15;          // Available for future features
const byte Reserved5           = ZP.M16;          // Available for future features
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
- Scan FATBuffer[2..255] for first free sector (FAT[i] == 0)
- Allocate sectors on-demand as file grows
- No pre-allocation or clustering

File deallocation: Chain following
- Start from file's startSector
- Follow FATBuffer chain marking each sector free
- No defragmentation needed (all sectors equal)

Directory management: Linear scan
- 16 entries maximum, linear search acceptable
- File creation/deletion compacts directory
```

### **Buffer Strategy**
```
Three-buffer approach for optimal performance:

FileDataBuffer (256 bytes):
- Current file sector being read/written
- Streaming I/O operations (AppendStream/NextStream)
- File content buffering during save/load

WorkingSectorBuffer (256 bytes):  
- Directory sector operations
- Temporary sector I/O during multi-step operations
- General workspace for file system metadata

FATBuffer (256 bytes):
- Dedicated FAT caching (major performance improvement)
- Loaded once per file operation, flushed on completion
- Eliminates repeated sector 0 reads
- Enables atomic FAT updates

Benefits:
- FAT stays resident throughout operations
- No buffer conflicts between file data and metadata
- Atomic updates reduce EEPROM wear
- Clean separation of concerns
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