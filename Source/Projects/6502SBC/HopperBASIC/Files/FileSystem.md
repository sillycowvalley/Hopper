# HopperBASIC File System Architecture (Complete Specification)
**Document Type: System Architecture Specification**

## Overview

This document provides the complete architecture for HopperBASIC's file system, designed for simple and reliable program storage on I2C EEPROM. The system uses a **single root directory** (no subdirectories) and integrates seamlessly with existing HopperBASIC infrastructure.

---

## Design Philosophy

### **Simplicity First**
- **Root directory only** (no subdirectories or folder hierarchy)
- Fixed 16-byte file descriptors
- 256-byte page-aligned storage
- Linear chain allocation (no complex tree structures)

### **BASIC-Centric**
- Optimized for program save/load operations
- Simple filename validation (8.3 style, max 12 characters)
- Integration with existing BASIC error handling
- Reuse existing string/character utilities

### **Safety and Robustness**
- All operations are transactional
- FORMAT command requires explicit confirmation
- Clear error paths with specific messages
- Defensive programming against corruption
- Graceful degradation when EEPROM fills up

---

## On-Disk Structure

### **EEPROM Memory Map**
```
┌─────────────────────────────────────────────────────────────┐
│ Page 0: Chain Block (File Allocation Table)                │
├─────────────────────────────────────────────────────────────┤
│ Page 1: Root Directory Block 1                             │
│ Page 2: Root Directory Block 2 (if root directory expands) │
│ Page N: Root Directory Block N (root directory chain)      │
├─────────────────────────────────────────────────────────────┤
│ Page M: File Data Blocks (allocated via Chain Block)       │
│ Page M+1: File Data Blocks                                 │
│ ...                                                         │
│ Page 255: File Data Blocks                                 │
└─────────────────────────────────────────────────────────────┘
```

### **Chain Block Structure (Page 0) - File Allocation Table**
```
Byte Layout:
┌──────┬─────────────────────────────────────────────────────────┐
│ 0    │ Reserved (value = 1, marks page 0 as system)           │
│ 1    │ Reserved (value = 1, marks page 1 as system)           │
│ 2    │ Block 2 status: 0=free, 1=end-of-chain, 2-255=next    │
│ 3    │ Block 3 status: 0=free, 1=end-of-chain, 2-255=next    │
│ ...  │ ...                                                     │
│ 255  │ Block 255 status: 0=free, 1=end-of-chain, 2-255=next  │
└──────┴─────────────────────────────────────────────────────────┘

Chain Block Values:
  0     = Free block (available for allocation)
  1     = End of chain marker (last block in file)
  2-255 = Next block number in chain
```

### **Root Directory Structure (Pages 1+)**
```
Root Directory Page Layout (256 bytes):
┌──────────────────────────────────────────────────────────────┐
│ File Entry 0  (bytes 0-15)   - 16 bytes                     │
│ File Entry 1  (bytes 16-31)  - 16 bytes                     │
│ File Entry 2  (bytes 32-47)  - 16 bytes                     │
│ ...                                                          │
│ File Entry 15 (bytes 240-255) - 16 bytes                    │
└──────────────────────────────────────────────────────────────┘

Total: 16 file entries per root directory page
Root directory can span multiple pages using chain system
All files are stored in this single root directory
```

### **File Entry Structure (16 bytes)**
```
File Descriptor Layout:
┌────────┬────────────────────────────────────────────────────────┐
│ Offset │ Description                                            │
├────────┼────────────────────────────────────────────────────────┤
│ 0-11   │ Filename (null-terminated, max 12 characters)         │
│        │ Examples: "HELLO.BAS", "GAME", "DATA.TXT"              │
├────────┼────────────────────────────────────────────────────────┤
│ 12     │ File Type and Name Length                              │
│        │ Bits 0-3: Actual filename length (1-12)               │
│        │ Bits 4-7: File type (0x0 = file, 0x1 = reserved)      │
├────────┼────────────────────────────────────────────────────────┤
│ 13     │ Start Block Number                                     │
│        │ 0     = Invalid/corrupted entry                        │
│        │ 1     = Empty file (no data blocks allocated)          │
│        │ 2-255 = First block of file data                       │
├────────┼────────────────────────────────────────────────────────┤
│ 14     │ File Size Low Byte (LSB)                               │
│ 15     │ File Size High Byte (MSB)                              │
│        │ Maximum file size: 65535 bytes                         │
└────────┴────────────────────────────────────────────────────────┘

Empty Entry Detection:
  - filename[0] == 0x00 indicates unused entry slot
  - Used for root directory compaction and free slot detection
```

### **File Data Storage**
```
File Data Chain Example:
┌─────────────────────────────────────────────────────────────┐
│ File: "HELLO.BAS" (450 bytes)                               │
│ Start Block: 15                                             │
│                                                             │
│ Block 15: First 256 bytes of file data                     │
│ Chain[15] = 23 (points to next block)                      │
│                                                             │
│ Block 23: Next 194 bytes of file data + padding            │
│ Chain[23] = 1 (end of chain marker)                        │
└─────────────────────────────────────────────────────────────┘
```

---

## Zero Page Allocation (Reusing Existing Infrastructure)

### **Memory Operations (0x5F-0x64)**
```
ZP.FSOURCEADDRESS (0x5F-0x60):     Source buffer/EEPROM address
  - EEPROM page addresses for readPage/writePage
  - Source buffer addresses for memory operations
  
ZP.FDESTINATIONADDRESS (0x61-0x62): Destination buffer/EEPROM address  
  - Target buffer addresses for EEPROM reads
  - Destination addresses for memory copies
  
ZP.FLENGTH (0x63-0x64):            Transfer length/operation count
  - Byte counts for memory operations
  - File sizes and transfer lengths
```

### **List/Chain Traversal (0x65-0x6E)**
```
ZP.LCURRENT (0x65-0x66):   Current block/root directory entry pointer
  - Current block number during chain following
  - Current file entry pointer during root directory searches
  
ZP.LHEAD (0x67-0x68):      Chain head/root directory start pointer
  - File start block number
  - Root directory page start pointer (always page 1)
  
ZP.LPREVIOUS (0x6B-0x6C):  Previous block in chain
  - For chain manipulation and compaction
  - Root directory entry management
  
ZP.LNEXT (0x6D-0x6E):      Next block in chain
  - Next block number in file chain
  - Next root directory page in chain
```

### **File System Specific (0x69-0x6A)**
```
ZP.FSIGN (0x69):           File operation flags/status
  - Bit 0: Read/Write mode flag
  - Bit 1: File exists flag
  - Bit 2: Root directory full flag
  - Bit 3: Operation success flag
  
ZP.LHEADX (0x6A):          Extended operation code/file handle index
  - File handle index (0-15)
  - Operation mode codes
  - Temporary calculation workspace
```

---

## Buffer Management Strategy

### **FunctionOpCodeBuffer Allocation (512 bytes)**
```
Memory Layout:
┌─────────────────────────────────────────────────────────────┐
│ Buffer A: FunctionOpCodeBuffer[0-255]                       │
│ Primary EEPROM page buffer                                  │
│ - Root directory page reads/writes                          │
│ - File data page operations                                 │
│ - Chain block operations                                    │
├─────────────────────────────────────────────────────────────┤
│ Buffer B: FunctionOpCodeBuffer[256-511]                     │
│ Secondary EEPROM page buffer                                │
│ - Backup/comparison operations                              │
│ - Multi-page operations                                     │
│ - Root directory compaction workspace                       │
└─────────────────────────────────────────────────────────────┘
```

### **Buffer Usage Patterns**
```
Root Directory Operations:
  Buffer A: Current root directory page
  Buffer B: Next root directory page (for chain following)

File I/O Operations:
  Buffer A: Current file data page
  Buffer B: Chain block (for navigation)

Root Directory Compaction:
  Buffer A: Target page (receiving moved entries)
  Buffer B: Source page (entries being moved)

Format Operation:
  Buffer A: Initialization workspace
  Buffer B: Verification buffer
```

---

## BASIC Command Integration

### **Complete Command Set**
```
SAVE "filename"    → Save current program to EEPROM
LOAD "filename"    → Load program from EEPROM and run NEW first
DIR                → List all files with sizes and free space
DEL "filename"     → Delete file with confirmation prompt
FORMAT             → Format file system with safety prompt
```

### **Command Parsing Integration**
```hopper
enum Token {
    // Existing BASIC tokens...
    SAVE = 0x91,
    LOAD = 0x92, 
    DIR  = 0x93,
    DEL  = 0x94,
    FORMAT = 0x95
}
```

### **Safety Prompts**

#### **FORMAT Command Safety Protocol**
```
User Input:  FORMAT
System:      "FORMAT will erase all files. Are you sure? (Y/N)"
User Input:  Y or y
System:      "Formatting..."
             [performs format operation]
             "Format complete"

User Input:  FORMAT  
System:      "FORMAT will erase all files. Are you sure? (Y/N)"
User Input:  N or n (or anything else)
System:      "Format cancelled"
```

#### **DEL Command Confirmation**
```
User Input:  DEL "HELLO.BAS"
System:      "Delete HELLO.BAS? (Y/N)"
User Input:  Y or y
System:      "File deleted"

User Input:  DEL "HELLO.BAS"
System:      "Delete HELLO.BAS? (Y/N)" 
User Input:  N or n (or anything else)
System:      "Delete cancelled"
```

#### **File Overwrite Protection**
```
User Input:  SAVE "EXISTING.BAS"
System:      "File exists. Overwrite? (Y/N)"
User Input:  Y or y
System:      [overwrites file]
             "Program saved"

User Input:  SAVE "EXISTING.BAS"
System:      "File exists. Overwrite? (Y/N)"
User Input:  N or n (or anything else)  
System:      "Save cancelled"
```

---

## Operation Flow Architecture

### **File Save Operation (SAVE "filename")**
```
1. Validate filename (length 1-12, valid characters)
2. Tokenize current program → TokenizerBuffer
3. Search root directory for existing file
4. If file exists → prompt "File exists. Overwrite? (Y/N)"
5. If confirmed or new file:
   a. Allocate/reuse file descriptor in root directory
   b. Allocate data blocks via chain system
   c. Write tokenized program to EEPROM blocks
   d. Update file descriptor with size and start block
   e. Write updated root directory to EEPROM
6. Report "Program saved" or error message
```

### **File Load Operation (LOAD "filename")**
```
1. Validate filename format
2. Execute NEW command (clear current program)
3. Search root directory for specified file
4. If file found:
   a. Read file data blocks following chain
   b. Load data into TokenizerBuffer
   c. Validate token structure
   d. Set up program state for execution
5. Report "Program loaded" or error message
```

### **File List Operation (DIR)**
```
1. Print header: "Files:"
2. Read root directory pages following chain
3. For each valid file descriptor:
   a. Extract filename and file size
   b. Format and print: "  FILENAME.BAS    1234 bytes"
4. Calculate and display totals:
   a. Count total files
   b. Calculate used space
   c. Calculate free space
5. Print footer: "X file(s), Y bytes used, Z bytes free"
```

### **File Delete Operation (DEL "filename")**
```
1. Validate filename format
2. Search root directory for file
3. If file found:
   a. Prompt "Delete FILENAME? (Y/N)"
   b. If confirmed:
      - Follow file chain and mark all blocks as free (0)
      - Clear file descriptor (set filename[0] = 0)
      - Compact root directory if needed
      - Report "File deleted"
   c. If cancelled: "Delete cancelled"
4. If not found: "FILE NOT FOUND"
```

### **Format Operation (FORMAT)**
```
1. Display safety prompt: "FORMAT will erase all files. Are you sure? (Y/N)"
2. Wait for user input
3. If confirmed (Y/y):
   a. Print "Formatting..."
   b. Initialize chain block:
      - chainBlock[0] = 1 (reserved)
      - chainBlock[1] = 1 (reserved) 
      - chainBlock[2-255] = 0 (free)
   c. Initialize empty root directory (all zeros)
   d. Write chain block and root directory to EEPROM
   e. Print "Format complete"
4. If cancelled: "Format cancelled"
```

---

## API Architecture

### **Core File Operations**
```hopper
// File handle management
FileOpen(filename, mode) → handle     // Returns file handle or null
FileClose(handle) → status            // Always succeeds
FileRead(handle, buffer, count) → bytes_read
FileWrite(handle, buffer, count) → bytes_written
FileSeek(handle, position) → status   // Future enhancement
FileSize(handle) → size              // Get file size

// Root directory operations  
RootDirFormat() → status             // Format file system
RootDirList() → file_count           // List all files
RootDirFind(filename) → descriptor_ptr // Find file descriptor
RootDirDelete(filename) → status     // Delete file
RootDirCompact() → status           // Defragment root directory

// Block-level operations
BlockRead(block_num, buffer) → status
BlockWrite(block_num, buffer) → status
BlockAllocate() → block_num          // Find free block
BlockFree(block_num) → status        // Mark block as free
ChainFollow(block_num) → next_block  // Get next in chain
```

### **Integration Functions**
```hopper
// BASIC command implementations
BasicSave(filename) → status         // SAVE command
BasicLoad(filename) → status         // LOAD command  
BasicDir() → status                  // DIR command
BasicDelete(filename) → status       // DEL command
BasicFormat() → status               // FORMAT command

// Utility functions
ValidateFilename(filename) → valid   // Check filename format
PromptYesNo(message) → confirmed     // Get Y/N confirmation
FormatFileSize(size, buffer)         // Format size for display
```

---

## Error Handling Integration

### **File System Error Codes**
```hopper
enum FileSystemErrors {
    FileNotFound = 0x30,        // File does not exist
    DiskFull = 0x31,           // No free blocks available
    InvalidFilename = 0x32,     // Filename format invalid
    FileExists = 0x33,         // File already exists (overwrite prompt)
    EEPROMError = 0x34,        // I2C communication failure
    CorruptedFS = 0x35,        // File system corruption detected
    RootDirFull = 0x36,        // No free root directory entries
    InvalidHandle = 0x37       // File handle is invalid
}
```

### **Error Message Table**
```
Error Code → User Message
0x30 → "FILE NOT FOUND"
0x31 → "DISK FULL"
0x32 → "INVALID FILENAME"
0x33 → "FILE EXISTS" (handled via prompt)
0x34 → "EEPROM ERROR" 
0x35 → "CORRUPTED FILE SYSTEM"
0x36 → "ROOT DIRECTORY FULL"
0x37 → "INVALID FILE HANDLE"
```

### **Error Integration Pattern**
```hopper
FileSystemError(errorCode)
{
    LDA errorCode
    STA ZP.LastError
    Error.CheckAndPrint();
    CLC  // Signal error to caller
}
```

---

## Implementation Phases

### **Phase 1: Foundation Infrastructure (Week 1)**
```
Components:
1. EEPROM I/O wrapper functions using FSOURCE/FDESTINATION/FLENGTH
2. Chain block manipulation using LCURRENT/LNEXT traversal patterns
3. Buffer management for FunctionOpCodeBuffer dual-page operations
4. Basic error handling integration with existing Error.asm

Deliverables:
- BlockRead()/BlockWrite() functions working
- Chain allocation/deallocation working
- Buffer allocation/cleanup working
- Error reporting integrated

Testing:
- Read/write known patterns to EEPROM
- Test chain allocation edge cases
- Verify buffer management doesn't leak
- Test error path cleanup
```

### **Phase 2: Root Directory System (Week 2)**
```
Components:
5. Root directory initialization (FORMAT command implementation)
6. Root directory search algorithms using chain traversal
7. File descriptor management (add/delete/update)
8. Root directory compaction and defragmentation

Deliverables:
- FORMAT command working with safety prompts
- File search by name working
- Root directory entry manipulation working
- Root directory compaction working

Testing:
- Format and verify empty file system
- Test root directory spanning multiple pages
- Test file descriptor lifecycle
- Test root directory compaction edge cases
```

### **Phase 3: File Operations (Week 3)**
```
Components:
9. File creation and deletion operations
10. File data I/O with automatic chain following
11. File handle management and validation
12. Data integrity checking and validation

Deliverables:
- File create/delete working
- File read/write with chaining working
- File handle lifecycle working
- Data validation working

Testing:
- Create files of various sizes
- Test file spanning multiple blocks
- Test file deletion and space reclamation
- Test data integrity across power cycles
```

### **Phase 4: BASIC Integration (Week 4)**
```
Components:
13. SAVE/LOAD command parsing and token integration
14. DIR command with formatted output and statistics
15. DEL command with confirmation prompts
16. FORMAT command with comprehensive safety checks
17. Complete error message integration and user experience

Deliverables:
- All BASIC commands working
- User-friendly prompts and confirmations
- Comprehensive error handling
- Performance optimization

Testing:
- Test complete SAVE/LOAD/DIR/DEL workflows
- Test all error conditions and recovery
- Test user experience with prompts
- Performance testing with various file sizes
```

---

## File System Capacity and Limits

### **Storage Capacity**
```
Total EEPROM: 256 pages × 256 bytes = 65,536 bytes (64KB)
Reserved: 2 pages (chain block + 1 root directory page) = 512 bytes
Available for files: 254 pages = 65,024 bytes

Maximum files: Limited by root directory space
- 16 entries per root directory page
- Root directory can span multiple pages via chaining
- Practical limit: ~200 files (depends on filename lengths)

Maximum file size: 65,535 bytes (16-bit size field)
Typical BASIC program: 1-10KB (file system very suitable)
```

### **Performance Characteristics**
```
File Access Times (estimated):
- Root directory search: 1-3 EEPROM page reads
- Small file read: 2-4 EEPROM page reads  
- Large file read: Linear with file size
- File write: Linear with file size + root directory update
- File listing: Linear with number of files

EEPROM Write Cycles:
- Typical EEPROM: 1,000,000 write cycles per page
- File system design minimizes writes to same pages
- Expected lifetime: Many years of normal use
```

---

## Conclusion

This complete architecture provides a robust, efficient file system that integrates seamlessly with HopperBASIC's existing infrastructure. The design uses a **simple single root directory** approach while leveraging proven patterns from the existing codebase.

Key benefits:
- **Simple single root directory** - no complex hierarchy to manage
- **Zero additional memory allocation** through smart reuse of existing buffers
- **Familiar development patterns** using established zero page conventions
- **Comprehensive safety features** with user confirmations for destructive operations
- **Robust error handling** integrated with existing BASIC error systems
- **Scalable implementation** through well-defined phases
- **Efficient storage** optimized for BASIC program storage and retrieval

The file system provides everything needed for practical BASIC program development: save your programs, load them back, organize them with file listings, and manage storage space through file deletion and formatting - all within a simple, flat file structure that's easy to understand and maintain.