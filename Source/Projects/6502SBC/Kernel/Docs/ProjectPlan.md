# Hopper 6502 Command Console & System Project Plan

## File Module API Reference
FAT-based filesystem for EEPROM storage with 256-byte sectors.

#### File Operations
- **Initialize()** - Formats filesystem, creates empty FAT and directory
- **Exists()** - Checks if file exists (Input: STR = filename pointer, Output: C set if exists)
- **Delete()** - Deletes file, frees sectors, compacts directory
- **Dir()** - Lists all files in directory

#### Save Operations
- **StartSave()** - Opens file for writing (Input: STR = filename pointer)
- **AppendStream()** - Writes data to open file (Input: SectorSource, TransferLength)
- **EndSave()** - Closes file and updates metadata

#### Load Operations  
- **StartLoad()** - Opens file for reading (Input: STR = filename pointer)
- **NextStream()** - Reads next chunk (Output: FileDataBuffer, TransferLength, NC at EOF)

#### Simplified Operations
- **Save()** - Complete file save (combines Start/Append/End)
- **Load()** - Complete file load (combines Start/Next operations)

---

## Implementation Plan

### Phase 1: Basic Console Commands

#### MEM Command
- Display heap statistics using Heap.Available()
- Show total RAM, used, free memory
- Format: `Total: 32K  Used: 1024  Free: 31744`

#### FORMAT Command
- Prompt: `Format filesystem? (Y/N):`
- If Y: Call File.Initialize() to create new filesystem
- Display completion message with available space

#### DIR Command
- List all files using File.Dir()
- **DEBUG mode**: Also dump FAT sectors, directory entries, file data
- Show file count and total bytes used
- Format: `filename.ext    1234 bytes`

#### HEX Command
- `HEX <filename>` - Parse Intel HEX from serial input
- Read lines until `:00000001FF` (end record)
- Parse address, data, checksum
- Use File.Save() to store binary data
- Display progress and final byte count

#### DEL Command  
- `DEL <filename>` - Delete specified file
- Confirm deletion: `Delete filename.ext? (Y/N):`
- Use File.Delete() and display result

---

### Phase 2: System Call Dispatch

#### Function Vector Table
- Create jump table for system services
- Standard calling convention using registers
- Error codes in accumulator

#### Core System Calls
- **Memory Management** - Allocate, Free, Available
- **File Operations** - Open, Read, Write, Close, Delete
- **Timer Services** - Delay, Millis, Seconds
- **Console I/O** - Print, Input, Character I/O

#### API Documentation
- Function numbers and parameters
- Register usage and preservation rules
- Error codes and handling

---

### Phase 3: Program Execution

#### Executable File Support
- `<filename>` - Load and execute if marked executable
- File header format with execution flag
- Load program into available memory
- Jump to entry point with clean state

#### Program Loading
- Use File.Load() to read executable
- Allocate memory using Heap.Allocate()
- Validate file format and checksums
- Set up execution environment

#### Error Handling
- File not found
- Not executable
- Insufficient memory
- Invalid file format

---

## Command Parser Implementation

### Input Processing
- Read characters from serial until Enter
- Handle backspace for editing
- Echo characters to console
- Maximum command line length

### Command Parsing
- Split input into command and parameters
- Case-insensitive command matching
- Parameter validation
- Error messages for invalid syntax

### Built-in Commands Table
```
Command    Parameters    Description
-------    ----------    -----------
MEM        none          Show memory statistics
FORMAT     none          Format filesystem
DIR        none          List files  
HEX        filename      Save Intel HEX file
DEL        filename      Delete file
```

---

## Testing Strategy

### Manual Testing via Console
- File operations tested through DIR, HEX, DEL commands
- Memory management verified with MEM command
- Filesystem integrity checked with FORMAT/DIR cycle
- Intel HEX parsing tested with real hex files

### Debug Features
- DIR command shows internal filesystem state
- Memory dumps for troubleshooting
- Error messages with specific failure codes
- Trace output for complex operations

---

## Success Criteria

1. **Console responds** - All commands execute without hanging
2. **File operations work** - Save, load, delete cycle successfully
3. **HEX parsing accurate** - Binary files match original data
4. **Memory stable** - No leaks after repeated operations
5. **System calls functional** - Programs can use API services
6. **Program execution** - Load and run executable files

---

## Development Notes

- Test File module through console commands rather than separate unit tests
- DEBUG mode provides detailed internal state information
- Intel HEX format provides real-world file I/O testing
- System call interface enables future program development