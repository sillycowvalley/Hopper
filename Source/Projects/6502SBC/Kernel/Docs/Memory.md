# Hopper 6502 BIOS Memory Layout

## Memory Map

| Address Range | Size | Usage |
|---------------|------|-------|
| **0x0000-0x00FF** | 256 bytes | Zero Page |
| **0x0100-0x01FF** | 256 bytes | Stack Page 0 |
| **0x0200-0x02FF** | 256 bytes | Stack Page 1 |
| **0x0300-0x03FF** | 256 bytes | Stack Page 2 |
| **0x0400-0x04FF** | 256 bytes | Stack Page 3 |
| **0x0500-0x05FF** | 256 bytes | Serial Input Buffer |
| **0x0600-0x06FF** | 256 bytes | I2C Input Buffer |
| **0x0700-0x07FF** | 256 bytes | Work Space |
| **0x0800-0x0AFF** | 768 bytes | File System Buffers |
| **0x0B00-0x7FFF** | ~29KB | User Memory (program + heap) |
| **0x8000-0xFFFF** | 32KB | ROM (with ROM_32K) |

## Work Space Subdivision (0x0700-0x07FF)

| Address | Size | Usage |
|---------|------|-------|
| **0x0700-0x073F** | 64 bytes | Line Buffer (command line parser) |
| **0x0740-0x07BF** | 128 bytes | Hex Buffer (Intel HEX data) |
| **0x07C0-0x07FF** | 64 bytes | General Buffer (temporary) |

## File System Buffers (0x0800-0x0AFF)

| Address | Size | Usage |
|---------|------|-------|
| **0x0800-0x08FF** | 256 bytes | FAT cache |
| **0x0900-0x09FF** | 256 bytes | Current directory sector |
| **0x0A00-0x0AFF** | 256 bytes | Data sector buffer |

## Buffer Details

### Serial Input Buffer (0x0500-0x05FF)
- 256-byte circular buffer
- Write pointer at ZP 0x06
- Read pointer at ZP 0x07

### I2C Input Buffer (0x0600-0x06FF)
- 256-byte buffer for I2C.RequestFrom
- Write pointer at ZP 0x08
- Read pointer at ZP 0x09

### File System Buffers (0x0800-0x0AFF)
- FAT cache: Keeps FAT in memory for efficient file operations
- Directory sector: Current directory sector for file enumeration
- Data sector: Buffer for file read/write operations

## User Memory Start
- Programs load at **0x0B00**
- Heap begins immediately after program end
- Total user space depends on ROM configuration