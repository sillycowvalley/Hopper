## Hopper 6502 Kernel Size Analysis

### Individual Component Sizes (ROM)

| Component | Size (bytes) | Description |
|-----------|-------------:|-------------|
| **Math & Types** | | |
| Float unit | 2,000 | IEEE 754 single precision (Add, Sub, Mul, Div, ToLong) |
| Long unit | 1,535 | 32-bit integer math, comparisons, conversions |
| **Memory Management** | | |
| Memory.asm | 240 | Initialize, Copy, Available |
| Allocate.asm | 488 | Dynamic allocation with best-fit |
| Free.asm | 545 | Freelist management, coalescing |
| *Subtotal* | *1,273* | *Complete heap management* |
| **I/O & Communication** | | |
| Serial core | 140 | Basic serial I/O routines |
| ACIA 6850 | 250 | Hardware UART driver |
| I2C core | 160 | Start, Stop, ByteOut, ByteIn, Scan |
| **Storage** | | |
| EEPROM driver | 143 | 24AA512/1024 page operations |
| EEPROM unit | 69 | Initialize, Detect, GetSize |
| File system | 1,656 | FAT filesystem, directory, Save/Load/Delete |
| *Subtotal* | *1,868* | *Complete storage subsystem* |

### Kernel Configuration Options

| Configuration | Components Included | Total Size | ROM Target |
|--------------|--------------------|-----------:|------------|
| **Full Kernel** | Float + Long + Memory + Serial + I2C + Storage | 7,226 bytes | 8K ROM |
| **Integer Kernel** | Long + Memory + Serial + I2C + Storage | 5,226 bytes | 8K ROM |
| **Minimal I/O** | Long + Memory + Serial | 3,198 bytes | 4K ROM |
| **Storage Focus** | Long + Memory + I2C + Storage | 4,836 bytes | 8K ROM |
| **Float Math** | Float + Long + Memory + Serial | 5,198 bytes | 8K ROM |

### ROM Utilization

| ROM Size | Configuration | Kernel Size | Free Space | Available For |
|----------|--------------|------------:|-----------:|---------------|
| **8K** | Integer Kernel | 5,226 | 2,966 | Small monitor, user code |
| **8K** | Minimal I/O | 3,198 | 4,994 | Application specific |
| **4K** | Bare Minimum | 3,198 | 898 | Embedded controller |

---

## Kernel RAM Usage

### Zero Page Allocation (0x00-0xFF)

| Address | Size | Module | Purpose |
|---------|-----:|--------|---------|
| **Memory Management** | | | |
| 0x06-0x07 | 2 | Memory | Heap FREELIST pointer |
| 0x08-0x09 | 2 | Memory | HEAPSTART, HEAPSIZE |
| 0x4F-0x5E | 16 | Allocate | Memory allocator workspace (ma*) |
| 0x4F-0x60 | 18 | Free | Memory free workspace (mf*) - overlaps |
| **Math Units** | | | |
| 0x0E-0x10 | 3 | Long/Float | ACC (accumulator with type) |
| 0x11-0x15 | 5 | Long/Float | TOP (top of stack value) |
| 0x16-0x1A | 5 | Long/Float | NEXT (next stack value) |
| 0x4F-0x56 | 8 | Long | RESULT registers - overlaps memory |
| 0x6F | 1 | Long/Float | FSIGN (sign for mul/div) |
| **General Purpose** | | | |
| 0x1B-0x1C | 2 | General | IDX pointer |
| 0x1D-0x1E | 2 | General | IDY pointer |
| **Serial/ACIA** | | | |
| 0x0A-0x0B | 2 | Serial | InWritePointer, InReadPointer |
| 0x0C | 1 | Serial | SerialFlags |
| 0xEC-0xED | 2 | ACIA | Hardware registers - HARDWARE MAPPED |
| **I2C** | | | |
| 0x77-0x78 | 2 | I2C | I2CInWritePtr, I2CInReadPtr |
| 0x88-0x8A | 3 | I2C | OutB, InB, LastAck |
| 0xF0-0xFF | 16 | VIA | Hardware registers - HARDWARE MAPPED |
| **File System** | | | |
| 0x61-0x66 | 6 | File | FSOURCE/FDEST/FLENGTH addresses |
| 0x79-0x7A | 2 | File | STR pointer (filename) |
| 0x8E-0x97 | 10 | File | Transfer state, file sectors |
| **Timer** | | | |
| 0x22-0x25 | 4 | Timer | TICK0-3 counters |


### Module RAM Requirements

| Module | Zero Page | Main RAM | Total | Notes |
|--------|----------:|---------:|------:|-------|
| **Memory Core** | | | | |
| Memory.asm | 4 bytes | 0 | 4 | FREELIST, HEAPSTART/SIZE |
| Allocate.asm | 16 bytes* | 0 | 16 | *Shared workspace |
| Free.asm | 18 bytes* | 0 | 18 | *Shared workspace |
| **Math** | | | | |
| Long unit | 19 bytes* | 0 | 19 | *Overlaps with memory |
| Float unit | 19 bytes* | 0 | 19 | *Shares with Long |
| **I/O** | | | | |
| Serial | 3 bytes | 256 | 259 | Input buffer |
| ACIA | 2 bytes | 0 | 2 | Hardware mapped |
| I2C | 5 bytes | 256 | 261 | Input buffer |
| **Storage** | | | | |
| EEPROM | 0 bytes | 0 | 0 | Uses I2C resources |
| File system | 18 bytes | 768 | 786 | 3×256 byte buffers |

### Main RAM Allocation (0x0100+) - Contiguous Layout

| Address Range | Size | Module | Purpose |
|---------------|-----:|--------|---------|
| **System** | | | |
| 0x0100-0x01FF | 256 | CPU | Hardware stack (JSR/RTS/interrupts) |
| **I/O Buffers** | | | |
| 0x0200-0x02FF | 256 | Serial | Serial input buffer |
| 0x0300-0x03FF | 256 | I2C | I2C input buffer |
| **File System Buffers** | | | |
| 0x0400-0x04FF | 256 | File | FAT buffer |
| 0x0500-0x05FF | 256 | File | Directory buffer |
| 0x0600-0x06FF | 256 | File | File data buffer |
| **Dynamic Heap** | | | |
| 0x0700-0x7FFF | 30,976 | Memory | **30.25 KB** heap (32K RAM) |
| 0x0700-0xBFFF | 47,360 | Memory | **46.25 KB** heap (48K RAM) |
| 0x0700-0xDFFF | 55,552 | Memory | **54.25 KB** heap (56K RAM) |

### Configuration-Specific Memory Maps

| Configuration | Buffers End | Heap Start | 32K RAM Heap | 48K RAM Heap | 56K RAM Heap |
|---------------|-------------|------------|-------------:|-------------:|-------------:|
| **Full Kernel** | 0x06FF | 0x0700 | 30.25 KB | 46.25 KB | 54.25 KB |
| **No Filesystem** | 0x03FF | 0x0400 | 31.00 KB | 47.00 KB | 55.00 KB |
| **Minimal (Serial only)** | 0x02FF | 0x0300 | 31.25 KB | 47.25 KB | 55.25 KB |
| **No I/O Buffers** | 0x01FF | 0x0200 | 31.50 KB | 47.50 KB | 55.50 KB |

### Memory Overhead Summary

| Component | Size | Cumulative Overhead |
|-----------|-----:|--------------------:|
| Hardware Stack | 256 bytes | 256 bytes |
| + Serial Buffer | 256 bytes | 512 bytes |
| + I2C Buffer | 256 bytes | 768 bytes |
| + File System | 768 bytes | **1,536 bytes** |

**Total kernel RAM overhead: 1.5 KB** (not including zero page)

This leaves **95.3%** of a 32K system available as heap space for the full kernel configuration!

### Zero Page Optimization Techniques

1. **Workspace Overlap**: Memory allocator (16 bytes) overlaps with Free (18 bytes) and Long RESULT (8 bytes)
2. **Hardware Mapping**: ACIA (0xEC-0xED) and VIA (0xF0-0xFF) use fixed hardware addresses
3. **Shared Math Registers**: Long and Float share all 19 bytes of working registers
4. **Reused Pointers**: File system reuses general IDX/IDY pointers

### Key Findings

- **Compact Code**: Hopper Assembly generates very efficient 6502 code
- **Minimal RAM Overhead**: Only 1.5KB of main RAM for full kernel with filesystem
- **Zero Page Efficient**: Only ~44 unique bytes used, leaving 200+ bytes for user applications
- **Huge Heap**: ~30KB available for dynamic allocation with 32K RAM
- **Modular Design**: Components can be mixed/matched for specific needs
- **8K Sweet Spot**: Integer kernel fits comfortably with room for extensions
- **Storage Efficient**: Complete filesystem in under 2KB ROM and 768 bytes RAM