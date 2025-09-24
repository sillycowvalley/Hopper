# Nano OS - Dynamic Module System for Hopper 6502

## Overview

Nano OS is a sophisticated yet minimal operating system for the Hopper 6502 platform that provides a CP/M-style command interface with three possible implementation strategies:

1. **Basic Nano OS** - Monolithic system with full kernel in ROM
2. **Bootstrap Nano OS** - Two-stage loader with modules in EEPROM
3. **Dynamic Nano OS** - On-demand module loading with memory management

## Current Memory Architecture

### Fixed Memory Regions (All Configurations)
```
$0000-$00FF  Zero Page (System variables, workspace, hardware I/O)
$0100-$01FF  Hardware Stack (6502 stack page)
$0200-$04FF  Extended Stack Space (3 pages)
$0500-$05FF  Serial Input Buffer (256 bytes)
$0600-$06FF  Reserved for I/O expansion
$0700-$07FF  Work Space (256 bytes, includes 64-byte line buffer)
$0800-$0AFF  Reserved for BIOS extensions
$0B00        Program Entry Point
```

### System Configurations

| Configuration | ROM Size | Typical RAM | User Space | Key Features |
|--------------|----------|-------------|------------|--------------|
| **Basic** | 8KB-32KB | 32KB-56KB | 30KB-54KB | Full kernel in ROM, instant boot |
| **Bootstrap** | 4KB | 32KB-56KB | 26KB-46KB | Loads modules from EEPROM at boot |
| **Dynamic** | 4KB | 56KB-60KB | 50KB-58KB | Loads modules on demand |

---

## Basic Nano OS (Monolithic System)

### Memory Map with 32KB ROM / 32KB RAM
```
ROM (32KB):
$8000-$FFFF  Complete Hopper Kernel
  - Memory management
  - File system
  - Serial I/O
  - Long math (32-bit)
  - Float math (IEEE 754)
  - BIOS interface
  - Nano OS command processor

RAM (32KB):
$0000-$0AFF  System areas (2.75KB)
$0B00-$7FFF  User programs (30KB free)
```

### Memory Map with 8KB ROM / 56KB RAM
```
ROM (8KB):
$E000-$FFFF  Essential kernel only
  - Core BIOS
  - Memory management  
  - File system
  - Serial I/O
  - Basic math
  - Nano OS shell

RAM (56KB):
$0000-$0AFF  System areas (2.75KB)
$0B00-$DFFF  User programs (53.25KB free)
```

### Advantages
- **Instant boot** - No loading delays
- **Maximum reliability** - No module loading failures
- **Simple memory map** - Contiguous user space
- **Best performance** - All code in ROM

### Disadvantages
- **Fixed functionality** - Can't add features without new ROM
- **Larger ROM required** - 8KB minimum for useful system

---

## Bootstrap Nano OS (Two-Stage Loader)

### Memory Map with 4KB ROM / 32KB RAM
```
ROM (4KB):
$F000-$FFFF  Bootstrap kernel
  - Minimal BIOS
  - EEPROM file loader
  - Memory allocator
  - Stage 2 loader

RAM after Stage 2:
$0000-$0AFF  System areas (2.75KB)
$0B00-$5FFF  User programs (21KB)
$6000-$7FFF  Loaded kernel modules (8KB)
  $6000-$67FF  Long math module
  $6800-$6FFF  Float math module  
  $7000-$77FF  Extended file ops
  $7800-$7FFF  Command extensions
```

### Boot Sequence
1. ROM bootstrap initializes hardware
2. Loads module index from EEPROM
3. Loads core modules to high RAM
4. Patches jump vectors
5. Starts command processor

### Module Format in EEPROM
```
Header (8 bytes):
  $00-$01  Load address
  $02-$03  Module size
  $04      Module ID
  $05      Version
  $06      Dependencies
  $07      Checksum

Code:
  Module binary code

Relocation table:
  List of addresses needing fixup
```

### Advantages
- **Small ROM footprint** - Only 4KB needed
- **Field upgradeable** - Update modules in EEPROM
- **Selective loading** - Choose which modules to load

### Disadvantages  
- **Slower boot** - Must load from EEPROM
- **RAM overhead** - Modules consume user RAM
- **Complex loader** - Relocation and patching logic

---

## Dynamic Nano OS (On-Demand Loading)

### Memory Map with 4KB ROM / 60KB RAM
```
ROM (4KB):
$F000-$FFFF  Dynamic kernel
  - Minimal BIOS
  - Dynamic module loader
  - Memory manager with relocation
  - Stub generator

RAM (60KB):
$0000-$0AFF  System areas (2.75KB)
$0B00-$EEFF  User space/Dynamic modules (57KB)
$EF00-$EFFF  Module management table (256 bytes)
```

### Module Management Table ($EF00-$EFFF)
```
Each entry (8 bytes):
  $00      Module ID
  $01      Status (loaded/stub)
  $02-$03  Memory address (if loaded)
  $04-$05  Jump vector
  $06      Reference count
  $07      Flags
```

### Dynamic Loading Process

1. **Initial State**: All vectors point to stub handler
2. **Module Request**: Program calls Float.Add()
3. **Stub Handler**:
   - Saves context
   - Checks module table
   - Loads module from EEPROM if needed
   - Relocates code to free RAM
   - Patches vector table
   - Restores context
   - Jumps to real routine

### Example Allocation Timeline

```
Boot:
$0B00-$EEFF  58KB completely free
$EF00-$EFFF  All vectors → stub handler

After Float.Add() called:
$0B00-$E6FF  User program (56KB)
$E700-$EEFF  Float module (2KB)
$EF00-$EFFF  Float vectors → $E700

After Long.Mul() called:
$0B00-$E1FF  User program (54.5KB)  
$E200-$E6FF  Long module (1.5KB)
$E700-$EEFF  Float module (2KB)
$EF00-$EFFF  Vectors updated

After program exits:
$0B00-$EEFF  58KB free again
$EF00-$EFFF  Vectors reset to stubs
```

### Module Unloading Strategy

**Reference Counting**:
- Increment on module load
- Decrement on program exit
- Unload when count reaches zero

**LRU Cache**:
- Keep frequently used modules resident
- Unload least recently used when memory needed

### Advantages
- **Maximum flexibility** - Load only what's needed
- **Best RAM utilization** - Modules only when required
- **Automatic management** - Transparent to programs
- **Field upgradeable** - Update modules in EEPROM

### Disadvantages
- **Complex implementation** - Stub handlers, relocation
- **Loading delays** - First call to module is slow
- **Fragmentation risk** - Dynamic allocation challenges

---

## BIOS System Call Interface

All configurations use the same BIOS interface through vector at $0022-$0023:

### Core System Calls (Always Available)
- Memory Management (Allocate, Free, Available)
- File Operations (Open, Read, Write, Delete)  
- Serial I/O (Read, Write, Available)
- Timer Services (Delay, Millis, Seconds)
- GPIO Control (PinMode, Read, Write)

### Extended System Calls (Module-Dependent)
- Long Math (32-bit integer operations)
- Float Math (IEEE 754 operations)
- Advanced File I/O (C-style fopen/fread/fwrite)

See the [BIOS API Documentation](API.md) for complete details.

---

## Implementation Status

### Currently Implemented
- ✅ Full BIOS with system call interface
- ✅ Memory management with allocation/free
- ✅ EEPROM file system
- ✅ Serial I/O with buffering
- ✅ Timer services
- ✅ GPIO control
- ✅ Basic command processor

### In Development
- 🚧 Module loader infrastructure
- 🚧 Relocation tables
- 🚧 Dynamic stub generation

### Future Enhancements
- ⏳ I2C support (system calls defined)
- ⏳ SPI support
- ⏳ Network stack (via ESP8266)
- ⏳ Graphics support (via TMS9918A)

---

## Choosing a Configuration

### Use Basic Nano OS when:
- You have sufficient ROM (8KB+)
- You need maximum reliability
- Boot time is critical
- Memory map simplicity is important

### Use Bootstrap Nano OS when:
- ROM is limited to 4KB
- You need field upgrades
- Module set is stable
- 8KB RAM overhead is acceptable

### Use Dynamic Nano OS when:
- You need maximum flexibility
- Programs have varying requirements
- RAM is plentiful (56KB+)
- Development convenience is priority

---

## Example: Hello World in Each Configuration

### Basic Nano OS
```hopper
program HelloWorld
{
    uses "/Source/Runtime/6502/BIOS"
    
    const string message = "Hello, World!\n";
    
    Hopper()
    {
        // Everything in ROM, just call it
        LDA #(message % 256)
        STA ZP.STRL
        LDA #(message / 256)
        STA ZP.STRH
        LDX #BIOSInterface.SysCall.PrintString
        JSR [ZP.BIOSDISPATCH]
    }
}
```

### Bootstrap Nano OS  
```hopper
// Same code, but PrintString loaded from EEPROM at boot
// Slightly slower boot, but identical runtime performance
```

### Dynamic Nano OS
```hopper
// Same code, but PrintString stub loads module on first call
// First call slower, subsequent calls full speed
// Module can be unloaded when program exits
```

---

## Performance Comparison

| Operation | Basic | Bootstrap | Dynamic (1st) | Dynamic (2nd+) |
|-----------|-------|-----------|---------------|----------------|
| Boot Time | <0.1s | 2-3s | <0.1s | - |
| Print String | 2 cycles | 2 cycles | ~500ms | 2 cycles |
| Float Add | 2 cycles | 2 cycles | ~200ms | 2 cycles |
| Program Load | Normal | Normal | Normal | Normal |
| Memory Free | Maximum | Good | Maximum* | Maximum* |

*Until modules are loaded

---

## Summary

Nano OS provides a flexible framework for 6502 systems that can adapt to different hardware configurations and requirements. Whether you need the simplicity of a monolithic system, the upgradeability of a bootstrap loader, or the flexibility of dynamic loading, Nano OS can be configured to match your needs while maintaining a consistent BIOS interface for all programs.