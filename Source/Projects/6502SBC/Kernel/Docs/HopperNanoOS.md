## Nano OS - Dynamic Module System for Hopper 6502 (Corrected)

### Overview

Nano OS is a sophisticated yet minimal operating system for the Hopper 6502 platform that demonstrates three evolutionary stages of system design:

1. **Basic Nano OS** - A CP/M-style command loader fitting in 966 bytes
2. **Bootstrap Nano OS** - A two-stage system loading components from EEPROM
3. **Dynamic Nano OS** - On-demand module loading with memory management

### System Configurations

| Configuration | ROM Size | Maximum RAM | User Space Available |
|--------------|----------|--------------|---------------------|
| Basic | 8KB (7.2KB kernel + 966B OS) | 56KB | 54.5KB free |
| Bootstrap | 4KB | 56KB | ~46KB free (after loading core modules) |
| Dynamic | 4KB | 60KB | 54-58KB (depends on loaded modules) |

---

## Basic Nano OS (8KB ROM / 56KB RAM)

### Memory Map with 56KB RAM
```
ROM (8KB):
$8000-$9C49  Hopper Kernel with Float (7,226 bytes)
$9C4A-$9FFF  Nano OS (966 bytes)

RAM (56KB):
$0000-$00FF  Zero Page
$0100-$01FF  Hardware Stack
$0200-$06FF  I/O & File Buffers (1.25KB)
$0700-$DFFF  User Programs (54.25KB free!)
```

With 56KB RAM, Basic Nano OS provides an impressive **54.25KB** of contiguous user space while having the full Float kernel in ROM. This is enough to run substantial programs without any module loading complexity.

---

## Bootstrap Nano OS (4KB ROM / 56KB RAM)

### Memory Map with 56KB RAM
```
ROM (4KB):
$E000-$EFFF  Minimal bootstrap kernel (3KB)

RAM (56KB) after Stage 2 loads:
$0000-$00FF  Zero Page
$0100-$01FF  Hardware Stack  
$0200-$06FF  I/O & File Buffers (1.25KB)
$0700-$BFFF  User Programs (~46KB free)
$C000-$DFFF  Loaded kernel modules (8KB)
  $C000-$C7FF  Float module (2KB)
  $C800-$CDFF  Long module (1.5KB)
  $CE00-$D3FF  Extended Serial (1.5KB)
  $D400-$DFFF  DOS commands (3KB)
```

Even after loading all kernel modules from EEPROM, you still have **56KB** free for user programs - more than most 8-bit computers ever offered!

---

## Dynamic Nano OS (4KB ROM / 60KB RAM)

### Up to 58KB RAM?

Dynamic loading needs extra RAM for:
- Module allocation space (modules load dynamically)
- Relocation tables during loading
- Module management structures
- Larger programs that expect more memory

### Memory Map with 60KB RAM
```
ROM (4KB):
$F000-$FFFF  Minimal bootstrap kernel (4KB)

RAM (60KB):
$0000-$00FF  Zero Page
$0100-$01FF  Hardware Stack
$0200-$06FF  I/O & File Buffers (1.25KB)
$0700-$EEFF  User Programs/Dynamic Modules (58KB)
$EF00-$EFFF  Vector table & module management (256 bytes)
```

### Dynamic Allocation Example

#### Initial (no modules loaded):
```
$0700-$EEFF  User space (58KB completely free!)
$EF00-$EFFF  Vector table (all stubs)
```

#### After program calls Float.Add:
```
$0700-$E6FF  User program (56KB still free)
$E700-$EEFF  Float module (2KB - dynamically allocated)
$EF00-$EFFF  Vector table (Float entries patched)
```

#### If program also needs Long math:
```
$0700-$E1FF  User program (54.5KB still free)
$E200-$E6FF  Long module (1.5KB)
$E700-$EEFF  Float module (2KB)
$EF00-$EFFF  Vector table (Float + Long patched)
```

### The Key Advantage

With 60KB RAM, Dynamic Nano OS can:
- Start with **58KB free** (more than Basic!)
- Load only what's needed
- Still have 54KB+ free even with Float and Long loaded
- Unload modules to reclaim memory

---

## RAM Size Comparison

| System | ROM | RAM | OS Overhead | User Space | Notes |
|--------|-----|-----|-------------|------------|-------|
| **Basic** | 8KB | 32KB | 1.5KB | 30KB | Works but limited |
| **Basic** | 8KB | **56KB** | 1.5KB | **54.5KB** | Excellent choice! |
| **Bootstrap** | 4KB | 56KB | ~10KB | 46KB | Modules eat RAM |
| **Dynamic** | 4KB | **60KB** | 1.5KB-6KB | **54-58KB** | Best of both worlds |

### Why These RAM Sizes Make Sense

**56KB for Basic**: 
- Full kernel in ROM means minimal RAM overhead
- 54.5KB free is massive for 6502 programs
- Simple, reliable, no loading delays

**60KB for Dynamic**:
- Extra 4KB over Basic compensates for module loading overhead
- Can actually provide MORE free space than Basic (58KB vs 54.5KB)
- Flexibility to load/unload modules as needed

### Practical Implications

With modern RAM being cheap, going from 32KB to 56KB or 60KB is trivial, and the benefits are huge:
- **56KB Basic**: Dead simple, massive program space, instant boot
- **60KB Dynamic**: Ultimate flexibility, even more space, field upgradeable

Both configurations give you **more free RAM than a Commodore 64** (38KB free) or Apple IIe (48KB typical), while providing floating point math and a filesystem!