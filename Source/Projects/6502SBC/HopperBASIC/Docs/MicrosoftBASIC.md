# Microsoft BASIC Version Comparison: 4K to 5.x Evolution

## Overview

| Feature | 4K BASIC (1975) | 8K BASIC (1976) | Extended BASIC (12K) | BASIC-80 5.x (1981) |
|---------|----------------|------------------|---------------------|---------------------|
| **Memory Size** | 4 KB | 8 KB | 12 KB | Variable (disk-based) |
| **Release Year** | 1975 | 1976 | ~1977 | 1981 |
| **Primary Use** | Altair 8800 | Home computers | Advanced systems | Business/CP/M systems |

## Data Types & Variables

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **Numeric Types** | Single precision only (32-bit) | Single precision only | **3 types**: `%` (16-bit int), `!` (32-bit single), `#` (64-bit double) | Same as Extended + enhancements |
| **String Support** | ❌ **No strings** | ✅ Full string variables (`$` suffix) | ✅ Advanced string handling | ✅ **Dynamic allocation** |
| **Variable Names** | **1-2 characters** (A-Z, A0-Z9) = 286 max | Same limitation | Same limitation | ✅ **Up to 40 characters** |
| **String Operations** | ❌ None | ✅ `MID$`, concatenation | ✅ Full string functions | ✅ Enhanced string handling |

## Language Constructs

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **Conditionals** | `IF...THEN` only | `IF...THEN` only | ✅ **`IF...THEN...ELSE`** | ✅ Full conditional blocks |
| **Loops** | `FOR...NEXT`, `GOTO` | `FOR...NEXT`, `GOTO` | `FOR...NEXT`, `GOTO` | ✅ **`WHILE...WEND`** loops |
| **Functions** | Built-in only | Built-in only | ✅ **User defined functions** | ✅ Enhanced user functions |
| **Error Handling** | Basic error numbers | Basic error numbers | ✅ **Descriptive error messages** | ✅ Advanced error handling |

## Built-in Commands & Functions

| Category | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|----------|----------|----------|----------------|---------------|
| **Core Commands** | `LIST`, `NEW`, `PRINT`, `INPUT`, `LET`, `REM`, `CLEAR`, `STOP`, `END` | Same + **31 additional** | Enhanced versions | Enhanced + new commands |
| **Math Functions** | `SQR`, `RND`, `SIN`, `ABS`, `INT` | Same + more | ✅ **Transcendental functions** | Enhanced math library |
| **Control Flow** | `GOTO`, `GOSUB`, `RETURN`, `FOR`, `NEXT` | Same | Same + `IF...ELSE` | Same + `WHILE...WEND` |
| **Data Handling** | `DATA`, `READ`, `RESTORE` | Same + string handling | Enhanced data handling | Advanced data structures |
| **I/O Functions** | `TAB` | Enhanced I/O | Advanced I/O formatting | Full I/O control |

## Memory & Program Management

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **Program Storage** | Memory only | Memory only | Memory + enhanced editing | **Dynamic string allocation** |
| **Line Editing** | Basic | Basic | ✅ **Advanced editing commands** | Enhanced editor |
| **Program Lines** | Numbered lines | Numbered lines | Numbered lines | ❌ **No line crunching** |
| **Memory Efficiency** | Very compact | Compact | Moderate | **No string space reservation** |

## Advanced Features

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **File Handling** | ❌ None | ❌ Limited | ✅ Basic file operations | ✅ **Advanced file I/O** |
| **Debugging** | ❌ None | ❌ Limited | ✅ Better error reporting | ✅ Advanced debugging |
| **Programming Aids** | ❌ None | ❌ Limited | ✅ Function definitions | ✅ Modern programming constructs |
| **Compatibility** | Basic | Enhanced | Good | ✅ **Cross-platform standard** |

## Platform Implementations

| Platform | Version Used | Notable Features | Limitations |
|----------|-------------|------------------|-------------|
| **Altair 8800** | 4K → 8K → Extended | Original Microsoft BASIC | Memory constraints |
| **TRS-80 Level II** | Modified Extended | Graphics commands (`SET`, `RESET`, `POINT`) | Cut to fit 12K ROM |
| **Commodore PET/64** | Modified 8K/Extended | Cost-optimized version | Missing advanced features |
| **Apple II (Applesoft)** | Modified Extended | Apple II graphics support | 2-character variables |
| **TRS-80 Model 4** | BASIC-80 5.x | **Full-featured implementation** | Business-focused |
| **CP/M (MBASIC)** | BASIC-80 5.x | **Complete Microsoft BASIC** | Disk-based only |

## Evolution Timeline

```
1975: 4K BASIC (Altair)
      ├─ Integer math only
      ├─ No strings
      └─ 286 variable limit

1976: 8K BASIC
      ├─ Added strings
      ├─ 31 new commands
      └─ Still limited variables

~1977: Extended BASIC (12K)
       ├─ Three data types
       ├─ IF...THEN...ELSE
       ├─ User functions
       └─ Better error messages

1981: BASIC-80 5.x
      ├─ 40-character variables
      ├─ WHILE...WEND loops
      ├─ Dynamic strings
      └─ Modern features
```

## Key Innovations by Version

### 4K BASIC Innovations
- First commercially successful BASIC interpreter for microcomputers
- Efficient tokenization system
- Established Microsoft's BASIC syntax standards

### 8K BASIC Innovations  
- **String variables**: Revolutionary for home computing
- **String concatenation**: Enabled text processing
- **Expanded command set**: Made practical programming possible

### Extended BASIC Innovations
- **Multiple data types**: Professional programming capability  
- **Structured conditionals**: `IF...THEN...ELSE` blocks
- **User-defined functions**: Code reusability
- **Descriptive errors**: Better debugging experience

### BASIC-80 5.x Innovations
- **Long variable names**: Modern programming practices
- **Structured loops**: `WHILE...WEND` for better code flow
- **Dynamic memory**: No more string space planning
- **Cross-platform portability**: Standard across systems

## Impact on 8-bit Computing

| Version | Market Impact | Legacy |
|---------|--------------|--------|
| **4K BASIC** | Established BASIC as microcomputer standard | Foundation for all Microsoft BASICs |
| **8K BASIC** | Enabled home computer software industry | Standard features still used today |
| **Extended BASIC** | Made serious programming accessible | Influenced structured programming adoption |
| **BASIC-80 5.x** | Set standard for business computing | Bridge to 16-bit era features |

---
*This evolution represents Microsoft's transformation from a small software company to the dominant force in microcomputer programming languages during the crucial 1975-1981 period.*