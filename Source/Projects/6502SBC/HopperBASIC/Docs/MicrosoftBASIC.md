# Microsoft BASIC Version Comparison: 4K to 5.x Evolution

## Overview

| Feature | 4K BASIC (1975) | 8K BASIC (1976) | Extended BASIC (12K) | BASIC-80 5.x (1981) |
|---------|----------------|------------------|---------------------|---------------------|
| **Memory Size** | 4 KB | 8 KB | 12 KB | Variable (disk-based) |
| **Release Year** | 1975 | 1976 | ~1977 | 1981 |
| **Primary Use** | Altair 8800 | Home computers | Advanced systems | Business/CP/M systems |

## Data Types & Variables

### Numeric Data Types

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **Early Development** | ⚠️ **Original prototypes were integer-only** | ⚠️ **Based on 4K floating-point version** | ✅ **Built from 8K foundation** | ✅ **Advanced from Extended** |
| **Released Version** | ✅ **32-bit floating point only** (Monte Davidoff's custom format) | ✅ **Same 32-bit floating point** as 4K + strings | ✅ **Three types**: `%` (16-bit int), `!` (32-bit single), `#` (64-bit double) | ✅ **Same as Extended** + optimizations |
| **Integer Support** | ❌ **No integer type in released version** | ❌ **No integer type** | ✅ **`%` suffix**: 16-bit signed integer (-32768 to 32767) | ✅ **Same as Extended** + better performance |
| **Single Precision Float** | ✅ **Only numeric type** (32-bit, ~6-7 digits, custom format) | ✅ **Same as 4K BASIC** (32-bit, ~6-7 digits) | ✅ **`!` suffix**: 32-bit float (default, rarely shown) | ✅ **Same as Extended** |
| **Double Precision Float** | ❌ **Not available** | ❌ **Not available** | ✅ **`#` suffix**: 64-bit float (~16 digits precision) | ✅ **Same as Extended** + optimizations |
| **Development History** | Gates & Allen started integer-only → Monte Davidoff added floating point | Built on 4K floating-point foundation | Added integer and double types to floating-point base | Enhanced all existing types |

### String Data Types & Variable Names

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **String Variables** | ❌ **No string support** | ✅ **`$` suffix required** (A$, NAME$) | ✅ **`$` suffix required** | ✅ **`$` suffix required** |
| **Variable Name Length** | **1-2 characters only** (A, A0, BC, X9) | **Same limitation** (A$, B1$) | **Same limitation** (A$, B1$) | ✅ **Up to 40 characters** (CUSTOMER_NAME$) |
| **Total Variables** | **286 numeric maximum** (A-Z + A0-Z9) | **286 numeric + 286 string** | **286 of each type** | **Unlimited** (within memory) |
| **String Storage** | ❌ N/A | ✅ **Fixed allocation** (must reserve space) | ✅ **Fixed allocation** (must reserve space) | ✅ **Dynamic allocation** (automatic) |

### Historical Development Note

The evolution of Microsoft BASIC's numeric types has an interesting history:

**Early 1975 Development**: When fellow Harvard student Monte Davidoff stated he believed the system should use floating-point arithmetic instead of the integer arithmetic of the original versions, and claimed he could write such a system that could still fit within the memory limits, they hired Davidoff to write the package.

**Released 4K BASIC (1975)**: The commercially released version was **floating-point only** using Monte Davidoff's custom 32-bit format, not integer-only as the early prototypes were.

**Key Point**: While development versions were integer-only, all released versions of 4K and 8K BASIC were floating-point based. Integer support didn't return until Extended BASIC as the `%` type.

### String Functions & Operations

| Category | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|----------|----------|----------|----------------|---------------|
| **Basic Operations** | ❌ None | ✅ **Concatenation** (`+` operator)<br>✅ **Assignment** (`A$ = "text"`) | ✅ **All 8K operations** | ✅ **All Extended operations** |
| **Substring Functions** | ❌ None | ✅ **`MID$(string, start)`**<br>✅ **`MID$(string, start, length)`** | ✅ **`LEFT$(string, length)`**<br>✅ **`RIGHT$(string, length)`**<br>✅ **All MID$ variations** | ✅ **All Extended functions**<br>✅ **Enhanced performance** |
| **String Analysis** | ❌ None | ✅ **`LEN(string)`** - returns length | ✅ **`LEN(string)`**<br>✅ **`ASC(character)`** - ASCII value<br>✅ **`INSTR(string, substring)`** - find position | ✅ **All Extended functions**<br>✅ **Faster searching** |
| **Conversion Functions** | ❌ None | ✅ **`STR$(number)`** - number to string<br>✅ **`VAL(string)`** - string to number | ✅ **`STR$(number)`** - enhanced<br>✅ **`VAL(string)`** - enhanced<br>✅ **`CHR$(ascii)`** - ASCII to character | ✅ **All Extended functions**<br>✅ **Better error handling** |
| **Formatting** | ❌ None | ❌ **Limited** | ✅ **`PRINT USING`** - formatted output<br>✅ **Format strings** (###.##, etc.) | ✅ **Enhanced PRINT USING**<br>✅ **More format options** |
| **Advanced String Handling** | ❌ None | ❌ **Manual memory management** | ✅ **`LSET`** - left justify in field<br>✅ **`RSET`** - right justify in field<br>❌ **Must pre-allocate space** | ✅ **All Extended functions**<br>✅ **Automatic memory management**<br>✅ **No space pre-allocation needed** |

### Variable Declaration Examples

#### 4K BASIC
```basic
A = 5           ' Single precision float only
B1 = 3.14159    ' All variables are float
X = 1000        ' No integer type available
' No string variables possible
```

#### 8K BASIC  
```basic
A = 5           ' Single precision float (default)
B1 = 3.14159    ' Still only float type
NAME$ = "JOHN"  ' String variable (must reserve space first)
A$ = "HELLO"    ' Another string variable
```

#### Extended BASIC
```basic
COUNT% = 100         ' 16-bit integer (-32768 to 32767)
PRICE! = 19.95       ' 32-bit single precision (default)
BALANCE# = 1234567.89# ' 64-bit double precision
NAME$ = "CUSTOMER"    ' String (must reserve space)
```

#### BASIC-80 5.x
```basic
CUSTOMER_COUNT% = 100           ' Long variable names
ITEM_PRICE! = 19.95            ' Clear type designation  
ACCOUNT_BALANCE# = 1234567.89# ' Double precision
CUSTOMER_NAME$ = "JOHN SMITH"   ' Dynamic string allocation
FULL_ADDRESS$ = FIRST$ + " " + LAST$ + ", " + CITY$ ' Dynamic concatenation
```

### Memory Management Evolution

| Aspect | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|--------|----------|----------|----------------|---------------|
| **String Space** | ❌ N/A | ⚠️ **Must use `CLEAR` command**<br>Example: `CLEAR 200` (reserves 200 bytes) | ⚠️ **Must reserve space**<br>Can specify: `CLEAR ,,,string_space` | ✅ **Automatic allocation**<br>No planning required |
| **Variable Storage** | Minimal (numbers only) | Fixed arrays for names | Fixed arrays for names | **Dynamic symbol table** |
| **Memory Efficiency** | Excellent | Good (if planned well) | Moderate (planning required) | **Excellent** (automatic optimization) |

### Type Conversion & Coercion

| Feature | 4K BASIC | 8K BASIC | Extended BASIC | BASIC-80 5.x |
|---------|----------|----------|----------------|---------------|
| **Automatic Conversion** | N/A (single type) | Limited | ⚠️ **Can lose precision**<br>`A% = 3.7` becomes `4` | ✅ **Better type checking**<br>More warnings |
| **Explicit Conversion** | N/A | `VAL()`, `STR$()` | Enhanced conversion functions | **Robust conversion** with error handling |
| **Mixed Expressions** | N/A | Float + String operations | **All three numeric types**<br>Automatic promotion | **Optimized mixed operations** |

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
      ├─ Started integer-only (prototypes)
      ├─ Released as floating-point only
      └─ 286 variable limit

1976: 8K BASIC
      ├─ Added strings
      ├─ 31 new commands
      └─ Still limited variables

~1977: Extended BASIC (12K)
       ├─ Three data types (%, !, #)
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
- **Monte Davidoff's floating-point breakthrough** - chose floating-point over integer
- Established Microsoft's BASIC syntax standards

### 8K BASIC Innovations  
- **String variables**: Revolutionary for home computing
- **String concatenation**: Enabled text processing
- **Expanded command set**: Made practical programming possible
- Built on 4K's floating-point foundation

### Extended BASIC Innovations
- **Multiple data types**: Professional programming capability  
- **Structured conditionals**: `IF...THEN...ELSE` blocks
- **User-defined functions**: Code reusability
- **Descriptive errors**: Better debugging experience
- **Returned integer support** with the `%` suffix

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

## The Integer/Floating Point Evolution

**The Paradox**: Microsoft BASIC actually went backwards before going forwards with numeric types:

1. **Development (Early 1975)**: Integer-only prototypes
2. **4K BASIC (Released 1975)**: Floating-point only (Monte Davidoff's innovation)
3. **8K BASIC (1976)**: Still floating-point only
4. **Extended BASIC (1977)**: Finally offered **both** integer (`%`) and floating-point (`!`, `#`)

This explains why Extended BASIC was such a breakthrough - it was the first Microsoft BASIC to offer programmers a choice of numeric types since the original integer-only prototypes.

---
*This evolution represents Microsoft's transformation from a small software company to the dominant force in microcomputer programming languages during the crucial 1975-1981 period.*