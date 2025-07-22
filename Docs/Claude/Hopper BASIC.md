# 6502 Structured BASIC Specification

## Language Features

**Data Types (5):**
- **INT** (16-bit signed: -32768 to 32767)
- **WORD** (16-bit unsigned: 0 to 65535)  
- **BYTE** (8-bit unsigned: 0 to 255)
- **STRING** (null-terminated, heap allocated)
- **BIT** (boolean type: 0 or 1, required for IF/WHILE conditions)

**Control Structures (4):**
- **IF/ELSE/ENDIF** - structured conditionals (BIT expressions only)
- **WHILE/ENDWHILE** - loops with conditions (BIT expressions only)
- **FOR/NEXT** - counted loops
- **BREAK** - early loop exit

**Program Structure (4):**
- **BEGIN/END** - main program block
- **FUNC/ENDFUNC** - function definitions with explicit parameter types
- **RETURN** - function return
- **DIM** - variable declaration without initialization

**I/O & Hardware (4):**
- **READ** - digital/analog pin input
- **WRITE** - digital pin output
- **PWM** - analog output
- **DELAY** - millisecond delays

**Variables & Operators (4):**
- **Type declarations** - INT, WORD, BYTE, BIT, STRING with explicit typing
- **AND/OR/NOT** - logical for BIT, bitwise for BYTE/WORD, illegal for INT/STRING
- **Assignment chaining** - A = B = C = 100 (with type compatibility)
- **PRINT** - debug output

## Language Behavior

### Case Sensitivity
- **Case-insensitive input**: Keywords and variable names can be entered in any case
- **Tokenization**: All keywords/identifiers converted to uppercase before tokenization
- **String preservation**: Only string literals (content between quotes) preserve mixed case
- **LIST output**: Always displays keywords and variables in uppercase for consistency

**Examples:**
```basic
Input:    print "Hello World"; count
Stored:   [TOK_PRINT] [STRING:"Hello World"] [VAR:COUNT]  
LIST:     PRINT "Hello World"; COUNT
```

### Variable Handling
- **Explicit declarations required**: All variables must be declared before use
- **Type declarations**: INT A = 42, WORD B, BYTE C = 255, BIT FLAG = 0, STRING NAME = "test"
- **No auto-creation**: Using undeclared variables is a compile error
- **Assignment vs declaration**: A = 100 (assignment) vs INT A = 100 (declaration)
- **Type safety**: BIT type required for IF/WHILE conditions, no implicit conversions
- **Default values**: INT/WORD/BYTE = 0, BIT = 0, STRING = ""

**Examples:**
```basic
INT COUNT = 42    // Declaration with initialization
COUNT = 100       // Assignment to existing variable
PRINT OTHER       // ERROR: Variable OTHER not declared
IF COUNT          // ERROR: COUNT is INT, need BIT for IF condition
BIT READY = 1
IF READY          // OK: READY is BIT type
```

### Type System
- **Strict typing**: No implicit conversions between signed/unsigned (INT vs WORD/BYTE)
- **Type promotion**: BYTE promotes to WORD or INT in mixed operations
- **Sign/unsigned mixing**: INT + WORD is a type error (explicit cast required)
- **Operator behavior**:
  - AND/OR: Logical for BIT, bitwise for BYTE/WORD, illegal for INT/STRING
  - Arithmetic: Promotes BYTE→WORD, BYTE→INT, but INT+WORD is type error
  - Comparisons: Return BIT type (A < B returns BIT, not INT)
- **Constant literals**: Future +INT type that adapts to context
- **Boolean context**: Only BIT type allowed in IF/WHILE conditions

**Examples:**
```basic
BYTE A = 100
WORD B = 1000
WORD RESULT = A + B    // OK: A promotes to WORD

INT C = -50
RESULT = C + B         // ERROR: signed + unsigned mismatch

BIT FLAG = (A < B)     // OK: comparison returns BIT
IF (A < B)             // OK: comparison result is BIT type
```

### Assignment Chaining
```basic
WORD A
BYTE B = 200
WORD C = 300
WORD D = A = B = C = 100    // Legal - all compatible numeric types
STRING S = "hello"
WORD E = S = 5              // ERROR - can't assign number to STRING
```

## System Commands (10 total)

### Program Management
- **`RUN`** - Execute the current program starting from BEGIN/END block
- **`LIST`** - Display the current program in memory (tokenized back to source)
- **`NEW`** - Clear current program from memory, reset all variables

### Variable Management
- **`CLEAR`** - Clear all global variables to default values (keeps declarations)

### EEPROM File System  
- **`SAVE "name"`** - Save current program to EEPROM with given name
- **`LOAD "name"`** - Load named program from EEPROM into memory
- **`DEL "name"`** - Delete named program from EEPROM
- **`DIR`** - List all saved programs in EEPROM (shows names + sizes)

### Development/Debug
- **`VARS`** - Show all global variables and their current values
- **`FUNCS`** - Show all defined functions and their signatures

## Behavioral Distinctions

- **`NEW`** - Nuclear option: clears program, functions, AND variables
- **`CLEAR`** - Surgical option: only clears global variable values, keeps declarations
- **`RUN`** - Should automatically CLEAR variables before execution (fresh start)

## Example Usage

```basic
> INT COUNT = 42
> STRING NAME = "test"
> VARS
COUNT         42
NAME          "test"
> CLEAR
> VARS
COUNT         0
NAME          ""
> RUN
(program runs with fresh variables)
```

## Syntax Examples

**Variable Declarations:**
```basic
INT LED_PIN = 13
WORD SIZE = 8190
BYTE PINS[8]        // Array declaration (future)
INT VALUES[10]      // Array declaration (future)
BIT FLAGS[SIZE]     // Bit array (future)

// Declaration without initialization
INT COUNTER         // COUNTER = 0
BIT ENABLED         // ENABLED = 0
STRING MESSAGE      // MESSAGE = ""
```

**Program Structure:**
```basic
FUNC BLINK(INT PIN, INT COUNT)    // Explicit parameter types
  FOR I = 1 TO COUNT
    WRITE PIN, 1
    DELAY 500
    WRITE PIN, 0
    DELAY 500
  NEXT
ENDFUNC

BEGIN
  INT LED_PIN = 13
  BLINK(LED_PIN, 5)
END
```

**Type-Safe Operations:**
```basic
INT TEMP = 72
WORD ADDRESS = 0x1000
BIT HOT = (TEMP > 80)      // Comparison returns BIT
BIT VALID = 1

IF HOT                     // OK: HOT is BIT type
  PRINT "Temperature high"
ENDIF

IF TEMP                    // ERROR: TEMP is INT, not BIT
  PRINT "This won't compile"
ENDIF

BYTE MASK = 0b11110000
BYTE DATA = 0b10101010
BYTE RESULT = MASK AND DATA  // Bitwise AND for BYTE types
```

**File Operations:**
```basic
SAVE "blink"       // Save to EEPROM
LOAD "sensor"      // Load from EEPROM  
DIR                // List all programs
DEL "old_prog"     // Delete program
```

## Implementation Architecture

**Memory Layout:**
```
$0000-$00FF: Zero Page (variables, pointers)
$0100-$01FF: Stack
$0200-$02FF: 256 byte serial buffer
$0300-$08FF: Runtime stacks and buffers
$0900-$xxxx: Dynamic heap (functions, strings)
$E000-$FFFF: 8K ROM (interpreter)
```

**8K ROM Contents:**
- **Core Interpreter** (1K) - bytecode execution engine
- **Tokenizer** (1K) - lexical analysis and parsing
- **Expression Evaluator** (1K) - type-safe arithmetic and logic
- **Variable Manager** (1K) - symbol tables and runtime resolution
- **I/O Routines** (1K) - serial, EEPROM, GPIO, timers
- **Bytecode Handlers** (1K) - optimized operation implementations
- **Built-in Functions** (1K) - string operations, utilities  
- **Constants/Tables** (1K) - lookup tables, vectors

## Performance Optimizations

**Compilation Strategy:**
- **Immediate tokenization** during program entry
- **Runtime name resolution** with caching for fast subsequent access
- **Type checking** at compile time prevents runtime errors
- **Late binding** with first-use resolution and caching

**Optimized Bytecode:**
- **OP_LOAD_VAR_CACHED** - cached variable access (5 cycles vs 50+)
- **OP_BIT_TEST** - optimized boolean operations (10 cycles vs 30+)
- **OP_TYPE_PROMOTE** - efficient BYTE→WORD promotion (8 cycles vs 25+)
- **OP_CALL_CACHED** - cached function calls (15 cycles vs 100+)

**Performance Targets:**
- **Variable access**: 5-10 cycles after first resolution
- **Function calls**: 15-25 cycles for cached calls
- **Type operations**: Zero-cost for compatible types
- **Boolean logic**: Optimized for BIT-only conditional expressions

## Key Design Principles

1. **Type safety first** - prevent runtime errors through compile-time checking
2. **Explicit over implicit** - no mysterious auto-creation or type coercion
3. **Performance through caching** - fast execution after first resolution
4. **Structured programming** - no line numbers, block-structured syntax
5. **Hardware-focused** - designed for microcontroller/embedded use
6. **Consistent behavior** - same rules in REPL and programs
7. **Forward compatibility** - designed to port back to 6502 assembly

**Total Keywords: ~35** (minimal for full structured, type-safe language)

This specification delivers **type-safe, structured programming** with **1980s assembly-level performance** using a **high-level BASIC** that fits in **8K ROM** and provides modern language safety features.

# Hopper BASIC Architecture Summary

## Key Design Decisions

### Memory Layout (ROM-based interpreter)
- **$E000-$FFFF**: ROM-based BASIC interpreter (~8K)
- **$0900-$xxxx**: Dynamic heap for function bodies, strings, and symbol tables
- **Function-based program storage**: Each function (including "MAIN") stored as separate allocated blocks
- **Runtime stacks**: 6502 hardware stack + Hopper type/value stacks for expression evaluation
- **Global variables**: Persistent symbol table with stack-based storage

### Execution Strategy: **Threaded Code Interpreter**
- **Bytecode as jump table indices**: Each bytecode points to optimized 6502 handler routines
- **Performance**: Near-native speed with bytecode flexibility
- **Simplicity**: Much easier than full compilation, more efficient than traditional interpretation
- **Handler-based**: Each BASIC operation becomes a specialized 6502 routine ending in RTS

### Variable System: **Runtime Resolution with Caching**
- **Symbol tables**: Name → (type, stack_index) mapping for globals and locals
- **Late binding**: First reference resolves name to index, subsequent references use cached index
- **Stack-based storage**: Global variables at bottom of value stack (GP=0), locals at BP+offset
- **Type safety**: Compile-time type checking with runtime type tags
- **No auto-creation**: All variables must be explicitly declared

**Variable Access Implementation:**
```hopper
// Runtime resolution with caching
OP_LOAD_VAR     = 0x09,  // + cache_slot + name_length + name_chars
OP_STORE_VAR    = 0x0A,  // + cache_slot + name_length + name_chars

// Cached resolution (fast path)
handleLoadVar() {
    cacheSlot = fetchByte();
    if (resolveCache[cacheSlot].resolved) {
        stackIndex = resolveCache[cacheSlot].value;
        pushFromStack(stackIndex);
    } else {
        // First-time resolution, then cache result
        name = fetchName();
        stackIndex = resolveName(name);
        cacheResult(cacheSlot, stackIndex);
        pushFromStack(stackIndex);
    }
}
```

### Type System: **Strict with Smart Promotion**
- **No auto-creation**: Variables must be declared before use
- **Explicit types**: INT A = 42, WORD B, BIT FLAG = 0
- **Type safety**: BIT required for IF/WHILE, no implicit signed/unsigned mixing
- **Smart promotion**: BYTE→WORD, BYTE→INT, but INT+WORD is error
- **Operator overloading**: AND/OR logical for BIT, bitwise for BYTE/WORD

### Function Management Architecture
- **Function blocks**: Each function stored as allocated memory block with header + bytecode
- **Function limit**: 256 functions maximum (cached lookup table)
- **Two-phase compilation**: Large temporary blocks during compilation, exact-size final block
- **Runtime resolution**: Function names resolved at first call, then cached

**Function Header Structure:**
```hopper
struct FunctionHeader
{
    uint functionID;        // 2 bytes (0 = never assigned)
    char name[14];          // Null-terminated, 13 chars max + \0
    uint nextFunction;      // Linked list pointer
    uint codeSize;          // Size of bytecode that follows
    byte paramCount;        // Number of parameters
    byte paramTypes[8];     // Parameter types (max 8 parameters)
    // Total: 29 bytes + alignment
    // Followed by: bytecode + literal data
}
```

### EEPROM Storage (64K)
- **Environment-level storage**: SAVE/LOAD entire program (all functions + global declarations)
- **Simple file system**: Single 256-byte directory block, 16 files maximum
- **File structure**: 13-char names (8.3 format), 1-byte start block, 2-byte length
- **Buffered I/O**: 256-byte blocks to match I2C transaction size

### Bytecode Design
- **Cached operations**: Most variable/function access uses cached indices
- **Type-aware opcodes**: Operations know expected types for safety
- **Compact encoding**: 1-byte opcode + minimal operands
- **Runtime optimization**: First-use resolution, subsequent cached access

## Why This Architecture

**Type Safety Benefits:**
- ✅ Compile-time error detection
- ✅ No runtime type mismatches
- ✅ Predictable operator behavior
- ✅ Self-documenting code

**Performance Benefits:**
- ✅ Cached variable access (5-10 cycles)
- ✅ Cached function calls (15-25 cycles)
- ✅ Type-specific optimizations
- ✅ Zero-cost type compatibility

**Development Benefits:**
- ✅ Clear error messages
- ✅ No mysterious auto-creation bugs
- ✅ Consistent REPL/program behavior
- ✅ Easy debugging and maintenance

## Implementation Priority
1. Symbol table management and type system
2. Variable declaration and assignment parsing
3. Runtime name resolution with caching
4. Expression evaluation with type checking
5. Control flow (IF/WHILE with BIT requirements)
6. Function definitions and calls
7. EEPROM save/load system

This architecture provides **modern language safety** with **classic BASIC simplicity** and **6502-optimized performance**.