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
- **CONST** - constant declarations with required initialization

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

## Interactive Development Model

Hopper BASIC follows the **FORTH interactive development paradigm** - build up your environment incrementally, test each component immediately, then save the complete working session.

## Variable Scoping and Visibility

### Scope Levels (in resolution order):
1. **Function locals** - True locals, scoped to individual functions only
2. **Session globals** - Variables, constants, and functions declared at REPL prompt

### Example Structure:
```basic
> CONST INT LED_PIN = 13    # Session global constant
> INT BLINK_COUNT = 0       # Session global variable  
> 
> FUNC BLINK()              # Session global function
>   WRITE LED_PIN, 1        # Can access session globals
>   BLINK_COUNT = BLINK_COUNT + 1
> ENDFUNC
>
> BEGIN                     # Main program block
>   FOR I = 1 TO 5          # I is local to BEGIN function
>     BLINK()               # Calls session global function
>   NEXT
> END
```

## Language Behavior

### Case Sensitivity
- **Case-insensitive input**: Keywords and variable names can be entered in any case
- **Tokenization**: All keywords/identifiers converted to uppercase before tokenization
- **String preservation**: Only string literals (content between quotes) preserve mixed case
- **LIST output**: Always displays keywords and variables in uppercase for consistency

### Variable Handling
- **Explicit declarations required**: All variables must be declared before use
- **Automatic redefinition**: Declaring an existing name automatically forgets the previous definition
- **Assignment vs declaration**: A = 100 (assignment) vs INT A = 100 (declaration)
- **Type safety**: BIT type required for IF/WHILE conditions, no implicit conversions
- **Default values**: INT/WORD/BYTE = 0, BIT = 0, STRING = ""

**Examples:**
```basic
INT COUNT = 42    // Declaration with initialization
COUNT = 100       // Assignment to existing variable
INT COUNT = 0     // Redeclaration - automatically forgets previous definition
IF COUNT          // ERROR: COUNT is INT, need BIT for IF condition
BIT READY = 1
IF READY          // OK: READY is BIT type
```

### Constants
- **Declaration required**: `CONST INT LED_PIN = 13` (must have initializer)
- **Session scope**: Declared at REPL prompt, saved with program
- **Immutable**: Cannot be modified after declaration
- **Automatic redefinition**: Redeclaring a constant automatically forgets the previous definition

### Type System
- **Strict typing**: No implicit conversions between signed/unsigned (INT vs WORD/BYTE)
- **Type promotion**: BYTE promotes to WORD or INT in mixed operations
- **Sign/unsigned mixing**: INT + WORD is a type error (explicit cast required)
- **Operator behavior**:
  - AND/OR: Logical for BIT, bitwise for BYTE/WORD, illegal for INT/STRING
  - Arithmetic: Promotes BYTE→WORD, BYTE→INT, but INT+WORD is type error
  - Comparisons: Return BIT type (A < B returns BIT, not INT)
- **Boolean context**: Only BIT type allowed in IF/WHILE conditions

### Assignment Chaining
```basic
WORD A
BYTE B = 200
WORD C = 300
WORD D = A = B = C = 100    // Legal - all compatible numeric types
STRING S = "hello"
WORD E = S = 5              // ERROR - can't assign number to STRING
```

## System Commands

### Program Management
- **`RUN`** - Execute BEGIN/END block (automatically resets program variables)
- **`LIST`** - Display current session in memory (constants, variables, functions, main)
- **`NEW`** - Nuclear option: clear entire session (constants, variables, functions)

### Variable Management
- **`CLEAR`** - Reset all global variables to their declaration defaults
- **`FORGET name`** - Selectively remove one definition (constant, variable, or function)

### EEPROM File System  
- **`SAVE "name"`** - Save complete session to EEPROM
- **`LOAD "name"`** - Load complete session from EEPROM, replacing current environment
- **`DEL "name"`** - Delete named program from EEPROM
- **`DIR`** - List all saved programs in EEPROM (shows names + sizes)

### Development/Debug
- **`VARS`** - Show all global variables and their current values
- **`FUNCS`** - Show all defined functions and their signatures

## Persistence and Variable Lifecycle

### Session Management:
- **`SAVE "name"`** saves **entire session**: all constants, variables, functions, and main program
- **`LOAD "name"`** restores complete session, replacing current environment  
- **Automatic redefinition** eliminates need for explicit FORGET before redefining

### Variable Lifecycle:
- **Session globals** - Persist across RUN calls, reset only by CLEAR or redefinition
- **Function locals** - Reinitialized every function call
- **Main program locals** - BEGIN/END executes as a function, locals reinitialized every RUN

### Development Workflow:
**FORTH-style incremental development:**
1. Declare constants and variables as needed
2. Define functions one at a time, test immediately  
3. Refine by simply redeclaring (automatic forget of previous version)
4. Create main program in BEGIN/END block
5. `RUN` to test complete program
6. `SAVE "name"` when satisfied with complete working environment

**Example refinement cycle:**
```basic
> FUNC DELAY_MS(INT MS) ... ENDFUNC   # First version
> DELAY_MS(1000)                      # Test - too slow!
> FUNC DELAY_MS(INT MS) ... ENDFUNC   # Just redefine - automatic forget!
> DELAY_MS(1000)                      # Test - perfect!
> SAVE "myproject"                    # Save complete session
```

## Behavioral Distinctions

- **`NEW`** - Nuclear option: clears constants, variables, functions, and main program
- **`CLEAR`** - Surgical option: resets global variables to declaration defaults, keeps all definitions
- **`RUN`** - Executes main program with fresh local variables (globals unchanged unless modified)
- **`FORGET`** - Optional selective cleanup (automatic redefinition usually sufficient)

## Syntax Examples

**Variable and Constant Declarations:**
```basic
CONST INT LED_PIN = 13      // Constant with required initializer
INT COUNTER = 0             // Variable with initialization
WORD SIZE                   // Variable with default value (0)
BIT ENABLED                 // Boolean with default value (0)
STRING MESSAGE = "Ready"    // String with initialization

// Redefinition (automatic forget)
INT COUNTER = 42            // Replaces previous COUNTER definition
```

**Program Structure:**
```basic
CONST INT BLINK_COUNT = 5
INT LED_PIN = 13

FUNC BLINK(INT PIN, INT COUNT)    // Explicit parameter types
  FOR I = 1 TO COUNT
    WRITE PIN, 1
    DELAY 500
    WRITE PIN, 0
    DELAY 500
  NEXT
ENDFUNC

BEGIN                             // Main program
  BLINK(LED_PIN, BLINK_COUNT)     // Uses session globals
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

## Implementation Architecture

### Memory Layout (ROM-based interpreter)
- **$E000-$FFFF**: ROM-based BASIC interpreter (~8K)
- **$0900-$xxxx**: Dynamic heap for function bodies, strings, and symbol tables
- **Runtime stacks**: 6502 hardware stack + Hopper type/value stacks for expression evaluation
- **Global variables**: Persistent symbol table with stack-based storage

### Execution Strategy: **Threaded Code Interpreter**
- **Bytecode as jump table indices**: Each bytecode points to optimized 6502 handler routines
- **Performance**: Near-native speed with bytecode flexibility
- **Handler-based**: Each BASIC operation becomes a specialized 6502 routine ending in RTS

### Variable System: **Runtime Resolution with Caching**
- **Symbol tables**: Name → (type, stack_index) mapping for globals and locals
- **Late binding**: First reference resolves name to index, subsequent references use cached index
- **8-character limit**: Space-padded to exactly 8 chars for consistent storage/comparison
- **Automatic redefinition**: New declarations replace existing definitions with same name

### Function Management Architecture
- **Function blocks**: Each function stored as allocated memory block with header + bytecode
- **Two-phase compilation**: Large temporary blocks during compilation, exact-size final block
- **Runtime resolution**: Function names resolved at first call, then cached

### EEPROM Storage: **Complete Session Persistence**
- **Environment-level storage**: SAVE/LOAD entire session (constants, variables, functions, main)
- **Self-contained programs**: No missing dependencies when loaded
- **Simple file system**: Directory-based with 16 files maximum

## Performance Optimizations

**Compilation Strategy:**
- **Immediate tokenization** during program entry
- **Runtime name resolution** with caching for fast subsequent access
- **Type checking** at compile time prevents runtime errors
- **Automatic redefinition** eliminates namespace conflicts

**Performance Targets:**
- **Variable access**: 5-10 cycles after first resolution
- **Function calls**: 15-25 cycles for cached calls
- **Type operations**: Zero-cost for compatible types
- **Redefinition**: Fast replacement without explicit cleanup

## Key Design Principles

1. **FORTH-style interactivity** - incremental development with immediate testing
2. **Type safety first** - prevent runtime errors through compile-time checking
3. **Explicit over implicit** - no mysterious auto-creation or type coercion
4. **Complete session persistence** - save entire working environment
5. **Automatic redefinition** - natural refinement without manual cleanup
6. **Performance through caching** - fast execution after first resolution
7. **Structured programming** - block-structured syntax, no line numbers

This specification delivers **interactive development** with **type safety** and **complete session persistence**, providing the **FORTH development experience** with **modern language features** and **6502-optimized performance**.