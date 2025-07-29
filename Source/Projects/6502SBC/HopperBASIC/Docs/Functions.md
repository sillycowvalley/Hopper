# HopperBASIC Function Definition and Execution Project

## Overview

This project implements function definition and execution for HopperBASIC, including both named functions (`FUNC`/`ENDFUNC`) and the main program (`BEGIN`/`END`). The design leverages the existing Hopper VM call stack and value stack infrastructure while maintaining compatibility with the current symbol table architecture.

## Design Principles

1. **Unified Token Execution**: Both immediate mode (REPL) and function bodies use the same token execution engine
2. **Stack-Based Architecture**: Leverage Hopper VM's call stack for return addresses and value stack for arguments/locals
3. **Self-Terminating Streams**: Token streams end with explicit terminators (EOF, RETURN) rather than length tracking
4. **Atomic Updates**: Functions are only replaced after successful parsing to prevent loss on errors
5. **Simple Local Scope**: All local variables must be declared before executable statements

## Key Architectural Changes

### 1. Program Counter Refactoring

**Current**: Uses `ZP.TokenizerPos` for token stream position tracking  
**New**: Use `ZP.PC` (already defined in Hopper VM) as an alias for TokenizerPos

```hopper
// In ZeroPage.asm
const TokenizerPos = PC  // Alias PC (0x12-0x13) for token execution
```

**Benefits**:
- Enables use of existing `Stacks.PushPC()`/`PopPC()` methods
- Semantic clarity: PC for "executing" tokens
- Saves zero page space
- Simplifies call/return implementation

### 2. Token Stream Termination

**Current**: Tracks `TokenBufferLength` during execution  
**New**: Self-terminating token streams

- **Direct Execution (REPL)**: Streams end with `EOF` token
- **Functions**: Streams end with `RETURN` token (explicit or implicit)
- **Main Program**: Streams end with `RETURN` followed by `EOF`

```hopper
// New token definition
const EOF = 0xFE  // End of file/stream marker (not 0x00 to avoid ambiguity)
```

### 3. Function Storage Enhancement

**Function Node Structure**:
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   local_count (number of local variables)
Offset 3-4: function body tokens pointer  
Offset 5-6: arguments list head pointer
Offset 7+:  null-terminated function name string
```

The `local_count` field enables efficient stack frame cleanup on RETURN.

## Memory Layout

### Call Stack Frame
Each function call uses 2 slots on the Hopper VM call stack:
- **2 bytes**: Return address (PC value)
- **1 byte**: Base pointer (BP value) (MSB for BP is always 0)

### Value Stack Frame
```
[...caller's data...]
[argument 1]          <- BP-2
[argument 2]          <- BP-1  
...
[argument N]          <- BP - <arg count> + (N-1)
[local var 1]         <- BP+0
[local var 2]         <- BP+1
...
[local var M]         <- BP+M
[...temporaries...]   <- SP
```

### State Management Buffers

**BasicProcessBuffer2** - Function Definition State:
```hopper
const FuncDefMode       = BasicProcessBuffer2 + 0   // 0=executing, 1=defining
const FuncDefTerminator = BasicProcessBuffer2 + 1   // ENDFUNC or END token
const FuncDefTempPtr    = BasicProcessBuffer2 + 2   // Temp accumulation pointer (16-bit)
const FuncDefTempLength = BasicProcessBuffer2 + 4   // Temp length during accumulation
const FuncDefNamePtr    = BasicProcessBuffer2 + 6   // Function name pointer (16-bit)
const FuncDefArgsHead   = BasicProcessBuffer2 + 8   // Arguments list head (16-bit)
const FuncDefLocalsOnly = BasicProcessBuffer2 + 10  // 1=only locals allowed, 0=done
```

**BasicProcessBuffer3** - Function Execution State:
```hopper
const FuncExecDepth     = BasicProcessBuffer3 + 0   // Call depth (for debugging)
const FuncExecFlags     = BasicProcessBuffer3 + 1   // Execution flags
const FuncExecReturnType = BasicProcessBuffer3 + 2  // Expected return type
```

## Implementation Phases

### Phase 1: Refactor Token Execution & Basic Commands
1. Replace all `ZP.TokenizerPos` references with `ZP.PC`
2. Remove `TokenBufferLength` from execution paths
3. Add EOF token generation at end of tokenized lines
4. Modify execution loop to stop at EOF token
5. **Implement FORGET command** for variables and constants (easy testing milestone)
   - Parse identifier name
   - Use `Variables.Remove()` for variables/constants
   - Provide appropriate success/error messages

### Phase 2: Function Definition
1. Implement `accumulateDefinitionBody()` for collecting multi-line definitions
2. Add Ctrl-C abort handling with proper cleanup
3. Implement `executeFunctionDefinition()` for FUNC statements
4. Implement `executeBeginDefinition()` for BEGIN/END blocks
5. Add implicit RETURN token insertion for functions without explicit RETURN

### Phase 3: Function Execution
1. Implement function call mechanism in `executeIdentifier()`
2. Add stack overflow checking before calls
3. Implement RETURN statement execution
4. Add local variable support with declaration-first constraint
5. Extend FORGET command to handle functions

### Phase 4: Management Commands
1. Update LIST to show both main program and functions
2. Implement RUN to execute __main function
3. Update FUNCS to list all defined functions

## Error Handling

### Definition-Time Errors
- **Syntax Errors**: Show token window around error location
- **Redefinition**: Preserve existing function if new definition fails
- **Ctrl-C Abort**: Clean up temporary buffers and restore normal mode
- **Nested Definitions**: Error if FUNC appears inside FUNC/BEGIN

### Runtime Errors
- **Stack Overflow**: Check before each call (need 3 bytes free)
- **Undefined Function**: Already handled by symbol table
- **Type Mismatch**: Return value type checking
- **Missing RETURN**: Functions must end with RETURN (added implicitly if needed)

### FORGET Command Errors
- **Undefined Identifier**: "Undefined identifier" error message
- **Protected Names**: Cannot forget keywords or built-in functions
- **Success Message**: "Forgotten: [name]" confirmation

## Special Behaviors

### Main Program (__main)
- Stored as a special function named "__main"
- No parameters allowed
- Executed by RUN command
- Replaced atomically on successful BEGIN/END parse

### Local Variables
- **Declaration Constraint**: All locals must be declared before any executable statements
- **Scope**: Only accessible within their function
- **Storage**: On value stack above arguments
- **Cleanup**: Automatic on RETURN (using local_count)

### Implicit RETURN
- Functions without explicit RETURN get one added automatically
- Main program always gets implicit RETURN
- RETURN without value returns 0 (type TBD based on function signature)

### FORGET Command
- **Variables/Constants**: Removes from symbol table, frees associated memory
- **Functions**: Removes function node, argument list, and token stream
- **Main Program**: Special handling for "__main" (clear it but keep function infrastructure)

## Token Stream Examples

### Function Definition
```
Input:  FUNC Add(INT a, INT b)
        INT sum
        sum = a + b
        RETURN sum
        ENDFUNC

Stored: [INT][ID "sum"][EOL]
        [ID "sum"][EQUALS][ID "a"][PLUS][ID "b"][EOL]
        [RETURN][ID "sum"][EOL]
```

### Main Program
```
Input:  BEGIN
        PRINT "Hello"
        x = 10
        END

Stored: [PRINT][STRING "Hello"][EOL]
        [ID "x"][EQUALS][NUMBER "10"][EOL]
        [EOF]
```

### FORGET Usage
```
> INT x = 10
> FORGET x
Forgotten: x
> FORGET x
Undefined identifier
```

## Testing Strategy

### Phase 1 Tests
1. **PC Refactoring**: Verify all existing functionality works with PC instead of TokenizerPos
2. **EOF Token**: Ensure proper termination of direct execution
3. **FORGET Variables**: Test removing INT, WORD, BIT variables
4. **FORGET Constants**: Verify constants can be removed
5. **FORGET Errors**: Test undefined identifiers, keywords

### Phase 2-4 Tests
1. **Definition Tests**: Parse various function signatures and bodies
2. **Execution Tests**: Call functions with different argument counts
3. **Error Tests**: Verify all error conditions are handled properly
4. **Recursion Tests**: Factorial, Fibonacci with stack overflow detection
5. **Integration Tests**: Functions calling functions, main calling functions

## Future Enhancements

1. **Return Type Checking**: Validate RETURN value matches function signature
2. **Default Parameters**: Allow optional parameters with default values
3. **Function Pointers**: Store function references in variables
4. **Closures**: Capture variables from enclosing scope (Phase 3+)