# HopperBASIC Function Definition and Execution Project

## Overview

This project implements function definition and execution for HopperBASIC, including both named functions (`FUNC`/`ENDFUNC`) and the main program (`BEGIN`/`END`). The design leverages the existing Hopper VM call stack and value stack infrastructure while maintaining compatibility with the current symbol table architecture.

## Design Principles

1. **Unified Token Execution**: Both immediate mode (REPL) and function bodies use the same token execution engine
2. **Stack-Based Architecture**: Leverage Hopper VM's call stack for return addresses and value stack for arguments/locals
3. **Self-Terminating Streams**: Token streams end with natural terminators (EOL for REPL, RETURN for functions)
4. **Atomic Updates**: Functions are only replaced after successful parsing to prevent loss on errors
5. **Simple Local Scope**: All local variables must be declared before executable statements
6. **Pascal Calling Convention**: Arguments below BP, locals above BP for efficient access

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

### 2. Token Stream Termination Strategy

**Simplified approach** - No explicit EOF tokens needed:

- **Direct Execution (REPL)**: Execute until EOL token
- **Functions**: Execute until RETURN token (explicit or implicit)
- **Main Program**: Treated as regular function, ends with RETURN

```hopper
// Token definitions
const EOL = 0xFF     // End of line (already defined)
const RETURN = 0xFD  // Return from function
// No EOF token needed
```

### 3. Function Storage Enhancement

**Function Node Structure** (Phase 2-3):
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   reserved for local_count (implemented in Phase 6)
Offset 3-4: function body tokens pointer  
Offset 5-6: arguments list head pointer (parsed but not used until Phase 6)
Offset 7+:  null-terminated function name string
```

**Function Node Structure** (Phase 6):
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   local_count (number of local variables)
Offset 3-4: function body tokens pointer  
Offset 5-6: arguments list head pointer
Offset 7+:  null-terminated function name string
```

## Memory Architecture

### Hopper VM Stack Design

The Hopper VM uses a split-page architecture for efficient stack management:

```hopper
const CallStackLSB  = 0x0300  // LSBs of 256 call stack slots
const CallStackMSB  = 0x0400  // MSBs of 256 call stack slots
const TypeStackLSB  = 0x0500  // 256 type stack slots
const ValueStackLSB = 0x0600  // LSBs of 256 value stack slots
const ValueStackMSB = 0x0700  // MSBs of 256 value stack slots
```

This design allows:
- Single-byte stack pointers (CSP, SP)
- Direct indexed access: `CallStackLSB[CSP]` and `CallStackMSB[CSP]`
- 256 slots per stack

### Call Stack Frame

Each function call uses 2 slots on the call stack:
- **Slot N**: Return address low byte (PCL)
- **Slot N+1**: Return address high byte (PCH) with BP in MSB page

Since BP (Base Pointer) is limited to value stack positions (0-255), we can store:
- `CallStackLSB[CSP]` = PCL
- `CallStackMSB[CSP]` = PCH
- `CallStackLSB[CSP+1]` = BP
- `CallStackMSB[CSP+1]` = 0 (unused)

### Value Stack Frame (Pascal Calling Convention) - Phase 6

```
[...caller's data...]
[argument N]          <- BP - 1
[argument N-1]        <- BP - 2
...
[argument 2]          <- BP - (N-1)
[argument 1]          <- BP - N
[local var 1]         <- BP + 0  ← BP points here
[local var 2]         <- BP + 1
...
[local var M]         <- BP + (M-1)
[...temporaries...]   <- SP
```

**Note**: In Phases 2-4, functions execute without argument/local support. BP is saved/restored but not used for variable access.

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
const FuncDefLocalCount = BasicProcessBuffer2 + 11  // Count of local variables
```

**BasicProcessBuffer3** - Function Execution State:
```hopper
const FuncExecDepth     = BasicProcessBuffer3 + 0   // Call depth (for debugging)
const FuncExecFlags     = BasicProcessBuffer3 + 1   // Execution flags
const FuncExecArgCount  = BasicProcessBuffer3 + 2   // Current function's argument count
```

## Implementation Phases

### Phase 1: Refactor Token Execution & Basic Commands
1. Replace all `ZP.TokenizerPos` references with `ZP.PC`
2. Remove `TokenBufferLength` from execution paths
3. Modify REPL execution to stop at EOL token
4. Modify function execution to stop at RETURN token
5. **Implement FORGET command** for variables and constants (easy testing milestone)
   - Parse identifier name
   - Use `Variables.Remove()` for variables/constants
   - Provide appropriate success/error messages

### Phase 2: Function Definition (No Arguments/Locals)
1. Implement `accumulateDefinitionBody()` for collecting multi-line definitions
2. Add Ctrl-C abort handling with proper cleanup
3. Implement `executeFunctionDefinition()` for FUNC statements
   - Parse function name and parameter list (store but don't use parameters)
   - Accumulate body tokens until ENDFUNC
4. Implement `executeBeginDefinition()` for BEGIN/END blocks
5. Add implicit RETURN token insertion for functions without explicit RETURN

### Phase 3: Function Execution (No Arguments/Locals)
1. Implement basic function call mechanism in `executeIdentifier()`
   - No argument passing yet (functions can only use globals)
2. Add stack overflow checking before calls
3. Implement RETURN statement execution with proper PC/BP restoration
4. Extend FORGET command to handle functions

### Phase 4: Management Commands
1. Update LIST to show both main program and functions
2. Implement RUN to execute __main function with dummy return address
3. Update FUNCS to list all defined functions with signatures

### Phase 5: EEPROM Storage (SAVE/LOAD)
1. Design tokenized program format for EEPROM storage
2. Implement SAVE command to store all functions and variables
3. Implement LOAD command to restore saved programs
4. Add DIR command to list saved programs
5. Add DEL command to delete saved programs
6. Enable development of test suites as saved programs

### Phase 6: Arguments and Local Variables
1. Add new token types for local/argument access:
   - `PUSHLOCAL` - Push local variable value
   - `POPLOCAL` - Pop value to local variable
2. Modify expression parser to resolve local/argument identifiers
3. Implement argument passing in function calls:
   - Evaluate argument expressions
   - Set up BP correctly for Pascal convention
4. Implement local variable allocation and access
5. Add scope resolution (locals shadow globals)
6. Update all error messages to include local context

## Execution Flow (Phases 2-4)

### REPL Execution
```hopper
executeREPL()
{
    // Execute until EOL token
    loop
    {
        getCurrentToken();  // Token at PC
        CMP #Tokens.EOL
        if (Z) { break; }  // End of line
        
        Statement.Execute();
        if (NC) { break; } // Error occurred
    }
}
```

### Function Execution (Without Arguments/Locals)
```hopper
executeFunction()
{
    // Execute until RETURN token
    loop
    {
        getCurrentToken();  // Token at PC
        CMP #Tokens.RETURN
        if (Z) 
        { 
            executeReturn();  // Restores PC/BP from call stack
            break;
        }
        
        Statement.Execute();
        if (NC) { break; } // Error occurred
    }
}
```

### Simple Function Call (Phase 3)
```hopper
callFunction()
{
    // 1. Save return address (next token position)
    advancePC();  // Move past function call
    Stacks.PushPC();
    
    // 2. Save current base pointer (not used until Phase 6)
    Stacks.PushBP();
    
    // 3. Point PC at function body
    Functions.GetBody();  // Sets ZP.IDY
    LDA ZP.IDYL
    STA ZP.PCL
    LDA ZP.IDYH
    STA ZP.PCH
    
    // 4. Execute function
    INC FuncExecDepth
    executeFunction();
    DEC FuncExecDepth
}
```

## Benefits of Phased Approach

### Early Testing (Phases 2-3)
- Functions work with global variables only
- Can test function definition/execution infrastructure
- Can implement recursive algorithms using globals
- Management commands (LIST, RUN, FUNCS) fully testable

### EEPROM Before Locals (Phase 5)
- Develop comprehensive test programs
- Save/load test suites for regression testing
- Share programs without copy/paste
- Build a library of test cases before tackling complex local variable implementation

### Deferred Complexity (Phase 6)
- Local variable implementation requires new tokens
- Scope resolution adds complexity
- Argument passing needs careful stack management
- Can be implemented with confidence after core system is stable

## Token Stream Examples

### Phase 2-3: Function Without Arguments
```
Input:  FUNC PrintBanner()
        PRINT "**********"
        PRINT "* HELLO  *"
        PRINT "**********"
        ENDFUNC

Stored: [PRINT][STRING "**********"][EOL]
        [PRINT][STRING "* HELLO  *"][EOL]
        [PRINT][STRING "**********"][EOL]
        [RETURN][EOL]
```

### Phase 2-3: Function With Ignored Parameters
```
Input:  FUNC Add(INT a, INT b)
        ' Parameters parsed but not accessible yet
        PRINT "Add function called"
        ENDFUNC

Stored: [REM][STRING " Parameters parsed but not accessible yet"][EOL]
        [PRINT][STRING "Add function called"][EOL]
        [RETURN][EOL]
```

### Phase 6: Function With Working Arguments
```
Input:  FUNC Add(INT a, INT b)
        INT sum
        INT extra = 10
        sum = a + b
        RETURN sum
        ENDFUNC

Stored: [INT][ID "sum"][EOL]
        [INT][ID "extra"][EQUALS][NUMBER 10][EOL]
        [ID "extra"][EQUALS][ID "a"][PLUS][ID "b"][EOL]
        [PUSHLOCAL 0][RETURN][EOL]
        
JIT Optimized: [ENTER 2][NUMBER 10][POPLOCAL 1][PUSHLOCAL -2][PLUS][PUSHLOCAL -1][POPLOCAL 0][PUSHLOCAL 0][RETURN]
```

## Testing Strategy by Phase

### Phase 1 Tests
- PC refactoring verification
- FORGET command for variables/constants

### Phase 2-3 Tests
- Function definition with various signatures
- Function calls using only globals
- Recursive functions with global counters
- FORGET for functions

### Phase 4 Tests
- LIST showing all functions
- RUN executing __main
- FUNCS listing signatures

### Phase 5 Tests
- SAVE/LOAD round trips
- Multiple program management
- EEPROM space management

### Phase 6 Tests
- Argument passing correctness
- Local variable scoping
- Stack frame management
- Nested function calls with locals