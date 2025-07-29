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

**Function Node Structure**:
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   local_count (number of local variables)
Offset 3-4: function body tokens pointer  
Offset 5-6: arguments list head pointer
Offset 7+:  null-terminated function name string
```

The `local_count` field enables efficient stack frame cleanup on RETURN.

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

### Value Stack Frame (Pascal Calling Convention)

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

**Benefits of Pascal Convention**:
- Local variables always at `BP + index` regardless of argument count
- Arguments naturally positioned by caller's evaluation order
- No BP adjustment needed based on parameter count

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

### Phase 2: Function Definition
1. Implement `accumulateDefinitionBody()` for collecting multi-line definitions
2. Add Ctrl-C abort handling with proper cleanup
3. Implement `executeFunctionDefinition()` for FUNC statements
4. Implement `executeBeginDefinition()` for BEGIN/END blocks
5. Add implicit RETURN token insertion for functions without explicit RETURN

### Phase 3: Function Execution
1. Implement function call mechanism in `executeIdentifier()`
2. Add stack overflow checking before calls
3. Implement RETURN statement execution with proper stack cleanup
4. Add local variable support with declaration-first constraint
5. Extend FORGET command to handle functions

### Phase 4: Management Commands
1. Update LIST to show both main program and functions
2. Implement RUN to execute __main function with dummy return address
3. Update FUNCS to list all defined functions

## Execution Flow

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

### Function Execution
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

### Function Call Mechanism
```hopper
callFunction()
{
    // 1. Arguments already on stack from expression evaluation
    
    // 2. Save return address (next token position)
    advancePC();  // Move past function call
    Stacks.PushPC();
    
    // 3. Save current base pointer
    Stacks.PushBP();
    
    // 4. Set new base pointer (points to first local)
    LDA ZP.SP
    SBC FuncExecArgCount  // Subtract argument count
    STA ZP.BP
    
    // 5. Point PC at function body
    Functions.GetBody();  // Sets ZP.IDY
    LDA ZP.IDYL
    STA ZP.PCL
    LDA ZP.IDYH
    STA ZP.PCH
    
    // 6. Execute function
    executeFunction();
}
```

### RUN Command Implementation
```hopper
cmdRun()
{
    // Find __main function
    LDA #(mainName % 256)  // "__main"
    STA ZP.TOPL
    LDA #(mainName / 256)
    STA ZP.TOPH
    
    Functions.Find();
    if (NC)
    {
        // No main program defined
        PrintError("No main program");
        RTS
    }
    
    // Set up dummy return address (0x0000)
    STZ ZP.PCL
    STZ ZP.PCH
    Stacks.PushPC();
    
    // Save BP and set up for call
    Stacks.PushBP();
    LDA ZP.SP
    STA ZP.BP  // No arguments for main
    
    // Execute __main as regular function
    Functions.GetBody();
    LDA ZP.IDYL
    STA ZP.PCL
    LDA ZP.IDYH
    STA ZP.PCH
    
    executeFunction();
    
    // Should never return here (dummy address)
}
```

## Error Handling

### Definition-Time Errors
- **Syntax Errors**: Show token window around error location
- **Redefinition**: Preserve existing function if new definition fails
- **Ctrl-C Abort**: Clean up temporary buffers and restore normal mode
- **Nested Definitions**: Error if FUNC appears inside FUNC/BEGIN

### Runtime Errors
- **Stack Overflow**: Check CSP < 254 before calls (need 2 slots)
- **Undefined Function**: Already handled by symbol table
- **Argument Mismatch**: Check argument count matches declaration
- **Missing RETURN**: Functions must end with RETURN (added implicitly if needed)

### FORGET Command Errors
- **Undefined Identifier**: "Undefined identifier" error message
- **Protected Names**: Cannot forget keywords or built-in functions
- **Success Message**: "Forgotten: [name]" confirmation

## Special Behaviors

### Main Program (__main)
- Stored as a regular function named "__main"
- No parameters allowed
- Executed by RUN command with dummy return address
- Replaced atomically on successful BEGIN/END parse
- No special execution path - uses standard function mechanism

### Local Variables
- **Declaration Constraint**: All locals must be declared before any executable statements
- **Scope**: Only accessible within their function
- **Storage**: On value stack starting at BP
- **Access**: Direct indexing via `BP + index`
- **Cleanup**: SP restored to BP on RETURN

### Implicit RETURN
- Functions without explicit RETURN get one added automatically
- Main program always gets implicit RETURN added
- RETURN without value leaves stack unchanged

### FORGET Command
- **Variables/Constants**: Removes from symbol table, frees associated memory
- **Functions**: Removes function node, argument list, and token stream
- **Main Program**: Can be forgotten like any other function

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
        [RETURN][EOL]
```

### Direct Execution (REPL)
```
Input:  PRINT 2 + 3

Stored: [PRINT][NUMBER "2"][PLUS][NUMBER "3"][EOL]
```

## Testing Strategy

### Phase 1 Tests
1. **PC Refactoring**: Verify all existing functionality works with PC instead of TokenizerPos
2. **EOL Termination**: Ensure REPL stops at EOL correctly
3. **FORGET Variables**: Test removing INT, WORD, BIT variables
4. **FORGET Constants**: Verify constants can be removed
5. **FORGET Errors**: Test undefined identifiers, keywords

### Phase 2-4 Tests
1. **Definition Tests**: Parse various function signatures and bodies
2. **Execution Tests**: Call functions with different argument counts
3. **Stack Tests**: Verify correct BP setup and argument access
4. **Error Tests**: Verify all error conditions are handled properly
5. **Recursion Tests**: Factorial, Fibonacci with stack overflow detection
6. **Integration Tests**: Functions calling functions, main calling functions

## Future Enhancements

1. **Return Type Checking**: Validate RETURN value matches function signature
2. **Default Parameters**: Allow optional parameters with default values
3. **Function Pointers**: Store function references in variables
4. **Better Error Messages**: Include function name in runtime errors
5. **Tail Call Optimization**: Detect and optimize tail recursion