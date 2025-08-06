# Resolve-and-Replace Architecture for HopperBASIC
**Document Type: Architecture Design**

## Core Concept: Runtime Symbol Resolution with Opcode Patching

Instead of runtime symbol lookups on every access, HopperBASIC uses a "resolve-and-replace" system:

1. **Initial compilation**: Emit unresolved opcodes with token buffer offsets to symbol names
2. **First execution**: Resolve names to node addresses, patch opcodes to fast versions
3. **Subsequent executions**: Use direct node address access (very fast)

This provides excellent performance while maintaining type safety through node-based access.

---

## Stack Frame Architecture: Always-Present Return Slot

### Consistent Return Value Handling
Every function call creates a return slot, regardless of whether the function returns a value or the caller uses it:

```
Value Stack Layout:
Before call:  [caller_data...]
After args:   [caller_data...] [return_slot(BIT 0)] [arg1] [arg2] ... [argN]  
After ENTER:  [caller_data...] [return_slot(BIT 0)] [arg1] [arg2] ... [argN] [saved_BP] [locals...]
After RETURN: [caller_data...] [return_value_or_zero]
```

### Return Statement Behavior
- **`RETURN` (no expression)**: Leaves return slot as `VOID 0` (zero-initialized)
- **`RETURNVAL` (with expression)**: Pops expression result into return slot, preserves type
- **Both opcodes**: Clean up arguments + locals using cleanup_count operand
- **Stack cleanup**: SP reduced by cleanup_count (arguments + locals, but NOT return slot)

### Always-Present Return Slot Benefits
1. **Predictable stack layout** for all function calls
2. **No crashes** on mixed `RETURN`/`RETURNVAL` patterns within same function  
3. **Consistent caller cleanup** (always exactly one return slot)
4. **Zero-initialized return slot** provides sensible default (`VOID 0`)
5. **Type information preserved** in parallel TypeStack
6. **Future-proof**: Return type checking already built-in via TypeStack

### Caller Responsibility
- **Function call as statement**: Emit `DECSP` after call to discard unused return value
- **Function call in expression**: Return value consumed directly (no cleanup needed)
- **Consistent pattern**: Every function call produces exactly one stack result

---

## Opcode Architecture: Unresolved → Resolved Pairs

### Function Calls
```hopper
// Unresolved (first execution)
CALL     functionNameOffset    // Look up function by name, patch to CALLF
// Resolved (subsequent executions)  
CALLF    functionNodeAddress   // Direct function call via node address
```

### Global Variable Access  
```hopper
// Unresolved (first execution)
PUSHGLOBAL variableNameOffset  // Look up variable by name, patch to PUSHGLOBALF
POPGLOBAL  variableNameOffset  // Look up variable by name, patch to POPGLOBALF

// Resolved (subsequent executions)
PUSHGLOBALF variableNodeAddress // Direct variable access via node address
POPGLOBALF  variableNodeAddress // Direct variable assignment via node address
```

### String Literals (Always Resolved)
```hopper
PUSHCSTRING stringAddress      // Direct pointer to string in token buffer
```

---

## Updated OpCode Definitions

### Function Management Opcodes
```hopper
// One byte operand opcodes (0x40-0x7F)
ENTER        = 0x48,  // Set up stack frame [arg_count]
RETURN       = 0x49,  // Return from function [cleanup_count]  
RETURNVAL    = 0x4A,  // Return with value [cleanup_count]

// Two byte operand opcodes (0x80-0xBF)  
CALL         = 0x83,  // Call function by name [name_offset_lsb] [name_offset_msb]
CALLF        = 0x84,  // Call function fast [node_lsb] [node_msb] (resolved)
```

### Stack Cleanup Semantics
- **`ENTER [arg_count]`**: Save current BP, set BP = SP - arg_count (point to first argument)
- **`RETURN [cleanup_count]`**: Restore BP, reduce SP by cleanup_count, leave return slot
- **`RETURNVAL [cleanup_count]`**: Pop value into return slot, restore BP, reduce SP by cleanup_count

---

## Implementation Roadmap

### Phase 1: Function Call Resolution (Priority 1)
**Goal**: Enable `RUN` command and function calls for benchmarks

**Required Changes:**
1. **OpCodes.asm**: Add function management opcodes with correct operand counts
2. **Compiler.asm**: Emit `CALL functionNameOffset` for function calls with return slot setup
3. **Executor.asm**: Implement resolution logic in `executeCall()` and stack frame management
4. **RUN command**: Find "BEGIN" function and execute it

**Test Case:**
```basic
FUNC Greet()
    PRINT 42
ENDFUNC

BEGIN
    Greet()  // Function call (return value discarded)
END

RUN  // Execute main program
```

### Phase 2: Global Variable Resolution (Priority 2)  
**Goal**: Fast global variable access with full type safety

**Required Changes:**
1. **OpCodes.asm**: Add `PUSHGLOBALF = 0x87`, `POPGLOBALF = 0x88`
2. **Compiler.asm**: Update `EmitPushGlobal()` to use name offsets
3. **Executor.asm**: Implement resolution logic in `executePushGlobal()`/`executePopGlobal()`

**Test Case:**
```basic
INT counter = 0
FUNC Increment()
    counter = counter + 1  // Global access
ENDFUNC
```

### Phase 3: CONSTSTRING Support (Priority 3)
**Goal**: String literals with full expression integration

**Required Changes:**
1. **BasicTypes.asm**: Add `CONSTSTRING = 0x13`
2. **Tokenizer.asm**: Enable string literal scanning (`"text"`)
3. **Compiler.asm**: Add `PUSHCSTRING` emission for string literals
4. **ComparisonInstructions.asm**: Smart string equality (pointer + content)
5. **Statement.asm**: Update `executePrint()` for CONSTSTRING handling

**Syntax Support:**
```basic
// Allowed (Phase 3)
CONST STRING greeting = "Hello World"    // Immutable string pointer
CONST STRING empty                       // Empty string (points to null terminator)
PRINT greeting                           // Direct string printing
IF message = "OK" THEN PRINT "Success"   // String comparison

// Not allowed (future)
STRING message = "Hello"                 // Mutable strings (Phase 4+)
```

---

## Technical Implementation Details

### Resolution Process (Functions and Variables)

**Step 1: Name Resolution**
```hopper
// At executeCall() first encounter:
// 1. Extract name offset from operand
// 2. Calculate absolute address: tokenBuffer + compilationStart + offset
// 3. Look up function via Functions.Find()
// 4. Get function node address
```

**Step 2: Opcode Patching**
```hopper
// 5. Patch opcode stream in place:
//    - Change CALL (0x83) to CALLF (0x84)  
//    - Replace name offset with node address
// 6. Execute CALLF immediately (don't defer to next run)
```

**Step 3: Fast Execution**
```hopper
// Subsequent executions hit CALLF directly:
// 1. Fetch node address from operand
// 2. Execute function immediately (no lookup)
```

### Function Call Compilation Strategy

**Detection Logic** in `compilePrimary()`:
```hopper
case Token.IDENTIFIER:
{
    // Look ahead to distinguish variable vs function call
    if (nextToken == LPAREN)
    {
        // Function call: functionName() 
        // 1. Push return slot (BIT 0)
        // 2. Compile arguments 
        // 3. Emit CALL nameOffset
    }
    else
    {
        // Variable reference: variableName
        // Emit PUSHGLOBAL nameOffset  
    }
}
```

**Argument List Compilation**:
```hopper
compileArgumentList()
{
    // ALWAYS push return slot first (BIT 0)
    EmitPushBit(0);
    
    // Then compile actual arguments
    if (args present)
    {
        compileExpression(); // arg1
        if (comma) compileExpression(); // arg2
        // etc.
    }
}
```

### Node-Based Access Benefits

**Type Safety:**
- Push operations get type from node: `Variables.GetValue()` → `ZP.TOP + ZP.TOPT`
- Pop operations validate compatibility: `Variables.GetType()` + `CheckRHSTypeCompatibility()`

**Performance:**
- Resolved opcodes are 2-3x faster (no symbol table traversal)
- Type information cached in node structure

**Memory Efficiency:**
- Function token streams persist with resolved opcodes
- No runtime symbol lookup overhead

---

## Recompilation Triggers

### Structural Changes (Require Recompilation)
- Adding/removing global variables → affects PUSHGLOBAL/POPGLOBAL resolution
- Adding/removing functions → affects CALL resolution  
- Renaming symbols → invalidates name-based resolution

### Value Changes (No Recompilation)
- Assigning new values to existing variables → resolved opcodes remain valid
- Calling existing functions → CALLF opcodes remain valid

**Implementation Note:** For MVP, trigger full recompilation on any structural change. Future optimization could track which functions need recompilation.

---

## String Syntax Constraints (Phase 3)

### Supported String Operations
```basic
CONST STRING msg = "Hello"       // String literal assignment
PRINT msg                        // String printing
PRINT "Direct literal"           // Direct string printing  
IF name = "admin" THEN ...       // String equality (smart: pointer + content)
```

### Deliberately Unsupported (This Phase)
```basic
STRING buffer = "Hello"          // Mutable strings (compile error)
msg = msg + " World"             // String concatenation (compile error)
INPUT msg                        // String input (compile error)
```

**Rationale:** Immutable strings via `CONST STRING` provide 90% of functionality needed for benchmarks while avoiding complex string memory management.

---

## Success Metrics

### Phase 1 Complete: Function Calls
```basic
FUNC Fibo(n)
    IF n <= 1 THEN RETURN n
    RETURN Fibo(n-1) + Fibo(n-2)  // Recursive calls work
ENDFUNC

BEGIN
    Fibo(10)  // Call from main program, return value discarded
END

RUN  // Executes BEGIN block
```

### Phase 2 Complete: Fast Globals
```basic
INT counter = 0
FUNC Test()
    counter = counter + 1  // Fast global access after first call
ENDFUNC
```

### Phase 3 Complete: String Literals
```basic
CONST STRING name = "Fibonacci"
FUNC Benchmark(arg, loops)
    PRINT name, "(", arg, ") = ", result
ENDFUNC
```

---

## Architecture Benefits

1. **Performance**: Resolved opcodes eliminate symbol lookup overhead
2. **Type Safety**: Node-based access preserves full type information
3. **Simplicity**: Same pattern for functions, variables, and strings
4. **Robustness**: Always-present return slot eliminates function return inconsistency crashes
5. **Extensibility**: Easy to add new symbol types (arrays, etc.)
6. **Debugging**: Unresolved opcodes clearly show first-time execution
7. **Consistent Cleanup**: Uniform stack management regardless of return value usage

This resolve-and-replace architecture with always-present return slots positions HopperBASIC for excellent performance while maintaining the simplicity, type safety, and robustness required for a reliable BASIC interpreter.