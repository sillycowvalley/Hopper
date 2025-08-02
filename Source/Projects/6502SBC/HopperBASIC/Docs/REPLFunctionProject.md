# REPL Function Unification Project

## Project Overview
Refactor HopperBASIC to eliminate the dual execution paths (REPL vs Functions) by treating REPL expressions as temporary hidden functions. This unifies compilation, execution, and error handling while reducing code duplication and maintenance burden.

## Current Architecture Problems

### Dual Code Paths
- **REPL Path**: `Statement.EvaluateExpression()` → `Compiler.CompileExpression()` → `Executor.ExecuteOpcodes()`
- **Function Path**: `Functions.Compile()` → `Compiler.CompileFunction()` → Function execution
- **Result**: Duplicate compilation logic, inconsistent behavior, maintenance burden

### Console Command Duplication
- `PRINT` exists as both console command and statement
- Expression evaluation has separate REPL and function implementations
- Inconsistent error handling between paths

### Fragile State Management
- C|NC flag propagation easily corrupted by intermediate operations
- Binary success/failure insufficient for complex flows (Success, Failure, Exiting, Breaking)
- No context about failure reasons

## New Unified Architecture

### Hidden Function System
- **`$REPL`** - Temporary function for current expression/statement execution
- **`$MAIN`** - Replace current "BEGIN" function for main program
- **`$` Prefix Rule** - System functions invisible to `LIST`, `FUNCS`, `FORGET`, etc.

### Single Execution Path
```
Input → Tokenize → Create $REPL Function → Functions.Compile() → Execute via Function Call
```

### Robust State Management
Replace fragile C|NC flag propagation with per-subsystem state variables:

```hopper
// Per-subsystem state storage
const uint executorState    = Address.BasicExecutorWorkspace + 17;    // 1 byte
const uint compilerState    = Address.BasicCompilerWorkspace + 12;    // 1 byte  
const uint consoleState     = Address.BasicConsoleWorkspace + 8;      // 1 byte

// Universal state enum
enum SystemState 
{
    Failure = 0,    // Zero for easy testing (CMP -> Z flag)
    Success = 1,    // Normal completion
    Exiting = 2,    // User exit request (BYE, Ctrl+C)
    Breaking = 3,   // Break from current context
    Continuing = 4, // Continue operation
    Retry = 5,      // Recoverable error
    Pending = 6     // Operation in progress
}

// Helper methods per unit
SetState() { STA executorState; /* set compatibility flags */ }
GetState() { LDA executorState; /* set compatibility flags */ }
IsFailure() { LDA executorState; CMP #SystemState.Failure; }
```

## Implementation Strategy

### 1. Hidden Function Management
```hopper
// Persistent temp function - created once, reused
$REPL Function Node:
- Pre-allocated, never destroyed
- Token stream replaced per execution
- Opcode stream replaced per compilation
- Preserved for postmortem diagnostics
```

### 2. Dynamic Function Signatures
`$REPL` function arguments determined by statement type:

**Variable Declaration:**
```hopper
INT x = 5        → $REPL() { INT x = 5; }           // Global scope only
```

**Assignment:**
```hopper
x = 10           → $REPL() { x = 10; }              // Shared with functions
```

**Expression with Print:**
```hopper
PRINT x + 5      → $REPL() { PRINT x + 5; }        // Standard statement
```

**Function Call:**
```hopper
func(a, b)       → $REPL() { func(a, b); }         // Standard call
```

**Pure Expression:**
```hopper
x + 5            → $REPL() { RETURN x + 5; }        // Return value
```

### 3. Return Value Handling
- **Pre-allocated Return Slot**: Caller creates stack slot before all function calls
- **Universal Pattern**: REPL and user functions use identical return mechanism
- **Expression Results**: Available in return slot after `$REPL` execution

### 4. Argument Management
- **Existing APIs**: Use `Functions` and `Arguments` units' established interfaces
- **Helper Addition**: May need `Arguments.Clear()` to easily dispose existing arguments
- **Dynamic Setup**: Parse statement to determine required argument signature

## Statement Classification

### ✅ **Eliminated Console Commands** (become standard statements)
- **`PRINT`** → Standard statement execution via `$REPL`
- **`IF` statements** → Standard conditional compilation
- **Expression evaluation** → All expressions use function pipeline
- **Variable assignment** → Shared implementation with functions

### ⚠️ **Special REPL-Only Statements** (legal only in `$REPL`)
- **Global variable declaration**: `INT x = 5`, `CONST pi = 3.14`
- **Global constant declaration**: Only allowed at global scope
- **Rationale**: Functions cannot declare globals, only REPL can

### 🚫 **Remaining Console Commands** (console-specific functionality)

**Memory/System Management:**
- `NEW` - Clear all symbols
- `CLEAR` - Clear variables only  
- `FORGET identifier` - Remove specific symbol
- `MEM` - Show available memory
- `HEAP`, `BUFFERS`, `DUMP` - Debug information

**Program Listing:**
- `LIST` - Show all code (filter `$` functions)
- `VARS` - Show variables/constants
- `FUNCS` - Show user functions (filter `$` functions)

**Program Execution:**
- `RUN` - Execute `$MAIN` function
- `BYE` - Exit interpreter

**File Operations:**
- `SAVE`, `LOAD`, `DIR`, `DEL` - File system (future)

## Technical Implementation

### Modified Console Flow
```hopper
Console.processTokens()
{
    switch (token)
    {
        case ConsoleCommands: // NEW, CLEAR, FORGET, LIST, etc.
        {
            executeConsoleCommand();
            Console.GetState(); // Check for exit/error states
        }
        default: // All statements become $REPL execution
        {
            createReplFunction();
            Functions.Compile();
            Functions.GetState();
            switch (A)
            {
                case SystemState.Success:   { executeReplFunction(); }
                case SystemState.Exiting:   { Console.SetState(SystemState.Exiting); }
                case SystemState.Failure:   { displayError(); }
            }
        }
    }
}
```

### Global Scope Enforcement
```hopper
// In statement compilation
if (isGlobalDeclaration() && !isReplFunction())
{
    // Error: Global declarations only allowed in REPL
    Compiler.SetState(SystemState.Failure);
    setError(Messages.IllegalInFunctionMode);
}
```

### Function Name Filtering
```hopper
// In LIST, FUNCS, FORGET commands
if (functionName[0] == '$')
{
    continue; // Skip system functions
}
```

### Enhanced Error Context
- **Current**: Debug builds show PC location (`0x1234`)
- **Future**: Include function name context (`"in function Calculate"`, `"in REPL"`)
- **Implementation**: Error messages can reference current function being executed

## Benefits

### 🎯 **Code Reduction**
- Eliminate `Statement.EvaluateExpression()`
- Eliminate `Statement.executePrint()` 
- Eliminate duplicate expression compilation
- Eliminate REPL-specific error handling
- **Philosophy**: Size and maintenance benefits far outweigh any minor performance overhead

### 🎯 **Consistency**
- All variable access uses same mechanism
- All function calls use same mechanism
- All expression evaluation uses same mechanism
- All error handling uses same mechanism

### 🎯 **Maintainability**
- Single compilation pipeline to maintain
- Single execution pipeline to debug
- Single error handling system
- Unified feature development

### 🎯 **Robustness**
- State management immune to flag corruption
- Rich error context with multiple outcome types
- Clean separation of concerns

### 🎯 **Architecture**
- Clean separation: Console commands vs Program statements
- Consistent function call interface
- Simplified debugging and tracing

## Migration Strategy

### Design Principles for Migration
- **Maximum Incrementalism**: Each step produces a fully working system that could ship
- **Zero Risk to Operations**: Changes isolated from operational functionality
- **Clear Rollback Points**: Ability to revert individual steps if issues arise
- **Side-by-Side Validation**: Run old and new paths in parallel during transition
- **Feature Flags**: Use conditional compilation to enable/disable new paths

### Phase 1: Independent Infrastructure (Zero Risk)
**Goal**: Add supporting systems without touching operational code

1. **Trace System** (Completely Independent)
   - Create dedicated `Trace` unit with `#ifdef TRACE`
   - Add `ZP.TraceIndent` to zero page allocations
   - Implement trace methods as NOPs when disabled
   - Test with simple `$TEST` functions to validate infrastructure
   - **Risk**: None - no impact on operational system

2. **State Management Infrastructure** (Per Unit, Gradual)
   - Add state variables to workspace areas (one unit at a time)
   - Implement helper methods (`SetState`, `GetState`, etc.)
   - Start with least interconnected unit (e.g., `Tokenizer`)
   - Maintain C|NC compatibility during transition
   - **Risk**: Minimal - helper methods don't change existing behavior

3. **Hidden Function Filtering** (Immediate Safety)
   - Add `# REPL Function Unification Project

## Project Overview
Refactor HopperBASIC to eliminate the dual execution paths (REPL vs Functions) by treating REPL expressions as temporary hidden functions. This unifies compilation, execution, and error handling while reducing code duplication and maintenance burden.

## Current Architecture Problems

### Dual Code Paths
- **REPL Path**: `Statement.EvaluateExpression()` → `Compiler.CompileExpression()` → `Executor.ExecuteOpcodes()`
- **Function Path**: `Functions.Compile()` → `Compiler.CompileFunction()` → Function execution
- **Result**: Duplicate compilation logic, inconsistent behavior, maintenance burden

### Console Command Duplication
- `PRINT` exists as both console command and statement
- Expression evaluation has separate REPL and function implementations
- Inconsistent error handling between paths

### Fragile State Management
- C|NC flag propagation easily corrupted by intermediate operations
- Binary success/failure insufficient for complex flows (Success, Failure, Exiting, Breaking)
- No context about failure reasons

## New Unified Architecture

### Hidden Function System
- **`$REPL`** - Temporary function for current expression/statement execution
- **`$MAIN`** - Replace current "BEGIN" function for main program
- **`$` Prefix Rule** - System functions invisible to `LIST`, `FUNCS`, `FORGET`, etc.

### Single Execution Path
```
Input → Tokenize → Create $REPL Function → Functions.Compile() → Execute via Function Call
```

### Robust State Management
Replace fragile C|NC flag propagation with per-subsystem state variables:

```hopper
// Per-subsystem state storage
const uint executorState    = Address.BasicExecutorWorkspace + 17;    // 1 byte
const uint compilerState    = Address.BasicCompilerWorkspace + 12;    // 1 byte  
const uint consoleState     = Address.BasicConsoleWorkspace + 8;      // 1 byte

// Universal state enum
enum SystemState 
{
    Failure = 0,    // Zero for easy testing (CMP -> Z flag)
    Success = 1,    // Normal completion
    Exiting = 2,    // User exit request (BYE, Ctrl+C)
    Breaking = 3,   // Break from current context
    Continuing = 4, // Continue operation
    Retry = 5,      // Recoverable error
    Pending = 6     // Operation in progress
}

// Helper methods per unit
SetState() { STA executorState; /* set compatibility flags */ }
GetState() { LDA executorState; /* set compatibility flags */ }
IsFailure() { LDA executorState; CMP #SystemState.Failure; }
```

## Implementation Strategy

### 1. Hidden Function Management
```hopper
// Persistent temp function - created once, reused
$REPL Function Node:
- Pre-allocated, never destroyed
- Token stream replaced per execution
- Opcode stream replaced per compilation
- Preserved for postmortem diagnostics
```

### 2. Dynamic Function Signatures
`$REPL` function arguments determined by statement type:

**Variable Declaration:**
```hopper
INT x = 5        → $REPL() { INT x = 5; }           // Global scope only
```

**Assignment:**
```hopper
x = 10           → $REPL() { x = 10; }              // Shared with functions
```

**Expression with Print:**
```hopper
PRINT x + 5      → $REPL() { PRINT x + 5; }        // Standard statement
```

**Function Call:**
```hopper
func(a, b)       → $REPL() { func(a, b); }         // Standard call
```

**Pure Expression:**
```hopper
x + 5            → $REPL() { RETURN x + 5; }        // Return value
```

### 3. Return Value Handling
- **Pre-allocated Return Slot**: Caller creates stack slot before all function calls
- **Universal Pattern**: REPL and user functions use identical return mechanism
- **Expression Results**: Available in return slot after `$REPL` execution

### 4. Argument Management
- **Existing APIs**: Use `Functions` and `Arguments` units' established interfaces
- **Helper Addition**: May need `Arguments.Clear()` to easily dispose existing arguments
- **Dynamic Setup**: Parse statement to determine required argument signature

## Statement Classification

### ✅ **Eliminated Console Commands** (become standard statements)
- **`PRINT`** → Standard statement execution via `$REPL`
- **`IF` statements** → Standard conditional compilation
- **Expression evaluation** → All expressions use function pipeline
- **Variable assignment** → Shared implementation with functions

### ⚠️ **Special REPL-Only Statements** (legal only in `$REPL`)
- **Global variable declaration**: `INT x = 5`, `CONST pi = 3.14`
- **Global constant declaration**: Only allowed at global scope
- **Rationale**: Functions cannot declare globals, only REPL can

### 🚫 **Remaining Console Commands** (console-specific functionality)

**Memory/System Management:**
- `NEW` - Clear all symbols
- `CLEAR` - Clear variables only  
- `FORGET identifier` - Remove specific symbol
- `MEM` - Show available memory
- `HEAP`, `BUFFERS`, `DUMP` - Debug information

**Program Listing:**
- `LIST` - Show all code (filter `$` functions)
- `VARS` - Show variables/constants
- `FUNCS` - Show user functions (filter `$` functions)

**Program Execution:**
- `RUN` - Execute `$MAIN` function
- `BYE` - Exit interpreter

**File Operations:**
- `SAVE`, `LOAD`, `DIR`, `DEL` - File system (future)

## Technical Implementation

### Modified Console Flow
```hopper
Console.processTokens()
{
    switch (token)
    {
        case ConsoleCommands: // NEW, CLEAR, FORGET, LIST, etc.
        {
            executeConsoleCommand();
            Console.GetState(); // Check for exit/error states
        }
        default: // All statements become $REPL execution
        {
            createReplFunction();
            Functions.Compile();
            Functions.GetState();
            switch (A)
            {
                case SystemState.Success:   { executeReplFunction(); }
                case SystemState.Exiting:   { Console.SetState(SystemState.Exiting); }
                case SystemState.Failure:   { displayError(); }
            }
        }
    }
}
```

### Global Scope Enforcement
```hopper
// In statement compilation
if (isGlobalDeclaration() && !isReplFunction())
{
    // Error: Global declarations only allowed in REPL
    Compiler.SetState(SystemState.Failure);
    setError(Messages.IllegalInFunctionMode);
}
```

### Function Name Filtering
```hopper
// In LIST, FUNCS, FORGET commands
if (functionName[0] == '$')
{
    continue; // Skip system functions
}
```

### Enhanced Error Context
- **Current**: Debug builds show PC location (`0x1234`)
- **Future**: Include function name context (`"in function Calculate"`, `"in REPL"`)
- **Implementation**: Error messages can reference current function being executed

## Benefits

### 🎯 **Code Reduction**
- Eliminate `Statement.EvaluateExpression()`
- Eliminate `Statement.executePrint()` 
- Eliminate duplicate expression compilation
- Eliminate REPL-specific error handling
- **Philosophy**: Size and maintenance benefits far outweigh any minor performance overhead

### 🎯 **Consistency**
- All variable access uses same mechanism
- All function calls use same mechanism
- All expression evaluation uses same mechanism
- All error handling uses same mechanism

### 🎯 **Maintainability**
- Single compilation pipeline to maintain
- Single execution pipeline to debug
- Single error handling system
- Unified feature development

### 🎯 **Robustness**
- State management immune to flag corruption
- Rich error context with multiple outcome types
- Clean separation of concerns

### 🎯 **Architecture**
- Clean separation: Console commands vs Program statements
- Consistent function call interface
- Simplified debugging and tracing

 prefix filtering to `LIST`, `FUNCS`, `FORGET` commands
   - Create empty `$REPL` and `$MAIN` function nodes (unused initially)
   - Test filtering logic with temporary `$TEST` functions
   - **Risk**: None - only affects console command output

### Phase 2: REPL Command Migration (One at a Time, Lowest → Highest Risk)
**Goal**: Migrate individual REPL commands to unified execution, preserving full functionality

4. **Variable/Constant Declaration** (Lowest Risk)
   - Route `INT x = 5` through `$REPL` function creation
   - Preserve existing global scope enforcement
   - Validate side-by-side with current implementation
   - **Risk**: Low - self-contained, clear scope rules
   - **Rollback**: Keep old path available via feature flag

5. **Pure Expressions** (Low Risk)
   - Route `2 + 3` through `$REPL` with return value
   - Test return slot mechanism
   - Compare results with current direct evaluation
   - **Risk**: Low - simple return value, no side effects
   - **Validation**: Identical numeric results

6. **PRINT Statements** (Medium Risk)
   - Route `PRINT x + 5` through `$REPL` function
   - Ensure identical output formatting
   - Test with complex expressions and string literals
   - **Risk**: Medium - user-visible output must match exactly
   - **Validation**: Character-by-character output comparison

7. **User Function Calls** (Medium-High Risk)
   - Route `myFunc(a, b)` through `$REPL` function calls
   - Handle argument passing and return values
   - Test nested calls and complex argument expressions
   - **Risk**: Medium-High - complex but well-established patterns
   - **Validation**: Return values and side effects must match

8. **Complex Statements** (Highest Risk - Future)
   - Route `IF`, loops, etc. through unified compilation
   - Handle control flow and nested constructs
   - **Risk**: Highest - complex integration points
   - **Strategy**: Save for later phases after core unification proven

### Phase 3: State Management Migration (Per Unit)
**Goal**: Replace fragile C|NC propagation with robust state management

9. **Unit-by-Unit State Conversion**
   - Convert one unit at a time to use `SystemState` enum
   - Maintain compatibility bridges during transition
   - Test complex state flows (exit, break, error scenarios)
   - **Order**: Start with least interconnected units
   - **Validation**: All state transitions preserved

10. **Cross-Unit State Propagation**
    - Implement robust state passing between units
    - Remove C|NC compatibility shims
    - Test complex execution flows end-to-end
    - **Focus**: Console exit handling, error propagation

### Phase 4: Code Elimination (After Validation)
**Goal**: Remove obsolete code paths and optimize unified system

11. **Legacy Code Removal**
    - Remove `Statement.EvaluateExpression()` and related REPL-specific code
    - Remove duplicate compilation paths
    - Remove C|NC compatibility infrastructure
    - **Prerequisite**: All functionality validated on new paths

12. **Optimization and Polish**
    - Optimize unified compilation pipeline
    - Enhance error messages with function context
    - Performance tuning for unified execution
    - Documentation updates

### Validation Strategy Per Step

**Regression Testing**: 
- Automated comparison of old vs new behavior
- All existing REPL functionality must work identically
- User-visible changes minimized

**Side-by-Side Execution**:
- Run both old and new paths during transition
- Compare results at each integration point
- Log discrepancies for investigation

**Clear Success Criteria**:
- Identical output for all test cases
- No memory leaks or corruption
- Error handling preservation
- Performance within acceptable bounds

**Rollback Safety**:
- Feature flags allow immediate reversion
- Each step can be undone independently
- Clear checkpoint before major changes

### Risk Mitigation

**Memory Management**: 
- Ensure no leaks when mixing old/new allocation patterns
- Test allocation/deallocation cycles thoroughly

**Error Context**: 
- Preserve current error reporting during transition
- Enhance with function names after unification complete

**Integration Points**:
- Careful handling of stack management transitions
- Debug output compatibility across old and new paths
- Consistent behavior at all system boundaries

### Success Metrics
- ✅ Zero regression in existing functionality
- ✅ Reduced codebase size and complexity
- ✅ Improved error handling robustness  
- ✅ Enhanced debugging capabilities
- ✅ Maintainable unified architecture

## Success Criteria
- ✅ All current REPL functionality preserved
- ✅ Reduced codebase size and maintenance burden
- ✅ Consistent behavior between REPL and functions
- ✅ Robust state management without flag corruption
- ✅ Clean architectural separation
- ✅ Enhanced error context and debugging

## Future Enhancements Enabled
- **Unified optimizations** - All expressions benefit from function optimizations
- **Consistent debugging** - Same debug tools work for REPL and functions  
- **Feature parity** - New function features automatically available in REPL
- **Simplified testing** - Single execution path to test and validate
- **Rich error context** - Function names in error messages for better user experience