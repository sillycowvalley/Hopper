# HopperBASIC Refactoring Project Document

## Project Overview
This document describes the refactoring of HopperBASIC's command processing and execution architecture to eliminate code duplication, simplify state management, and create a unified execution pipeline.

## Current Problems to Solve

### 1. Multiple Execution Paths
The system currently has four different paths that all compile and execute code:
- `cmdRun()` → `Statement.EvaluateExpression()` → `Executor.ExecuteOpCodes()`
- `executeReplFunction()` → `Statement.Execute()` → `Statement.ExecuteStatement()` → `Executor.ExecuteOpCodes()`
- `processTokensAndCheckFunction()` → `processTokens()` → `Statement.Execute()` → `Statement.ExecuteStatement()` → `Executor.ExecuteOpCodes()`
- `processSingleSymbolDeclaration()` → `Statement.EvaluateExpression()` → `Executor.ExecuteOpCodes()`

### 2. Inconsistent State Management
Three different mechanisms are used inconsistently:
- **C/NC flags**: Fragile across call boundaries
- **SystemState**: Clear but not universally used
- **LastError**: Sometimes set, sometimes not

### 3. Duplicate Switch Statements
Command keyword handling is duplicated across:
- `Statement.Execute()` switch
- `Console.processTokens()` switch  
- Console command switch
- `Compiler.compilePrimary()` switch

### 4. $REPL Function Overhead
Creating temporary functions for simple REPL operations adds unnecessary complexity and performance overhead.

## Refactored Architecture Design

### Core Principle: SystemState-Centric Communication
**Decision**: Abandon C/NC flags for inter-system communication. Use SystemState as the primary mechanism, with C/NC only for leaf-node APIs.

### 1. State Management (State.asm)

```asm
unit State
{
    flags SystemState 
    {
        Failure = 0,    // Zero for easy testing
        Success = 1,    // Normal completion
        Exiting = 2,    // User exit request
        Return  = 3     // Function returned value
    }
    
    // Composite state setter
    SetError()
    {
        // Input: ZP.ACCL/H = error message pointer
        PHA
        LDA ZP.ACCL
        STA ZP.LastErrorL
        LDA ZP.ACCH
        STA ZP.LastErrorH
        LDA #SystemState.Failure
        STA ZP.SystemState
        PLA
    }
    
    // Clear all state for fresh operation
    Reset()
    {
        STZ ZP.LastErrorL
        STZ ZP.LastErrorH
        LDA #SystemState.Success
        STA ZP.SystemState
    }
    
    // Check if we should continue processing
    ShouldContinue()
    {
        // Output: C if should continue, NC if should stop
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z) { CLC } else { SEC }
        PLA
    }
}
```

### 2. Unified Execution Layer (Execute.asm)

Create a new unit that provides a single entry point for all compilation and execution:

```asm
unit Execute
{
    enum ExecutionContext
    {
        Program    = 0,  // Running $MAIN - no return value expected
        Function   = 1,  // Running user function - may return value
        Statement  = 2,  // Console statement - print result if expression
        Expression = 3,  // RHS evaluation - always leaves value on stack
    }
    
    // Single unified execution entry point
    // Input: ZP.ACCL = ExecutionContext
    //        Tokens in BasicTokenizerBuffer
    //        ZP.TokenBufferLength set
    // Output: SystemState set (Success/Failure/Return/Exiting)
    //         Result on stack if applicable
    //         LastError set if Failure
    Run()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        STA executeContext
        
        State.Reset();
        
        loop
        {
            compile();
            State.IsFailure();
            if (C) { break; }
            
            execute();
            State.IsFailure();
            if (C) { break; }
            
            handleResult();
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    compile()
    {
        Compiler.InitOpCodeBuffer();
        State.IsFailure();
        if (C) { return; }
        
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        Tokenizer.NextToken();
        State.IsFailure();
        if (C) { return; }
        
        LDA executeContext
        switch (A)
        {
            case ExecutionContext.Expression:
            {
                Compiler.CompileExpression();
            }
            default:
            {
                compileStatements();
            }
        }
    }
    
    execute()
    {
        Executor.ExecuteOpCodes();
    }
    
    handleResult()
    {
        State.IsFailure();
        if (C) { return; }
        
        LDA executeContext
        switch (A)
        {
            case ExecutionContext.Program:
            {
                State.IsReturn();
                if (C)
                {
                    Value.Pop(); // Discard any return value
                }
                State.SetSuccess();
            }
            
            case ExecutionContext.Function:
            {
                State.IsReturn();
                if (NC)
                {
                    Value.PushVoid();
                }
                State.SetSuccess();
            }
            
            case ExecutionContext.Statement:
            {
                // Leave result on stack if expression
                State.SetSuccess();
            }
            
            case ExecutionContext.Expression:
            {
                // Value already on stack
                State.SetSuccess();
            }
        }
    }
    
    uint executeContext
}
```

### 3. Command Dispatch System (CommandDispatch.asm)

Implement a table-driven dispatch system using Hopper Assembly's optimized switch:

```asm
unit CommandDispatch
{
    flags TokenFlags
    {
        ConsoleCommand = 0b10000000,
        Statement      = 0b01000000,
        Declaration    = 0b00100000,
        Expression     = 0b00010000,
        Operator       = 0b00001000,
        Literal        = 0b00000100,
    }
    
    // Token category table (256 bytes, indexed by token value)
    // Located in ROM, indexed directly by token value
    tokenCategories:
    {
        .byte 0                                    // Tokens.EOF
        .byte TokenFlags.ConsoleCommand           // Tokens.RUN
        .byte TokenFlags.ConsoleCommand           // Tokens.NEW
        .byte TokenFlags.Statement                // Tokens.PRINT
        .byte TokenFlags.Statement                // Tokens.IF
        .byte TokenFlags.Declaration              // Tokens.INT
        .byte TokenFlags.Expression               // Tokens.ABS
        // ... complete for all tokens
    }
    
    // Check token category
    // Input: A = token
    // Output: A = token flags
    GetTokenFlags()
    {
        TAX
        LDA tokenCategories, X
    }
    
    // Optimized dispatch using Hopper Assembly jump table
    // Input: X = token, Y = allowed flags
    // Output: Calls appropriate handler, sets SystemState
    Dispatch()
    {
        STY allowedFlags
        
        // Get token flags
        LDA tokenCategories, X
        AND allowedFlags
        if (Z)
        {
            State.SetError(); // Token not allowed in this context
            return;
        }
        
        // X already contains token, perfect for switch optimization
        switch (X)  // Note: Using X register for jump table optimization
        {
            // Console commands
            case Tokens.RUN:     { cmdRun(); }
            case Tokens.NEW:     { cmdNew(); }
            case Tokens.LIST:    { Listing.CmdList(); }
            case Tokens.VARS:    { Listing.CmdVars(); }
            case Tokens.FUNCS:   { Listing.CmdFuncs(); }
            case Tokens.FORGET:  { cmdForget(); }
            case Tokens.CLEAR:   { cmdClear(); }
            case Tokens.MEM:     { cmdMem(); }
            case Tokens.BYE:     { cmdBye(); }
            case Tokens.HEAP:    { cmdHeap(); }
            case Tokens.BUFFERS: { cmdBuffers(); }
            case Tokens.DUMP:    { cmdDump(); }
            
            // Statement commands
            case Tokens.PRINT:   { executePrint(); }
            case Tokens.IF:      { executeIf(); }
            case Tokens.WHILE:   { executeWhile(); }
            case Tokens.RETURN:  { executeReturn(); }
            case Tokens.END:     { executeEnd(); }
            
            // Declaration commands
            case Tokens.INT:     { executeDeclaration(); }
            case Tokens.WORD:    { executeDeclaration(); }
            case Tokens.BIT:     { executeDeclaration(); }
            case Tokens.BYTE:    { executeDeclaration(); }
            case Tokens.STRING:  { executeDeclaration(); }
            case Tokens.CONST:   { executeConstDeclaration(); }
            case Tokens.FUNC:    { FunctionDeclaration.ExecuteFunctionDeclaration(); }
            case Tokens.BEGIN:   { FunctionDeclaration.ExecuteBeginDeclaration(); }
            
            default:
            {
                State.SetError(); // Unknown token
            }
        }
        return; // Required for jump table optimization
    }
    
    uint allowedFlags
}
```

### 4. Simplified Console Layer (Console.asm modifications)

```asm
// New simplified console input processing
processInput()
{
    Tokenizer.TokenizeLine();
    State.IsFailure();
    if (C) 
    { 
        Error.CheckAndPrint();
        return;
    }
    
    // Try console commands first
    LDX ZP.CurrentToken
    LDY #TokenFlags.ConsoleCommand
    CommandDispatch.Dispatch();
    State.IsFailure();
    if (NC)  // Not a console command or command succeeded
    {
        State.IsSuccess();
        if (C) { return; } // Console command handled successfully
        
        // Not a console command - execute as statement
        LDA #ExecutionContext.Statement
        STA ZP.ACCL
        Execute.Run();
    }
    
    // Handle execution result
    State.IsFailure();
    if (C)
    {
        Error.CheckAndPrint();
        return;
    }
    
    State.IsExiting();
    if (C) { return; }
    
    // Check if there's a result to print
    Value.HasValue(); // New method to check if stack has value
    if (C)
    {
        Messages.PrintStackTop();
    }
    else
    {
        Messages.PrintOK();
    }
}
```

### 5. Value Stack Helper (Value.asm - new unit)

```asm
unit Value
{
    // Check if value stack has a value
    // Output: C if value exists, NC if empty
    HasValue()
    {
        // Check value stack pointer
        LDA ZP.VSP
        if (Z)
        {
            CLC  // Stack empty
        }
        else
        {
            SEC  // Has value(s)
        }
    }
    
    // Push void value (for functions with no return)
    PushVoid()
    {
        LDA #Types.Void
        PHA
        STZ ZP.ACCL
        STZ ZP.ACCH
        Stacks.PushValue();
    }
    
    // Pop and discard top value
    Pop()
    {
        Stacks.PopValue();
    }
}
```

## Implementation Steps

### Phase 1: Foundation (Week 1)
1. Implement enhanced State.asm methods (SetError, Reset, ShouldContinue)
2. Create Value.asm unit with stack helpers
3. Add TokenFlags enumeration and tokenCategories table

### Phase 2: Unified Execution (Week 2)
1. Create Execute.asm with Run() method
2. Implement three internal methods: compile(), execute(), handleResult()
3. Test with simple expressions first

### Phase 3: Command Dispatch (Week 3)
1. Create CommandDispatch.asm
2. Populate tokenCategories table for all tokens
3. Implement Dispatch() with optimized switch statement
4. Update all command handlers to use SystemState

### Phase 4: Console Simplification (Week 4)
1. Rewrite Console.processInput() to use new architecture
2. Remove executeReplFunction() and createReplFunction()
3. Remove processTokens() and related duplicate code
4. Update cmdRun() to use Execute.Run()

### Phase 5: Cleanup (Week 5)
1. Remove all duplicate switch statements
2. Update all Statement.Execute() paths to use Execute.Run()
3. Remove Statement.EvaluateExpression() (replaced by Execute.Run)
4. Update all error checking to use State.ShouldContinue()

### Phase 6: Testing & Optimization (Week 6)
1. Test all console commands
2. Test all statement types
3. Test function execution and recursion
4. Profile and optimize hot paths

## Success Criteria

1. **Single Execution Path**: All code compilation/execution goes through Execute.Run()
2. **Consistent State Management**: SystemState used everywhere, C/NC only in leaf nodes
3. **No Duplicate Switches**: Each token handled in exactly one place
4. **No $REPL Functions**: Direct execution of console statements
5. **Clear Error Handling**: Every error sets both SystemState.Failure and LastError
6. **Performance**: No regression in execution speed despite additional abstraction

## Key Design Decisions

1. **SystemState over C/NC**: More robust across call boundaries, clearer semantics
2. **Table-Driven Dispatch**: More maintainable than cascading switches
3. **Execution Contexts**: Makes result handling explicit and consistent
4. **Token Flags**: Enables context-aware command processing
5. **Jump Table Optimization**: Leverages Hopper Assembly's efficient switch for dispatch

## Testing Strategy

### Unit Tests
- Each new unit gets dedicated test cases
- State transitions tested exhaustively
- Command dispatch tested for all tokens

### Integration Tests
- All existing BASIC programs must continue working
- Console commands tested in all contexts
- Error paths tested thoroughly

### Performance Tests
- Measure execution time before/after refactoring
- Ensure no performance regression
- Profile hot paths for optimization opportunities

## Notes for Future Implementation

1. **Preserve Working Code**: Keep old implementation alongside new until fully tested
2. **Incremental Migration**: Move one subsystem at a time to new architecture
3. **Debug Support**: Add trace points in new code for debugging
4. **Documentation**: Update all method headers with new calling conventions
5. **Regression Prevention**: Create test suite before starting refactoring

## Expected Benefits

1. **Maintainability**: 40% less code through deduplication
2. **Clarity**: Single clear execution model
3. **Performance**: Faster dispatch through jump tables
4. **Extensibility**: Easy to add new commands/contexts
5. **Reliability**: Consistent state management reduces bugs

This refactoring will transform HopperBASIC from a system with multiple ad-hoc execution paths into a clean, unified architecture that's easier to understand, maintain, and extend.