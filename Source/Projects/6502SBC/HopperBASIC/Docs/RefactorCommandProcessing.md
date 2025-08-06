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

### 4. Buffer Munting Problem
**Critical Issue**: Compilation uses BasicTokenizerBuffer and BasicOpCodeBuffer as temporary workspace. Only one thing can be compiled at a time. This causes:
- Function calls during REPL overwrite REPL tokens and opcodes
- Colon-separated statements fail when first statement compiles/executes (e.g., `INT FOO = 10*10 : INT BAR = 20*20`)
- RUN command's tokens get munted when $MAIN is JIT compiled

## Key Design Decisions

1. **SystemState over C/NC**: More robust across call boundaries, clearer semantics
2. **Table-Driven Dispatch**: More maintainable than cascading switches
3. **Execution Contexts**: Makes result handling explicit and consistent
4. **Token Flags**: Enables context-aware command processing
5. **Jump Table Optimization**: Leverages Hopper Assembly's efficient switch for dispatch
6. **Buffer Save/Restore**: Direct solution to buffer munting without function overhead
   - Save/restore buffers when nested compilation needed
   - Simpler and faster than $REPL function approach
   - Uses ~1KB RAM for save areas

## Refactored Architecture Design

### Core Principle: SystemState-Centric Communication
**Decision**: Abandon C/NC flags for inter-system communication. Use SystemState as the primary mechanism, with C/NC only for leaf-node APIs.

### 1. State Management (States.asm)

```asm
unit State
{
    flags SystemState 
    {
        Failure = 0,    // Zero for easy testing
        Success = 1,    // Normal completion
        Exiting = 2,    // User exit request (Ctrl+C or BYE)
        Return  = 3     // Function returned value
    }
    
    // Composite state setter
    // Input: ZP.ACCL/H = error message pointer
    // Output: SystemState set to Failure, LastError set
    // Modifies: ZP.SystemState, ZP.LastErrorL/H
    SetError()
    {
        loop
        {
            PHA
            LDA ZP.ACCL
            STA ZP.LastErrorL
            LDA ZP.ACCH
            STA ZP.LastErrorH
            LDA #State.Failure
            STA ZP.SystemState
            PLA
            break;
        }
    }
    
    // Clear all state for fresh operation
    Reset()
    {
        loop
        {
            STZ ZP.LastErrorL
            STZ ZP.LastErrorH
            LDA #State.Success
            STA ZP.SystemState
            break;
        }
    }
    
    // Check if we should continue processing
    // Output: C if should continue, NC if should stop
    ShouldContinue()
    {
        loop
        {
            PHA
            LDA ZP.SystemState
            CMP #State.Failure
            if (Z) { CLC } else { SEC }
            PLA
            break;
        }
    }
    
    // Check for break and set appropriate state
    // Output: C if break detected, NC if not
    CheckBreak()
    {
        loop
        {
            BIT ZP.SerialBreakFlag
            if (MI)
            {
                // Break detected (bit 7 set)
                LDA #State.Exiting
                STA ZP.SystemState
                SEC
            }
            else
            {
                CLC
            }
            break;
        }
    }
    
    // Helper predicates
    IsFailure()  { LDA ZP.SystemState; CMP #State.Failure; }
    IsSuccess()  { LDA ZP.SystemState; CMP #State.Success; }
    IsExiting()  { LDA ZP.SystemState; CMP #State.Exiting; }
    IsReturn()   { LDA ZP.SystemState; CMP #State.Return; }
    
    SetSuccess() { LDA #State.Success; STA ZP.SystemState; }
    SetExiting() { LDA #State.Exiting; STA ZP.SystemState; }
    SetReturn()  { LDA #State.Return;  STA ZP.SystemState; }
}
```

### 2. Buffer Management (BufferManager.asm - new unit)

Direct buffer save/restore solution to buffer munting problem:

```asm
unit BufferManager
{
    // Buffer save areas (in available RAM)
    const uint savedTokenBuffer = 0x1000;  // 512 bytes at 0x1000-0x11FF
    const uint savedOpcodeBuffer = 0x1200; // 512 bytes at 0x1200-0x13FF
    
    // Save state storage
    uint savedTokenBufferLengthL
    uint savedTokenBufferLengthH
    uint savedTokenizerPosL
    uint savedTokenizerPosH
    uint savedOpcodeBufferLengthL
    uint savedOpcodeBufferLengthH
    uint savedCurrentToken
    
    // Save current compilation context
    // Input: None
    // Output: Token and opcode buffers saved
    // Modifies: Save areas, saved state variables
    SaveContext()
    {
        loop
        {
            // Save lengths and positions
            LDA ZP.TokenBufferLengthL
            STA savedTokenBufferLengthL
            LDA ZP.TokenBufferLengthH
            STA savedTokenBufferLengthH
            LDA ZP.TokenizerPosL
            STA savedTokenizerPosL
            LDA ZP.TokenizerPosH
            STA savedTokenizerPosH
            LDA ZP.OpCodeBufferLengthL
            STA savedOpcodeBufferLengthL
            LDA ZP.OpCodeBufferLengthH
            STA savedOpcodeBufferLengthH
            LDA ZP.CurrentToken
            STA savedCurrentToken
            
            // Copy buffers (512 bytes each)
            // Using page-by-page copy for efficiency
            LDY #0
            LDX #0
            copyLoop:
            {
                LDA Address.BasicTokenizerBuffer, X
                STA savedTokenBuffer, X
                LDA Address.BasicOpCodeBuffer, X
                STA savedOpcodeBuffer, X
                INX
                if (NZ) { JMP copyLoop; }
                INY
                CPY #2  // 512 bytes = 2 pages
                if (NZ) { JMP copyLoop; }
            }
            break;
        }
    }
    
    // Restore previous compilation context
    RestoreContext()
    {
        loop
        {
            // Restore buffers
            LDY #0
            LDX #0
            restoreLoop:
            {
                LDA savedTokenBuffer, X
                STA Address.BasicTokenizerBuffer, X
                LDA savedOpcodeBuffer, X
                STA Address.BasicOpCodeBuffer, X
                INX
                if (NZ) { JMP restoreLoop; }
                INY
                CPY #2
                if (NZ) { JMP restoreLoop; }
            }
            
            // Restore state
            LDA savedTokenBufferLengthL
            STA ZP.TokenBufferLengthL
            LDA savedTokenBufferLengthH
            STA ZP.TokenBufferLengthH
            LDA savedTokenizerPosL
            STA ZP.TokenizerPosL
            LDA savedTokenizerPosH
            STA ZP.TokenizerPosH
            LDA savedOpcodeBufferLengthL
            STA ZP.OpCodeBufferLengthL
            LDA savedOpcodeBufferLengthH
            STA ZP.OpCodeBufferLengthH
            LDA savedCurrentToken
            STA ZP.CurrentToken
            break;
        }
    }
}
```

### 3. Unified Execution Layer (Execute.asm)

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
    
    // Single unified execution entry point with buffer management
    // Input: ZP.ACCL = ExecutionContext
    //        ZP.IDX = function node (for Function/Program contexts)
    //        Tokens in BasicTokenizerBuffer (for Statement/Expression contexts)
    // Output: SystemState set (Success/Failure/Return/Exiting)
    //         Result on stack if applicable
    //         LastError set if Failure
    Run()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            LDA ZP.ACCL
            STA executeContext
            
            States.Reset();
            
            // Save context for nested compilation (functions only)
            LDA executeContext
            CMP #ExecutionContext.Function
            if (Z)
            {
                BufferManager.SaveContext();
            }
            
            // Check for break before compilation
            States.CheckBreak();
            if (C) { JMP runCleanup; }
            
            compile();
            States.IsFailure();
            if (C) { JMP runCleanup; }
            
            // Check for break after compilation
            States.CheckBreak();
            if (C) { JMP runCleanup; }
            
            execute();
            States.IsFailure();
            if (C) { JMP runCleanup; }
            
            handleResult();
            
        runCleanup:
            // Restore context if saved
            LDA executeContext
            CMP #ExecutionContext.Function
            if (Z)
            {
                BufferManager.RestoreContext();
            }
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Internal compilation phase
    compile()
    {
        loop
        {
            Compiler.InitOpCodeBuffer();
            States.IsFailure();
            if (C) { break; }
            
            LDA executeContext
            switch (A)
            {
                case ExecutionContext.Program:
                case ExecutionContext.Function:
                {
                    // Compile function from node in ZP.IDX
                    Compiler.CompileFunctionNode();
                }
                case ExecutionContext.Expression:
                {
                    // Reset tokenizer and compile expression
                    STZ ZP.TokenizerPosL
                    STZ ZP.TokenizerPosH
                    Tokenizer.NextToken();
                    States.IsFailure();
                    if (C) { break; }
                    Compiler.CompileExpression();
                }
                default:
                {
                    // Compile statements with break checking
                    STZ ZP.TokenizerPosL
                    STZ ZP.TokenizerPosH
                    Tokenizer.NextToken();
                    States.IsFailure();
                    if (C) { break; }
                    Compiler.CompileStatements();
                }
            }
            break;
        }
    }
    
    // Internal execution phase
    execute()
    {
        loop
        {
            Executor.ExecuteOpCodes();
            break;
        }
    }
    
    // Internal result handling
    handleResult()
    {
        loop
        {
            States.IsFailure();
            if (C) { break; }
            
            LDA executeContext
            switch (A)
            {
                case ExecutionContext.Program:
                {
                    States.IsReturn();
                    if (C)
                    {
                        Value.Pop(); // Discard any return value
                    }
                    States.SetSuccess();
                }
                case ExecutionContext.Function:
                {
                    States.IsReturn();
                    if (NC)
                    {
                        Value.PushVoid();
                    }
                    States.SetSuccess();
                }
                case ExecutionContext.Statement:
                {
                    // Leave result on stack if expression
                    States.SetSuccess();
                }
                case ExecutionContext.Expression:
                {
                    // Value already on stack
                    States.SetSuccess();
                }
            }
            break;
        }
    }
    
    uint executeContext
}
```

### 4. Command Dispatch System (CommandDispatch.asm)

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
        loop
        {
            TAX
            LDA tokenCategories, X
            break;
        }
    }
    
    // Optimized dispatch using Hopper Assembly jump table
    // Input: X = token, Y = allowed flags
    // Output: Calls appropriate handler, sets SystemState
    Dispatch()
    {
        loop
        {
            STY allowedFlags
            
            // Get token flags
            LDA tokenCategories, X
            AND allowedFlags
            if (Z)
            {
                LDA #(Messages.TokenNotAllowed % 256)
                STA ZP.ACCL
                LDA #(Messages.TokenNotAllowed / 256)
                STA ZP.ACCH
                States.SetError();
                break;
            }
            
            // X contains token - perfect for jump table optimization
            switch (X)  // Using X for Hopper Assembly optimization
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
                
                // Statement commands - all go through unified execution
                case Tokens.PRINT:
                case Tokens.IF:
                case Tokens.WHILE:
                case Tokens.RETURN:
                case Tokens.END:
                case Tokens.INT:
                case Tokens.WORD:
                case Tokens.BIT:
                case Tokens.BYTE:
                case Tokens.STRING:
                case Tokens.CONST:
                case Tokens.FUNC:
                case Tokens.BEGIN:
                case Tokens.IDENTIFIER:
                {
                    executeStatement();
                }
                
                default:
                {
                    LDA #(Messages.UnknownToken % 256)
                    STA ZP.ACCL
                    LDA #(Messages.UnknownToken / 256)
                    STA ZP.ACCH
                    States.SetError();
                }
            }
            break;
        }
    }
    
    // Execute statement through unified path
    executeStatement()
    {
        loop
        {
            // Execute through unified path
            LDA #ExecutionContext.Statement
            STA ZP.ACCL
            Execute.Run();
            break;
        }
    }
    
    uint allowedFlags
}
```

### 5. Break Handling Strategy

Strategic break check points throughout the system:

#### In Executor.asm - main dispatch loop
```asm
executeOpcodesLoop:
{
    // Check for break at top of each opcode
    BIT ZP.SerialBreakFlag
    if (MI)
    {
        LDA #State.Exiting
        STA ZP.SystemState
        break;
    }
    
    // Fetch and execute opcode...
}
```

#### In Compiler.asm - between statements
```asm
compileStatements()
{
    loop
    {
        // Check for break between statements
        States.CheckBreak();
        if (C) { break; }
        
        // Compile next statement...
        compileStatement();
        
        // Check for colon separator
        LDA ZP.CurrentToken
        CMP #Tokens.COLON
        if (Z)
        {
            Tokenizer.NextToken();
            JMP compileStatements; // Continue with next statement
        }
        break;
    }
}
```

#### In Console.asm - input processing
```asm
processInput()
{
    loop
    {
        // Check for break before processing
        BIT ZP.SerialBreakFlag
        if (MI)
        {
            // Clear break flag
            STZ ZP.SerialBreakFlag
            
            // Exit capture mode if active
            Statement.IsCaptureModeOn();
            if (C)
            {
                Console.ExitFunctionCaptureMode();
            }
            
            // Print message and reset
            Messages.PrintBreak();
            States.SetSuccess();
            break;
        }
        
        // Tokenize input
        Tokenizer.TokenizeLine();
        States.IsFailure();
        if (C) 
        { 
            Error.CheckAndPrint();
            break;
        }
        
        // Dispatch based on first token
        LDX ZP.CurrentToken
        LDY #(TokenFlags.ConsoleCommand | TokenFlags.Statement | TokenFlags.Declaration)
        CommandDispatch.Dispatch();
        
        // Handle execution result
        States.IsFailure();
        if (C)
        {
            Error.CheckAndPrint();
            break;
        }
        
        States.IsExiting();
        if (C) { break; }
        
        // Check if there's a result to print
        Value.HasValue();
        if (C)
        {
            Messages.PrintStackTop();
        }
        else
        {
            Messages.PrintOK();
        }
        break;
    }
}
```



## Statement.asm Disposition

After refactoring, **90% of Statement.asm becomes dead code**. The unit essentially disappears as its core functionality is replaced by the new unified architecture.

### Replaced/Removed:
- `Execute()` method → Replaced by CommandDispatch.Dispatch()
- `ExecuteStatement()` → Replaced by Execute.Run(ExecutionContext.Statement)
- `EvaluateExpression()` → Replaced by Execute.Run(ExecutionContext.Expression)
- All execute*Declaration() methods → Consolidated through unified execution
- Buffer management code → Handled by BufferManager
- Complex state tracking → Unified through SystemState

### Retained (but relocated):
- `ResolveIdentifier()` → Move to Compiler.asm (its only client)
- Capture mode management → Move to Console.asm (console-specific state)
- Constant/variable helpers → Move to relevant units or delete if unused

### Final Disposition:
**Delete Statement.asm entirely**. Its surviving functionality should be moved to more appropriate units:
- Identifier resolution belongs in the Compiler
- Capture mode belongs in the Console
- The unit name "Statement" no longer reflects its minimal remaining purpose

This elimination of an entire unit demonstrates the effectiveness of the refactoring.

## Implementation Strategy

### Phase 1: Core Infrastructure (Day 1)
1. Enhance States.asm with new methods
2. Create BufferManager.asm
3. Create Execute.asm with unified execution
4. Create CommandDispatch.asm with token tables
5. Create Value.asm helpers

### Phase 2: Integration (Day 2)
1. Modify Console.asm to use new dispatch
2. Update all command handlers to use SystemState
3. Add break checking at strategic points
4. Update Compiler/Executor for buffer management
5. Test basic functionality

### Phase 3: Cleanup (Day 3)
1. Remove Statement.asm entirely
2. Move ResolveIdentifier() to Compiler.asm
3. Move capture mode to Console.asm
4. Remove duplicate switch statements
5. Full testing suite

### Code Generation Requirements
- All methods use single-exit pattern (loop/break)
- No `return` statements
- All major methods include TRACE blocks (when defined)
- Method headers document inputs/outputs/modifies
- Use SystemState for all inter-system communication
- C/NC only for leaf-node APIs
- Check for break (Ctrl+C) at strategic points

## Manual Testing Scenarios

### Basic Console Commands
1. `NEW` - Should clear everything
2. `LIST` - Should show empty program
3. `VARS` - Should show no variables
4. `MEM` - Should show memory usage
5. `BYE` - Should exit cleanly

### Variable Operations
1. `INT X = 10` - Create with initializer
2. `INT Y` - Create without initializer
3. `X = X + 5` - Assignment with expression
4. `PRINT X` - Should print 15
5. `INT A = 10 : INT B = 20` - Colon-separated declarations

### Expression Evaluation
1. `PRINT 2 + 3 * 4` - Should print 14 (precedence)
2. `PRINT (2 + 3) * 4` - Should print 20 (parentheses)
3. `X + Y` - Expression as statement (result printed)

### Function Definition and Calls
1. Define function:
   ```
   FUNC ADD(A, B)
   RETURN A + B
   ENDFUNC
   ```
2. `PRINT ADD(10, 20)` - Should print 30
3. `X = ADD(5, 7)` - Function in assignment

### Nested Function Calls
1. Define recursive function:
   ```
   FUNC FACT(N)
   IF N <= 1 THEN RETURN 1 ENDIF
   RETURN N * FACT(N-1)
   ENDFUNC
   ```
2. `PRINT FACT(5)` - Should print 120

### Main Program
1. Define main:
   ```
   BEGIN
   PRINT "HELLO"
   END
   ```
2. `RUN` - Should print HELLO

### Complex Colon-Separated Statements
1. `INT X = 10 : PRINT X : X = X + 1 : PRINT X` - Should print 10 then 11
2. `FUNC F() RETURN 42 ENDFUNC : PRINT F()` - Define and call in one line

### Error Cases
1. `PRINT UNDEFINED` - Should show undefined identifier error
2. `INT X = "STRING"` - Type mismatch error
3. `1 / 0` - Division by zero error
4. Ctrl+C during function definition - Should exit capture mode cleanly

### Control Flow
1. IF/THEN/ELSE with expressions
2. WHILE loops with function calls
3. RETURN statements in functions

### Break Handling
1. Ctrl+C during program execution - Should stop cleanly
2. Ctrl+C during WHILE loop - Should exit loop
3. Ctrl+C during function definition - Should exit capture mode
4. Ctrl+C during compilation - Should abort compilation

## Success Criteria

1. **Single Execution Path**: All code compilation/execution goes through Execute.Run()
2. **Consistent State Management**: SystemState used everywhere, C/NC only in leaf nodes
3. **No Duplicate Switches**: Each token handled in exactly one place
4. **Buffer Save/Restore**: Nested compilation preserves outer context without function overhead
5. **No Buffer Munting**: Problem solved with ~1KB RAM overhead
6. **Clear Error Handling**: Every error sets both State.Failure and LastError
7. **Responsive Break Handling**: Ctrl+C works reliably at any point
8. **All Test Scenarios Pass**: Manual testing covers all code paths

## Expected Benefits

1. **Maintainability**: 40% less code through deduplication, entire Statement.asm unit eliminated
2. **Clarity**: Single clear execution model with explicit buffer management
3. **Performance**: Faster dispatch through jump tables, direct buffer save/restore
4. **Extensibility**: Easy to add new commands/contexts
5. **Reliability**: Consistent state management reduces bugs
6. **Correctness**: Buffer munting problem solved permanently
7. **Responsiveness**: Proper break handling throughout

## Risk Mitigation

### RAM Usage
- 1KB for buffer save areas at 0x1000-0x13FF
- Acceptable given we have 32KB+ RAM
- Could reduce to 512 bytes if needed (save only used portions)

### Performance Impact
- Buffer copy: ~2000 cycles (negligible vs. current inefficiencies)
- State checks: <10 cycles per check
- Net positive due to eliminated redundant compilation

### Compatibility
- All existing BASIC programs continue working
- No changes to user-visible behavior
- Error messages remain the same

### Testing Coverage
- Manual test suite covers all paths
- Each phase tested before proceeding
- Rollback plan: Keep Statement.asm until proven

## Conclusion

This refactoring transforms HopperBASIC from a system with multiple ad-hoc execution paths into a clean, unified architecture that solves the fundamental buffer munting problem while being easier to understand, maintain, and extend. The investment of ~1KB RAM for buffer save areas is minimal compared to the benefits of correct operation and reduced code complexity.