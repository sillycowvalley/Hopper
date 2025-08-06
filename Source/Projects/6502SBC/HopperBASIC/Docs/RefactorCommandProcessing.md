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
6. **Embrace $REPL Function**: Use it for ALL execution paths to solve buffer munting
   - Keep $REPL function alive and reuse it
   - Update its token stream for each REPL line
   - Ensures nested compilation (function calls) doesn't corrupt REPL state

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
    // Input: ZP.ACCL/H = error message pointer
    // Output: SystemState set to Failure, LastError set
    // Modifies: ZP.SystemState, ZP.LastErrorL/H
    const string setErrorTrace = "SetErr";
    SetError()
    {
#ifdef TRACE
        PHA
        LDA #(setErrorTrace % 256) STA ZP.TraceMessageL 
        LDA #(setErrorTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            PHA
            LDA ZP.ACCL
            STA ZP.LastErrorL
            LDA ZP.ACCH
            STA ZP.LastErrorH
            LDA #SystemState.Failure
            STA ZP.SystemState
            PLA
            break;
        }
#ifdef TRACE
        PHA
        LDA #(setErrorTrace % 256) STA ZP.TraceMessageL 
        LDA #(setErrorTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
    
    // Clear all state for fresh operation
    // Input: None
    // Output: SystemState set to Success, LastError cleared
    // Modifies: ZP.SystemState, ZP.LastErrorL/H
    const string resetTrace = "Reset";
    Reset()
    {
#ifdef TRACE
        PHA
        LDA #(resetTrace % 256) STA ZP.TraceMessageL 
        LDA #(resetTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            STZ ZP.LastErrorL
            STZ ZP.LastErrorH
            LDA #SystemState.Success
            STA ZP.SystemState
            break;
        }
#ifdef TRACE
        PHA
        LDA #(resetTrace % 256) STA ZP.TraceMessageL 
        LDA #(resetTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
    
    // Check if we should continue processing
    // Input: None
    // Output: C if should continue, NC if should stop
    // Modifies: Flags only
    const string shouldContinueTrace = "ShouldCont";
    ShouldContinue()
    {
#ifdef TRACE
        PHA
        LDA #(shouldContinueTrace % 256) STA ZP.TraceMessageL 
        LDA #(shouldContinueTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            PHA
            LDA ZP.SystemState
            CMP #SystemState.Failure
            if (Z) { CLC } else { SEC }
            PLA
            break;
        }
#ifdef TRACE
        PHA
        LDA #(shouldContinueTrace % 256) STA ZP.TraceMessageL 
        LDA #(shouldContinueTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
}
```

### 2. REPL Function Management (ReplFunction.asm - new unit)

```asm
unit ReplFunction
{
    // Storage for $REPL function node address
    uint replFunctionNode
    
    // Initialize $REPL function (called once at startup)
    // Input: None
    // Output: $REPL function created in function table
    // Modifies: Function table, memory allocation
    const string initializeTrace = "InitREPL";
    Initialize()
    {
#ifdef TRACE
        LDA #(initializeTrace % 256) STA ZP.TraceMessageL 
        LDA #(initializeTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            // Set up $REPL function name
            LDA #(Messages.ReplFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.ReplFunctionName / 256)
            STA ZP.TOPH
            
            // No arguments
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            // Empty initial body
            STZ ZP.IDYL
            STZ ZP.IDYH
            
            // Declare the function
            Functions.Declare();
            State.IsFailure();
            if (C) { break; }
            
            // Save function node for reuse
            LDA ZP.IDXL
            STA replFunctionNode
            LDA ZP.IDXH
            STA (replFunctionNode + 1)
            
            State.SetSuccess();
            break;
        }
#ifdef TRACE
        LDA #(initializeTrace % 256) STA ZP.TraceMessageL 
        LDA #(initializeTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
    
    // Update $REPL function with new token stream
    // Input: BasicTokenizerBuffer contains new tokens
    //        ZP.TokenBufferLength set
    // Output: $REPL function body updated
    // Modifies: Function node token stream
    const string updateTrace = "UpdateREPL";
    UpdateTokens()
    {
#ifdef TRACE
        LDA #(updateTrace % 256) STA ZP.TraceMessageL 
        LDA #(updateTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            // Load $REPL function node
            LDA replFunctionNode
            STA ZP.IDXL
            LDA (replFunctionNode + 1)
            STA ZP.IDXH
            
            // Update its token stream
            Functions.UpdateTokenStream(); // New method needed
            State.IsFailure();
            if (C) { break; }
            
            State.SetSuccess();
            break;
        }
#ifdef TRACE
        LDA #(updateTrace % 256) STA ZP.TraceMessageL 
        LDA #(updateTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
    
    // Execute the $REPL function
    // Input: $REPL function updated with current tokens
    // Output: Function executed, result may be on stack
    // Modifies: Execution state, stack
    const string executeTrace = "ExecREPL";
    Execute()
    {
#ifdef TRACE
        LDA #(executeTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            // Load $REPL function node
            LDA replFunctionNode
            STA ZP.IDXL
            LDA (replFunctionNode + 1)
            STA ZP.IDXH
            
            // Execute it through unified path
            LDA #ExecutionContext.Function
            STA ZP.ACCL
            Execute.Run();
            
            break;
        }
#ifdef TRACE
        LDA #(executeTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
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
    
    // Single unified execution entry point
    // Input: ZP.ACCL = ExecutionContext
    //        ZP.IDX = function node (for Function/Program contexts)
    //        Tokens in BasicTokenizerBuffer (for Statement/Expression contexts)
    // Output: SystemState set (Success/Failure/Return/Exiting)
    //         Result on stack if applicable
    //         LastError set if Failure
    const string runTrace = "ExecRun";
    Run()
    {
#ifdef TRACE
        LDA #(runTrace % 256) STA ZP.TraceMessageL 
        LDA #(runTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        PHA
        PHX
        PHY
        
        loop
        {
            LDA ZP.ACCL
            STA executeContext
            
            State.Reset();
            
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
#ifdef TRACE
        LDA #(runTrace % 256) STA ZP.TraceMessageL 
        LDA #(runTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
    
    // Internal compilation phase
    const string compileTrace = "Compile";
    compile()
    {
#ifdef TRACE
        LDA #(compileTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            Compiler.InitOpCodeBuffer();
            State.IsFailure();
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
                    State.IsFailure();
                    if (C) { break; }
                    Compiler.CompileExpression();
                }
                default:
                {
                    // Compile statements
                    STZ ZP.TokenizerPosL
                    STZ ZP.TokenizerPosH
                    Tokenizer.NextToken();
                    State.IsFailure();
                    if (C) { break; }
                    Compiler.CompileStatements();
                }
            }
            break;
        }
#ifdef TRACE
        LDA #(compileTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
    
    // Internal execution phase
    const string executeTrace = "Execute";
    execute()
    {
#ifdef TRACE
        LDA #(executeTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            Executor.ExecuteOpCodes();
            break;
        }
#ifdef TRACE
        LDA #(executeTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
    
    // Internal result handling
    const string handleResultTrace = "HandleRes";
    handleResult()
    {
#ifdef TRACE
        LDA #(handleResultTrace % 256) STA ZP.TraceMessageL 
        LDA #(handleResultTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            State.IsFailure();
            if (C) { break; }
            
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
            break;
        }
#ifdef TRACE
        LDA #(handleResultTrace % 256) STA ZP.TraceMessageL 
        LDA #(handleResultTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
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
    // Modifies: A, X
    const string getTokenFlagsTrace = "GetTokFlg";
    GetTokenFlags()
    {
#ifdef TRACE
        PHA
        LDA #(getTokenFlagsTrace % 256) STA ZP.TraceMessageL 
        LDA #(getTokenFlagsTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            TAX
            LDA tokenCategories, X
            break;
        }
#ifdef TRACE
        PHA
        LDA #(getTokenFlagsTrace % 256) STA ZP.TraceMessageL 
        LDA #(getTokenFlagsTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
    
    // Optimized dispatch using Hopper Assembly jump table
    // Input: X = token, Y = allowed flags
    // Output: Calls appropriate handler, sets SystemState
    // Modifies: SystemState, LastError if error
    const string dispatchTrace = "Dispatch";
    Dispatch()
    {
#ifdef TRACE
        PHA
        LDA #(dispatchTrace % 256) STA ZP.TraceMessageL 
        LDA #(dispatchTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
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
                State.SetError();
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
                
                // Statement commands - all go through $REPL
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
                    executeViaRepl();
                }
                
                default:
                {
                    LDA #(Messages.UnknownToken % 256)
                    STA ZP.ACCL
                    LDA #(Messages.UnknownToken / 256)
                    STA ZP.ACCH
                    State.SetError();
                }
            }
            break;
        }
#ifdef TRACE
        PHA
        LDA #(dispatchTrace % 256) STA ZP.TraceMessageL 
        LDA #(dispatchTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
    
    // Execute statement/declaration via $REPL function
    const string executeViaReplTrace = "ExecViaREPL";
    executeViaRepl()
    {
#ifdef TRACE
        LDA #(executeViaReplTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeViaReplTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
#endif
        loop
        {
            // Update $REPL with current token buffer
            ReplFunction.UpdateTokens();
            State.IsFailure();
            if (C) { break; }
            
            // Execute through $REPL
            ReplFunction.Execute();
            break;
        }
#ifdef TRACE
        LDA #(executeViaReplTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeViaReplTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
    
    uint allowedFlags
}
```

### 5. Simplified Console Layer (Console.asm modifications)

```asm
// Initialize console (called at startup)
const string initConsoleTrace = "InitCon";
InitializeConsole()
{
#ifdef TRACE
    LDA #(initConsoleTrace % 256) STA ZP.TraceMessageL 
    LDA #(initConsoleTrace / 256) STA ZP.TraceMessageH 
    Trace.MethodEntry();
#endif
    loop
    {
        // Initialize $REPL function once
        ReplFunction.Initialize();
        State.IsFailure();
        if (C) { break; }
        
        // Other initialization...
        break;
    }
#ifdef TRACE
    LDA #(initConsoleTrace % 256) STA ZP.TraceMessageL 
    LDA #(initConsoleTrace / 256) STA ZP.TraceMessageH 
    Trace.MethodExit();
#endif
}

// New simplified console input processing
const string processInputTrace = "ProcInput";
processInput()
{
#ifdef TRACE
    LDA #(processInputTrace % 256) STA ZP.TraceMessageL 
    LDA #(processInputTrace / 256) STA ZP.TraceMessageH 
    Trace.MethodEntry();
#endif
    loop
    {
        Tokenizer.TokenizeLine();
        State.IsFailure();
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
        State.IsFailure();
        if (C)
        {
            Error.CheckAndPrint();
            break;
        }
        
        State.IsExiting();
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
#ifdef TRACE
    LDA #(processInputTrace % 256) STA ZP.TraceMessageL 
    LDA #(processInputTrace / 256) STA ZP.TraceMessageH 
    Trace.MethodExit();
#endif
}
```

### 6. Value Stack Helper (Value.asm - new unit)

```asm
unit Value
{
    // Check if value stack has a value
    // Input: None
    // Output: C if value exists, NC if empty
    // Modifies: Flags only
    const string hasValueTrace = "HasVal";
    HasValue()
    {
#ifdef TRACE
        PHA
        LDA #(hasValueTrace % 256) STA ZP.TraceMessageL 
        LDA #(hasValueTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            LDA ZP.VSP
            if (Z) { CLC } else { SEC }
            break;
        }
#ifdef TRACE
        PHA
        LDA #(hasValueTrace % 256) STA ZP.TraceMessageL 
        LDA #(hasValueTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
    
    // Push void value
    // Input: None
    // Output: Void value pushed to stack
    // Modifies: Stack, ZP.ACCL/H
    const string pushVoidTrace = "PushVoid";
    PushVoid()
    {
#ifdef TRACE
        PHA
        LDA #(pushVoidTrace % 256) STA ZP.TraceMessageL 
        LDA #(pushVoidTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            LDA #Types.Void
            PHA
            STZ ZP.ACCL
            STZ ZP.ACCH
            Stacks.PushValue();
            break;
        }
#ifdef TRACE
        PHA
        LDA #(pushVoidTrace % 256) STA ZP.TraceMessageL 
        LDA #(pushVoidTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
    
    // Pop and discard top value
    // Input: Stack has at least one value
    // Output: Top value removed
    // Modifies: Stack
    const string popTrace = "Pop";
    Pop()
    {
#ifdef TRACE
        PHA
        LDA #(popTrace % 256) STA ZP.TraceMessageL 
        LDA #(popTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
        PLA
#endif
        loop
        {
            Stacks.PopValue();
            break;
        }
#ifdef TRACE
        PHA
        LDA #(popTrace % 256) STA ZP.TraceMessageL 
        LDA #(popTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
        PLA
#endif
    }
}
```

## Statement.asm Disposition

### What Happens to Statement.asm

After refactoring, **90% of Statement.asm becomes dead code**. The unit essentially disappears as its core functionality is replaced by the new unified architecture.

#### Replaced/Removed:
- `Execute()` method → Replaced by CommandDispatch.Dispatch()
- `ExecuteStatement()` → Replaced by Execute.Run(ExecutionContext.Statement)
- `EvaluateExpression()` → Replaced by Execute.Run(ExecutionContext.Expression)
- All execute*Declaration() methods → Consolidated through $REPL function
- Buffer management code → Handled by $REPL function isolation
- Complex state tracking → Unified through SystemState

#### Retained (but relocated):
- `ResolveIdentifier()` → Move to Compiler.asm (its only client)
- Capture mode management → Move to Console.asm (console-specific state)
- Constant/variable helpers → Move to relevant units or delete if unused

#### Final Disposition:
**Delete Statement.asm entirely**. Its surviving functionality should be moved to more appropriate units:
- Identifier resolution belongs in the Compiler
- Capture mode belongs in the Console
- The unit name "Statement" no longer reflects its minimal remaining purpose

This is a positive outcome - eliminating an entire unit that was the source of execution path complexity demonstrates the effectiveness of the refactoring.

## Implementation Strategy

### Single-Step Implementation
1. Create all new units (State.asm enhancements, ReplFunction.asm, Execute.asm, CommandDispatch.asm, Value.asm)
2. Modify Console.asm to use new architecture
3. Update all existing command handlers to use SystemState
4. Remove old execution paths
5. Test manually using scenarios below

### Code Generation Requirements
- All methods use single-exit pattern (loop/break)
- No `return` statements
- All major methods include TRACE blocks
- Method headers document inputs/outputs/modifies
- Use SystemState for all inter-system communication
- C/NC only for leaf-node APIs

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

## Success Criteria

1. **Single Execution Path**: All code compilation/execution goes through Execute.Run()
2. **Consistent State Management**: SystemState used everywhere, C/NC only in leaf nodes
3. **No Duplicate Switches**: Each token handled in exactly one place
4. **$REPL Function Reuse**: Single $REPL function updated and reused for all REPL operations
5. **No Buffer Munting**: Nested compilation preserves outer context
6. **Clear Error Handling**: Every error sets both SystemState.Failure and LastError
7. **All Test Scenarios Pass**: Manual testing covers all code paths

## Expected Benefits

1. **Maintainability**: 40% less code through deduplication
2. **Clarity**: Single clear execution model
3. **Performance**: Faster dispatch through jump tables, reused $REPL function
4. **Extensibility**: Easy to add new commands/contexts
5. **Reliability**: Consistent state management reduces bugs
6. **Correctness**: Buffer munting problem solved permanently

This refactoring will transform HopperBASIC from a system with multiple ad-hoc execution paths into a clean, unified architecture that solves the fundamental buffer munting problem while being easier to understand, maintain, and extend.