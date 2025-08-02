# HopperBASIC Debug & Error Handling Refactoring Specification

## Overview
Complete refactoring of debug output and error handling systems in HopperBASIC to support:
- Enhanced method call tracing for REPL/Function unification debugging
- Simplified error handling with one-liner methods
- Robust state management system replacing fragile C|NC flags
- Modular debug capabilities with selective compilation
- Human-readable structure dumps and literate output

## Debug Symbols
- `DEBUG` - General debug output and structure dumps
- `TRACE` - Method entry/exit tracing
- `TERSE_ERRORS` - Error numbers instead of strings (future)

## Architecture Refactoring

### Four-Unit Split: Tools.asm → Tools.asm + Debug.asm + Trace.asm + Error.asm + State.asm

**Tools.asm** (Production utilities only)
- String operations (`StringLength`, `StringCompare`, `CopyBytes`)
- Type operations (`PrintType`, `PrintVariableValue`, `PrintDecimalWord`)
- Basic I/O helpers (`NL`, `COut`)

**Debug.asm** (Structure dumps and literate output)
- Stream rendering (`RenderTokenStream`, `RenderOpcodeStream`)
- Structure dumps (`DumpVariables`, `DumpStack`, `DumpHeap`, etc.)
- Register dumps (`XOut`, `YOut`, `TOut`, `NOut`, etc.)
- Literate rendering (`RenderFunction`, `RenderVariable`)
- Context-aware dumps (`DumpSymbolTable`, `DumpFunctionTable`)

**Trace.asm** (Method call tracing)
- Method tracing (`MethodEntry`, `MethodExit`, `PrintIndent`)
- Call hierarchy management
- Convergence point tracking (`MarkConvergence`)

**Error.asm** (Error handling and messages)
- One-liner error methods (`SyntaxError()`, `TypeMismatch()`, etc.)
- Error message strings (moved from Messages.asm)
- Future: Terse mode compilation support

**State.asm** (System state management)
- Universal state management replacing fragile C|NC flags
- State helper methods (`SetSuccess()`, `SetFailure()`, `IsExiting()`, etc.)
- Robust state propagation for complex flows

## Error Handling Simplification ✅ **COMPLETE**

### Before (verbose pattern):
```hopper
LDA #(Messages.SyntaxError % 256)
STA ZP.LastErrorL
LDA #(Messages.SyntaxError / 256)
STA ZP.LastErrorH
BIT ZP.EmulatorPCL
CLC
```

### After (simplified pattern):
```hopper
Error.SyntaxError(); BIT ZP.EmulatorPCL
```

### Error Unit API ✅ **IMPLEMENTED**
```hopper
unit Error
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    // Error message strings (moved from Messages.asm)
    #ifdef TERSE_ERRORS
    const string syntaxError = "E001";
    const string typeMismatch = "E002";
    const string notImplemented = "E003";
    // ... (future enhancement)
    #else
    const string syntaxError = "SYNTAX ERROR";
    const string typeMismatch = "TYPE MISMATCH";
    const string notImplemented = "NOT IMPLEMENTED";
    const string internalError = "INTERNAL ERROR";
    const string functionExists = "FUNCTION EXISTS";
    const string constantExists = "CONSTANT EXISTS";
    const string variableExists = "VARIABLE EXISTS";
    const string outOfMemory = "OUT OF MEMORY";
    const string divisionByZero = "DIVISION BY ZERO";
    const string numericOverflow = "NUMERIC OVERFLOW";
    const string undefinedIdentifier = "UNDEFINED IDENTIFIER";
    const string illegalAssignment = "ILLEGAL ASSIGNMENT";
    const string invalidOperator = "INVALID OPERATOR";
    const string bufferOverflow = "BUFFER OVERFLOW";
    const string expectedRightParen = ") EXPECTED";
    const string expectedLeftParen = "( EXPECTED";
    const string expectedQuote = "QUOTE EXPECTED";
    const string expectedExpression = "EXPRESSION EXPECTED";
    const string invalidBitValue = "INVALID BIT VALUE";
    const string constantExpressionExpected = "CONSTANT EXPRESSION EXPECTED";
    const string illegalIdentifier = "ILLEGAL IDENTIFIER";
    #endif
    
    // One-liner error methods (PC must be set at call site)
    SyntaxError() 
    { 
        LDA #(syntaxError % 256)
        STA ZP.LastErrorL
        LDA #(syntaxError / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    TypeMismatch() 
    { 
        LDA #(typeMismatch % 256)
        STA ZP.LastErrorL
        LDA #(typeMismatch / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    // ... (additional error methods follow same pattern)
}
```

## State Management System

### New Zero Page Allocation
```hopper
// Added to ZeroPage.asm  
#if defined(HOPPER_BASIC)
const byte SystemState          = 0x4A; // Universal system state (1 byte)
const byte TraceIndent          = 0x0F; // used by Trace.asm
const byte TraceMessageL        = 0xF2; // used by Trace.asm
const byte TraceMessageH        = 0xF3;
#endif
```

### State System Enum
```hopper
enum SystemState 
{
    Failure = 0,        // Zero for easy testing (CMP -> Z flag)
    Success = 1,        // Normal completion
    Exiting = 2         // User exit request (BYE, Ctrl+C)
}
```

### State Unit API
```hopper
unit State
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    enum SystemState 
    {
        Failure = 0,        // Zero for easy testing
        Success = 1,        // Normal completion  
        Exiting = 2         // User exit request (BYE, Ctrl+C)
    }
    
    // Set system state
    // Input: A = SystemState value
    // Output: ZP.SystemState updated, flags set for compatibility
    // Modifies: ZP.SystemState, processor flags
    SetState()
    {
        STA ZP.SystemState
        CMP #SystemState.Failure
        if (Z) { CLC } else { SEC }  // Maintain C|NC compatibility
    }
    
    // Get current system state  
    // Output: A = current SystemState, flags set for compatibility
    // Modifies: A, processor flags
    GetState()
    {
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z) { CLC } else { SEC }  // Set flags for existing code
    }
    
    // Test for specific states (preserves A register)
    IsFailure()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure  // Sets Z if failure
        PLA
    }
    
    IsSuccess()
    {
        PHA  
        LDA ZP.SystemState
        CMP #SystemState.Success  // Sets Z if success
        PLA
    }
    
    IsExiting()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Exiting  // Sets Z if exiting
        PLA
    }
    
    // Convenience methods
    SetSuccess()
    {
        LDA #SystemState.Success
        SetState();
    }
    
    SetFailure()
    {
        LDA #SystemState.Failure  
        SetState();
    }
    
    SetExiting()
    {
        LDA #SystemState.Exiting
        SetState(); 
    }
    
    // Initialize system state
    Initialize()
    {
        SetSuccess();
    }
}
```

## Trace System ✅ **COMPLETE**

### Enhanced Method Naming Convention with Operator Context
```hopper
#ifdef TRACE
const string traceMethodName = "ShortName";  // 6-8 chars, descriptive
#endif
// Method description
// Input: Parameters
// Output: Results
MethodName()
{
#ifdef TRACE
LDA #(traceMethodName % 256) STA ZP.TraceMessageL LDA #(traceMethodName / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
    // Method implementation
    
#ifdef TRACE  
LDA #(traceMethodName % 256) STA ZP.TraceMessageL LDA #(traceMethodName / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
}
```

**Enhanced trace output with operator context:**
```
CompMult { // '*'
} // CompMult '*'
```

Examples:
- `"EvalExpr"` - EvaluateExpression
- `"CompFn"` - CompileFunction  
- `"ExecOp"` - ExecuteOpcodes
- `"FindSym"` - FindSymbol

### Trace Unit Implementation ✅ **IMPLEMENTED**
```hopper
unit Trace
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Tools"
    uses "Debug"

#if defined(DEBUG) || defined(TRACE)
    // Shared utilities (needed by Debug unit too)
    PrintIndent()
    {
        PHA
        PHX
        
        LDX ZP.TraceIndent
        loop
        {
            CPX #0
            if (Z) { break; }
            
            Debug.Space(); Debug.Space();
            
            DEX
        }
        
        PLX
        PLA
    }
#endif

#ifdef TRACE
    const string convergenceMarker = " <- CONVERGENCE";

    Initialize()
    {
        STZ ZP.TraceIndent
    }

    MethodEntry()
    {
        PHA
        PHX
        PHY
        
        PrintIndent();
        
        LDA ZP.TraceMessageL STA ZP.ACCL LDA ZP.TraceMessageH STA ZP.ACCH Tools.PrintStringACC(); Debug.Space(); LDA #'{' Debug.COut(); Debug.NL();
        
        INC ZP.TraceIndent
        
        PLY
        PLX
        PLA
    }
    
    MethodExit()
    {
        PHA
        PHX
        PHY
        
        DEC ZP.TraceIndent
        
        PrintIndent();
        
        LDA ZP.TraceMessageL STA ZP.ACCL LDA ZP.TraceMessageH STA ZP.ACCH Tools.PrintStringACC(); Debug.Space(); LDA #'}' Debug.COut(); Debug.NL();
        
        PLY
        PLX
        PLA
    }
    
    MarkConvergence()
    {
        PHA
        
        LDA #(convergenceMarker % 256) STA ZP.ACCL LDA #(convergenceMarker / 256) STA ZP.ACCH Tools.PrintStringACC();
        
        PLA
    }
#else
    Initialize() { }
#endif
}
```

## Debug System ✅ **COMPLETE**

### Debug Unit API
```hopper
unit Debug  
{
#if defined(DEBUG) || defined(TRACE)
    // Shared utilities (needed by Trace)
    HOut()          // Hex byte preserving carry
    COut()          // Character preserving carry
    Space()         // Single space output
#endif

#ifdef DEBUG
    // Stream rendering (human readable)
    RenderTokenStream()     // Token stream as readable BASIC code
    RenderOpcodeStream()    // Opcode stream as readable assembly
    RenderFunction()        // Function signature and body
    RenderVariable()        // Variable with type and value

    // Structure dumps
    DumpVariables()         // Key zero page variables
    DumpStack()            // Value stack contents  
    DumpHeap()             // Heap with state preservation
    DumpHeapSummary()      // Lightweight heap summary
    DumpIterationState()   // Current IDX pointer
    DumpBasicBuffers()     // Input and tokenizer buffers
    DumpPage()             // 256-byte page in hex+ASCII

    // New literate dumps
    DumpSymbolTable()      // All variables/constants in readable format
    DumpFunctionTable()    // All functions with signatures
    DumpTokenBuffer()      // Current token stream as readable BASIC
    DumpOpcodeBuffer()     // Current opcodes as readable assembly

    // Register output
    XOut(), YOut(), TOut(), NOut(), AOut(), ATOut(), ALOut(), IOut()
    DumpRegisters()        // All key registers in one line
    DumpCompilerState()    // Compiler workspace variables
    DumpExecutorState()    // Executor workspace variables
#endif
}
```

## Clean API Coding Conventions

### Register and Flag State Management
```hopper
// RULE: Only preserve what we modify, knowing called methods are Clean APIs
SomeDebugMethod()
{
    // Only preserve what this method actually modifies
    PHA  // If we modify A
    PHX  // If we modify X
    
    // Debug implementation
    
    PLX  // Restore in reverse order
    PLA
}
```

### Debug Code Alignment Convention
```hopper
// RULE: Debug code aligns to left margin for easy removal
NormalMethod()
{
    PHA
    
#ifdef TRACE
LDA #(traceMethodName % 256) STA ZP.TraceMessageL LDA #(traceMethodName / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

    // Normal method logic with proper indentation
    
#ifdef TRACE  
LDA #(traceMethodName % 256) STA ZP.TraceMessageL LDA #(traceMethodName / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif

    PLA
}
```

### State Management Integration
```hopper
// RULE: Use State system for complex flows, maintain C|NC compatibility
ComplexMethod()
{
    PHA
    
    // Complex operation
    processOperation();
    
    // Set appropriate state
    State.GetState();
    switch (A)
    {
        case SystemState.Success:   { continue; }
        case SystemState.Failure:   { cleanup(); }
        case SystemState.Exiting:   { propagateExit(); }
    }
    
    PLA
}
```

### Method Name String Convention
```hopper
// RULE: Method name constants defined just before each method
#ifdef TRACE
const string myMethodTrace = "MyMethod";
#endif
// Method description
// Input: Parameters  
// Output: Results
MyMethod()
{
#ifdef TRACE
LDA #(myMethodTrace % 256) STA ZP.TraceMessageL LDA #(myMethodTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
    // Implementation
    
#ifdef TRACE
LDA #(myMethodTrace % 256) STA ZP.TraceMessageL LDA #(myMethodTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
}
```

### Debug Output Consistency
```hopper
// RULE: All debug output preserves execution state
// Pattern: Use Debug.COut() not Serial.WriteChar()
// Reason: Debug methods preserve flags, Serial methods don't

// Good (preserves flags):
Debug.COut();
Debug.HOut();
Debug.NL();

// Bad (munts flags):
Serial.WriteChar();
Serial.HexOut();
```

### Conditional Compilation Safety
```hopper
// RULE: Production code behavior identical regardless of debug symbols
// Pattern: All debug code in #ifdef blocks, no logic dependencies

loop
{
#ifdef DEBUG
DebugOutputHere();
#endif
    
    // Production logic never depends on debug state
    if (production_condition)
    {
        break;
    }
    
#ifdef TRACE
TracePointHere();
#endif
}
```

### Error Handling in Debug Code
```hopper
// RULE: Debug methods never set ZP.LastError
// Pattern: Silent failure or internal error indicators only

DebugMethod()
{
    PHA
    
    // If debug operation fails, fail silently
    // Don't corrupt main program's error state
    Memory.Allocate();
    LDA ZP.IDXL
    ORA ZP.IDXH
    if (Z)
    {
        // Debug allocation failed - just exit quietly
        PLA
        return;
    }
    
    // Continue with debug operation
    PLA
}
```

### Debug Buffer Management
```hopper
// RULE: Debug methods never corrupt main program buffers
// Pattern: Use separate debug buffers or stack-based formatting

RenderTokenStream()
{
    // Save any buffer state we need to examine
    LDA ZP.TokenBufferLengthL
    PHA
    LDA ZP.TokenBufferLengthH
    PHA
    
    // Do debug rendering without modifying source
    
    // Restore original state
    PLA
    STA ZP.TokenBufferLengthH
    PLA
    STA ZP.TokenBufferLengthL
}
```

### Documentation Standards for Debug Methods
```hopper
// RULE: Debug methods require enhanced documentation
// Pattern: Document purpose, triggers, and preservation guarantees

#ifdef DEBUG
// Render token stream as human-readable BASIC code
// Input: ZP.IDY = token stream pointer 
// Output: Formatted code printed to serial
// Triggers: Manual debugging, trace points
// Preserves: ALL registers, ALL zero page, ALL flags
// Note: Uses stack-based formatting to avoid buffer corruption
RenderTokenStream()
{
    // Implementation
}
#endif
```

## Usage Scenarios

### TRACE only (minimal overhead)
```hopper
#define TRACE
// Gets: method call hierarchy, convergence tracking
// No structure dumps, no literate output
```

### DEBUG only (traditional)
```hopper  
#define DEBUG
// Gets: all dumps and literate output
// No method call tracing
```

### Both (full debugging)
```hopper
#define DEBUG
#define TRACE
// Gets: everything - call tracing + structure dumps
```

## Sample Output

### TRACE Only ✅ **WORKING**
```
> print 4 * 4 + 5
EvalExpr {
  CompExpr {
  } // CompExpr
  ExecOpCodes {
  } // ExecOpCodes
} // EvalExpr
21
```

### TRACE with Enhanced Context (Future)
```
> print 4 * 4 + 5
EvalExpr {
  CompExpr {
    CompLog { // 'OR'
      CompCmp { // '='
        CompAdd { // '+'
          CompMult { // '*'
          } // CompMult '*'
        } // CompAdd '+'
      } // CompCmp '='
    } // CompLog 'OR'
  } // CompExpr
  ExecOpCodes {
  } // ExecOpCodes
} // EvalExpr
21
```

### DEBUG + TRACE (Future)
```
EvalExpr {
  CompExpr {
    Token Stream [FUNC test() RETURN 42 ENDFUNC]
    Opcode Stream [ENTER, PUSHINT 42, RETURNVAL]
  } // CompExpr
  ExecOpCodes { ← CONVERGENCE
    Register State: IDX:4020 TOP:INT-42
  } // ExecOpCodes
} // EvalExpr
```

### State Management Examples
```hopper
// Robust Console capture mode
Console.processTokens();
State.GetState();
switch (A)
{
    case SystemState.Success:   { continue; }
    case SystemState.Exiting:   { ExitFunctionCaptureMode(); return; }
    case SystemState.Failure:   { displayError(); }
}

// REPL unification with clear state flow
executeUnifiedStatement();
State.IsExiting();
if (Z) { cleanup(); exit(); }
```

### Literate Output Examples (Future)

**Token Stream Rendering:**
```
Token Stream [0x4010, 12 bytes]:
  FUNC test ( )
  RETURN INT 42
  ENDFUNC
```

**Opcode Stream Rendering:**
```
Opcode Stream [0x4020, 8 bytes]:
  0x4020: ENTER           ; Setup function frame
  0x4021: PUSHINT 42 00   ; Push INT 42
  0x4024: RETURNVAL       ; Return with value
```

**Function Rendering:**
```
Function test() [Node: 0x4010]
  Arguments: (none)
  Tokens: 0x4015 [12 bytes]
  Opcodes: 0x4020 [8 bytes] ✓ Compiled
  Body: RETURN 42
```

## Target Methods for REPL/Function Tracing

### REPL Execution Path ✅ **IMPLEMENTED**
- `Statement.EvaluateExpression()` → `"EvalExpr"` ✅
- `Compiler.CompileExpression()` → `"CompExpr"` ✅
- `Executor.ExecuteOpcodes()` → `"ExecOpCodes"` ✅

### Function Execution Path (Next Target)
- `Functions.Compile()` → `"FnComp"`
- `Compiler.CompileFunction()` → `"CompFn"`
- `Functions.JumpToOpCodes()` → `"FnJump"`
- `Executor.ExecuteOpcodes()` → `"ExecOpCodes"` (convergence)

### Key Decision Points (Future)
- `Console.processTokens()` → `"ProcTk"` (REPL vs statement)
- `Statement.Execute()` → `"ExecSt"` (statement dispatch)
- `executeCall()` vs `executeCallF()` → `"Call"` vs `"CallF"`

### Expression Compilation Details (Future)
- `Compiler.compilePrimary()` → `"CompPrim"`
- `Compiler.compileLogical()` → `"CompLog"`
- `Compiler.compileComparison()` → `"CompCmp"`
- `Compiler.compileAdditive()` → `"CompAdd"`
- `Compiler.compileMultiplicative()` → `"CompMult"`

## Convergence Points
**Convergence points** are where different execution paths merge back into the same code. The key convergence point is `Executor.ExecuteOpcodes()` - both REPL expressions and function calls should generate the same opcodes and use the same execution engine.

This validates that the unification is working correctly:
- **Same opcodes generated**: Both paths produce identical bytecode
- **Same execution behavior**: Identical opcodes execute identically
- **Same results**: Both paths produce the same answer

## Benefits
- **Dramatically reduced code clutter**: Error handling 5 lines → 1 line ✅
- **Consistent error handling**: All errors follow same pattern ✅
- **Robust state management**: Replaces fragile C|NC flag propagation
- **Future-proof**: Easy to add terse mode or error logging ✅
- **Granular debug control**: Choose tracing level independently ✅
- **Single responsibility**: Each unit has clear, focused purpose ✅
- **Visual call hierarchy**: Easy to see execution flow ✅
- **Literate debugging**: Human-readable structure dumps ✅
- **Path validation**: Direct verification of REPL vs Function convergence ✅
- **Zero overhead**: Production builds unaffected when symbols undefined ✅

## Implementation Checklist

### Phase 1: State Management System
- [ ] **Create State.asm**
  - [ ] Define SystemState enum (Failure, Success, Exiting)
  - [ ] Implement SetState(), GetState(), IsFailure(), IsSuccess(), IsExiting()
  - [ ] Add convenience methods (SetSuccess(), SetFailure(), SetExiting())
  - [ ] Implement Initialize() method
- [ ] **Update ZeroPage.asm**
  - [ ] Add ZP.SystemState allocation (1 byte at 0x4A)
- [ ] **Update InitializeBASIC()**
  - [ ] Add State.Initialize() call
- [ ] **Target high-value conversion areas:**
  - [ ] Console capture mode (function definition across multiple lines)
  - [ ] Function compilation pipeline error handling
  - [ ] Main interpreter loop exit handling

### Phase 2: Function Execution Tracing
- [ ] **Add trace calls to function execution path:**
  - [ ] Functions.Compile() → "FnComp"
  - [ ] Compiler.CompileFunction() → "CompFn"
  - [ ] Functions.JumpToOpCodes() → "FnJump"
  - [ ] Executor.executeCall() → "Call"
  - [ ] Executor.executeCallF() → "CallF"

### Phase 3: Enhanced Trace Context
- [ ] **Add operator context to trace output:**
  - [ ] Modify compilation methods to show current operator
  - [ ] Format: `CompMult { // '*'` and `} // CompMult '*'`
  - [ ] Add convergence markers where paths merge

### Code Quality Standards (All Phases) ✅ **MAINTAINED**
- [x] **Flag preservation**: Methods preserve only what they modify
- [x] **Register preservation**: Proper save/restore for modified registers  
- [x] **Left-margin alignment**: Debug code easy to identify/remove
- [x] **Single exit pattern**: Clean error handling and cleanup
- [x] **No ZP corruption**: Debug uses only designated scratch space
- [x] **No error pollution**: Debug failures don't set ZP.LastError
- [x] **PC at call site**: Error PC set at call site with BIT ZP.EmulatorPCL
  
## Future Enhancements
- Implement new literate rendering methods (RenderTokenStream, RenderFunction, etc.)
- Path-specific markers (R: for REPL, F: for Function)
- Timing information for performance analysis
- Conditional tracing (specific methods only)
- 6502 Stack depth monitoring
- Memory usage tracking
- Interactive debugging commands (in addition to DUMP, BUFFERS, HEAP)
- Breakpoint simulation
- Error logging and statistics (to file via changes to the Windows emulator)
- Additional SystemState values as needed (Breaking, Continuing, Retry, Pending)