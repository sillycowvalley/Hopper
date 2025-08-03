# HopperBASIC Debug & Error Handling Refactoring Specification
**Document Type: Debug Architecture**

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

### Five-Unit Split: Tools.asm → Tools.asm + Debug.asm + Trace.asm + Error.asm + State.asm

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
- Comprehensive error checking (`CheckErrorAndStatus()`)
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
    uses "State"
    
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
    
    // Comprehensive error check covering both error systems
    // Input: None
    // Output: C set if ok, NC if not ok (error occurred)
    // Modifies: Processor flags only
    CheckErrorAndStatus()
    {
        PHA
        Error.CheckError(); // C if ok, NC if not ok (error)
        if (C)
        {
            // LastError not set, check SystemState
            State.CanContinue(); // C if all good, NC if error or exit
        }
        PLA
    }
}
```

## State Management System ✅ **IMPLEMENTED**

### Design Principles

**API Layer Distinction:**
- **Leaf-level APIs**: Use C|NC flags for immediate success/failure
- **Orchestration methods**: Use SystemState for complex flows that need to propagate status
- **Rule**: If you're passing status to your caller and you're not a leaf API, use SystemState

**Flag Preservation Simplification:**
- **Before**: PHP/PLP everywhere to preserve fragile C flags
- **After**: Remove PHP/PLP except for small utility/debug APIs used near C|NC checks
- **Benefit**: Cleaner code, fewer register stack operations, more robust state management

**Error Detection Enhancement:**
- **State.IsError()**: Checks only SystemState.Failure
- **Error.CheckErrorAndStatus()**: Comprehensive check of both error systems
- **Benefit**: Granular control when needed, comprehensive checking at decision points

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

### State Unit API ✅ **IMPLEMENTED**
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
    // Output: ZP.SystemState updated, A preserved
    // Modifies: ZP.SystemState only
    SetState()
    {
        STA ZP.SystemState
    }
    
    // Get current system state  
    // Output: A = current SystemState
    // Modifies: A only
    GetState()
    {
        LDA ZP.SystemState
    }
    
    // Test for specific states (preserves A register)
    // Output: C set if condition true, NC if condition false
    IsFailure()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z) { SEC } else { CLC }
        PLA
    }
    
    IsSuccess()
    {
        PHA  
        LDA ZP.SystemState
        CMP #SystemState.Success
        if (Z) { SEC } else { CLC }
        PLA
    }
    
    IsExiting()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Exiting
        if (Z) { SEC } else { CLC }
        PLA
    }
    
    // Check if current state indicates continuation is possible
    // Output: C set if can continue (Success), NC if should stop (Failure or Exiting)
    // Preserves: A register
    CanContinue()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Success
        if (Z) { SEC } else { CLC }
        PLA
    }
    
    // Check if current state indicates an error condition (SystemState only)
    // Output: C set if error (Failure), NC if not error (Success or Exiting)
    // Preserves: A register
    IsError()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z) { SEC } else { CLC }
        PLA
    }
    
    // Convenience methods (preserve A register)
    SetSuccess()
    {
        PHA
        LDA #SystemState.Success
        STA ZP.SystemState
        PLA
    }
    
    SetFailure()
    {
        PHA
        LDA #SystemState.Failure  
        STA ZP.SystemState
        PLA
    }
    
    SetExiting()
    {
        PHA
        LDA #SystemState.Exiting
        STA ZP.SystemState
        PLA
    }
    
    // Initialize system state
    Initialize()
    {
        SetSuccess();
    }
}
```

### Usage Guidelines

**Leaf-level APIs (continue using C|NC):**
```hopper
// Memory allocation, string operations, basic I/O
Memory.Allocate();  // Returns C for success, NC for failure
if (NC) { /* handle allocation failure */ }

Tools.StringCompare();  // Returns C for match, NC for no match
if (C) { /* strings match */ }

// Example leaf API that should use C|NC
createReplFunction()
{
    // Implementation
    if (success) { SEC } else { CLC }
}
```

**Orchestration methods (use SystemState):**
```hopper
// Complex flows that coordinate multiple operations
Console.ProcessLine()
{
    // No PHP/PLP needed - not preserving fragile C flags
    
    executeUnifiedStatement();  // Leaf API call
    if (NC) { State.SetFailure(); return; }
    
    // Handle complex state flows
    checkForExit();
    State.GetState();
    switch (A)
    {
        case SystemState.Success:   { /* continue */ }
        case SystemState.Failure:   { /* handle error */ }
        case SystemState.Exiting:   { /* propagate exit */ }
    }
}

// Caller checks SystemState instead of fragile C flag
interpreterLoop()
{
    Console.ProcessLine();
    State.IsExiting();
    if (C) { cleanup(); exit(); }
}
```

**Comprehensive Error Checking:**
```hopper
// At key decision points, check both error systems
complexOperation();
Error.CheckErrorAndStatus();
if (NC) { 
    State.IsExiting();
    if (C) { exit(); }
    else { displayError(); }
}
```

**Debug/Utility APIs (preserve flags when needed):**
```hopper
// Small utilities that might be used near C|NC checks
Debug.HOut()
{
    PHP  // Preserve flags for debugging context
    Serial.HexOut();
    PLP  // Restore flags
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
    
    // Method implementation (no PHP/PLP needed for orchestration methods)
    
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
    // Shared utilities (preserve flags for debugging context)
    HOut()
    {
        PHP  // Preserve flags for debugging context
        Serial.HexOut();
        PLP  // Restore flags
    }
    
    COut()
    {
        PHP  // Preserve flags for debugging context
        Serial.WriteChar();
        PLP  // Restore flags
    }
    
    Space()
    {
        PHP  // Preserve flags for debugging context
        LDA #' ' 
        Serial.WriteChar();
        PLP  // Restore flags
    }
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

    // Register output (preserve flags for debugging context)
    XOut(), YOut(), TOut(), NOut(), AOut(), ATOut(), ALOut(), IOut()
    DumpRegisters()        // All key registers in one line
    DumpCompilerState()    // Compiler workspace variables
    DumpExecutorState()    // Executor workspace variables
#endif
}
```

## Clean API Coding Conventions

### Register Preservation (Simplified)
```hopper
// NEW RULE: Remove PHP/PLP except for small debug utilities
// Orchestration methods don't need to preserve fragile C flags

// Before (unnecessarily complex):
ComplexMethod()
{
    PHP  // Why preserve flags?
    PHA
    PHX
    PHY
    
    // Implementation
    
    PLY
    PLX
    PLA
    PLP  // Fragile flag restoration
}

// After (clean and simple):
ComplexMethod()
{
    PHA  // Only preserve what we modify
    PHX
    PHY
    
    // Implementation
    
    PLY
    PLX
    PLA
    // No flag preservation needed for SystemState APIs
}
```

### API Layer Distinction
```hopper
// Leaf-level API (use C|NC):
Memory.Allocate()
{
    // Implementation
    if (success) { SEC } else { CLC }
}

// Orchestration method (use SystemState):
Functions.Compile()
{
    // No PHP/PLP needed
    
    Memory.Allocate();
    if (NC) { State.SetFailure(); return; }
    
    Tokenizer.TokenizeLine();
    if (NC) { State.SetFailure(); return; }
    
    State.SetSuccess();
}

// Caller uses SystemState:
someHigherLevel()
{
    Functions.Compile();
    State.GetState();
    switch (A)
    {
        case SystemState.Success: { continue; }
        case SystemState.Failure: { handleError(); }
    }
}
```

### Error Checking Patterns
```hopper
// Granular SystemState-only check:
State.IsError();
if (C) { handleStateFailure(); }

// Comprehensive error check at decision points:
Error.CheckErrorAndStatus();
if (NC) { 
    State.IsExiting();
    if (C) { exit(); }
    else { displayError(); }
}

// Quick continuation check:
State.CanContinue();
if (NC) { return; }  // Stop execution
```

### Debug Code Alignment Convention
```hopper
// RULE: Debug code aligns to left margin for easy removal
NormalMethod()
{
    PHA  // No PHP/PLP for orchestration methods
    
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
// RULE: Use State system for complex flows, leaf APIs use C|NC
ComplexMethod()
{
    PHA
    
    // Call leaf APIs directly
    Memory.Allocate();
    if (NC) 
    { 
        State.SetFailure(); 
        PLA
        return; 
    }
    
    // Call other orchestration methods
    someOtherOrchestrationMethod();
    State.GetState();
    switch (A)
    {
        case SystemState.Success:   { /* continue */ }
        case SystemState.Failure:   { PLA; return; }
        case SystemState.Exiting:   { State.SetExiting(); PLA; return; }
    }
    
    State.SetSuccess();
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
    
    // Implementation (no PHP/PLP needed for orchestration)
    
#ifdef TRACE
LDA #(myMethodTrace % 256) STA ZP.TraceMessageL LDA #(myMethodTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
}
```

### Debug Output Consistency
```hopper
// RULE: Debug utilities preserve flags, orchestration methods don't

// Debug utilities (preserve flags):
Debug.COut();      // Uses PHP/PLP internally
Debug.HOut();      // Uses PHP/PLP internally
Debug.NL();        // Uses PHP/PLP internally

// Orchestration methods (don't preserve flags):
Statement.Execute();    // No PHP/PLP needed
Console.ProcessLine();  // No PHP/PLP needed
Functions.Compile();    // No PHP/PLP needed
```

### Conditional Compilation Safety
```hopper
// RULE: Production code behavior identical regardless of debug symbols
// Pattern: All debug code in #ifdef blocks, no logic dependencies

loop
{
#ifdef DEBUG
DebugOutputHere();  // May preserve flags internally
#endif
    
    // Production logic never depends on debug state
    leafApi();
    if (NC) { State.SetFailure(); break; }
    
#ifdef TRACE
TracePointHere();   // May preserve flags internally
#endif
}
```

### Error Handling in Debug Code
```hopper
// RULE: Debug methods never set ZP.LastError or SystemState
// Pattern: Silent failure or internal error indicators only

DebugMethod()
{
    PHA
    
    // If debug operation fails, fail silently
    // Don't corrupt main program's error state
    Memory.Allocate();
    if (NC)
    {
        // Debug allocation failed - just exit quietly
        // Don't call State.SetFailure() - this is debug code
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
// Robust Console capture mode (no fragile flag preservation)
Console.ProcessTokens()
{
    // No PHP/PLP needed
    
    Tokenizer.TokenizeLine();
    if (NC) { State.SetFailure(); return; }
    
    Statement.Execute();
    State.GetState();
    switch (A)
    {
        case SystemState.Success:   { /* continue */ }
        case SystemState.Exiting:   { ExitFunctionCaptureMode(); return; }
        case SystemState.Failure:   { /* handle error */ }
    }
}

// Leaf API example
executeUnifiedStatement()
{
    createReplFunction();  // Leaf API - returns C|NC
    if (NC) { cleanup(); return; }  // Direct C|NC check
}

// Comprehensive error checking at decision points
mainInterpreterLoop()
{
    Console.ProcessLine();
    Error.CheckErrorAndStatus();  // Check both error systems
    if (NC) 
    { 
        State.IsExiting();
        if (C) { cleanup(); exit(); }
        else { displayError(); }
    }
}
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
- **Robust state management**: Replaces fragile C|NC flag propagation ✅
- **Simplified register management**: Remove PHP/PLP from orchestration methods ✅
- **Comprehensive error detection**: Both SystemState and ZP.LastError checking ✅
- **Future-proof**: Easy to add terse mode or error logging ✅
- **Granular debug control**: Choose tracing level independently ✅
- **Single responsibility**: Each unit has clear, focused purpose ✅
- **Visual call hierarchy**: Easy to see execution flow ✅
- **Literate debugging**: Human-readable structure dumps ✅
- **Path validation**: Direct verification of REPL vs Function convergence ✅
- **Zero overhead**: Production builds unaffected when symbols undefined ✅

## Implementation Checklist

### Phase 1: State Management System ✅ **COMPLETE**
- [x] **Create State.asm**
  - [x] Define SystemState enum (Failure, Success, Exiting)
  - [x] Implement SetState(), GetState(), IsFailure(), IsSuccess(), IsExiting()
  - [x] Add convenience methods (SetSuccess(), SetFailure(), SetExiting())
  - [x] Implement Initialize() and CanContinue() methods
  - [x] Use C|NC return convention for all Is*() methods
- [x] **Update Error.asm**
  - [x] Add CheckErrorAndStatus() for comprehensive error checking
  - [x] Add State dependency for enhanced error detection
- [x] **Update ZeroPage.asm**
  - [x] Add ZP.SystemState allocation (1 byte at 0x4A)
- [x] **Update InitializeBASIC()**
  - [x] Add State.Initialize() call
- [ ] **Refactor PHP/PLP usage:**
  - [ ] Remove PHP/PLP from orchestration methods (Console, Statement, Functions, etc.)
  - [ ] Keep PHP/PLP only in small debug utilities (Debug.COut(), Debug.HOut(), etc.)
  - [ ] Convert complex state flows to use SystemState instead of C|NC
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
- [x] **Flag preservation**: Only for small debug utilities, not orchestration methods
- [x] **Register preservation**: Proper save/restore for modified registers  
- [x] **Left-margin alignment**: Debug code easy to identify/remove
- [x] **Single exit pattern**: Clean error handling and cleanup
- [x] **No ZP corruption**: Debug uses only designated scratch space
- [x] **No error pollution**: Debug failures don't set ZP.LastError or SystemState
- [x] **PC at call site**: Error PC set at call site with BIT ZP.EmulatorPCL
- [x] **API layer distinction**: Leaf APIs use C|NC, orchestration uses SystemState
- [x] **Comprehensive error checking**: Both granular and comprehensive options available
  
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