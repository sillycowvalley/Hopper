# HopperBASIC Debug & Error Handling Refactoring Specification

## Overview
Complete refactoring of debug output and error handling systems in HopperBASIC to support:
- Enhanced method call tracing for REPL/Function unification debugging
- Simplified error handling with one-liner methods
- Modular debug capabilities with selective compilation
- Human-readable structure dumps and literate output

## Debug Symbols
- `DEBUG` - General debug output and structure dumps
- `TRACE` - Method entry/exit tracing (new)
- `TERSE_ERRORS` - Error numbers instead of strings (future)

## Architecture Refactoring

### Four-Unit Split: Tools.asm → Tools.asm + Debug.asm + Trace.asm + Errors.asm

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

**Errors.asm** (Error handling and messages)
- One-liner error methods (`SyntaxError()`, `TypeMismatch()`, etc.)
- Error message strings (moved from Messages.asm)
- Future: Terse mode compilation support

## Error Handling Simplification

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
Errors.SyntaxError(); BIT ZP.EmulatorPCL
```

### Errors Unit API
```hopper
unit Errors
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    // Error message strings (moved from Messages.asm)
    #ifdef TERSE_ERRORS
    const string SyntaxErrorMsg = "E001";
    const string TypeMismatchMsg = "E002";
    const string NotImplementedMsg = "E003";
    // ... (future enhancement)
    #else
    const string SyntaxErrorMsg = "SYNTAX ERROR";
    const string TypeMismatchMsg = "TYPE MISMATCH";
    const string NotImplementedMsg = "NOT IMPLEMENTED";
    const string InternalErrorMsg = "INTERNAL ERROR";
    const string FunctionExistsMsg = "FUNCTION EXISTS";
    const string ConstantExistsMsg = "CONSTANT EXISTS";
    const string VariableExistsMsg = "VARIABLE EXISTS";
    const string OutOfMemoryMsg = "OUT OF MEMORY";
    const string DivisionByZeroMsg = "DIVISION BY ZERO";
    const string NumericOverflowMsg = "NUMERIC OVERFLOW";
    const string UndefinedIdentifierMsg = "UNDEFINED IDENTIFIER";
    const string IllegalAssignmentMsg = "ILLEGAL ASSIGNMENT";
    const string InvalidOperatorMsg = "INVALID OPERATOR";
    const string BufferOverflowMsg = "BUFFER OVERFLOW";
    const string ExpectedRightParenMsg = ") EXPECTED";
    const string ExpectedLeftParenMsg = "( EXPECTED";
    const string ExpectedQuoteMsg = "QUOTE EXPECTED";
    const string ExpectedExpressionMsg = "EXPRESSION EXPECTED";
    const string InvalidBitValueMsg = "INVALID BIT VALUE";
    const string ConstantExpressionExpectedMsg = "CONSTANT EXPRESSION EXPECTED";
    const string IllegalIdentifierMsg = "ILLEGAL IDENTIFIER";
    #endif
    
    // One-liner error methods (PC must be set at call site)
    SyntaxError() { setError(SyntaxErrorMsg); }
    TypeMismatch() { setError(TypeMismatchMsg); }
    NotImplemented() { setError(NotImplementedMsg); }
    InternalError() { setError(InternalErrorMsg); }
    FunctionExists() { setError(FunctionExistsMsg); }
    ConstantExists() { setError(ConstantExistsMsg); }
    VariableExists() { setError(VariableExistsMsg); }
    OutOfMemory() { setError(OutOfMemoryMsg); }
    DivisionByZero() { setError(DivisionByZeroMsg); }
    NumericOverflow() { setError(NumericOverflowMsg); }
    UndefinedIdentifier() { setError(UndefinedIdentifierMsg); }
    IllegalAssignment() { setError(IllegalAssignmentMsg); }
    InvalidOperator() { setError(InvalidOperatorMsg); }
    BufferOverflow() { setError(BufferOverflowMsg); }
    ExpectedRightParen() { setError(ExpectedRightParenMsg); }
    ExpectedLeftParen() { setError(ExpectedLeftParenMsg); }
    ExpectedQuote() { setError(ExpectedQuoteMsg); }
    ExpectedExpression() { setError(ExpectedExpressionMsg); }
    InvalidBitValue() { setError(InvalidBitValueMsg); }
    ConstantExpressionExpected() { setError(ConstantExpressionExpectedMsg); }
    IllegalIdentifier() { setError(IllegalIdentifierMsg); }
    
    // Internal helper
    setError()
    {
        // A contains message address (from calling method setup)
        STA ZP.LastErrorL
        LDA ZP.ACCH
        STA ZP.LastErrorH
        CLC
    }
}
```

## Trace System

### New Zero Page Allocation
```hopper
// Add to ZeroPage.asm  
#if defined(DEBUG) || defined(TRACE)
ZP.TraceIndent   = 0x??;  // 1 byte - current call depth
ZP.DebugTemp0    = 0x??;  // 1 byte - debug temporary
ZP.DebugTemp1    = 0x??;  // 1 byte - debug temporary
#endif
```

### Method Naming Convention
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
LDA #(traceMethodName / 256); STA ZP.ACCH; LDA #(traceMethodName % 256); STA ZP.ACCL; Trace.MethodEntry()
#endif
    
    // Method implementation
    
#ifdef TRACE  
LDA #(traceMethodName / 256); STA ZP.ACCH; LDA #(traceMethodName % 256); STA ZP.ACCL; Trace.MethodExit()
#endif
}
```

Examples:
- `"EvExpr"` - EvaluateExpression
- `"CompFn"` - CompileFunction  
- `"ExecOp"` - ExecuteOpcodes
- `"FindSym"` - FindSymbol

### Trace Unit API
```hopper
unit Trace
{
    uses "Debug"  // For shared utilities
    
#if defined(DEBUG) || defined(TRACE)
    // Shared utilities (available to both DEBUG and TRACE)
    PrintIndent()   // Print current indentation level
#endif

#ifdef TRACE
    // Pure tracing functionality
    MethodEntry()   // Print method name + '{', increase indent
    MethodExit()    // Decrease indent, print method name + '}'
    MarkConvergence()  // Special marker for convergence points
#else
    // No-op versions for production builds
    MethodEntry() { }
    MethodExit()  { }
    MarkConvergence() { }
#endif
}
```

## Debug System

### Debug Unit API
```hopper
unit Debug  
{
#if defined(DEBUG) || defined(TRACE)
    // Shared utilities (needed by Trace)
    HOut()          // Hex byte preserving carry
    COut()          // Character preserving carry
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
LDA #(traceMethodName / 256); STA ZP.ACCH; LDA #(traceMethodName % 256); STA ZP.ACCL; Trace.MethodEntry()
#endif

    // Normal method logic with proper indentation
    
#ifdef TRACE  
LDA #(traceMethodName / 256); STA ZP.ACCH; LDA #(traceMethodName % 256); STA ZP.ACCL; Trace.MethodExit()
#endif

    PLA
}
```

### Single Point of Exit for Debug Methods
```hopper
// RULE: Debug methods use single exit pattern
DebugMethod()
{
    PHA  // Only preserve what we modify
    
    loop // Single exit block
    {
        // Debug implementation
        if (error_condition) { break; }
        
        // More debug work
        if (another_condition) { break; }
        
        // Success path
        break;
    }
    
    // Cleanup always executes
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
LDA #(myMethodTrace / 256); STA ZP.ACCH; 
LDA #(myMethodTrace % 256); STA ZP.ACCL; 
Trace.MethodEntry()
#endif
    
    // Implementation
    
#ifdef TRACE
LDA #(myMethodTrace / 256); STA ZP.ACCH; 
LDA #(myMethodTrace % 256); STA ZP.ACCL; 
Trace.MethodExit()
#endif
}
```

### Debug Output Consistency
```hopper
// RULE: All debug output preserves execution state
// Pattern: Use Tools.COut() not Serial.WriteChar()
// Reason: Tools methods preserve flags, Serial methods don't

// Good (preserves flags):
Tools.COut();
Tools.HOut();
Tools.NL();

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

### TRACE Only
```
EvExpr {
  CompEx {
  }
  ExecOp { ← CONVERGENCE
  }
}
```

### DEBUG + TRACE
```
EvExpr {
  CompEx {
    Token Stream [FUNC test() RETURN 42 ENDFUNC]
    Opcode Stream [ENTER, PUSHINT 42, RETURNVAL]
  }
  ExecOp { ← CONVERGENCE
    Register State: IDX:4020 TOP:INT-42
  }
}
```

### Literate Output Examples

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

### REPL Execution Path
- `Statement.EvaluateExpression()` → `"EvExpr"`
- `Compiler.CompileExpression()` → `"CompEx"`
- `Compiler.compilePrimary()` → `"CompPr"`
- `Executor.ExecuteOpcodes()` → `"ExecOp"`

### Function Execution Path  
- `Functions.Compile()` → `"CompFn"`
- `Compiler.CompileFunction()` → `"CompFu"`
- `Functions.JumpToOpCodes()` → `"JumpOp"`
- `Executor.ExecuteOpcodes()` → `"ExecOp"` (convergence)

### Key Decision Points
- `Console.processTokens()` → `"ProcTk"` (REPL vs statement)
- `Statement.Execute()` → `"ExecSt"` (statement dispatch)
- `executeCall()` vs `executeCallF()` → `"Call"` vs `"CallF"`

## Convergence Points
**Convergence points** are where different execution paths merge back into the same code. The key convergence point is `Executor.ExecuteOpcodes()` - both REPL expressions and function calls should generate the same opcodes and use the same execution engine.

This validates that the unification is working correctly:
- **Same opcodes generated**: Both paths produce identical bytecode
- **Same execution behavior**: Identical opcodes execute identically
- **Same results**: Both paths produce the same answer

## Benefits
- **Dramatically reduced code clutter**: Error handling 5 lines → 1 line
- **Consistent error handling**: All errors follow same pattern
- **Future-proof**: Easy to add terse mode or error logging
- **Granular debug control**: Choose tracing level independently
- **Single responsibility**: Each unit has clear, focused purpose
- **Visual call hierarchy**: Easy to see execution flow
- **Literate debugging**: Human-readable structure dumps
- **Path validation**: Direct verification of REPL vs Function convergence
- **Zero overhead**: Production builds unaffected when symbols undefined

## Implementation Checklist

### Phase 1: Errors
- [ ] **Create Errors.asm**
  - [ ] Move error message strings from Messages.asm
  - [ ] Implement `setError()` helper method
  - [ ] Create one-liner error methods (SyntaxError, TypeMismatch, etc.)
- [ ] **Update Messages.asm**
  - [ ] Remove error message strings (moved to Errors.asm)
  - [ ] Keep status messages (Welcome, OK, MemoryMsg, etc.)
  - [ ] Update any references to moved constants
- [ ] **Systematically replace verbose error patterns with one-liners:**
  - [ ] Compiler.asm: Replace all error pattern occurrences
  - [ ] Statement.asm: Replace all error pattern occurrences  
  - [ ] Tokenizer.asm: Replace all error pattern occurrences
  - [ ] Variables.asm: Replace all error pattern occurrences
  - [ ] Functions.asm: Replace all error pattern occurrences
  - [ ] Console.asm: Replace all error pattern occurrences
  - [ ] Instructions.asm: Replace all error pattern occurrences
  - [ ] Add `uses "Errors"` to all affected units

### Phase 2: Tools -> Tools, Debug
- [ ] **Create Debug.asm** 
  - [ ] Move debug methods from Tools.asm (DumpVariables, DumpStack, etc.)
  - [ ] Move register output methods (XOut, YOut, TOut, etc.)
  - [ ] Add `#ifdef DEBUG` guards around all methods
- [ ] **Refactor Tools.asm**
  - [ ] Remove debug methods (moved to Debug.asm)
  - [ ] Remove register output methods (moved to Debug.asm) 
  - [ ] Keep only production utilities (string ops, type ops, basic I/O)
  - [ ] Update any internal dependencies

### Phase 3: Trace
- [ ] **Create Trace.asm**
  - [ ] Add ZP.TraceIndent and debug temps to ZeroPage.asm with guards
  - [ ] Implement PrintIndent() with shared compilation guard
  - [ ] Implement MethodEntry() and MethodExit() with TRACE guards  
  - [ ] Implement MarkConvergence() for convergence point detection
  - [ ] Add no-op versions for production builds

### Phase 4: Insert Trace Calls
- [ ] **Add method entry/exit tracing to key units:**
  - [ ] Statement
  - [ ] Compiler
  - [ ] Executor
  - [ ] Functions

### Code Quality Standards (All Phases)
- [ ] **Flag preservation**: Preserve only what methods actually modify
- [ ] **Register preservation**: Proper save/restore for modified registers  
- [ ] **Left-margin alignment**: Debug code easy to identify/remove
- [ ] **Single exit pattern**: No early returns without cleanup
- [ ] **No ZP corruption**: Debug uses only designated scratch space
- [ ] **No error pollution**: Debug failures don't set ZP.LastError
- [ ] **Buffer isolation**: Debug doesn't corrupt main program buffers
- [ ] **PC at call site**: Error PC set at call site, not in error method
  
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