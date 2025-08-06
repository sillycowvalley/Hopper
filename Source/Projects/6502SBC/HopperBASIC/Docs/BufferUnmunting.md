Looking at the refactored Console.asm and Statement.asm, I can now see the actual execution paths and create a concrete action plan. The code is cleaner than expected with the refactoring you've already done.

## Action Plan: Solving the Buffer Munting Problem

### Current Execution Paths (After Refactoring)

Based on the source code, there are actually **3 main execution paths** that compile and execute:

1. **cmdRun()** → `Statement.EvaluateExpression()` → `Compiler.CompileExpression()` → `Executor.ExecuteOpCodes()`
2. **processTokens() default case** → `Statement.Execute()` → `Statement.ExecuteStatement()` → `Compiler.CompileStatement()` → `Executor.ExecuteOpCodes()`  
3. **processSingleSymbolDeclaration()** → `Statement.EvaluateExpression()` → `Compiler.CompileExpression()` → `Executor.ExecuteOpCodes()`

### Phase 1: Define REPL Buffers and BufferManager (Day 1 Morning)

#### 1.1 Define memory map (in appropriate header/constants file):
```asm
const uint BasicTokenizerBuffer   = 0x0A00;  // 512 bytes - main tokenizer buffer
const uint BasicOpCodeBuffer      = 0x0C00;  // 512 bytes - main opcode buffer
const uint BasicREPLTokensBuffer  = 0x0E00;  // 256 bytes - REPL tokenizer buffer
const uint BasicREPLOpCodeBuffer  = 0x0F00;  // 256 bytes - REPL opcode buffer
```

#### 1.2 Create BufferManager.asm:
```asm
unit BufferManager
{
    // Use bit 7 of ZP.FLAGS for REPL mode flag
    // BBR7 = Branch if Bit 7 Reset (main mode)
    // BBS7 = Branch if Bit 7 Set (REPL mode)
    // SMB7 = Set Memory Bit 7 (enter REPL mode)
    // RMB7 = Reset Memory Bit 7 (exit REPL mode)
    
    UseREPLBuffers()
    {
        PHA
        
        // Set REPL mode flag
        SMB7 ZP.FLAGS
        
        // Update buffer pointers to REPL buffers
        LDA #<BasicREPLTokensBuffer
        STA ZP.TokenBufferL
        LDA #>BasicREPLTokensBuffer
        STA ZP.TokenBufferH
        
        LDA #<BasicREPLOpCodeBuffer
        STA ZP.OpCodeBufferL
        LDA #>BasicREPLOpCodeBuffer
        STA ZP.OpCodeBufferH
        
        // Clear the REPL buffers
        STZ ZP.TokenBufferLengthL
        STZ ZP.TokenBufferLengthH
        STZ ZP.OpCodeBufferLengthL
        STZ ZP.OpCodeBufferLengthH
        
        PLA
    }
    
    UseMainBuffers()
    {
        PHA
        
        // Clear REPL mode flag
        RMB7 ZP.FLAGS
        
        // Update buffer pointers to main buffers
        LDA #<BasicTokenizerBuffer
        STA ZP.TokenBufferL
        LDA #>BasicTokenizerBuffer
        STA ZP.TokenBufferH
        
        LDA #<BasicOpCodeBuffer
        STA ZP.OpCodeBufferL
        LDA #>BasicOpCodeBuffer
        STA ZP.OpCodeBufferH
        
        // Clear the main buffers
        STZ ZP.TokenBufferLengthL
        STZ ZP.TokenBufferLengthH
        STZ ZP.OpCodeBufferLengthL
        STZ ZP.OpCodeBufferLengthH
        
        PLA
    }
    
    IsREPLMode()
    {
        // Test bit 7 of FLAGS
        // Returns: C set if REPL mode, NC if main mode
        BBS7 ZP.FLAGS, replMode
        CLC
        RTS
    replMode:
        SEC
        RTS
    }
}
```

### Phase 2: Update Console.asm - Start with cmdRun() (Day 1 Afternoon)

Update `cmdRun()` to be the first test case:

```asm
cmdRun()
{
    // ... existing code up to finding $MAIN ...
    
    // Switch to main buffers for $MAIN compilation
    BufferManager.UseMainBuffers();
    
    // ... existing code to construct and execute $MAIN call ...
    
    // At end, return to REPL mode
    BufferManager.UseREPLBuffers();
}
```

### Phase 3: Update Console.processInput() (Day 2 Morning)

Add buffer management at the console entry point:

```asm
ProcessLine()
{
    // Ensure REPL mode for console processing
    BufferManager.UseREPLBuffers();
    
    Statement.IsCaptureModeOn();
    if (C)
    {
        processLineFunctionCapture();
    }
    else
    {
        processLineNormal();
    }
    // ... rest of existing code
}
```

### Phase 4: Handle Function Calls from REPL (Day 2 Afternoon)

When REPL calls a user function, we need to compile it to main buffers:

In Statement.asm, update `EvaluateExpression()` to handle function calls:

```asm
EvaluateExpression()
{
    // ... existing setup ...
    
    // Check if we're in REPL mode and might call functions
    BufferManager.IsREPLMode();
    if (C)
    {
        // Save REPL execution context
        LDA ZP.OpCodePCL
        PHA
        LDA ZP.OpCodePCH
        PHA
    }
    
    // ... existing compilation and execution ...
    
    BufferManager.IsREPLMode();
    if (C)
    {
        // Restore REPL execution context
        PLA
        STA ZP.OpCodePCH
        PLA
        STA ZP.OpCodePCL
    }
}
```

### Phase 5: Update Tokenizer (Day 3)

Tokenizer needs to respect current buffer mode:

```asm
TokenizeLine()
{
    // Check which buffer to use
    BBS7 ZP.FLAGS, useREPLBuffer
    
    // Use main buffer addresses
    LDA #<BasicTokenizerBuffer
    STA ZP.TOPL
    LDA #>BasicTokenizerBuffer
    STA ZP.TOPH
    JMP continueTokenize
    
useREPLBuffer:
    // Use REPL buffer addresses
    LDA #<BasicREPLTokensBuffer
    STA ZP.TOPL
    LDA #>BasicREPLTokensBuffer
    STA ZP.TOPH
    
continueTokenize:
    // ... rest of tokenization logic
}
```

### Testing Checkpoints

After each phase, test:

1. **Phase 1 Test**: Buffer switching works (use DEBUG to verify addresses)
2. **Phase 2 Test**: RUN command works without buffer corruption
3. **Phase 3 Test**: Simple REPL commands (PRINT 2+2) work
4. **Phase 4 Test**: Function calls from REPL work (PRINT ADD(5,3))
5. **Phase 5 Test**: Colon-separated statements work (INT X=10 : PRINT X)

### Key Implementation Notes

1. **Use bit 7 of ZP.FLAGS** for REPL mode (you already use bits 0-1 for other flags)
2. **256-byte buffers** are sufficient for REPL (one line max)
3. **Always start in REPL mode** when processing console input
4. **Switch to main mode** only for:
   - Running $MAIN (RUN command)
   - Compiling user functions when called from REPL
   - Loading programs from disk
5. **Keep the old $REPL code** until all paths are converted and tested

This approach is simpler than the document's save/restore strategy and uses only 512 bytes of additional RAM.