# HopperBASIC Program Deserialization - Implementation Guide

## **Overview**
Implement loading of saved HopperBASIC programs from EEPROM. The saved format is a tokenized BASIC program containing constants, variables, functions, and main program. Use a 1024-byte sliding window buffer with enhanced TokenIterator to parse declarations and rebuild symbol tables.

## **Phase 1: Variable Initialization Refactoring**

### **Extract Common Default Initialization**
**Problem**: Four different places initialize variables to defaults (globals, locals, RUN reset, deserialization)

**Solution**: Create common initialization in Variables unit

```asm
// Initialize variable to appropriate default value based on type
// Input: ZP.IDX = variable node address
// Output: Variable initialized to default (strings allocated, arrays allocated, numbers=0)
// Munts: Memory allocation registers, string/array creation
Variables.InitializeToDefault()
```

**Implementation Details:**
- **Strings**: Allocate empty string `""`  
- **Arrays**: Allocate array with specified dimensions, elements zeroed
- **Numbers**: Set to `0` (BYTE, WORD, INT, LONG)
- **Chars**: Set to `'\0'`
- **BIT**: Set to `false`

**Refactor These Locations:**
1. Global variable declarations (when no `= expression`)
2. Local variable/parameter initialization  
3. RUN command global reset
4. Deserialization of variables without initializers

## **Phase 2: Sliding Window Buffer System**

### **Buffer Architecture**
- **Combined Buffer**: TokenizerBuffer (512) + FunctionOpCodeBuffer (512) = 1024 bytes
- **Trigger Point**: When <512 bytes remain, slide and refill
- **Benefit**: Guarantees any function token stream fits contiguously

### **Key Methods**

```asm
// Set up 1024-byte sliding window, load initial data
// Input: File opened with File.StartLoad()
// Output: LoadBuffer filled, TokenIterator positioned at start
// ZP Variables: LoadBufferContentL/H track current data size
setupSlidingWindowBuffer()
```

```asm
// Check remaining space, refill if needed
// Called before each TokenIterator.NextToken()
// Slides remaining data to front, loads new sectors to fill buffer
checkAndRefillBuffer()
```

```asm
// Move remaining data to buffer start, load new sectors
// Input: LoadBytesToMoveL/H = bytes to preserve
// Uses Memory.Copy for data movement
refillSlidingWindow()
```

### **Buffer Operation Flow**
1. **Initial Load**: Read up to 4 sectors (1024 bytes) into combined buffer
2. **Token Processing**: TokenIterator reads from buffer normally
3. **Refill Trigger**: When <512 bytes remain in buffer
4. **Slide Operation**: Move remaining bytes to buffer start using Memory.Copy
5. **Reload**: Fill rest of buffer with new sectors
6. **Reset Position**: TokenIterator position reset to buffer start

## **Phase 3: Enhanced TokenIterator**

### **Sector-Aware Token Iteration**
Enhance existing TokenIterator to work with sliding window buffer.

```asm
// Get next token from sliding window buffer
// Automatically triggers refill when needed
// Same interface as existing TokenIterator.NextToken()
// Output: ZP.TOKCUR = current token, position advanced
TokenIterator.NextToken()
```

**Implementation Notes:**
- Call `checkAndRefillBuffer()` before each token read
- Handle literal tokens (IDENTIFIER, STRING, NUMBER) with inline data
- Use existing `skipInlineString()` logic - no cross-boundary issues now
- EOF when no more sectors available

## **Phase 4: Declaration Parsers**

### **Parse from TokenIterator**
Create parsers that consume tokens from the sliding window buffer and call existing declaration APIs.

```asm
// Parse single declaration based on current token
// Input: ZP.TOKCUR = declaration token (CONST, BYTE, FUNC, etc.)
// Output: Declaration processed, symbol table updated
parseDeclarationFromTokenIterator()
```

**Token Dispatch:**
- **Token.CONST + type**: → `parseConstantFromTokenIterator()`
- **Token.BYTE/WORD/INT/etc**: → `parseVariableFromTokenIterator()`  
- **Token.FUNC**: → `parseFunctionFromTokenIterator()`
- **Token.BEGIN**: → `parseMainProgramFromTokenIterator()`

### **Function Token Stream Extraction**
Since functions can be up to 512 bytes and buffer is 1024 bytes, function token streams are guaranteed contiguous.

```asm
// Parse function declaration, extract contiguous token stream
// Uses existing Functions.Declare() with allocated token stream copy
parseFunctionFromTokenIterator()
```

**Process:**
1. Mark function start position in buffer
2. Parse to function end (ENDFUNC token)
3. Calculate total function length
4. Use Memory.Allocate() for token stream copy
5. Use Memory.Copy() to copy contiguous tokens
6. Call Functions.Declare() with copied token stream

## **Phase 5: Main Load Infrastructure**

### **Program State Management**

```asm
// Clear all program state (like NEW command)
// Clears variables, functions, locals, execution state
clearProgramState()
```

```asm
// Main entry point for program loading
// Input: ZP.STR = filename
// Output: C set if successful, program state restored
// Error handling: Restore clean state on any failure
Storage.LoadProgram()
```

### **Load Process Flow**
1. **Save Current State**: In case rollback needed
2. **Clear State**: Start with clean slate (Variables.Clear(), Functions.Clear())
3. **Open File**: File.StartLoad() 
4. **Setup Buffer**: Initialize 1024-byte sliding window
5. **Parse Declarations**: Loop through all tokens, dispatch to parsers
6. **Success**: Program loaded and ready
7. **Error Handling**: Restore clean state if any step fails

### **Error Recovery Strategy**
- **Atomic Operation**: Either complete success or clean failure
- **Early Validation**: Check file format before clearing existing state
- **Memory Cleanup**: Free any partially allocated structures on error
- **State Restoration**: Always leave system in consistent state

## **Phase 6: Integration**

### **Console Command Integration**
Add LOAD command parsing to Console.asm:
```asm
// Parse: LOAD "filename"
// Validates filename, calls Storage.LoadProgram()
parseLoad()
```

### **Zero Page Allocation**
Extend File unit zero page allocations for Storage operations:
- LoadBufferContentL/H - current buffer data size
- LoadBytesToMoveL/H - bytes to slide during refill  
- LoadRemainingL/H - remaining bytes calculation

### **Memory Management**
- Use Memory.Copy() for all bulk data movement
- Use Memory.Allocate() for token stream copies
- Ensure all allocations are cleaned up on errors
- Use proper 16-bit arithmetic throughout

## **Testing Strategy**

### **Round-Trip Testing**
1. Create test program with all declaration types
2. Save with existing Storage.SaveProgram()
3. Clear state with NEW
4. Load saved program
5. Verify identical symbol tables and program behavior

### **Edge Case Testing**
- Maximum-size functions (approaching 512 bytes)
- Programs with many small functions
- Cross-sector variable initializations
- File corruption scenarios
- Out-of-memory conditions

### **Implementation Order**
1. **Variables.InitializeToDefault()** - refactor existing code
2. **Sliding window buffer** - core infrastructure
3. **Enhanced TokenIterator** - sector-aware iteration
4. **Simple parsers** - constants and simple variables first
5. **Function parsing** - most complex case
6. **Integration and testing** - console commands, error handling

This architecture leverages existing infrastructure while providing robust file loading with proper error handling and memory management.