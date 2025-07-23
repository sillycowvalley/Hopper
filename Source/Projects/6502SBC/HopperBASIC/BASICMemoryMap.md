## HopperBASIC Memory Layout

### Buffer and Workspace Allocation

| Address Range | Size | Buffer Name | Purpose |
|---------------|------|-------------|---------|
| $0200-$02FF | 256 bytes | SerialInBuffer | Serial input buffer (Hopper VM) |
| $0300-$03FF | 256 bytes | CallStackLSB | Call stack low bytes (Hopper VM) |
| $0400-$04FF | 256 bytes | CallStackMSB | Call stack high bytes (Hopper VM) |
| $0500-$05FF | 256 bytes | TypeStackLSB | Type stack (Hopper VM) |
| $0600-$06FF | 256 bytes | ValueStackLSB | Value stack low bytes (Hopper VM) |
| $0700-$07FF | 256 bytes | ValueStackMSB | Value stack high bytes (Hopper VM) |
| $0800-$08FF | 256 bytes | I2CInBuffer | I2C/EEPROM buffer (Hopper VM) |
| $0900-$097F | 128 bytes | BasicInputBuffer | Raw user input line storage |
| $0980-$09BF | 64 bytes | BasicProcessBuffer1 | Variable/general workspace |
| $09C0-$09DF | 32 bytes | BasicProcessBuffer2 | Secondary workspace |
| $09E0-$09FF | 32 bytes | BasicProcessBuffer3 | Tertiary workspace |
| $0A00-$0AFF | 256 bytes | BasicTokenizerBuffer | Token processing workspace (full page) |
| $0B00+ | Dynamic | HopperData | Dynamic heap (programs, variables, functions) |

### Buffer Layout Design

**Page Boundary Optimization**: All buffers are arranged to avoid crossing page boundaries for better 6502 performance:

- **Page $09**: Mixed-size buffers (128 + 64 + 32 + 32 = 256 bytes total)
  - Input buffer for user commands
  - Three workspace buffers for processing
- **Page $0A**: Single 256-byte tokenizer buffer (full page)
  - Dedicated token processing workspace
  - No boundary crossings during string operations

### Buffer Usage Notes

- **BasicInputBuffer**: Stores raw command line input as user types
- **BasicTokenizerBuffer**: Workspace for tokenizing each line before heap allocation
- **BasicProcessBuffer1-3**: Multi-purpose workspace for variable processing, expression parsing, and temporary operations
- **HopperData**: Dynamic heap managed by Hopper VM memory allocator for program storage, variable tables, and function definitions

### Multi-Line Function Handling

**Design Decision**: The original 512-byte `BasicFunctionDefBuffer` has been eliminated in favor of line-by-line parsing:

1. **Each line** is read into `BasicInputBuffer` (128 bytes)
2. **Tokenized** using `BasicTokenizerBuffer` (256 bytes)  
3. **Parsed and stored directly** into the heap at its final location
4. **Buffers reused** for the next line

This approach:
- **Saves 512 bytes** of precious RAM
- **Simplifies** the parsing pipeline
- **Maintains full multi-line function capability**
- **Eliminates** redundant buffering steps

**Example Function Definition Flow**:
```
FUNC AddNumbers(a, b)     // Line 1: Parse, create function header in heap
    result = a + b        // Line 2: Parse, append statement to function body
    RETURN result         // Line 3: Parse, append return statement  
ENDFUNC                   // Line 4: Parse, finalize function definition
```

### Memory Savings Summary

- **Before**: 1024 bytes for HopperBASIC-specific buffers
- **After**: 512 bytes for HopperBASIC-specific buffers  
- **Savings**: 512 bytes (50% reduction)
- **Available Dynamic Memory**: Significantly increased

**Total Fixed Buffer Usage**: 512 bytes for HopperBASIC-specific buffers  
**Available Dynamic Memory**: Depends on system RAM size minus fixed allocations