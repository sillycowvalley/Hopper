## HopperBASIC Memory Layout (Refactored)

### Buffer and Workspace Allocation

| Address Range | Size | Buffer Name | Purpose |
|---------------|------|-------------|---------|
| 0x0200-0x02FF | 256 bytes | SerialInBuffer | Serial input buffer (Hopper VM) |
| 0x0300-0x03FF | 256 bytes | CallStackLSB | Call stack low bytes (Hopper VM) |
| 0x0400-0x04FF | 256 bytes | CallStackMSB | Call stack high bytes (Hopper VM) |
| 0x0500-0x05FF | 256 bytes | TypeStackLSB | Type stack (Hopper VM) |
| 0x0600-0x06FF | 256 bytes | ValueStackLSB | Value stack low bytes (Hopper VM) |
| 0x0700-0x07FF | 256 bytes | ValueStackMSB | Value stack high bytes (Hopper VM) |
| 0x0800-0x08FF | 256 bytes | I2CInBuffer | I2C/EEPROM buffer (Hopper VM) |
| 0x0900-0x097F | 128 bytes | BasicInputBuffer | Raw user input line storage |
| 0x0980-0x09BF | 64 bytes | BasicProcessBuffer1 | Variable/general workspace |
| 0x09C0-0x09DF | 32 bytes | BasicProcessBuffer2 | Secondary workspace |
| 0x09E0-0x09FF | 32 bytes | BasicProcessBuffer3 | Tertiary workspace |
| 0x0A00-0x0BFF | 512 bytes | BasicTokenizerBuffer | **Tokenized multi-line storage** |
| 0x0E00+ | Dynamic | HopperData | Dynamic heap (programs, variables, functions) |

### Key Architecture Changes

**New Token Buffer Format:**
- **BasicTokenizerBuffer** (512 bytes): Stores complete tokenized line
- **Token Format**: Simple byte stream with inline literals
- **Example**: `[PRINT][NUMBER]["123"][0][PLUS][NUMBER]["456"][0][EOL]`

**Unified Processing:**
1. **ReadLine()**: Raw input → BasicInputBuffer
2. **TokenizeLine()**: Complete line → BasicTokenizerBuffer  
3. **NextToken()**: Steps through tokens in buffer
4. **Execute()**: Same code path for REPL and function definition

**Zero Page Changes:**
- **TokenBufferLength** (16-bit): Length of tokens in BasicTokenizerBuffer
- **TokenizerPos** (16-bit): Current position in token buffer
- **Removed**: Character-by-character position tracking

**Benefits:**
- **Function Definition**: Can accumulate multiple tokenized lines
- **Lookahead**: Can examine tokens before consuming them
- **Error Recovery**: Full line context available
- **Debugging**: Complete token stream visible in buffer dumps

**Total Fixed Buffer Usage**: 1280 bytes for HopperBASIC-specific buffers  
**Available Dynamic Memory**: Depends on system RAM size minus fixed allocations