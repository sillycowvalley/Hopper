## HopperBASIC Memory Layout (Current)
**Document Type: Memory Layout**

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
| 0x0980-0x099F | 32 bytes | BasicCompilerWorkspace | Compiler state workspace |
| 0x09A0-0x09BF | 32 bytes | BasicStatementWorkspace | Statement layer storage |
| 0x09C0-0x09DF | 32 bytes | BasicExecutorWorkspace | Executor state workspace |
| 0x09E0-0x09FF | 32 bytes | BasicProcessBuffer | Tokenizer workspace (uppercasing) |
| 0x0A00-0x0BFF | 512 bytes | BasicTokenizerBuffer | **Tokenized multi-line storage** |
| 0x0C00-0x0DFF | 512 bytes | BasicOpCodeBuffer | **JIT compiled opcodes buffer** |
| 0x0E00+ | Dynamic | HopperData | Dynamic heap (programs, variables, functions) |

### Key Architecture Evolution

**Enhanced Token Buffer Format:**
- **BasicTokenizerBuffer** (512 bytes): Stores complete tokenized line/function
- **Token Format**: Simple byte stream with inline literals
- **Example**: `[PRINT][NUMBER]["123"][0][PLUS][NUMBER]["456"][0][EOL]`

**JIT Compilation System:**
- **BasicOpCodeBuffer** (512 bytes): Temporary opcode compilation workspace
- **Dynamic Allocation**: Compiled opcodes stored in heap for permanent execution
- **Function Flags**: Track compilation state in function nodes

**Unified Processing Pipeline:**
1. **ReadLine()**: Raw input → BasicInputBuffer
2. **TokenizeLine()**: Complete line → BasicTokenizerBuffer  
3. **NextToken()**: Steps through tokens in buffer
4. **Compile()**: Tokens → BasicOpCodeBuffer (temporary)
5. **Store()**: Opcodes → Dynamic heap allocation (permanent)
6. **Execute()**: Direct execution from compiled opcodes

**Zero Page Changes:**
- **TokenBufferLength** (16-bit): Length of tokens in BasicTokenizerBuffer
- **TokenizerPos** (16-bit): Current position in token buffer
- **OpCodeBufferLength** (16-bit): Length of compiled opcodes
- **CompilerTokenPos** (16-bit): Token position during compilation
- **CompilerFlags**: Compilation state flags
- **SystemState**: Universal success/failure/exit state tracking

**Symbol Table System:**
- **VariablesList** (16-bit): Head pointer for variables and constants
- **FunctionsList** (16-bit): Head pointer for functions and arguments
- **Symbol Node Layout**: Next(2) + Type(1) + Tokens(2) + Value(2) + OpCodes(2) + Name(variable)

**Benefits:**
- **Function Definition**: Can accumulate multiple tokenized lines
- **JIT Compilation**: Fast execution of compiled opcodes
- **Lookahead**: Can examine tokens before consuming them
- **Error Recovery**: Full line context available
- **Debugging**: Complete token and opcode streams visible
- **State Management**: Unified error and execution state tracking

**Total Fixed Buffer Usage**: 2432 bytes for HopperBASIC-specific buffers  
**Available Dynamic Memory**: Depends on system RAM size minus fixed allocations

### Platform-Specific RAM Limits

| Platform | Total RAM | HopperData Start | Available Heap |
|----------|-----------|------------------|----------------|
| BENEATER_IO | 20KB | 0x0E00 | ~16.5KB |
| PD6502 | 40KB | 0x0E00 | ~36.5KB |
| Default | 32KB | 0x0E00 | ~28.5KB |

### Buffer Size Limits (from Limits.asm)

| Buffer | Size Constant | Actual Size |
|--------|---------------|-------------|
| BasicInputLength | 128 bytes | Raw input line limit |
| BasicTokenizerBufferLength | 512 bytes | Tokenized storage limit |
| BasicOpCodeBufferLength | 512 bytes | Compiled opcodes workspace |
| BasicCompilerWorkspaceLength | 32 bytes | Compiler state storage |
| BasicStatementWorkspaceLength | 32 bytes | Statement processing |
| BasicExecutorWorkspaceLength | 32 bytes | Execution state |
| BasicProcessBufferLength | 32 bytes | General workspace |