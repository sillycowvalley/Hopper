# HopperBASIC Memory Dump Analysis Guide
**Document Type: Dump and Decode Guide**

## Critical First Step: Get Current Source

**ALWAYS search project knowledge FIRST** before attempting any analysis:

1. Search `"tokenizer.asm unit program"` for latest token enum values
2. Search `"OpCodes.asm enum OpCodeType"` for opcode definitions  
3. Search `"BASICSysCalls.asm enum SysCallType"` for OpCode.SYSCALL sub-opcode definitions  
4. Search `"ZeroPage.asm HOPPER_BASIC ifdef"` for current ZP layout
5. Search `"Objects.asm structure node offset"` for heap object structure
6. Search `"Functions.asm Variables.asm"` for symbol table specifics
7. Search `"Zero Page Map"` for additional Zero Page documentation

**Rule**: Project knowledge is authoritative and current. Never use cached/remembered values.

## Buffer Type Identification

### TokenizerBuffer Format
```
TokPos: XXXX TokBufLen: XXXX CurTok: XX
```
- **TokPos**: Current position in token buffer (16-bit)
- **TokBufLen**: Total length of tokenized data (16-bit) 
- **CurTok**: Current token value (8-bit, see Tokens enum)

### OpCodeBuffer Format  
```
OpCodeLen: XXXX PC: XXXX
```
- **OpCodeLen**: Length of compiled opcodes (16-bit)
- **PC**: Program counter - execution position (16-bit)

### Heap Dump Format
```
VL:XXXX FL:XXXX
[NN] XXXX:SSSS (USED/FREE) [hex bytes...] [ASCII...]
```
- **VL**: Variables List head pointer (16-bit)
- **FL**: Functions List head pointer (16-bit)  
- **Each entry**: Address, size, status, hex data, ASCII view

## TokenizerBuffer Decoding

### Token Identification Process
1. **Get current Tokens enum** from latest Tokenizer.asm
2. **For each token byte**: Look up value in enum
3. **Handle inline data**: These tokens have null-terminated strings following:
   - `NUMBER` (0xD2) - followed by number string like "42\0"
   - `IDENTIFIER` (0xD4) - followed by identifier name  
   - `STRINGLIT` (0xD3) - followed by string content
   - `REM` (0x92) - followed by comment text
   - `COMMENT` (0x93) - followed by comment text

### Table Format
```markdown
| Offset | Hex | Token | Description | Inline Data |
|--------|-----|-------|-------------|-------------|
| 0A00 | D2 | NUMBER | Numeric literal | "20" |
| 0A01-0A03 | 32 30 00 | - | Inline string data | - |
| 0A04 | 94 | EOL | End of line marker | - |
```

## OpCodeBuffer Decoding

### OpCode Structure Analysis
1. **Get current OpCodeType enum** from OpCodes.asm  
2. **Operand count encoding** (bits 7-6):
   - `00` (0x00-0x3F): No operands
   - `01` (0x40-0x7F): One byte operand  
   - `10` (0x80-0xBF): Two byte operands
   - `11` (0xC0-0xFF): Reserved/variable length

### Table Format
```markdown
| Offset | Hex | OpCode | Operands | Description |
|--------|-----|--------|----------|-------------|
| 0C00 | 19 | PUSHVOID | None | Push VOID 0 value |
| 0C01 | 83 | PUSHWORD | 01 0A | Push word 2561 (0x0A01) |
```

## Heap Object Analysis

### Object Structure Reference
From Objects.asm - **Variable/Constant Node**:
```
Offset 0-1: next pointer  
Offset 2:   symbolType|dataType (packed byte)
Offset 3-4: tokens pointer (16-bit)
Offset 5-6: value (16-bit) 
Offset 7-8: opcode stream pointer (16-bit) - unused for variables
Offset 9+:  null-terminated name string
```

From Functions.asm - **Function Node**:
```
Offset 0-1: next pointer
Offset 2:   function flags byte (FunctionFlags enum)
Offset 3-4: function body tokens pointer (16-bit)
Offset 5-6: arguments list head pointer (16-bit)
Offset 7-8: opcode stream pointer (16-bit)
Offset 9+:  null-terminated name string
```

From Locals.asm - **Argument/Local Node**:
```
Offset 0-1: next pointer
Offset 2:   symbolType|dataType (packed byte)
Offset 3:   BP offset (signed byte: negative for args, positive for locals)
Offset 4+:  null-terminated name
```

### Heap Analysis Process
1. **Start with VL/FL pointers** from dump header
2. **Follow linked lists** using next pointers at offset 0-1
3. **Identify object types**:
   - **Small objects (8-12 bytes)**: Usually token streams or argument nodes
   - **Medium objects (16-20 bytes)**: Variable nodes, function nodes  
   - **Large objects (24+ bytes)**: Function nodes with long names
4. **Decode based on size and context**

### Table Format
```markdown
| Object | Address | Size | Type Analysis | Content Breakdown |
|--------|---------|------|---------------|-------------------|
| **0E0A** | 0E0A | 18 bytes | **Variable Node** | Variable "C" |
| **0E02** | 0E02 | 8 bytes | **Token Stream** | NUMBER "20" + EOL |
```

### Detailed Breakdown Tables
```markdown
| Variable | Node Addr | Next→ | Type | Value | Tokens→ | OpCodes→ | Name |
|----------|-----------|-------|------|-------|---------|----------|------|
| **C** | 0E0A | 0E22 | VAR+INT (0x21) | 0 | 0E02 | 0000 | "C" |
```

```markdown
| Function | Node Addr | Next→ | Flags | Body→ | Args→ | OpCodes→ | Name |
|----------|-----------|-------|-------|-------|-------|----------|------|
| **FOO** | 0E4A | 0000 | 0x01 | 0E3A | 0E62 | 0E80 | "FOO" |
```

## Zero Page Analysis

### Current Zero Page Layout (Updated for 2025)
From ZeroPage.asm - **BASIC-specific allocations**:

#### Core VM Registers (0x00-0x1F) - IMMOVABLE
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x00-0x01 | PC/PCL/PCH | Program counter |
| 0x02 | FLAGS | System flags register |
| 0x03-0x05 | SP/BP/CSP | Stack pointers |
| 0x06-0x07 | FREELIST | Heap free list pointer |
| 0x08-0x09 | HEAPSTART/HEAPSIZE | Heap parameters |
| 0x0A-0x0C | SerialIn* | Serial buffer management |
| 0x0D | TraceIndent | Debug trace indentation |
| 0x0E-0x0F | ACC/ACCL/ACCH | Accumulator register |
| 0x10-0x11 | TOP/TOPL/TOPH | Top of stack value |
| 0x12-0x13 | NEXT/NEXTL/NEXTH | Next stack value |
| 0x14-0x15 | IDX/IDXL/IDXH | Index X register |
| 0x16-0x17 | IDY/IDYL/IDYH | Index Y register |
| 0x18-0x1A | ACCT/TOPT/NEXTT | Type bytes |
| 0x1B-0x1C | I2CIn* | I2C buffer pointers |
```

#### Timer Interface (0x1D-0x22) - IMMOVABLE
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x1D-0x20 | TICK0-3 | Timer tick counter (LSB-MSB) |
| 0x21-0x22 | EmulatorPC | BIT $21 captures PC for debug |
```

#### Serial & Misc Workspace (0x23-0x24)
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x23 | WorkSpaceHexIn | Serial.asm hex input workspace |
| 0x24 | WorkSpaceWaitForChar | Serial.asm wait workspace |
```

#### BASIC Core (0x25-0x34) - Current BASIC allocation
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x25 | BasicInputLength | Input buffer character count |
| 0x26-0x27 | TokenBufferLength | Token buffer size (16-bit) |
| 0x28-0x29 | TokenizerPos | Current tokenizer position (16-bit) |
| 0x2A-0x2B | LastError | Error message pointer (16-bit) |
| 0x2C | CurrentToken | Cached current token type |
| 0x2D-0x2E | TokenLiteralPos | Literal data position (16-bit) |
| 0x2F-0x30 | OpCodeBufferLength | JIT buffer length (16-bit) |
| 0x31-0x32 | CompilerTokenPos | Compiler position (16-bit) |
| 0x33 | CompilerFlags | Compilation state flags |
| 0x34 | OpCodeTemp | Temporary opcode construction |
```

#### Buffer Pointers (0x35-0x38)
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x35-0x36 | TokenBuffer | Current tokenizer buffer pointer |
| 0x37-0x38 | OpCodeBuffer | Current opcode buffer pointer |
```

#### Symbol Table Management (0x39-0x48)
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x39-0x3A | VariablesList | Variables table head pointer |
| 0x3B-0x3C | FunctionsList | Functions table head pointer |
| 0x3D | SymbolType | Current symbol type|datatype |
| 0x3E-0x3F | SymbolValue | Symbol value (16-bit) |
| 0x40-0x41 | SymbolName | Symbol name pointer (16-bit) |
| 0x42-0x43 | SymbolTokens | Symbol tokens pointer (16-bit) |
| 0x44 | SymbolIteratorFilter | Symbol iteration filter |
| 0x45 | SymbolLength | Symbol name length |
| 0x46-0x48 | SymbolTemp0-2 | Symbol workspace |
```

#### Debug & State (0x49-0x4B)
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x49-0x4A | TraceMessage | Trace message pointer (16-bit) |
| 0x4B | SystemState | Success/Failure/Exiting state |
```

#### Shared Leaf Function Workspace (0x4C-0x5B) - M0-M15
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x4C-0x5B | M0-M15 | Multi-use workspace (16 bytes) |
```

**Critical**: These variables are shared between leaf functions that never call each other:
- **Memory.Allocate/Free**: Uses M0-M15
- **Debug.asm**: Uses M0-M15 (aliased as DB0-DB15) 
- **Time.Delay()**: Uses M0-M3 (aliased as TARGET0-3)
- **Time.Seconds()**: Uses M0-M7 (aliased as LRESULT0-7)
- **IntMath**: Uses M0-M3 (aliased as UWIDE4-7)

#### Function Parameter Workspace (0x5C-0x77)
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x5C-0x5D | FSOURCEADDRESS | Source address parameter |
| 0x5E-0x5F | FDESTINATIONADDRESS | Destination address parameter |
| 0x60-0x61 | FLENGTH | Length parameter |
| 0x62-0x63 | LCURRENT | List current pointer |
| 0x64-0x65 | LHEAD | List head pointer |
| 0x66 | FSIGN | Sign flag for math operations |
| 0x67 | LHEADX | List head extension |
| 0x68-0x69 | LPREVIOUS | List previous pointer |
| 0x6A-0x6B | LNEXT | List next pointer |
| 0x6C-0x6F | UWIDE0-3 | IntMath 32-bit multiply workspace |
| 0x70-0x73 | LNEXT0-3 | Long math next operand |
| 0x74-0x75 | LTOP0-3 | Long math top operand (partial) |
| 0x76-0x77 | STR/STRL/STRH | String pointer |
```

#### Extended Workspace (0x78-0x79)
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0x78-0x79 | STR2/STR2L/STR2H | String pointer 2 |
```

#### Available Space (0x7A-0xEB) - 114 bytes available!
This is a significant expansion from the previous layout.

#### Hardware I/O (0xEC-0xFF) - IMMOVABLE
```markdown
| Address | Variable | Description |
|---------|----------|-------------|
| 0xEC | ACIACONTROL/ACIASTATUS | 6850 ACIA control/status |
| 0xED | ACIADATA | 6850 ACIA data register |
| 0xF0-0xF3 | PORTB/PORTA/DDRB/DDRA | VIA GPIO ports |
| 0xF4-0xF9 | T1CL/T1CH/T1LL/T1LH/T2CL/T2CH | VIA Timer registers |
| 0xFA-0xFF | SR/ACR/PCR/IFR/IER/ORA_NO_HANDSHAKE | VIA control registers |
```

### Multi-byte Values
- **16-bit values**: LSB first (little-endian)
- **Example**: Bytes `0A 0E` = 0x0E0A = 3594

### Table Format
```markdown
| Address | Variable | Value | Description |
|---------|----------|-------|-------------|
| 0x39-0x3A | VariablesList | 0x0E0A | Variables table head |
| 0x3B-0x3C | FunctionsList | 0x0E4A | Functions table head |
```

## Data Type Decoding

### SymbolType|DataType Packed Byte (NEW FORMAT - 2025)
The packed byte format uses:
- **Top 3 bits (mask 0xE0)**: SymbolType 
- **Bottom 5 bits (mask 0x1F)**: BASICType

**No shifting required** - values are directly comparable after masking.

### SymbolType Values (from Objects.asm)
```
VARIABLE = 0x20   // Mutable values
CONSTANT = 0x40   // Immutable values
FUNCTION = 0x60   // Executable code blocks
ARGUMENT = 0x80   // Function parameters (negative BP offset)
LOCAL    = 0xA0   // Local variables (positive BP offset)
MASK     = 0xE0   // Top 3 bits
```

### BASICType Values (from BasicTypes.asm)
```
VOID   = 0x00   // Function return type (internal use)
INT    = 0x01   // Signed 16-bit integer
BYTE   = 0x02   // Unsigned 8-bit value
WORD   = 0x03   // Unsigned 16-bit value
BIT    = 0x04   // Boolean value (0 or 1)
ARRAY  = 0x05   // Array type
STRING = 0x06   // String type
VAR    = 0x10   // Runtime-determined type bit
MASK   = 0x1F   // Bottom 5 bits
```

### Decoding Examples
```markdown
| Packed Byte | SymbolType (top 3 bits) | BASICType (bottom 5 bits) | Meaning |
|-------------|--------------------------|---------------------------|---------|
| 0x21 | VARIABLE (0x20) | INT (0x01) | INT variable |
| 0x42 | CONSTANT (0x40) | BYTE (0x02) | BYTE constant |
| 0x26 | VARIABLE (0x20) | STRING (0x06) | STRING variable |
| 0x61 | FUNCTION (0x60) | INT (0x01) | Function returning INT |
| 0x81 | ARGUMENT (0x80) | INT (0x01) | INT argument |
| 0xA3 | LOCAL (0xA0) | WORD (0x03) | WORD local variable |
```

### Extraction Process
```assembly
; To extract SymbolType:
LDA packed_byte
AND #0xE0        ; Mask top 3 bits
CMP #SymbolType.VARIABLE  ; Direct comparison (0x20)

; To extract BASICType:
LDA packed_byte
AND #0x1F        ; Mask bottom 5 bits
CMP #BASICType.INT        ; Direct comparison (0x01)
```

## Linked List Following

### Variables List Chain
1. **Start**: VariablesList pointer (0x39-0x3A)
2. **Follow**: next pointer at offset 0-1 of each node
3. **Terminate**: When next pointer = 0x0000

### Functions List Chain  
1. **Start**: FunctionsList pointer (0x3B-0x3C)
2. **Follow**: next pointer at offset 0-1 of each node
3. **Terminate**: When next pointer = 0x0000

### Arguments/Locals List Chain (per function)
1. **Start**: Arguments pointer from function node offset 5-6
2. **Follow**: next pointer at offset 0-1 of each argument/local node
3. **Terminate**: When next pointer = 0x0000

## Token Stream Decoding

### Token-by-Token Analysis
1. **Read token byte**: Compare against Tokens enum
2. **Check for inline data**: NUMBER, IDENTIFIER, STRINGLIT, REM, COMMENT
3. **Skip past strings**: Find null terminator (0x00)
4. **Continue until**: EOL (0x94) or null terminator

### Key Token Values (from Tokenizer.asm - check current source!)
Common tokens include:
- Control: NEW, LIST, RUN, CLEAR, VARS, FUNCS
- Types: INT, WORD, BIT, BYTE, STRING, CONST, VAR
- Statements: PRINT, INPUT, IF, THEN, FUNC, ENDFUNC
- Flow: FOR, TO, STEP, NEXT, WHILE, WEND, DO, UNTIL
- Operators: EQUALS, PLUS, MINUS, MULTIPLY, DIVIDE
- Literals: NUMBER, STRINGLIT, IDENTIFIER, TRUE, FALSE

### Example Pattern
```
D2 32 30 00 94 = NUMBER "20" + EOL
```

## Function Flags Analysis

### FunctionFlags enum (from Functions.asm)
- **None = 0x00**: Token stream only, not compiled
- **Compiled = 0x01**: Has compiled opcodes  
- **NotCompiledMask = 0xFE**: Mask to clear compiled flag
- **Bits 1-7**: Reserved for future use (optimization flags, etc.)

## Output Formatting Rules

### Use Actual Addresses
- **Object references**: Use real heap addresses (0E0A, not [01])
- **Pointer values**: Show actual memory addresses
- **Linked lists**: Display real address chains

### Table Organization
- **Summary table**: High-level object identification
- **Detail tables**: Specific breakdowns by type
- **Linked structure**: Show actual address relationships

### ASCII Interpretation
- **Token strings**: Show decoded content ("20", "FOO", etc.)
- **Function names**: Extract from node name fields
- **Variable names**: Extract from node name fields

## Common Patterns

### Variable Declaration Sequence
1. **Token stream created**: Contains initialization expression
2. **Variable node created**: Points to token stream (offset 3-4)
3. **Added to list**: Linked into VariablesList chain (0x39-0x3A)

### Function Definition Sequence  
1. **Function node created**: Basic structure with flags=0x00
2. **Token stream created**: Contains function body tokens
3. **Arguments added**: Separate linked list (offset 5-6 in function node)
4. **Compilation**: Opcodes generated, flags updated to 0x01, opcode pointer set (offset 7-8)

### Memory Consistency Checks
- **VL/FL pointers**: Must match heap object addresses
- **Next pointers**: Must form valid chains or be NULL
- **Token pointers**: Must point to valid heap objects
- **Opcode pointers**: Must point to valid heap objects or be NULL
- **Size validation**: Object sizes must match content

## Error Patterns to Watch For

### Corrupted Links
- **Circular references**: Next pointer chains that loop
- **Invalid addresses**: Pointers outside heap range
- **Misaligned objects**: Addresses not matching heap structure

### Type Inconsistencies  
- **Wrong object sizes**: 8-byte objects claiming to be 18-byte nodes
- **Invalid type values**: SymbolType/BASICType outside valid ranges
- **Mismatched pointers**: Token pointers not pointing to token streams
- **Invalid flags**: Function flags with reserved bits set

### Buffer Mismatches
- **TokenizerBuffer vs Heap**: Different token content suggests separate operations
- **OpCode buffer vs Functions**: PC not matching function opcode ranges
- **Zero page vs Heap**: List pointers not matching heap structure

## New Features to Analyze

### JIT Compilation Support
- **Function flags**: Check bit 0 for compiled status
- **Opcode streams**: Follow offset 7-8 pointers in function nodes
- **OpCode buffer**: 512 bytes at 0x0C00-0x0DFF for active compilation

### Enhanced Node Structure  
- **Opcode pointers**: All nodes now have offset 7-8 for future expansion
- **Function separation**: Clear distinction between variables and functions
- **Argument/Local lists**: Proper linked list structure with BP offsets

### Zero Page Optimization
- **114 bytes available**: Large expansion at 0x7A-0xEB
- **Shared workspace**: M0-M15 efficiently shared between leaf functions
- **Clean separation**: BASIC core uses 0x25-0x48, shared workspace at 0x4C-0x77

### Important Changes for 2025
- **Packed type format**: Top 3 bits for SymbolType, bottom 5 for BASICType
- **No shifting needed**: Direct comparison after masking
- **VAR type bit**: New BASICType.VAR (0x10) for runtime-determined types
- **Extended types**: Room for 32 BASICTypes with 5-bit field

Remember: **Always verify consistency** between related structures and use **latest source definitions** from project knowledge. The new packed format simplifies type checking by eliminating shifts while providing more room for type expansion.