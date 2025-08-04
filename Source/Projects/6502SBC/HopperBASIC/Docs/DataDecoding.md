# HopperBASIC Memory Dump Analysis Guide
**Document Type: Debug Analysis Guide**

## Critical First Step: Get Current Source

**ALWAYS search project knowledge FIRST** before attempting any analysis:

1. Search `"tokenizer.asm unit program"` for latest token enum values
2. Search `"OpCodes.asm enum OpCodeType"` for opcode definitions  
3. Search `"ZeroPage.asm HOPPER_BASIC ifdef"` for current ZP layout
4. Search `"Objects.asm structure node offset"` for heap object structure
5. Search `"Functions.asm Variables.asm"` for symbol table specifics
6. Search `"Zero Page Map"` for additional Zero Page documentation

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
   - `NUMBER` (0xC7) - followed by number string like "42\0"
   - `IDENTIFIER` (0xC9) - followed by identifier name  
   - `STRINGLIT` (0xC8) - followed by string content
   - `REM` (0x92) - followed by comment text
   - `COMMENT` (0x93) - followed by comment text

### Table Format
```markdown
| Offset | Hex | Token | Description | Inline Data |
|--------|-----|-------|-------------|-------------|
| 0A00 | C7 | NUMBER | Numeric literal | "20" |
| 0A01-0A02 | 32 30 00 | - | Inline string data | - |
| 0A03 | 94 | EOL | End of line marker | - |
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
Offset 7-8: unused for variables
Offset 9+:  null-terminated name string
```

From Functions.asm - **Function Node**:
```
Offset 0-1: next pointer
Offset 2:   function flags byte  
Offset 3-4: function body tokens pointer
Offset 5-6: arguments list head pointer
Offset 7-8: opcode stream pointer  
Offset 9+:  null-terminated name string
```

### Heap Analysis Process
1. **Start with VL/FL pointers** from dump header
2. **Follow linked lists** using next pointers at offset 0-1
3. **Identify object types**:
   - **Small objects (8 bytes)**: Usually token streams
   - **Medium objects (16 bytes)**: Variable nodes, function nodes  
   - **Large objects (24+ bytes)**: Function nodes with long names
4. **Decode based on size and context**

### Table Format
```markdown
| Object | Address | Size | Type Analysis | Content Breakdown |
|--------|---------|------|---------------|-------------------|
| **0E0A** | 0E0A | 16 bytes | **Variable Node** | Variable "C" |
| **0E02** | 0E02 | 8 bytes | **Token Stream** | NUMBER "20" + EOL |
```

### Detailed Breakdown Tables
```markdown
| Variable | Node Addr | Next→ | Type | Value | Tokens→ | Name |
|----------|-----------|-------|------|-------|---------|------|
| **C** | 0E0A | 0E22 | INT (0x12) | 0 | 0E02 | "C" |
```

## Zero Page Analysis

### Variable Identification Process  
1. **Search ZeroPage.asm** for `#ifdef HOPPER_BASIC` section
2. **Focus on BASIC-specific allocations**:
   - 0x30-0x4F: BASIC project allocation
   - 0x70-0x7F: Symbol table allocation  
   - 0xF0-0xF4: System state variables
3. **Group by functionality** (tokenizer, compiler, symbol table, etc.)

### Multi-byte Values
- **16-bit values**: LSB first (little-endian)
- **Example**: Bytes `0A 0E` = 0x0E0A = 3594

### Table Format
```markdown
| Address | Variable | Value | Description |
|---------|----------|-------|-------------|
| 0x70-0x71 | VariablesList | 0x0E0A | Variables table head |
| 0x72-0x73 | FunctionsList | 0x0E4A | Functions table head |
```

## Data Type Decoding

### SymbolType|DataType Packed Byte
- **High nibble**: SymbolType (VARIABLE=1, CONSTANT=2, FUNCTION=3)
- **Low nibble**: BasicType (INT=2, WORD=4, BIT=6, BYTE=3, STRING=0xF)
- **Example**: 0x12 = VARIABLE(1) + INT(2)

### BasicType Values (from BasicTypes.asm)
```
VOID = 0x00, INT = 0x02, BYTE = 0x03, WORD = 0x04, 
BIT = 0x06, STRING = 0x0F, ARRAY = 0x12
```

## Linked List Following

### Variables List Chain
1. **Start**: VL pointer from heap header
2. **Follow**: next pointer at offset 0-1 of each node
3. **Terminate**: When next pointer = 0x0000

### Functions List Chain  
1. **Start**: FL pointer from heap header
2. **Follow**: next pointer at offset 0-1 of each node
3. **Terminate**: When next pointer = 0x0000

## Token Stream Decoding

### Token-by-Token Analysis
1. **Read token byte**: Compare against Tokens enum
2. **Check for inline data**: NUMBER, IDENTIFIER, STRINGLIT, REM, COMMENT
3. **Skip past strings**: Find null terminator (0x00)
4. **Continue until**: EOL (0x94) or null terminator

### Example Pattern
```
C7 32 30 00 94 = NUMBER "20" + EOL
```

## Function Flags Analysis

### FunctionFlags enum (from Functions.asm)
- **None = 0x00**: Token stream only
- **Compiled = 0x01**: Has compiled opcodes  
- **Others**: Reserved for future use

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
2. **Variable node created**: Points to token stream
3. **Added to list**: Linked into VariablesList chain

### Function Definition Sequence  
1. **Function node created**: Basic structure  
2. **Token stream created**: Contains function body
3. **Arguments added**: Separate linked list
4. **Compilation**: Opcodes generated, flags updated

### Memory Consistency Checks
- **VL/FL pointers**: Must match heap object addresses
- **Next pointers**: Must form valid chains or be NULL
- **Token pointers**: Must point to valid heap objects
- **Size validation**: Object sizes must match content

## Error Patterns to Watch For

### Corrupted Links
- **Circular references**: Next pointer chains that loop
- **Invalid addresses**: Pointers outside heap range
- **Misaligned objects**: Addresses not matching heap structure

### Type Inconsistencies  
- **Wrong object sizes**: 8-byte objects claiming to be 16-byte nodes
- **Invalid type values**: SymbolType/BasicType outside valid ranges
- **Mismatched pointers**: Token pointers not pointing to token streams

### Buffer Mismatches
- **TokenizerBuffer vs Heap**: Different token content suggests separate operations
- **OpCode buffer vs Functions**: PC not matching function opcode ranges
- **Zero page vs Heap**: List pointers not matching heap structure

Remember: **Always verify consistency** between related structures and use **latest source definitions** from project knowledge.