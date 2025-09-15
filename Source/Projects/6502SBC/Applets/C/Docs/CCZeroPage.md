## Zero Page Slot Usage Table for C Compiler

| Address | Size | Unit | Symbol | Description |
|---------|------|------|--------|-------------|
| **0x60-0x68** | | | **Runtime Slots** | *Used during program execution* |
| 0x60 | 1 | CodeGen | runtimeBP | Base pointer for stack frame |
| 0x61-0x62 | 2 | CodeGen | runtimeStack0/L/H | Runtime stack pointer 0 |
| 0x63-0x64 | 2 | CodeGen | runtimeStack1/L/H | Runtime stack pointer 1 |
| 0x65-0x66 | 2 | CodeGen | runtimeStack2/L/H | Runtime stack pointer 2 |
| 0x67-0x68 | 2 | CodeGen | runtimeStack3/L/H | Runtime stack pointer 3 |
| | | | | |
| **0x60-0x63** | | | **CC Main** | *Overlaps runtime - used during compilation only* |
| 0x60-0x61 | 2 | CC | sourceName/L/H | Source filename pointer |
| 0x62-0x63 | 2 | CC | outputName/L/H | Output filename pointer |
| | | | | |
| **0x70-0x7B** | | | **Lexer** | |
| 0x70-0x71 | 2 | Lexer | currentLine/L/H | Line number for error reporting |
| 0x72 | 1 | Lexer | currentChar | Current character being processed |
| 0x73 | 1 | Lexer | peekChar | Lookahead character |
| 0x74-0x75 | 2 | Lexer | TokenBuffer/L/H | Pointer to token string buffer |
| 0x76 | 1 | Lexer | tokenLength | Current token length |
| 0x77 | 1 | Lexer | TokenType | Current token type |
| 0x78-0x79 | 2 | Lexer | TokenValue/L/H | Token value (for numbers) |
| 0x7A-0x7B | 2 | Lexer | bufferIndex/L/H | Index into FileDataBuffer |
| | | | | |
| **0x80-0x85** | | | **AST** | |
| 0x80-0x81 | 2 | AST | astRoot/L/H | Root of AST tree |
| 0x82-0x83 | 2 | AST | astNode/L/H | Current AST node pointer |
| 0x84-0x85 | 2 | AST | astTempNode/L/H | Temporary AST node pointer |
| | | | | |
| **0x90-0xA1** | | | **CodeGen** | |
| 0x90-0x91 | 2 | CodeGen | codeBuffer/L/H | Pointer to allocated code buffer |
| 0x92-0x93 | 2 | CodeGen | codeOffset/L/H | Relative offset in code buffer |
| 0x94-0x95 | 2 | CodeGen | codeSize/L/H | Current allocated buffer size |
| 0x96-0x97 | 2 | CodeGen | stringBuffer/L/H | String literals buffer pointer |
| 0x98-0x99 | 2 | CodeGen | stringOffset/L/H | Offset in string buffer |
| 0x9A | 1 | CodeGen | functionLocals | Count of locals in current function |
| 0x9B | 1 | CodeGen | storeOp | Store operation type |
| 0x9C-0x9D | 2 | CodeGen | forwardPatch/L/H | Forward patch address (also elsePatch) |
| 0x9E-0x9F | 2 | CodeGen | backwardPatch/L/H | Backward patch address (also endPatch) |
| 0xA0-0xA1 | 2 | CodeGen | functionNode/L/H | Current function being compiled |
| | | | | |
| **0xB0-0xBE** | | | **Parser** | |
| 0xB0 | 1 | Parser | currentToken | Current token type |
| 0xB1-0xB2 | 2 | Parser | functionNode/L/H | parseFunction's node |
| 0xB3-0xB4 | 2 | Parser | compoundNode/L/H | parseCompoundStatement's node |
| 0xB5-0xB6 | 2 | Parser | stmtNode/L/H | Statement node (expr/var decl) |
| 0xB7-0xB8 | 2 | Parser | exprNode/L/H | parseCallExpression's node |
| 0xB9-0xBA | 2 | Parser | rhsExprNode/L/H | Right-hand side expression node |
| 0xBB-0xBC | 2 | Parser | binNode/L/H | Binary operation node |
| 0xBD | 1 | Parser | binOp | Binary operator type |
| 0xBE | 1 | Parser | bpOffset | Base pointer offset |
| | | | | |
| **0xC0-0xC3** | | | **Library** | |
| 0xC0-0xC1 | 2 | Library | libArg/L/H | Library argument pointer |
| 0xC2-0xC3 | 2 | Library | libStr/L/H | Library string pointer |

### Notes:
- **0x60-0x68**: The CC main program slots (0x60-0x63) overlap with runtime slots since compilation and execution never happen simultaneously
- **0x58-0x5F**: Available/unused in this compiler
- **0x6C-0x6F**: Available/unused in this compiler  
- **0x7C-0x7F**: Available/unused in this compiler
- **0x86-0x8F**: Available/unused in this compiler
- **0xA2-0xAF**: Available/unused in this compiler
- **0xBF**: Available/unused in this compiler
- **0xC4-0xEB**: Available/unused in this compiler

The compiler uses zero page slots from 0x60 to 0xC3, with gaps that could be utilized if more storage is needed. The allocation shows good separation between different compilation phases, with no conflicts between units that operate simultaneously.