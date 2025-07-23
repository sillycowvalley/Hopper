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
| $0980-$0A7F | 256 bytes | BasicTokenizerBuffer | Token processing workspace |
| $0A80-$0C7F | 512 bytes | BasicFunctionDefBuffer | Multi-line function accumulation |
| $0C80-$0CBF | 64 bytes | BasicProcessBuffer1 | Variable/general workspace |
| $0CC0-$0CDF | 32 bytes | BasicProcessBuffer2 | Secondary workspace |
| $0CE0-$0CFF | 32 bytes | BasicProcessBuffer3 | Tertiary workspace |
| $0D00+ | Dynamic | HopperData | Dynamic heap (programs, variables, functions) |

### Buffer Usage Notes

- **BasicInputBuffer**: Stores raw command line input as user types
- **BasicTokenizerBuffer**: Workspace for tokenizing before heap allocation  
- **BasicFunctionDefBuffer**: Accumulates function definitions spanning multiple lines
- **BasicProcessBuffer1-3**: Multi-purpose workspace for variable processing, expression parsing, and temporary operations
- **HopperData**: Dynamic heap managed by Hopper VM memory allocator for program storage, variable tables, and function definitions

**Total Fixed Buffer Usage**: 1024 bytes for HopperBASIC-specific buffers  
**Available Dynamic Memory**: Depends on system RAM size minus fixed allocations