# HopperBASIC Memory Map
**Document Type: Memory Layout**

## Fixed Buffer Allocation

| Address | Size | Symbol | Purpose |
|---------|------|--------|---------|
| 0x0200 | 256B | SerialInBuffer | Serial input buffer |
| 0x0300 | 256B | CallStackLSB | Call stack low bytes |
| 0x0400 | 256B | CallStackMSB | Call stack high bytes |
| 0x0500 | 256B | TypeStackLSB | Type stack |
| 0x0600 | 256B | ValueStackLSB | Value stack low bytes |
| 0x0700 | 256B | ValueStackMSB | Value stack high bytes |
| 0x0800 | 256B | I2CInBuffer | I2C request buffer |

## BASIC-Specific Buffers

| Address | Size | Symbol | Purpose |
|---------|------|--------|---------|
| 0x0900 | 128B | BasicInputBuffer | Raw user input |
| 0x0980 | 32B | BasicCompilerWorkspace | Compiler state |
| 0x09A0 | 32B | BasicStatementWorkspace | Statement processing |
| 0x09C0 | 32B | BasicExecutorWorkspace | Executor state |
| 0x09E0 | 32B | BasicProcessBuffer | String uppercasing |
| 0x0A00 | 512B | BasicTokenizerBuffer | Tokenized line storage |
| 0x0C00 | 512B | BasicOpCodeBuffer | JIT compilation workspace |

## Dynamic Memory

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x0E00 | HopperData | Start of dynamic heap |
| 0x8000 | RamSize | Total system RAM (32KB) |

## Memory Usage Summary

- **VM Stacks & Buffers**: 0x0200-0x08FF (1.75KB)
- **BASIC Buffers**: 0x0900-0x0DFF (1.25KB)
- **Total Fixed**: 3KB
- **Available Heap**: 29KB (0x0E00-0x7FFF)