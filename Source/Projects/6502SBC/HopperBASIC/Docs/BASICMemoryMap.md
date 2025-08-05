# HopperBASIC Memory Map
**Document Type: Memory Layout**

## Overview
HopperBASIC uses a 32KB RAM system (0x0000-0x7FFF) with the following layout:
- Fixed buffers for VM and BASIC operations (0x0200-0x0DFF)
- Dynamic heap starting at 0x0E00
- Zero page reserved for system variables (see BASICZeroPage.md)

## VM Stack and I/O Buffers (0x0200-0x08FF)

| Address | Size | Symbol | Purpose |
|---------|------|--------|---------|
| 0x0200 | 256B | SerialInBuffer | Serial input buffer |
| 0x0300 | 256B | CallStackLSB | Call stack low bytes |
| 0x0400 | 256B | CallStackMSB | Call stack high bytes |
| 0x0500 | 256B | TypeStackLSB | Type stack |
| 0x0600 | 256B | ValueStackLSB | Value stack low bytes |
| 0x0700 | 256B | ValueStackMSB | Value stack high bytes |
| 0x0800 | 256B | I2CInBuffer | I2C request buffer |

## BASIC-Specific Buffers (0x0900-0x0DFF)

| Address | Size | Symbol | Purpose |
|---------|------|--------|---------|
| 0x0900 | 128B | BasicInputBuffer | Raw user input line |
| 0x0980 | 32B | BasicCompilerWorkspace | Compiler state (compiler.asm) |
| 0x09A0 | 32B | BasicStatementWorkspace | Statement processing (statement.asm) |
| 0x09C0 | 32B | BasicExecutorWorkspace | Executor state (executor.asm) |
| 0x09E0 | 32B | BasicProcessBuffer | String uppercasing (tokenizer.asm) |
| 0x0A00 | 512B | BasicTokenizerBuffer | Tokenized line storage |
| 0x0C00 | 512B | BasicOpCodeBuffer | JIT compiled opcodes |
| 0x0E00 | - | HopperData | Start of dynamic heap |

## Buffer Size Constants (from Limits.asm)

| Constant | Value | Description |
|----------|-------|-------------|
| BasicInputLength | 128 | Maximum input line length |
| BasicTokenizerBufferLength | 512 | Token buffer size (16-bit) |
| BasicOpCodeBufferLength | 512 | JIT buffer size (16-bit) |
| BasicCompilerWorkspaceLength | 32 | Compiler workspace |
| BasicStatementWorkspaceLength | 32 | Statement workspace |
| BasicExecutorWorkspaceLength | 32 | Executor workspace |
| BasicProcessBufferLength | 32 | Tokenizer processing buffer |

## Memory Usage Summary

- **Zero Page**: 0x0000-0x00FF (256B) - System variables
- **6502 Hardware Stack**: 0x0100-0x01FF (256B) - Reserved
- **VM Stacks & I/O**: 0x0200-0x08FF (1.75KB)
- **BASIC Buffers**: 0x0900-0x0DFF (1.25KB)
- **Total Fixed Allocation**: 3.25KB
- **Available Heap**: 0x0E00-0x7FFF (~29KB)
- **Total RAM**: 32KB (RamSize = 0x8000)

## Notes
- All VM stacks are 256 bytes to allow 8-bit indexing
- BASIC buffers are aligned for efficient access
- The heap grows upward from HopperData (0x0E00)
- Memory above 0x8000 is ROM space (not RAM)