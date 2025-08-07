# HopperBASIC Memory Map
**Document Type: Memory Layout**

## Overview
HopperBASIC uses a 32KB RAM system (0x0000-0x7FFF) with the following layout:
- Fixed buffers for VM and BASIC operations (0x0200-0x0FFF)
- Dynamic heap starting at 0x1000
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

## BASIC-Specific Buffers (0x0900-0x0FFF)

| Address | Size | Symbol | Purpose |
|---------|------|--------|---------|
| 0x0900 | 128B | BasicInputBuffer | Raw user input line |
| 0x0980 | 32B | BasicCompilerWorkspace | Compiler state (compiler.asm) |
| 0x09A0 | 32B | BasicStatementWorkspace | Statement processing (statement.asm) |
| 0x09C0 | 32B | BasicExecutorWorkspace | Executor state (executor.asm) |
| 0x09E0 | 32B | BasicProcessBuffer | String uppercasing (tokenizer.asm) |
| 0x0A00 | 512B | BASICTokenizerBuffer | Tokenized BASIC function storage |
| 0x0C00 | 512B | BASICOpCodeBuffer | JIT compiled BASIC function opcodes |
| 0x0E00 | 256B | REPLTokenizerBuffer | Tokenized REPL line storage |
| 0x0F00 | 256B | REPLOpCodeBuffer | Compiled REPL line OpCode storage |
| 0x1000 | - | HopperData | Start of dynamic heap |

## Buffer Size Constants (from Limits.asm)

| Constant | Value | Description |
|----------|-------|-------------|
| BasicInputLength | 128 | Maximum input line length |
| TokenizerBufferLength | 512 | Token buffer size (16-bit) |
| OpCodeBufferLength | 512 | JIT buffer size (16-bit) |
| BasicCompilerWorkspaceLength | 32 | Compiler workspace |
| BasicStatementWorkspaceLength | 32 | Statement workspace |
| BasicExecutorWorkspaceLength | 32 | Executor workspace |
| BasicProcessBufferLength | 32 | Tokenizer processing buffer |

Note: The BASIC buffers use the full sizes defined above (512 bytes each). The REPL buffers are smaller:
- REPLTokenizerBuffer: 256 bytes (at 0x0E00-0x0EFF)
- REPLOpCodeBuffer: 256 bytes (at 0x0F00-0x0FFF)

## Memory Usage Summary

- **Zero Page**: 0x0000-0x00FF (256B) - System variables
- **6502 Hardware Stack**: 0x0100-0x01FF (256B) - Reserved
- **VM Stacks & I/O**: 0x0200-0x08FF (1.75KB)
- **BASIC Buffers**: 0x0900-0x0DFF (1.25KB)
- **REPL Buffers**: 0x0E00-0x0FFF (512B)
- **Total Fixed Allocation**: 4KB
- **Available Heap**: 0x1000-0x7FFF (28KB)
- **Total RAM**: 32KB (RamSize = 0x8000)

## Notes
- All VM stacks are 256 bytes to allow 8-bit indexing
- BASIC buffers are aligned for efficient access
- REPL buffers are separate from BASIC function buffers to allow interactive execution alongside stored programs
- The heap grows upward from HopperData (0x1000)
- Memory above 0x8000 is ROM space (not RAM)