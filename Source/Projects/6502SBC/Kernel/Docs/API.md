Good point about not doubling up. Here's the updated table with suggestions:

## SysCall API with Suggestions

| SysCall | Current Inputs | Current Outputs | **Suggestions** |
|---------|---------------|-----------------|-----------------|
| **Memory Management** |
| MemAllocate | ZP.ACC = size | ZP.IDX = address<br>C = success | ✓ Good as is |
| MemFree | ZP.IDX = address | C = success | ✓ Good as is |
| MemAvailable | - | ZP.ACC = free bytes | ✓ Good as is |
| MemMaximum | - | ZP.ACC = largest block | ✓ Good as is |
| **File Operations** |
| FileExists | ZP.STR = filename<br>A = DirWalkAction | C = exists | ⚠️ Move A param to ZP.ACCL for consistency |
| FileDelete | ZP.STR = filename | C = success | ✓ Good as is |
| FileDir | - | C = success | ✓ Good as is |
| FileStartSave | ZP.STR = filename | C = success | ✓ Good as is |
| FileAppendStream | SectorSource = ptr<br>TransferLength = count | C = success | ✓ Good as is |
| FileEndSave | A = file type | C = success | ⚠️ Move A param to ZP.ACCL |
| FileStartLoad | ZP.STR = filename<br>A = DirWalkAction | C = success | ⚠️ Move A param to ZP.ACCL |
| FileNextStream | - | C = data available<br>TransferLength = count | ✓ Good as is |
| FileFormat | - | C = success | ✓ Good as is |
| **Serial I/O** |
| SerialWriteChar | A = character | - | ⚠️ Move A param to ZP.ACCL |
| SerialWaitForChar | - | A = character | ✓ Good - return in A |
| SerialIsAvailable | - | C = available | ✓ Good as is |
| IsBreak | - | C = break detected | ✓ Good as is |
| **Print/Console** |
| PrintString | ZP.STR = string | - | ⚠️ Add C = success |
| PrintChar | ZP.ACCL = character | - | ✓ Good as is |
| PrintHex | A = byte | - | ⚠️ Move A param to ZP.ACCL |
| PrintNewLine | - | - | ✓ Good as is |
| PrintSpace | - | - | ✓ Good as is |
| PrintSpaces | X = count | - | ⚠️ Move X param to ZP.ACCL |
| **Timer Services** |
| TimeDelay | ZP.TOP = ms | - | ⚠️ Add C = success |
| TimeMillis | - | ZP.TOP = ms (32-bit) | ✓ Good as is |
| TimeSeconds | - | ZP.TOP = seconds | ✓ Good as is |
| **Long Math** |
| LongAdd | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| LongSub | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| LongMul | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| LongDiv | ZP.NEXT, ZP.TOP | ZP.NEXT = quotient | ⚠️ Add C = success (NC on div by 0) |
| LongMod | ZP.NEXT, ZP.TOP | ZP.NEXT = remainder | ⚠️ Add C = success (NC on div by 0) |
| LongPrint | ZP.TOP = value | - | ⚠️ Add C = success |
| LongLT | ZP.NEXT, ZP.TOP | C = (NEXT < TOP) | ✓ Good as is |
| LongGT | ZP.NEXT, ZP.TOP | C = (NEXT > TOP) | ✓ Good as is |
| LongEQ | ZP.NEXT, ZP.TOP | C = (NEXT == TOP) | ✓ Good as is |
| LongNE | ZP.NEXT, ZP.TOP | C = (NEXT != TOP) | ✓ Good as is |
| LongLE | ZP.NEXT, ZP.TOP | C = (NEXT <= TOP) | ✓ Good as is |
| LongGE | ZP.NEXT, ZP.TOP | C = (NEXT >= TOP) | ✓ Good as is |
| **Float Math** |
| FloatAdd | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| FloatSub | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| FloatMul | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| FloatDiv | ZP.NEXT, ZP.TOP | ZP.NEXT = quotient | ⚠️ Add C = success (NC on div by 0) |
| FloatToLong | ZP.NEXT = float | ZP.NEXT = long | ⚠️ Add C = success (NC on overflow) |
| FloatLT | ZP.NEXT, ZP.TOP | C = (NEXT < TOP) | ✓ Good as is |
| FloatEQ | ZP.NEXT, ZP.TOP | C = (NEXT == TOP) | ✓ Good as is |
| **GPIO** |
| PinMode | ZP.ACCL = pin<br>ZP.ACCH = mode | - | ⚠️ Add C = success |
| PinRead | ZP.ACCL = pin | ZP.ACCH = value | ⚠️ Return value in A instead |
| PinWrite | ZP.ACCL = pin<br>ZP.ACCH = value | - | ⚠️ Add C = success |

## Summary of Proposed Conventions:

1. **Inputs**: Always via ZP slots (never A or X registers)
2. **8-bit returns**: Use A register when appropriate
3. **16/32-bit returns**: Keep in ZP slots
4. **Success/failure**: Always use C flag (set = success)
5. **Preserve**: Y register (caller can rely on it)
6. **May modify**: A, X, flags, ZP.LastError, BIOS working areas

This makes the API more predictable and consistent while maintaining the existing ZP slot structure.