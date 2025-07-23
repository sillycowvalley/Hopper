# Zero Page Usage Matrix by Method - Hopper BASIC (Updated)

## Usage Legend
- 🔴 **Writes** - Method modifies this location
- 🟡 **Reads/Writes** - Method both reads and modifies
- 🟢 **Reads** - Method only reads this location
- ⚫ **No Use** - Method doesn't access
- ✅ **Resolved** - Previously conflicted, now clean

## Method Abbreviations
- **Tok**: Tokenizer methods
- **FM**: FunctionManager methods  
- **BC**: BytecodeCompiler methods
- **BE**: BytecodeExecutor methods
- **Int**: Interpreter methods
- **Mem**: Memory methods
- **Fr**: Free methods
- **All**: Allocate methods

## Core Runtime Variables (0x00-0x2F)

| Addr | Variable | Tok.nextToken | Tok.ReadLine | FM.EmitByte | FM.EmitWord | BC.CompileREPL | BE.fetchByte | BE.ExecuteREPL | Int.CmdNew | Int.processCmd | Mem.Allocate | Fr.Free | All.Allocate |
|------|----------|---------------|--------------|-------------|-------------|----------------|--------------|----------------|------------|----------------|--------------|---------|--------------|
| 0x00-01 | PC/PCL/PCH | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x02 | FLAGS | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x03 | SP | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ |
| 0x04 | BP | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x05 | CSP | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x06-07 | FREELIST | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟢 | 🟡 | 🟡 |
| 0x08 | HEAPSTART | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟢 | ⚫ | 🟢 | ⚫ | ⚫ |
| 0x09 | HEAPSIZE | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟢 | ⚫ | 🟢 | ⚫ | ⚫ |
| 0x0A-0B | Serial Ptrs | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x0C | SerialBreakFlag | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x0D-0E | CODESTART | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟢 | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x0F | CNP | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x10-11 | ACC | ⚫ | ⚫ | 🟢 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | 🟡 | 🟡 |
| 0x12-13 | TOP | 🟡 | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ |
| 0x14-15 | NEXT | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ | 🟡 | 🟡 | 🟡 |
| 0x16-17 | IDX | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ | 🟡 | ⚫ | 🟡 | ⚫ | 🟡 | 🔴 | 🔴 |
| 0x18-19 | IDY | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | ⚫ | 🟡 | 🟡 | 🟡 |
| 0x1A-1C | Type bytes | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | ⚫ | 🟡 | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ |
| 0x1D | PROGSIZE | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | 🟡 | ⚫ | ⚫ |

## HOPPER_BASIC Dedicated Space (0x30-0x4F)

| Addr | Variable | Tok.nextToken | Tok.ReadLine | FM.StartREPL | FM.EmitByte | FM.EmitWord | FM.FinishREPL | BC.CompileREPL | BE.ExecuteREPL | Int.CmdNew | Int.cmdClear | Int.processCmd |
|------|----------|---------------|--------------|--------------|-------------|-------------|---------------|----------------|----------------|------------|--------------|----------------|
| 0x30 | BasicInputLength | 🟢 | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x31 | TokenizerPos | 🟡 | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | 🔴 |
| 0x32 | TokenStart | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x33 | TokenLen | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x34 | CurrentToken | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟢 | ⚫ | ⚫ | ⚫ | 🟢 |
| 0x35 | ExprValueLo | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x36 | ExprValueHi | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x37 | ExprType | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x38-39 | FuncListHead | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x3A | FuncCount | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x3B-3C | CurrentFunc | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x3D | CompileState | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x3E-3F | WritePos | ⚫ | ⚫ | 🔴 | 🟡 | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x40-41 | TempBlock | ⚫ | ⚫ | 🔴 | 🟢 | ⚫ | 🟢 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x42-43 | BytecodeSize | ⚫ | ⚫ | 🔴 | 🟡 | 🟡 | 🟢 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x44 | FileNamePtr | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x45 | FileNameLen | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x46-47 | **PgmListHead** | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | 🟡 | 🟡 |
| 0x48-49 | **VarListHead** | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | 🟡 | 🟡 |
| 0x4A-4F | **AVAILABLE** | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |

## Memory Management Methods (0x50-0x5F)

| Addr | Variable | Fr.Free | Fr.freeHelper1 | All.Allocate | Mem.InitHeap | Mem.Available | Mem.ReadByte | Mem.WriteByte |
|------|----------|---------|----------------|--------------|--------------|---------------|--------------|---------------|
| 0x50-51 | M0-M1 | 🔴 | 🟡 | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x52-53 | M2-M3 | 🔴 | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x54-55 | M4-M5 | 🔴 | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x56-57 | M6-M7 | 🔴 | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x58-59 | M8-M9 | 🔴 | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x5A-5B | M10-M11 | 🔴 | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x5C-5D | M12-M13 | 🔴 | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x5E-5F | M14-M15 | 🟡 | ⚫ | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ |

## General Function Space (0x60-0x6F) - Now Clean!

| Addr | Variable | FM.FinishREPL | Int.CmdNew | Int.cmdClear | Int.processCmd | Future.Str.ops | Future.Arr.ops | Time.DelayTOP | Util.CopyBytes |
|------|----------|---------------|------------|--------------|----------------|----------------|----------------|---------------|----------------|
| 0x60 | F0/FSIGN | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x61-62 | F1-F2/FSIZE | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | 🔴 | ⚫ | ⚫ |
| 0x63-64 | F3-F4/FSOURCEADDR | 🟡 | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | 🟡 |
| 0x65-66 | F5-F6/FDESTADDR | 🟡 | ✅ | ✅ | ✅ | 🔴 | ⚫ | ⚫ | 🟡 |
| 0x67 | F7/FTYPE | ⚫ | ✅ | ✅ | ✅ | 🔴 | 🔴 | ⚫ | ⚫ |
| 0x68-69 | F8-F9/FLENGTH | ⚫ | ✅ | ✅ | ✅ | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x6A-6B | F10-F11/FVALUE | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x6C-6F | F12-F15 | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |

## ✅ **Conflict Resolution Status**

### 🎉 **RESOLVED - Critical F5-F8 Conflict**
- **Before**: Interpreter methods conflicted with FunctionManager.FinishREPL
- **After**: Interpreter moved to dedicated 0x46-0x49 space
- **Result**: F5-F8 now available for FunctionManager and future string operations

### 🟡 **Manageable Conflicts (Unchanged)**
1. **IDX (0x16-17)**: Heavy contention between Free.Free, Allocate.Allocate, and various other methods *(Expected - serialized calls)*
2. **M-space (0x50-5F)**: Free.Free and Allocate.Allocate both modify entire range *(Expected - serialized calls)*

### 🟢 **No Active Conflicts**
1. **BASIC workspace (0x30-0x45)**: Clean separation maintained
2. **Interpreter storage (0x46-0x49)**: Now in dedicated space
3. **F-space (0x60-0x6F)**: Available for FunctionManager and future features

## Current Status: ✅ **CLEAN**

The zero page usage is now well-organized with:
- **Dedicated spaces** for each BASIC component
- **No active conflicts** between modules
- **Clear expansion path** (6 bytes unused at 0x4A-0x4F)
- **F-space preserved** for string/array operations when needed

### Memory Efficiency
- **Total BASIC-specific ZP usage**: 20 bytes (0x30-0x43)
- **Dedicated storage**: 4 bytes (0x46-0x49) 
- **Available expansion**: 6 bytes (0x4A-0x4F)
- **Shared runtime space**: Properly coordinated

Your zero page architecture is now solid and ready for expansion!