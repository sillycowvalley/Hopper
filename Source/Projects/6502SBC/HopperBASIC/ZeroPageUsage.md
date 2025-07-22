# Zero Page Usage Matrix by Method - Hopper BASIC

## Usage Legend
- 🔴 **Writes** - Method modifies this location
- 🟡 **Reads/Writes** - Method both reads and modifies
- 🟢 **Reads** - Method only reads this location
- ⚫ **No Use** - Method doesn't access
- 🔥 **Conflict** - Multiple methods write same location

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

| Addr | Variable | Tok.nextToken | Tok.ReadLine | FM.StartREPL | FM.EmitByte | FM.EmitWord | FM.FinishREPL | BC.CompileREPL | BE.ExecuteREPL | Int.processCmd |
|------|----------|---------------|--------------|--------------|-------------|-------------|---------------|----------------|----------------|----------------|
| 0x30 | BasicInputLength | 🟢 | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x31 | TokenizerPos | 🟡 | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | 🔴 |
| 0x32 | TokenStart | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x33 | TokenLen | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x34 | CurrentToken | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟢 | ⚫ | 🟢 |
| 0x35 | ExprValueLo | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x36 | ExprValueHi | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x37 | ExprType | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x38-39 | FuncListHead | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x3A | FuncCount | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x3B-3C | CurrentFunc | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | 🟡 | ⚫ | ⚫ | ⚫ |
| 0x3D | CompileState | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x3E-3F | WritePos | ⚫ | ⚫ | 🔴 | 🟡 | 🟡 | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x40-41 | TempBlock | ⚫ | ⚫ | 🔴 | 🟢 | ⚫ | 🟢 | ⚫ | ⚫ | ⚫ |
| 0x42-43 | BytecodeSize | ⚫ | ⚫ | 🔴 | 🟡 | 🟡 | 🟢 | ⚫ | ⚫ | ⚫ |
| 0x44 | FileNamePtr | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x45 | FileNameLen | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |

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

## General Function Space (0x60-0x6F) - Conflict Zone

| Addr | Variable | FM.FinishREPL | Int.CmdNew | Int.cmdClear | Int.processCmd | Str.getLength | Arr.new | Time.DelayTOP | Util.CopyBytes |
|------|----------|---------------|------------|--------------|----------------|---------------|---------|---------------|----------------|
| 0x60 | F0/FSIGN | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ | ⚫ |
| 0x61-62 | F1-F2/FSIZE | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | 🔴 | ⚫ | ⚫ |
| 0x63-64 | F3-F4/FSOURCEADDR | 🟡 | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | 🟡 |
| 0x65-66 | F5-F6/FDESTADDR | 🟡 | 🔥 | 🔥 | 🔥 | 🔴 | ⚫ | ⚫ | 🟡 |
| 0x67 | F7/FTYPE | ⚫ | 🔥 | 🔥 | 🔥 | 🔴 | 🔴 | ⚫ | ⚫ |
| 0x68-69 | F8-F9/FLENGTH | ⚫ | 🔥 | 🔥 | 🔥 | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x6A-6B | F10-F11/FVALUE | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x6C-6F | F12-F15 | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |

## String Operations (from reference code - potential future conflicts)

| Addr | Variable | Str.new | Str.getLength | Str.setLength | Str.NewFromConstant | Str.Build | Str.compareEqual |
|------|----------|---------|---------------|---------------|---------------------|-----------|------------------|
| 0x61-62 | F1-F2/FSIZE | 🔴 | ⚫ | ⚫ | 🔴 | 🔴 | ⚫ |
| 0x63-64 | F3-F4/FSOURCEADDR | ⚫ | ⚫ | ⚫ | 🔴 | 🔴 | 🔴 |
| 0x65-66 | F5-F6/FDESTADDR | ⚫ | ⚫ | ⚫ | 🔴 | 🔴 | 🔴 |
| 0x67 | F7/FTYPE | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ |
| 0x68-69 | F8-F9/FLENGTH | ⚫ | 🔴 | 🔴 | 🔴 | 🔴 | ⚫ |
| 0x6A-6B | F10-F11/FVALUE | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | ⚫ |
| 0x6C-6F | F12-F15 | ⚫ | ⚫ | ⚫ | ⚫ | 🔴 | 🔴 |

## Array Operations (from reference code - potential future conflicts)

| Addr | Variable | Arr.new | Arr.NewFromConstant | Arr.GetItem | Arr.SetItem | Arr.getIndexAndMask |
|------|----------|---------|---------------------|-------------|-------------|---------------------|
| 0x61-62 | F1-F2/FSIZE | 🔴 | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x63-64 | F3-F4/FSOURCEADDR | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x65-66 | F5-F6/FDESTADDR | ⚫ | 🔴 | ⚫ | ⚫ | ⚫ |
| 0x67 | F7/FTYPE | 🔴 | 🔴 | 🔴 | 🔴 | 🔴 |
| 0x6E | F14/ACARRY | 🔴 | ⚫ | ⚫ | ⚫ | ⚫ |

## Key Conflicts Identified

### 🔥 **Critical Active Conflicts**
1. **F5-F8 (0x65-0x69)**: 
   - **Interpreter methods** (CmdNew, cmdClear, processCmd) use F5-F8 for `pgmLIST_HEAD` and `varLIST_HEAD`
   - **FunctionManager.FinishREPL** uses F5-F6 for `FDESTINATIONADDRESS`
   - **Future String operations** would heavily use F5-F9

### 🟡 **Medium Conflicts**
1. **IDX (0x16-17)**: Heavy contention between Free.Free, Allocate.Allocate, and various other methods
2. **M-space (0x50-5F)**: Free.Free and Allocate.Allocate both modify entire range (but calls are serialized)

### 🟢 **Manageable Conflicts**
1. **TOP/NEXT (0x12-15)**: Expected sharing across execution methods
2. **TokenizerPos (0x31)**: Shared between BytecodeCompiler.CompileREPL and Interpreter.processCmd (but sequential)

## Immediate Action Required

**F5-F8 Conflict Resolution:**
- Move Interpreter list heads to unused space (0x46-0x49)
- Reserve F5-F8 for FunctionManager and future string operations
- Update Interpreter.asm to use new addresses

**Code Changes Needed:**
```hopper
// In Interpreter.asm - change from F5-F8 to 0x46-0x49
const byte pgmLIST_HEAD  = 0x46;  // was ZP.F5
const byte pgmLIST_HEADH = 0x47;  // was ZP.F6  
const byte varLIST_HEAD  = 0x48;  // was ZP.F7
const byte varLIST_HEADH = 0x49;  // was ZP.F8
```