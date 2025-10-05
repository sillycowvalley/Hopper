## Complete BIOS Emulator Specification (Revised v3)

### VMB File Format
**Header Structure:**
- Bytes 0-2: "VMB" magic signature (0x56, 0x4D, 0x42)
- Byte 3: Function count (n)
- Bytes 4-5: Data section size (LSB first)
- Bytes 6 to 6+2n-1: Function sizes (n × 2-byte values, LSB first)

**Content Layout:**
1. Data section immediately after header
2. Function code sections in order (Function 0 = MAIN = ID 2, Function 1 = ID 4, etc.)

### Memory Layout
- **0x0000-0x00FF:** Zero Page (256 bytes) - BIOS marshalling registers
  - 0x10-0x11: ZP.ACC (LSB at 0x10)
  - 0x12-0x15: ZP.TOP (32-bit, LSB at 0x12)
  - 0x16-0x19: ZP.NEXT (32-bit, LSB at 0x16)
  - 0x1A-0x1B: ZP.IDX (LSB at 0x1A)
  - 0x1C-0x1D: ZP.IDY (LSB at 0x1C)
  - 0x1E-0x1F: ZP.STR (LSB at 0x1E)
- **0x0100-0x01FF:** Hardware Stack (256 bytes)
- **0x0200-0x02FF:** Global Variables Page (256 bytes)
- **0x0300-0x03FF:** Function Table (128 × 2-byte entries, LSB first)
- **0x0400+:** Code Pages (loaded VM code)
- **0x1000+:** Data Section (strings and constants)

### Stack Behavior (6502 Hardware Stack Convention)
- **Stack grows DOWNWARD** - SP decrements when pushing, increments when popping
- **SP points to the last pushed item** (top of stack)
- Stack Pointer (SP) starts at 0xFF (representing empty stack)
- Stack location in memory = 0x100 + SP
- **Push sequence:** Decrement SP, then write to [0x100+SP]
- **Pop sequence:** Read from [0x100+SP], then increment SP
- **Multi-byte values:** 
  - **Push order:** LSB first (pushed deeper), then MSB (at lower SP value)
  - **Pop order:** MSB first, then LSB (reverse of push order)
  - This maintains LSB at higher memory address on stack

### Stack Machine Model (Critical Concept)
**This VM is a pure stack machine:**
- **NO processor flags** are used for VM conditional logic
- **Comparison instructions** push 0 (false) or 1 (true) onto the stack
- **Branch instructions** pop a value from stack and test if it's 0 or non-zero
- **PUSHC/PUSHZ** are special - they push the saved flags from the last SYSCALL

### Stack Frame Layout
```
High Memory (Higher addresses, Lower SP values)
    [Argument N]            ; Last argument pushed
    ...
    [Argument 2]            ; Second argument
    [Argument 1]            ; First argument pushed
    [Return Y]              ; 1 byte - PC within calling function's page
    [Return Page Low]       ; 2 bytes - calling function's page address
    [Return Page High]      
    [Saved BP]              ; Previous base pointer  <- BP-1 in stack memory
    [Local 1]               ; <-- BP points here (first local at BP+0)
    [Local 2]               ; BP-1 (second local)
    [Local 3]               ; BP-2 (third local)
    ...                     ; <-- SP points here (last pushed item)
Low Memory (Lower addresses, Higher SP values)
```

**Critical Stack Frame Facts:**
- **BP points to the first local variable** (not to saved BP)
- **Saved BP is at stack location BP-1** (one position above BP in stack memory)
- **First local variable is at BP+0**
- **Arguments start at BP+5** for single-byte values
- **Return address is 3 bytes total**: Y (1 byte) + codePage (2 bytes)

### Program Counter Architecture
The VM uses a **3-byte program counter** model:
- **CodePage** (2 bytes): Base address of current function (e.g., 0x0400)
- **Y** (1 byte): Offset within current function (0x00-0xFF)
- **Effective PC** = CodePage + Y

**Implications:**
- All branches within a function only modify Y (byte operations)
- Y naturally wraps: 0xFF + 1 = 0x00, 0x00 - 1 = 0xFF
- Functions are limited to 256 bytes (one page)
- Branches cannot escape the current function's page

### Class Architecture

#### 1. **BiosEmulator** (Main Class)
**Responsibilities:**
- Command shell loop (prompt ">")
- File system operations (DIR, DEL, execute)
- VMB file loading and parsing
- VM execution control
- Memory management coordination

**Key Methods:**
- `Run(string storageDirectory)` - Main shell loop
- `LoadProgram(string filename)` - Parse VMB and load into memory
- `ExecuteProgram(List<string> args)` - Run loaded program
- `ProcessCommand(string command)` - Handle DIR/DEL/execute

**Properties:**
- `Memory memory`
- `VmState state`
- `SyscallHandler syscalls`
- `OpcodeExecutor executor`
- `string currentDirectory`

#### 2. **Memory** (Helper Class)
**Responsibilities:**
- Manage all memory pages
- Provide typed access (byte, word, quad)
- Handle multi-byte storage (LSB first)
- Stack operations following 6502 hardware behavior

**Key Methods:**
- `byte ReadByte(ushort address)`
- `void WriteByte(ushort address, byte value)`
- `ushort ReadWord(ushort address)` - LSB at address, MSB at address+1
- `void WriteWord(ushort address, ushort value)` - LSB first
- `uint ReadQuad(ushort address)` - 32-bit, LSB first
- `void WriteQuad(ushort address, uint value)` - LSB first
- `void PushByte(byte value, ref byte sp)` - Decrement sp, write at 0x100+sp
- `byte PopByte(ref byte sp)` - Read from 0x100+sp, increment sp
- `void PushWord(ushort value, ref byte sp)` - Push LSB then MSB
- `ushort PopWord(ref byte sp)` - Pop MSB then LSB (reverse of push)

**Stack Implementation:**
```csharp
void PushByte(byte value, ref byte sp) {
    sp--;  // Decrement first
    WriteByte((ushort)(0x100 + sp), value);
}

byte PopByte(ref byte sp) {
    byte value = ReadByte((ushort)(0x100 + sp));
    sp++;  // Increment after
    return value;
}

void PushWord(ushort value, ref byte sp) {
    PushByte((byte)(value & 0xFF), ref sp);      // LSB first
    PushByte((byte)(value >> 8), ref sp);        // MSB second
}

ushort PopWord(ref byte sp) {
    byte msb = PopByte(ref sp);                  // MSB first
    byte lsb = PopByte(ref sp);                  // LSB second
    return (ushort)((msb << 8) | lsb);
}
```

#### 3. **VmState** (State Container)
**Properties:**
- `ushort CodePage` - Current function base address
- `byte Y` - Offset within current function (0-255)
- `byte SP` - Stack pointer (0xFF = empty, points to TOS)
- `byte BP` - Base pointer for stack frame (points to first local)
- `byte A` - Accumulator for SYSCALL marshalling
- `byte X` - X register for SYSCALLX
- `byte VmFlags` - Saved flags from last SYSCALL (bit 6=Z, bit 7=C)
- `Dictionary<int, ushort> FunctionTable` - Function ID to address mapping
- `ushort DataSectionBase` - Location of string data (typically 0x1000)

**Computed Property:**
- `ushort PC { get { return (ushort)(CodePage + Y); } }`

#### 4. **SyscallHandler** (Helper Class)
**Responsibilities:**
- Implement BIOS system calls
- Marshal data from zero page
- File I/O operations
- Set VmFlags for PUSHC/PUSHZ operations

**Key Methods:**
- `void Execute(byte syscallId, VmState state, Memory memory)`
- Private methods for each syscall:
  - `void PrintString(Memory memory)` - 0x11
  - `void PrintChar(VmState state)` - 0x12
  - `void PrintHex(VmState state)` - 0x13
  - `void PrintNewLine()` - 0x14
  - `void FOpen(Memory memory, VmState state)` - 0x30
  - `void FClose(Memory memory)` - 0x31
  - `void FGetC(Memory memory)` - 0x32
  - `void ArgGet(Memory memory, VmState state)` - 0x37

**Important:** After each SYSCALL, update VmState.VmFlags:
- Bit 7 (0x80): Set if operation returned carry set
- Bit 6 (0x40): Set if operation returned zero flag set

**Properties:**
- `List<string> CommandLineArgs`
- `Dictionary<ushort, FileStream> OpenFiles` - File handle management

#### 5. **OpcodeExecutor** (Helper Class)
**Responsibilities:**
- Decode and execute VM opcodes using enum for clarity
- Manage Y register for PC operations
- Handle stack operations with proper 6502 behavior
- Implement pure stack machine comparison/branch model

**Opcode Enum:**
```csharp
enum Opcode : byte
{
    NOP = 0x00,
    HALT = 0x02,
    PUSHB = 0x04,
    PUSHB0 = 0x06,
    PUSHB1 = 0x08,
    PUSHW = 0x0A,
    PUSHW0 = 0x0C,
    PUSHW1 = 0x0E,
    PUSHA = 0x10,
    PUSHC = 0x12,
    PUSHZ = 0x14,
    DUPB = 0x16,
    DUPW = 0x18,
    DROPB = 0x1A,
    DROPW = 0x1C,
    SWAPB = 0x1E,
    SWAPW = 0x20,
    ADDB = 0x24,
    SUBB = 0x26,
    NEGB = 0x28,
    ADDW = 0x2A,
    SUBW = 0x2C,
    NEGW = 0x2E,
    INCLB = 0x30,
    INCLW = 0x32,
    EQB = 0x34,
    NEB = 0x36,
    LTB = 0x38,
    LEB = 0x3A,
    EQW = 0x3C,
    NEW = 0x3E,
    LTW = 0x40,
    LEW = 0x42,
    ANDB = 0x44,
    ORB = 0x46,
    XORB = 0x48,
    NOTB = 0x4A,
    XORW = 0x4C,
    SHLW = 0x4E,
    SHRW = 0x50,
    PUSHZB = 0x54,
    PUSHZW = 0x56,
    PUSHZQ = 0x58,
    POPZB = 0x5A,
    POPZW = 0x5C,
    POPZQ = 0x5E,
    POPA = 0x60,
    POPY = 0x62,
    PUSHLB = 0x64,
    PUSHLW = 0x66,
    PUSHLQ = 0x68,
    POPLB = 0x6A,
    POPLW = 0x6C,
    POPLQ = 0x6E,
    PUSHGB = 0x70,
    PUSHGW = 0x72,
    POPGB = 0x74,
    POPGW = 0x76,
    BRAF = 0x7C,
    BRAR = 0x7E,
    BZF = 0x80,
    BZR = 0x82,
    BNZF = 0x84,
    BNZR = 0x86,
    CALL = 0x88,
    RET = 0x8A,
    SYSCALL = 0x8C,
    SYSCALLX = 0x8E,
    ENTER = 0x90,
    LEAVE = 0x92,
    DUMP = 0x94,
    PUSHD = 0x98,
    PUSHD2 = 0x9A,
    STRC = 0x9C,
    STRCMP = 0x9E,
    READB = 0xA0,
    WRITEB = 0xA2
}
```

**Key Methods:**
- `void Execute(Memory memory, VmState state, SyscallHandler syscalls)`
- All PC operations work with Y register (byte operations)
- Y wraps naturally: branches can wrap from 0xFF to 0x00 or vice versa

**Critical Implementation Details:**

**Comparison Instructions (Stack Machine Model):**
```csharp
// Example: EQB
case Opcode.EQB:
    byte next = memory.PopByte(ref state.SP);
    byte top = memory.PopByte(ref state.SP);
    byte result = (byte)(next == top ? 1 : 0);
    memory.PushByte(result, ref state.SP);
    break;

// Example: LTW (unsigned comparison)
case Opcode.LTW:
    ushort topW = memory.PopWord(ref state.SP);
    ushort nextW = memory.PopWord(ref state.SP);
    byte result = (byte)(nextW < topW ? 1 : 0);
    memory.PushByte(result, ref state.SP);
    break;
```

**Branch Instructions (Pop and Test):**
```csharp
// Example: BNZF (Branch if Not Zero Forward)
case Opcode.BNZF:
    byte value = memory.PopByte(ref state.SP);
    byte offset = memory.ReadByte((ushort)(state.CodePage + ++state.Y));
    if (value != 0) {  // Branch if value is non-zero
        state.Y = (byte)(state.Y + offset);  // Will wrap naturally
    }
    break;

// Example: BZR (Branch if Zero Reverse)
case Opcode.BZR:
    byte value = memory.PopByte(ref state.SP);
    byte offset = memory.ReadByte((ushort)(state.CodePage + ++state.Y));
    if (value == 0) {  // Branch if value is zero
        state.Y = (byte)(state.Y - offset);  // Will wrap naturally
    }
    break;
```

**PUSHC/PUSHZ (Use Saved SYSCALL Flags):**
```csharp
case Opcode.PUSHC:
    byte carry = (byte)((state.VmFlags & 0x80) != 0 ? 1 : 0);
    memory.PushByte(carry, ref state.SP);
    break;

case Opcode.PUSHZ:
    byte zero = (byte)((state.VmFlags & 0x40) != 0 ? 1 : 0);
    memory.PushByte(zero, ref state.SP);
    break;
```

**Stack Frame Operations:**
```csharp
case Opcode.ENTER:
    byte locals = memory.ReadByte((ushort)(state.CodePage + ++state.Y));
    memory.PushByte(state.BP, ref state.SP);  // Save old BP
    state.BP = state.SP;  // BP points to first local
    // Allocate locals
    for (int i = 0; i < locals; i++) {
        memory.PushByte(0, ref state.SP);
    }
    break;

case Opcode.LEAVE:
    state.SP = state.BP;  // Restore SP to BP
    state.BP = memory.PopByte(ref state.SP);  // Restore old BP
    break;
```

### Execution Flow

1. **Program Load:**
   - Parse VMB header
   - Load data section at 0x1000
   - Load functions starting at 0x0400
   - Build function table at 0x0300
   - Set CodePage to MAIN function address (from table[2])
   - Set Y to 0
   - Set SP to 0xFF (empty stack)

2. **Execution Loop:**
   - Fetch opcode at CodePage + Y
   - Increment Y after fetch
   - Decode and execute via OpcodeExecutor
   - Continue until HALT

3. **Stack Frame Management:**
   - SP starts at 0xFF (stack empty)
   - ENTER: 
     - Push BP (SP--, write to 0x100+SP)
     - Set BP=SP (BP now points to first local)
     - Allocate locals (push zeros)
   - LEAVE: 
     - Restore SP=BP
     - Pop BP (read from 0x100+SP, SP++)
   - Stack frame locals accessed via 0x100+BP+offset

4. **Function Calls (3-byte PC):**
   - CALL: 
     - Push Y (current offset)
     - Push CodePage LSB
     - Push CodePage MSB
   - Jump: Set CodePage from function table, set Y=0
   - RET: 
     - Pop CodePage MSB
     - Pop CodePage LSB  
     - Pop Y

5. **Branch Instructions:**
   - All branches modify Y only (signed byte offset)
   - Forward branch: Y = Y + 1 + offset (after fetching offset)
   - Backward branch: Y = Y + 1 - offset
   - Y wraps naturally (0xFF + 2 = 0x01, 0x00 - 1 = 0xFF)

### Summary of Key Concepts

1. **Pure Stack Machine:** Comparisons and branches work through stack values (0/1), not processor flags
2. **6502 Stack Convention:** Stack grows downward, SP points to TOS
3. **PUSHC/PUSHZ Special Case:** These use saved SYSCALL flags from VmFlags register
4. **3-byte Program Counter:** CodePage + Y with natural byte wrapping
5. **Stack Frame:** BP points to first local, saved BP is at BP-1 in stack memory
6. **Multi-byte Values:** LSB pushed first (deeper), MSB popped first

### Opcode Implementation Notes

- **Stack Operations:** Follow 6502 hardware convention (dec then write for push, read then inc for pop)
- **Local Variable Access:**
  - PUSHLB offset: Read from 0x100 + BP + offset
  - POPLB offset: Write to 0x100 + BP + offset
  - First local at BP+0, second at BP-1, etc. (offsets in stack direction)
- **Arguments Access:** Start at BP+5 (after saved BP and 3-byte return address)
- **Comparison instructions**: Push 0 (false) or 1 (true) based on unsigned comparison
- **Branch instructions**: Pop value and branch based on 0 or non-zero
- **Global variable access** (PUSHGB/POPGB): Direct access to 0x200+offset
- **Zero page access** (PUSHZB/POPZB): Direct access to 0x00+offset
- **Data references** (PUSHD): Push address of DataSectionBase+offset