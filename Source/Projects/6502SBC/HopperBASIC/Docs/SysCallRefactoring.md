# SYSCALL Refactoring Subproject
**Document Type: Implementation Plan**

## Project Overview
Refactor HopperBASIC built-in methods (ABS, MILLIS, DELAY, SECONDS, RND, PEEK, POKE) from dedicated opcodes to use the SYSCALL infrastructure with flags-based encoding.

## SYSCALL Flags Design

### Bit Layout (8-bit SYSCALL ID)
```
Bit 7-3: Function ID (32 functions total)
Bit 2:   Return Value (0=void, 1=returns value)  
Bit 1-0: Argument Count (00=0, 01=1, 10=2, 11=3)
```

### Key Benefits
- **6502 Efficient**: `AND #0x03` gets argument count directly into index register
- **Simple dispatch**: `AND #0x04` tests return value flag, `LSR LSR LSR` extracts function ID
- **32 functions maximum**: Keeps switch statement manageable
- **Future expansion**: Add SYSCALLW opcode for 16-bit function IDs when needed

## Implementation Changes

### 1. OpCodes.asm
**Remove these dedicated opcodes:**
```hopper
// DELETE THESE:
ABS      = 0x1B,  // Pop value, push absolute value
MILLIS   = 0x1C,  // Push current milliseconds (WORD)
SECONDS  = 0x1D,  // Push current seconds (WORD)
RND      = 0x1E,  // Pop max value, push random number (0 to max-1)
PEEK     = 0x1F,  // Pop address, push memory byte at that address
POKE     = 0x20,  // Pop value, pop address, write value to memory address
DELAY    = 0x21,  // Pop delay value in milliseconds
```

**Update SysCallType enum:**
```hopper
enum SysCallType
{
    // Function ID bits 7-3, Return bit 2, Args bits 1-0
    
    // System functions (ID 1-2)
    PrintValue   = 0x08 | 0x00 | 0x01,  // ID=1, void, 1 arg    = 0x09
    PrintNewLine = 0x10 | 0x00 | 0x00,  // ID=2, void, 0 args   = 0x10
    
    // Built-in functions (ID 3-9)
    Abs          = 0x18 | 0x04 | 0x01,  // ID=3, returns, 1 arg = 0x1D
    Rnd          = 0x20 | 0x04 | 0x01,  // ID=4, returns, 1 arg = 0x25
    Millis       = 0x28 | 0x04 | 0x00,  // ID=5, returns, 0 args = 0x2C  
    Seconds      = 0x30 | 0x04 | 0x00,  // ID=6, returns, 0 args = 0x34
    Delay        = 0x38 | 0x00 | 0x01,  // ID=7, void, 1 arg     = 0x39
    Peek         = 0x40 | 0x04 | 0x01,  // ID=8, returns, 1 arg  = 0x45
    Poke         = 0x48 | 0x00 | 0x02,  // ID=9, void, 2 args    = 0x4A
}
```

### 2. Emit.asm
**Modify existing emission methods** (keep methods, change implementation):
```hopper
// MODIFY THESE - keep method names, change to emit SYSCALL:

Millis()
{
    LDA #SysCallType.Millis
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}

Seconds()
{
    LDA #SysCallType.Seconds
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}

Abs()
{
    LDA #SysCallType.Abs
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}

Delay()
{
    LDA #SysCallType.Delay
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}

// Add new methods for PEEK/POKE/RND (if not already present)
Rnd()
{
    LDA #SysCallType.Rnd
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}

Peek()
{
    LDA #SysCallType.Peek
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}

Poke()
{
    LDA #SysCallType.Poke
    STA Compiler.compilerOperand1
    LDA #OpCode.SYSCALL
    STA Compiler.compilerOpCode
    Emit.OpCodeWithByte();
}
```

### 3. Executor.asm
**Remove dedicated execution methods:**
```hopper
// DELETE THESE METHODS:
executeAbs()
executeMillis()
executeSeconds() 
executeDelay()
executeRnd()
executePeek() 
executePoke()
```

**Remove switch cases from DispatchOpCode():**
```hopper
// DELETE THESE CASES:
case OpCode.ABS: { executeAbs(); }
case OpCode.MILLIS: { executeMillis(); }
case OpCode.SECONDS: { executeSeconds(); }
case OpCode.DELAY: { executeDelay(); }
case OpCode.RND: { executeRnd(); }
case OpCode.PEEK: { executePeek(); }
case OpCode.POKE: { executePoke(); }
```

**Replace executeSysCall() method completely:**
```hopper
// Execute SYSCALL opcode - system call with flags-based dispatch
const string executeSysCallTrace = "SYSCALL // System call";
executeSysCall()
{
#ifdef TRACE
    LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL 
    LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH 
    Trace.MethodEntry();
#endif
    
    FetchOperandByte();  // A = SYSCALL ID
    States.CanContinue();
    if (NC) 
    { 
#ifdef TRACE
        LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
        return; 
    }
    
    TAX  // Preserve full SYSCALL ID in X
    
    // Handle arguments based on count (bits 1-0)
    AND #0x03        // A = argument count
    TAY              // Y = argument count for jump table
    switch (Y)
    {
        case 0: { /* No arguments to pop */ }
        case 1: 
        { 
            Stacks.PopTop();  // Arg in ZP.TOP*
        }
        case 2: 
        { 
            Stacks.PopTop();   // Second arg in ZP.TOP*
            // Store second arg in temp location
            LDA ZP.TOPL STA ZP.Temp0L
            LDA ZP.TOPH STA ZP.Temp0H  
            LDA ZP.TOPT STA ZP.Temp0T
            Stacks.PopTop();   // First arg in ZP.TOP*
        }
        case 3: 
        { 
            // Handle 3-argument functions (future expansion)
            // TODO: Implement when needed
            TODO(); BIT ZP.EmulatorPCL
            States.SetFailure();
#ifdef TRACE
            LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL 
            LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH 
            Trace.MethodExit();
#endif
            return;
        }
    }
    
    // Extract function ID (bits 7-3) and dispatch
    TXA              // Restore full SYSCALL ID
    LSR LSR LSR      // A = function ID (0-31)
    switch (A)
    {
        case (SysCallType.PrintValue >> 3):    // ID = 1
        {
            Tools.PrintVariableValue();  // Uses ZP.TOP*
        }
        case (SysCallType.PrintNewLine >> 3):  // ID = 2  
        {
            LDA #'\n' Serial.WriteChar();
        }
        case (SysCallType.Abs >> 3):           // ID = 3
        {
            // ABS function - compute absolute value
            // Input: ZP.TOP* contains value and type
            // Output: ZP.TOP* contains absolute value
            LDA ZP.TOPT
            switch (A)
            {
                case BASICType.INT:
                {
                    // INT absolute value - check if negative
                    LDA ZP.TOPH
                    BMI negativeInt
                    // Already positive, leave unchanged
                    break;
                    
                negativeInt:
                    // Negate using two's complement
                    SEC
                    LDA #0
                    SBC ZP.TOPL
                    STA ZP.TOPL
                    LDA #0
                    SBC ZP.TOPH
                    STA ZP.TOPH
                    break;
                }
                case BASICType.WORD:
                case BASICType.BYTE:
                {
                    // WORD/BYTE always positive (unsigned)
                    break;
                }
                default:
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
#ifdef TRACE
                    LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL 
                    LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH 
                    Trace.MethodExit();
#endif
                    return;
                }
            }
        }
        case (SysCallType.Rnd >> 3):           // ID = 4
        {
            // RND function - random number generation
            // Input: ZP.TOP* contains max value
            // Output: ZP.TOP* contains random number 0 to max-1
            
            // TODO: Replace with actual implementation
            LDA #(Messages.NotImplemented % 256)
            STA ZP.LastErrorL
            LDA #(Messages.NotImplemented / 256)
            STA ZP.LastErrorH
            BRK
        }
        case (SysCallType.Millis >> 3):        // ID = 5
        {
            // MILLIS function - get system timer
            LDA ZP.TICK0 STA ZP.TOPL     // Get system timer
            LDA ZP.TICK1 STA ZP.TOPH
            LDA #BASICType.WORD STA ZP.TOPT
        }
        case (SysCallType.Seconds >> 3):       // ID = 6
        {
            // SECONDS function - get elapsed seconds
            Time.Seconds();              // Sets ZP.TOP*
        }
        case (SysCallType.Delay >> 3):         // ID = 7
        {
            // DELAY function - delay in milliseconds
            Time.Delay();                // Uses ZP.TOP*
        }
        case (SysCallType.Peek >> 3):          // ID = 8
        {
            // PEEK function - read memory byte
            // Input: ZP.TOP* contains address
            // Output: ZP.TOP* contains byte value
            
            // TODO: Replace with actual implementation
            LDA #(Messages.NotImplemented % 256)
            STA ZP.LastErrorL
            LDA #(Messages.NotImplemented / 256)
            STA ZP.LastErrorH
            BRK
        }
        case (SysCallType.Poke >> 3):          // ID = 9
        {
            // POKE function - write memory byte
            // Input: ZP.TOP* contains address, ZP.Temp0* contains value
            
            // TODO: Replace with actual implementation
            LDA #(Messages.NotImplemented % 256)
            STA ZP.LastErrorL
            LDA #(Messages.NotImplemented / 256)
            STA ZP.LastErrorH
            BRK
        }
        default:
        {
            TODO(); BIT ZP.EmulatorPCL
            States.SetFailure();
            
#ifdef TRACE
            LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL 
            LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH 
            Trace.MethodExit();
#endif
            return;
        }
    }
    
    // Handle return value (bit 2)
    TXA              // Restore full SYSCALL ID
    AND #0x04        // Test return value bit
    if (NZ) 
    {
        Stacks.PushTop();  // Push return value from ZP.TOP*
    }
    
    States.SetSuccess();
    
#ifdef TRACE
    LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL 
    LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH 
    Trace.MethodExit();
#endif
}
```

## Files NOT Requiring Changes

### Compiler.asm
- **Keep existing compilation methods** (compileAbsFunction, compileMillisFunction, etc.)
- **No changes needed** - they call Emit.Millis(), Emit.Abs(), etc. which we're updating
- **Tokens remain the same** - ABS, MILLIS, etc. are still language keywords

### Tokenizer.asm  
- **No changes needed** - built-in function tokens remain in keyword table

## Implementation Order

1. **OpCodes.asm** - Update enums (breaks compilation until other files updated)
2. **Emit.asm** - Update emission methods to use SYSCALL
3. **Executor.asm** - Remove old methods, implement new executeSysCall()
4. **Test** - Verify all built-in functions work identically

## TODO Items for Full Implementation

1. **Complete RND implementation** - Replace stub with actual random number generator
2. **Complete PEEK implementation** - Memory read with bounds checking  
3. **Complete POKE implementation** - Memory write with bounds checking
4. **Add error handling** - Invalid memory addresses, type mismatches
5. **Performance testing** - Measure SYSCALL dispatch overhead vs dedicated opcodes

## Success Criteria

- ✅ **Functional Compatibility**: All existing BASIC programs using built-in functions run unchanged
- ✅ **Opcode Space Freed**: 7 opcodes (0x1B-0x21) available for other uses  
- ✅ **Unified Architecture**: All built-ins use consistent SYSCALL mechanism
- ✅ **Extensibility**: Easy to add new built-in functions without changing opcode enum
- ✅ **Performance**: SYSCALL dispatch overhead acceptable for built-in function usage patterns

---

**Note**: This refactoring maintains the same external API while consolidating the internal implementation. The flags-based SYSCALL design provides efficient 6502 dispatch while keeping the architecture clean and extensible.