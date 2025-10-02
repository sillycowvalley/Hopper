# Hopper BIOS Applet Complete Guidelines

## 🚨 CRITICAL: Always Define CPU for Programs
```hopper
program MyApplet
{
    #define CPU_65C02S    // ALWAYS REQUIRED FOR PROGRAMS!
    
    uses "System/Definitions"
    // ... rest of program
}
```

## Environment: Always 65C02S
No conditional compilation needed for CPU features - we always have the enhanced instruction set.

---

# 📐 SYNTAX RULES

## Semicolon Usage - C-Style Statements ONLY
**Semicolons terminate C-style statements, NEVER 6502 assembly instructions:**

### ✅ CORRECT - Semicolons Required
```hopper
// C-style flow control
break;
continue;
return;

// Function calls
myFunction();
Serial.WriteChar();
Memory.Allocate();
```

### ❌ WRONG - No Semicolons on Assembly Instructions
```hopper
// WRONG - assembly instructions don't use semicolons!
LDA #0x10;    // NO!
STA ZP.VAR;   // NO!
INX;          // NO!

// CORRECT - no semicolons
LDA #0x10
STA ZP.VAR
INX
```

### Mixed Code Example
```hopper
processData()
{
    LDA #0x10        // No semicolon - assembly instruction
    STA ZP.ACCL      // No semicolon - assembly instruction
    
    Memory.Allocate();  // Semicolon - function call
    if (NC)
    {
        break;       // Semicolon - C-style statement
    }
    
    INX              // No semicolon - assembly instruction
    CPX #8           // No semicolon - assembly instruction
    if (Z)
    {
        continue;    // Semicolon - C-style statement
    }
    
    return;          // Semicolon - C-style statement
}
```

## Enum Usage in Switch Statements
**ALWAYS use `EnumName.Value` format in switch cases:**

### ✅ CORRECT - Enum.Value Format
```hopper
enum Key
{
    Up    = 128,
    Down  = 129,
    Left  = 130,
    Right = 131,
}

processKey()
{
    switch (A)
    {
        case Key.Up:      // CORRECT: EnumName.Value
        {
            View.CursorUp();
        }
        case Key.Down:    // CORRECT: EnumName.Value
        {
            View.CursorDown();
        }
    }
}
```

### ❌ WRONG - Other Formats
```hopper
// WRONG - Don't use Unit.Enum.Value format:
switch (A)
{
    case Keyboard.Key.Up:    // NO! Even if Key is in Keyboard unit
    {
        View.CursorUp();
    }
}

// WRONG - Don't use bare value:
switch (A)
{
    case Up:                 // NO! Must include enum name
    {
        View.CursorUp();
    }
}
```

### Even Within the Declaring Unit
```hopper
unit Keyboard
{
    enum Key
    {
        Up = 128,
        Down = 129,
    }
    
    ProcessInput()
    {
        switch (A)
        {
            case Key.Up:     // CORRECT even within Keyboard unit
            {
                // Not Keyboard.Key.Up or just Up
            }
        }
    }
}
```

## Zero Page Slot Naming
**ALWAYS use full public names for zero page slots, not internal aliases:**

### ✅ CORRECT - Use Public Names
```hopper
// Use the full public names from ZeroPage.asm
LDA ZP.TransferLengthL    // CORRECT
STA ZP.TransferLengthH    // CORRECT

LDY #0
loop
{
    CPY ZP.TransferLengthL  // CORRECT - full public name
    if (NC)
    {
        LDA ZP.TransferLengthH  // CORRECT - full public name
        if (Z) { break; }
    }
}
```

### ❌ WRONG - Don't Use Internal Aliases
```hopper
// WRONG - Don't use internal workspace names
LDA ZP.FS2    // NO! Use ZP.TransferLengthL
STA ZP.FS3    // NO! Use ZP.TransferLengthH

// WRONG - Even if you know FS2/FS3 are the internal names
CPY ZP.FS2    // NO!
LDA ZP.FS3    // NO!
```

### Why This Matters
```hopper
// ZeroPage.asm shows internal organization:
const byte FS2 = 0x44;  // Internal name
const byte FS3 = 0x45;  // Internal name

// But also provides public interfaces:
const byte TransferLength  = 0x44;  // Public name (uint)
const byte TransferLengthL = 0x44;  // Public name (low byte)
const byte TransferLengthH = 0x45;  // Public name (high byte)

// ALWAYS use the public names for clarity and maintainability
```

---

# 🔐 VISIBILITY RULES

## Public vs Private Members

### Naming Convention Determines Visibility
**First letter case determines access level:**
- **lowercase** = private to the unit
- **Uppercase** = public, accessible from other units

### Private Members (lowercase first letter)
```hopper
unit View
{
    // PRIVATE - lowercase first letter
    const uint vwLogicalCursor = viewSlots+0;
    const byte vwLogicalCursorL = viewSlots+0;
    const byte vwLogicalCursorH = viewSlots+1;
    
    // PRIVATE method - lowercase first letter
    updateInternal()
    {
        STZ vwLogicalCursorL  // Can access within this unit
    }
}

unit OtherUnit
{
    doSomething()
    {
        STZ View.vwLogicalCursorL  // ERROR! Cannot access private member
        View.updateInternal();      // ERROR! Cannot call private method
    }
}
```

### Public Members (Uppercase first letter)
```hopper
unit View
{
    // PUBLIC - Uppercase first letter
    const uint VwLogicalCursor = viewSlots+0;
    const byte VwLogicalCursorL = viewSlots+0;
    const byte VwLogicalCursorH = viewSlots+1;
    
    // PUBLIC method - Uppercase first letter
    UpdateDisplay()
    {
        STZ VwLogicalCursorL  // Can access
    }
}

unit OtherUnit
{
    doSomething()
    {
        STZ View.VwLogicalCursorL  // OK - public member
        View.UpdateDisplay();       // OK - public method
    }
}
```

### Friend Access Pattern
**Friend units can access private members:**

```hopper
unit View
{
    friend Editor, Buffer;  // These units can access private members
    
    // PRIVATE members
    const uint vwLogicalCursor = viewSlots+0;
    const byte vwLogicalCursorL = viewSlots+0;
    const byte vwLogicalCursorH = viewSlots+1;
    
    // PRIVATE method
    updateInternal()
    {
        // Implementation
    }
}

unit Editor  // Friend of View
{
    editText()
    {
        STZ View.vwLogicalCursorL  // OK - Editor is a friend
        View.updateInternal();      // OK - can call private method
    }
}

unit Buffer  // Friend of View
{
    resetBuffer()
    {
        STZ View.vwLogicalCursorL  // OK - Buffer is a friend
    }
}

unit Random  // NOT a friend of View
{
    doStuff()
    {
        STZ View.vwLogicalCursorL  // ERROR! Not a friend
    }
}
```

### Complete Visibility Example
```hopper
unit ScreenBuffer
{
    friend Renderer;  // Only Renderer can access private members
    
    // Base for all allocations
    const byte screenSlots = 0x58;
    
    // PUBLIC properties (Uppercase)
    const byte CursorCol = screenSlots+0;
    const byte CursorRow = screenSlots+1;
    
    // PRIVATE properties (lowercase)
    const byte sbWidth = screenSlots+2;
    const byte sbHeight = screenSlots+3;
    const uint sbBuffer = screenSlots+4;
    const byte sbBufferL = screenSlots+4;
    const byte sbBufferH = screenSlots+5;
    
    // PUBLIC method
    Initialize()  // Uppercase = public
    {
        STZ CursorCol
        STZ sbWidth    // Can access own private members
    }
    
    // PRIVATE method
    calculateOffset()  // lowercase = private
    {
        // Only callable within this unit or by friends
    }
}

unit Renderer  // Friend of ScreenBuffer
{
    Render()
    {
        LDA ScreenBuffer.sbWidth     // OK - friend access
        ScreenBuffer.calculateOffset();  // OK - friend access
    }
}

unit Application  // Not a friend
{
    Main()
    {
        STZ ScreenBuffer.CursorCol    // OK - public
        ScreenBuffer.Initialize();    // OK - public
        
        LDA ScreenBuffer.sbWidth      // ERROR - private!
        ScreenBuffer.calculateOffset();  // ERROR - private!
    }
}
```

---

# 📝 ZERO PAGE MANAGEMENT

## Critical Zero Page Map
```hopper
// BIOS RESERVED - NEVER TOUCH:
0x00-0x05   System flags, error, heap management
0x06-0x0D   Serial, I2C, file system  
0x0E-0x0F   Debug only
0x10-0x1F   Core registers (ACC, TOP, NEXT, IDX, IDY, STR)
0x20-0x29   System slots (jump table, BIOS dispatch, emulator)
0x24-0x27   Timer ticks
0x30-0x41   M0-M17 workspace (SPECIAL - see below!)
0x42-0x57   File system workspace

// FREE FOR APPLETS:
0x58-0xEB   YOUR SPACE (148 bytes available)

// HARDWARE I/O (immovable):
0xEC-0xFF   ACIA, VIA ports
```

## 🎯 Zero Page Best Practices

### 1. Use Single Base + Offsets Pattern
**Make your code relocatable by using a single const and offsets:**

```hopper
unit MyUnit
{
    // ALL zero page definitions at TOP of unit!
    
    // Single base constant - easy to relocate
    const byte mySlots = 0x58;
    
    // Public properties using offsets (Uppercase)
    const byte Property1 = mySlots+0;
    const byte Property2 = mySlots+1;
    const byte Property3 = mySlots+2;
    
    // 16-bit values need all three definitions
    const uint Pointer   = mySlots+3;
    const byte PointerL  = mySlots+3;
    const byte PointerH  = mySlots+4;
    
    // Private workspace continues with offsets (lowercase)
    const byte workspace1 = mySlots+5;
    const byte workspace2 = mySlots+6;
    
    // Methods follow...
}
```

### 2. ALL Zero Page Definitions at Top
**No scattered definitions! All ZP allocations must be at the top of the unit for clarity:**

```hopper
unit BadExample
{
    const byte var1 = 0x58;
    
    SomeMethod()
    {
        const byte var2 = 0x59;  // NO! Don't hide ZP definitions in methods
    }
}

unit GoodExample  
{
    // ALL zero page definitions here
    const byte var1 = 0x58;
    const byte var2 = 0x59;
    
    SomeMethod()
    {
        // Use them, don't define them here
    }
}
```

### 3. 16-bit Zero Page Naming Convention
**ALWAYS define uint AND L/H bytes separately to prevent errors:**

```hopper
// CORRECT - Define all three:
const uint myPointer = 0x5A;   // For [myPointer] syntax
const byte myPointerL = 0x5A;  // For LDA myPointerL
const byte myPointerH = 0x5B;  // For LDA myPointerH

// Usage patterns:
LDA [myPointer], Y    // Indirect addressing
LDA myPointerL        // Direct low byte access
STA myPointerH        // Direct high byte access
```

## 🔧 M0-M17 Workspace - The Tricky Part

### What Are M0-M17?
These are **shared leaf function workspace** locations (0x30-0x41) that can be reused by different functions under strict conditions.

### Rules for Using M0-M17:

1. **Leaf Functions Only** - Can only be used by functions that don't call other API methods
2. **No Survival Expected** - Values don't survive beyond the current function
3. **Check Conflicts** - Never use if calling functions that also use them
4. **Document Usage** - Update ZeroPage.asm documentation when adding new uses
5. **Prefer for Temporary Heavy Use** - Ideal for functions needing many temporary variables

### Who Currently Uses M0-M17?
From ZeroPage.asm documentation:
- **Memory.Allocate()** and **Memory.Free()** - Never call these if using M0-M17!
- **Time.Delay()** - Uses M0-M3 as TARGET0-3
- **Time.Seconds()** - Uses M0-M7 as RESULT0-7
- **Long math operations** - Use M0-M7 for RESULT
- **Debug functions** - Use M0-M15 as DB0-DB15
- **ScreenBuffer.Update()** - Major leaf API using M0-M9

### Example: ScreenBuffer Using M0-M17
```hopper
unit ScreenBuffer
{
    // Regular allocations
    const byte zeroPageSlots = 0x58;
    
    const byte CursorCol  = zeroPageSlots+0;
    const byte CursorRow  = zeroPageSlots+1;
    const byte Foreground = zeroPageSlots+2;
    const byte Background = zeroPageSlots+3;
    const byte Attributes = zeroPageSlots+4;
    
    // ... more regular slots ...
    
    // LEAF FUNCTION workspace using M0-M17
    // Update() is a leaf - doesn't call Memory.Allocate or other APIs
    const byte sbSize    = ZP.M0;   // Temporary use in Update()
    const byte sbSizeL   = ZP.M0;
    const byte sbSizeH   = ZP.M1;
    
    const byte sbRow     = ZP.M2;   // Only during Update()
    const byte sbCol     = ZP.M3;
    
    const byte sbOffset  = ZP.M4;
    const byte sbOffsetL = ZP.M4;
    const byte sbOffsetH = ZP.M5;
    
    Update()  // LEAF function - safe to use M0-M17
    {
        // Can freely use M0-M9 here
        // But CANNOT call Memory.Allocate() or Memory.Free()!
    }
    
    Initialize()  // NOT a leaf - calls Memory.Allocate
    {
        // CANNOT use M0-M17 here!
        Memory.Allocate();  // This uses M0-M17
    }
}
```

### When to Use M0-M17 vs New Slots?

**USE M0-M17 when:**
- Function is a true leaf (no API calls)
- Need many temporary variables
- Function is performance-critical
- Variables are purely temporary

**ALLOCATE NEW SLOTS when:**
- Function calls other APIs
- Values must survive function calls
- Not sure about conflicts
- Need permanent storage

---

# 📚 CONTROL FLOW IN HOPPER ASSEMBLY

## 🔄 LOOPS - Structured, Not Labels!

### ❌ FORBIDDEN - Traditional Assembly Style
```hopper
// NEVER DO THIS IN HOPPER:
loop_start:         // NO! No labels!
    DEX
    BNE loop_start  // NO! No branch instructions!
    
rowLoop:            // NO! Named loops don't exist!
    // ...
    JMP rowLoop     // NO! No JMP for loops!
```

### ✅ CORRECT - Hopper Structured Loops
```hopper
loop
{
    // Loop body
    if (done) { break; }      // Exits current loop
    if (skip) { continue; }    // Goes to top of current loop
}
```

### Loop Control Instructions
- **`break;`** - Exits the current loop immediately (note semicolon!)
- **`continue;`** - Jumps to the top of the current loop (note semicolon!)
- **NO `goto`** - Doesn't exist in Hopper
- **NO labels** - Loops are anonymous blocks

### Nested Loops - The Right Way
```hopper
// CORRECT nested loop pattern:
STZ rowIndex
loop    // Outer loop (anonymous!)
{
    STZ colIndex
    loop    // Inner loop (also anonymous!)
    {
        // Work with current position
        processCell();  // Function call needs semicolon
        
        INC colIndex    // Assembly - no semicolon
        LDA colIndex    // Assembly - no semicolon
        CMP width       // Assembly - no semicolon
        if (Z) { break; }  // C-style statement needs semicolon
    }
    
    INC rowIndex
    LDA rowIndex
    CMP height
    if (Z) { break; }  // Exits OUTER loop only
}
```

### Common Loop Patterns

#### Count from 0 to N-1:
```hopper
LDX #0
loop
{
    // Process element X
    INX
    CPX count
    if (Z) { break; }
}
```

#### Count down from N to 1:
```hopper
LDX count
loop
{
    // Process element X
    DEX
    if (Z) { break; }
}
```

#### Infinite loop with conditional exit:
```hopper
loop
{
    getInput();         // Function call - semicolon
    if (done) { break; }  // C-style - semicolon
    processInput();     // Function call - semicolon
}
```

#### Process null-terminated string:
```hopper
LDY #0
loop
{
    LDA [STR], Y        // Assembly - no semicolon
    if (Z) { break; }   // Null terminator
    
    processChar();      // Function call - semicolon
    INY                 // Assembly - no semicolon
}
```

---

## 🔀 SWITCHES - No Fall-Through!

### Critical Difference from C
**Hopper switches NEVER fall through!** Each case is isolated.

### ❌ WRONG - C-Style Thinking
```c
// This is C, NOT Hopper:
switch(x) {
    case 1:
        doA();
        break;  // In C, break exits the switch
    case 2:
        doB();
        // Falls through to case 3 in C
    case 3:
        doC();
        break;
}
```

### ✅ CORRECT - Hopper Switch
```hopper
switch (A)
{
    case 1:
    {
        doA();  // Function call - semicolon
        // No break needed - cases don't fall through
    }
    case 2:
    {
        doB();  // Function call - semicolon
        // Isolated from other cases
    }
    case 3:
    {
        doC();  // Function call - semicolon
    }
    default:
    {
        handleUnknown();  // Function call - semicolon
    }
}
```

### 🚨 CRITICAL: Break in Switch Cases
**`break;` inside a switch case exits the ENCLOSING LOOP, not the switch!**

```hopper
loop
{
    switch (A)
    {
        case 1:
        {
            if (error) 
            { 
                break;  // Exits the LOOP, not the switch!
            }
            processOne();  // Function call - semicolon
        }
        case 2:
        {
            processTwo();  // Function call - semicolon
            break;         // Exits the LOOP, not the switch!
        }
        default:
        {
            // Unknown
        }
    }
    // Only reached if no break was executed
    continueProcessing();  // Function call - semicolon
}
```

### Switch with Enum Values
```hopper
enum State
{
    Idle    = 0,
    Running = 1,
    Error   = 2,
}

processState()
{
    switch (A)
    {
        case State.Idle:     // ALWAYS EnumName.Value
        {
            handleIdle();
        }
        case State.Running:  // NOT just Running
        {
            handleRunning();
        }
        case State.Error:    // NOT Module.State.Error
        {
            handleError();
        }
    }
}
```

### Advanced Pattern: Switch with Dual Exit Paths
```hopper
// Elegant pattern using break semantics
processState()
{
    loop
    {
        switch (state)
        {
            case State.Active:    // EnumName.Value format
            {
                handleActive();  // Function call - semicolon
                SEC             // Assembly - no semicolon
            }
            case State.Waiting:   // EnumName.Value format
            {
                handleWaiting(); // Function call - semicolon
                SEC             // Assembly - no semicolon
            }
            default:
            {
                CLC             // Assembly - no semicolon
                break;          // C-style - semicolon (exit loop with error)
            }
        }
        // Success cases reach here
        break;  // Exit loop with success
    }
}
```

### Switch Optimization Rules
Switches can be optimized into jump tables when:
- Switch operates on **X or Y register** (not A)
- Each case contains **exactly one subroutine call**
- **Default case is present**
- **More than 8 cases** total
- Switch is followed immediately by **return;** or end of method

```hopper
// Optimizable switch:
switch (X)
{
    case 0: { Routine0(); }
    case 1: { Routine1(); }
    case 2: { Routine2(); }
    // ... more cases ...
    case 9: { Routine9(); }
    default: { DefaultRoutine(); }
}
return;  // Required for optimization
```

---

## 🎯 SINGLE EXIT PATTERN

### Purpose
Single exit makes code more maintainable and predictable. All paths converge to one point where cleanup can occur.

### ❌ WRONG - Multiple Returns
```hopper
badMethod()
{
    if (error1)
    {
        return;  // Early exit
    }
    
    if (error2)
    {
        return;  // Another exit
    }
    
    doWork();    // Function call - semicolon
    return;      // Yet another exit
}
```

### ✅ CORRECT - Single Exit
```hopper
goodMethod()
{
    loop
    {
        if (error1)
        {
            CLC      // Assembly - no semicolon
            break;   // C-style - semicolon
        }
        
        if (error2)
        {
            CLC      // Assembly - no semicolon
            break;   // C-style - semicolon
        }
        
        doWork();    // Function call - semicolon
        SEC          // Assembly - no semicolon
        break;       // C-style - semicolon
    }
    // Single exit point - cleanup here if needed
}
```

### Single Exit with Result Processing
```hopper
processData()
{
    loop
    {
        openFile();      // Function call - semicolon
        if (NC)          // Failed
        {
            LDA #Error.FileNotFound  // Assembly - no semicolon
            break;       // C-style - semicolon
        }
        
        readData();      // Function call - semicolon
        if (NC)          // Failed
        {
            LDA #Error.ReadError  // Assembly - no semicolon
            break;       // C-style - semicolon
        }
        
        processBuffer(); // Function call - semicolon
        LDA #Success     // Assembly - no semicolon
        break;           // C-style - semicolon
    }
    // Single exit - A contains result code
    // Any cleanup code goes here
}
```

---

## 🚫 NO BRANCH INSTRUCTIONS OR LABELS

### The Hopper Philosophy
Hopper Assembly uses **structured control flow** exclusively. Direct branches and labels are forbidden in normal code.

### ❌ FORBIDDEN Instructions
```hopper
// NEVER use these in regular Hopper code:
BEQ label    // NO! Use if (Z) { }
BNE label    // NO! Use if (NZ) { }
BCS label    // NO! Use if (C) { }
BCC label    // NO! Use if (NC) { }
BMI label    // NO! Use if (MI) { }
BPL label    // NO! Use if (PL) { }
JMP label    // NO! Use loop/break/continue
BRA label    // NO! Use structured flow
```

### ✅ CORRECT - Structured Equivalents
```hopper
// Instead of: BEQ skip_code
if (NZ)
{
    // Code to execute if not zero
}

// Instead of: BCS error_handler
if (NC)
{
    // Success path
}
else
{
    // Error path
}

// Instead of: JMP loop_start
loop
{
    // Loop body
    if (done) { break; }
}
```

### The ONLY Exception
Labels and JMP are allowed **ONLY** within a single method for optimization, and should be rare:

```hopper
optimizedMethod()
{
    // RARE EXCEPTION - optimization within single method
    LDX #8              // Assembly - no semicolon
    JMP entry           // Special case - no semicolon
    loop
    {
        INC counter     // Assembly - no semicolon
entry:
        ASL data        // Assembly - no semicolon
        DEX             // Assembly - no semicolon
        if (Z) { break; }
    }
}
```

---

## 65C02S Enhanced Instructions Always Available
```hopper
STZ address          // Store zero directly
PHX/PLX, PHY/PLY    // Direct stack ops for X,Y
BRA target          // Branch always (but don't use!)
TSB/TRB             // Test and set/reset bits
SMB0-7/RMB0-7       // Set/reset memory bits directly
BBS0-7/BBR0-7       // Branch on bit set/reset
INC A/DEC A         // Modify accumulator directly
[ZP.PTR]            // Zero page indirect (cleaner than (ZP),Y)
```

## BIOS Call Pattern
```hopper
// All BIOS calls:
LDX #SysCall.FunctionName
JMP [ZP.BIOSDISPATCH]

// Common patterns:
Print.String();       // ZP.STR = string pointer
Memory.Allocate();    // ZP.ACC = size → returns ZP.IDX
Time.Delay();         // ZP.TOP = milliseconds (32-bit)
GPIO.PinMode();       // A = pin, Y = mode
Serial.WriteChar();   // A = character
Long.Add();          // ZP.NEXT + ZP.TOP → ZP.NEXT
```

## String Handling - Do It Right
```hopper
// ALWAYS use string constants:
const string message = "Hello World!\n";

// Print it properly:
LDA #(message % 256)    // Assembly - no semicolon
STA ZP.STRL            // Assembly - no semicolon
LDA #(message / 256)   // Assembly - no semicolon
STA ZP.STRH            // Assembly - no semicolon
Print.String();        // Function call - semicolon

// NEVER spell out strings character by character!
```

## Success/Failure Convention
```hopper
// ALWAYS use carry flag:
DoSomething()
{
    // Try operation...
    if (failed)
    {
        CLC      // Clear carry = failure
        return;  // C-style - semicolon
    }
    
    SEC          // Set carry = success
}

// Caller checks:
DoSomething();   // Function call - semicolon
if (NC)          // No carry = failed
{
    // Handle error
}
```

## Register Preservation Rules
```hopper
PublicMethod()  // Uppercase = public
{
    // Only preserve registers THIS METHOD modifies:
    PHX         // ONLY if this method changes X
    PHY         // ONLY if this method changes Y
    // NEVER preserve A - caller's responsibility
    
    // Do work...
    
    PLY         // Only if we pushed Y
    PLX         // Only if we pushed X
    // Return with meaningful flag (usually C for success/failure)
}

privateHelper() // Lowercase = private  
{
    // Private methods called only internally
    // Check if caller already preserved - avoid double-preservation!
}
```

---

## 🎨 COMPLETE PATTERN EXAMPLES

### State Machine with Single Exit
```hopper
enum EscState
{
    Normal = 0,
    GotEsc = 1,
    GotBracket = 2,
}

processEscapeSequence()
{
    loop
    {
        LDA escState    // Assembly - no semicolon
        switch (A)
        {
            case EscState.Normal:  // EnumName.Value format
            {
                readChar();      // Function call - semicolon
                if (isEscape)
                {
                    LDA #EscState.GotEsc  // Assembly - no semicolon
                    STA escState         // Assembly - no semicolon
                    continue;            // C-style - semicolon
                }
                // Have regular char
                SEC              // Assembly - no semicolon
            }
            case EscState.GotEsc:   // EnumName.Value format
            {
                readChar();      // Function call - semicolon
                if (isBracket)
                {
                    LDA #EscState.GotBracket  // Assembly - no semicolon
                    STA escState             // Assembly - no semicolon
                    continue;                // C-style - semicolon
                }
                // Invalid sequence
                CLC              // Assembly - no semicolon
                break;           // Exit loop with error
            }
            default:
            {
                // Reset on error
                STZ escState     // Assembly - no semicolon
                CLC              // Assembly - no semicolon
                break;           // Exit loop with error
            }
        }
        break;  // Exit loop (success path reaches here)
    }
    // Single exit point
}
```

### File Processing with Proper ZP Names
```hopper
loadFileData()
{
    loop
    {
        File.StartLoad();     // Function call - semicolon
        if (NC) 
        { 
            break;            // Exit on error
        }
        
        loop
        {
            File.NextStream();  // Function call - semicolon
            if (NC) 
            { 
                break;         // EOF
            }
            
            // Process data using PUBLIC names
            LDY #0
            loop
            {
                // Use full public ZP names
                CPY ZP.TransferLengthL    // CORRECT - not ZP.FS2
                if (NC)
                {
                    LDA ZP.TransferLengthH // CORRECT - not ZP.FS3
                    if (Z) { break; }
                }
                
                // Process byte at 0x0600,Y
                LDA FileDataBuffer, Y     // Assembly - no semicolon
                processDataByte();        // Function call - semicolon
                
                INY                       // Assembly - no semicolon
            }
        }
        
        SEC     // Success
        break;
    }
    // Single exit point
}
```

### Nested Processing with Error Handling
```hopper
processMatrix()
{
    STZ error       // Assembly - no semicolon
    STZ row         // Assembly - no semicolon
    loop  // Process rows
    {
        STZ col     // Assembly - no semicolon
        loop  // Process columns
        {
            processElement();    // Function call - semicolon
            if (NC)              // Error occurred
            {
                INC error        // Assembly - no semicolon
                break;           // Exit column loop
            }
            
            INC col              // Assembly - no semicolon
            LDA col              // Assembly - no semicolon
            CMP width            // Assembly - no semicolon
            if (Z) { break; }    // Done with columns
        }
        
        // Check if column loop had error
        LDA error                // Assembly - no semicolon
        if (NZ) { break; }       // Exit row loop on error
        
        INC row                  // Assembly - no semicolon
        LDA row                  // Assembly - no semicolon
        CMP height               // Assembly - no semicolon
        if (Z) { break; }        // Done with rows
    }
    
    // Single exit - check final status
    LDA error                    // Assembly - no semicolon
    if (Z)
    {
        SEC                      // Assembly - no semicolon (Success)
    }
    else
    {
        CLC                      // Assembly - no semicolon (Failure)
    }
}
```

---

## 📋 Quick Reference Card

### Syntax Rules
1. **Semicolons on C-style statements only**: `break;` `continue;` `return;` `function();`
2. **NO semicolons on assembly instructions**: `LDA #0` `STA ZP.VAR` `INX`
3. **Enum usage**: Always `EnumName.Value` in switch cases, never `Unit.EnumName.Value` or bare `Value`
4. **Zero page names**: Always use full public names like `ZP.TransferLengthL`, not internal aliases like `ZP.FS2`
5. **Visibility by naming**: lowercase = private, Uppercase = public
6. **Friend access**: Declare friends to allow private member access

### Control Flow Rules
1. **NO labels** (except rare optimization within single method)
2. **NO branch instructions** (BEQ, BNE, JMP, etc.)
3. **Use `loop { }`** for all loops
4. **Use `if (condition) { }`** for all conditionals
5. **Switch cases don't fall through**
6. **`break;` in switch exits the enclosing loop**
7. **Single exit pattern** for maintainability

### Zero Page Rules
1. **Single base + offsets** for easy relocation
2. **ALL definitions at top** of unit
3. **Define uint AND L/H bytes** for 16-bit values
4. **M0-M17 for leaf functions only**
5. **Document M0-M17 usage** in ZeroPage.asm
6. **Use public names** not internal workspace aliases

### Flag Conditions
- `if (Z)` - Zero flag set
- `if (NZ)` - Zero flag clear
- `if (C)` - Carry set (usually success)
- `if (NC)` - Carry clear (usually failure)
- `if (MI)` - Negative flag set
- `if (PL)` - Positive flag clear

### Remember
**Hopper Assembly is STRUCTURED ASSEMBLY** - it combines the power of assembly with the clarity of structured programming. Embrace the structure!