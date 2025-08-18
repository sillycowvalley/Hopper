# LONG Type Implementation Chores

Our current stat is reflected in BASIC.asm where the HopperBASIC program is.
While we have a lot of reference source code, our project only includes what is included by BASIC.asm, etc.

RULE #0:  TOP PRIORITY
Assume that the project knowledge is current and up to date. That includes source code, guideline documentation and specifications.

Read it all BEFORE (including all the other rules here) try to solve a problem. If you do not have access to the project knowledge or are having problems accessing the project knowledge, tell me (and don't generate any code).

Search Strategy for this project:
- Hopper Assembly files (.asm): Search "unit" and "program" to find all source files
- Documentation (.md): Search "Document Type" to find all documentation, or search specific types
- Always search before coding - the answer is usually already implemented or documented

Or, in short "Use the source, Luke!"

Always warn me if you spot a duplicate document in the project knowledge so that I can remove the older one.

RULE #1:   
SILENT FAILURE IS NEVER ACCEPTABLE!
BRK is not a silent failure (the emulator throws an exception on BRK). A good pattern for a 'TODO' is:
`// TODO: Show variables
LDA #(Messages.NotImplemented % 256)
STA ZP.LastErrorL
LDA #(Messages.NotImplemented / 256)
STA ZP.LastErrorH
BRK`

RULE #2:
You may not use ZeroPage slots that are not part of the BASIC project without asking first.  For storing temporary values over a short simple section of code, prefer the stack over zero page variables (PHX, PHY, PHA ..)

RULE #3:
For comments about flags, don't use '0' and '1'. Use comments like these which are much easier for a dyslexic person to understand:
// Set Z
// Set NZ

RULE #4:
NEVER DO THIS:
    // Rest of the function remains the same...
Always generate complete methods. The above is just crying for cut and paste errors and loss of good code.

RULE #5:
When asked to analyze a bug, don't start by generating tons of code because you think you know what it is. You are usually wrong and do multiple iterations of this tedious approach. Start by suggesting where we should insert debug output statements (using our debug output methods from the Tools unit in preference to Serial.WriteChar methods since they preserve flags, etc) so we can further diagnose the issue.
"Thoughts?" is shorthand for "Rule #5" - don't generate code, just do some analysis, etc.
Align debug output code on the left margin (no indentation) so that it makes it easier to see and remove later.
When I ask you to annotate a Crash dump you have the zero page available so should be able to annotate any pertinent ZP.xxx variables by name. If there is a stack from the TRACE MethodEntry and MethodExit calls, you can examine the source at the near the location when Debug.Crash was called and give relevant ZP.xx variables values.

RULE # 6:
The "X-Indexed Zero Page Indirect" addressing mode for 6502 is forbidden for this project (opcodes like A1 and 81 for example). It is an evil and highly error prone addressing mode.

RULE #7:
Prefer C | NC flags over Z | NZ when using a flag to return the success (C) or failure (NC) status of a method call.

Rule #8:
Identifiers like NEXT_POINTER_OFFSET are just plain ugly and loud. Prefer CamelCase | camelCase where possible.

**RULE #9:**
When using enum values, don't qualify them with the unit namespace. Use the direct enum syntax:
Correct:
LDA #SymbolType.VARIABLE
Incorrect:
LDA #Objects.SymbolType.VARIABLE
The enum names are imported into the current scope when you `uses` the unit that defines them, so the shorter, cleaner syntax is preferred.

RULE #10: Hopper Assembly Switch Statements
Hopper Assembly switch statements automatically break at the end of each case - there is no fall-through behavior. Avoid break in switch cases because it breaks out of the surrounding loop, not the case, creating nasty bugs.

**RULE #11: What Changed?**
When something that was working stops working during incremental development, always start with "what changed?" not "what's broken?" The bug is almost certainly in the new/modified code, not in the code that was working before.

This rule pairs well with your Rule #5 ("Thoughts?" - analyze before generating code). Together they promote systematic debugging:
1. What changed?
2. Where in that change could the issue be?
3. Add targeted debug output there
4. Only then generate fixes





Notes:

The large FREE block at the end of the heap dump is the remaining from part of the heap (remaining RAM). It is not corruption.

namespace / unit qualification (like ZP.) is optional for Hopper Assembler - only required to resolve ambiguity where two units have the same public symbol (that's why it compiles).
However it is good practice to include the namespace to avoid nasty defects in future (when a 2nd unit adds the same name - actually the assembler should call that out as an error (ambiguity)).

Avoid excessive promotional language in documentation and stick to simple checkmarks (✅) and crosses (❌) to indicate implementation status.


## Target Benchmark
```basic
NEW
CLS
MEM
! Noel's RetroLab Benchmark
BEGIN
    LONG START = MILLIS()
    LONG S
    WORD J
    FOR i=1 TO 10
        s=0
        FOR j=1 TO 1000
            s=s+j
        NEXT j
        PRINT ".";
    NEXT i
    PRINT s
    PRINT MILLIS() - START; " ms"
END
RUN
```

## Phase 1: Stack Operations (Ready to implement)

### ✅ Infrastructure Already Complete
- **Zero Page Variables**: LNEXT0-3 (0x73-0x76), LTOP0-3 (0x77-0x7A), LRESULT0-7 (M0-M7)
- **Stack Memory Layout**: 5-byte slots already allocated
  ```
  0x0500: TypeStackLSB     (type information)
  0x0600: ValueStackLSB    (byte 0 - LSB)  
  0x0700: ValueStackMSB    (byte 1)
  0x0800: ValueStackMSB2   (byte 2) 
  0x0900: ValueStackMSB3   (byte 3 - MSB)
  ```

### Task 1.1: LONG Stack Operations
- **File**: `stacks.asm` 
- **New Functions**:
  ```hopper
  Stacks.PushLong()    // Push 4-byte value from LNEXT0-3 + BASICType.LONG
  {
      LDY ZP.SP                    // Current stack pointer
      LDA #BASICType.LONG
      STA TypeStackLSB,Y           // Store type
      LDA ZP.LNEXT0
      STA ValueStackLSB,Y          // Store byte 0 (LSB)
      LDA ZP.LNEXT1  
      STA ValueStackMSB,Y          // Store byte 1
      LDA ZP.LNEXT2
      STA ValueStackMSB2,Y         // Store byte 2
      LDA ZP.LNEXT3
      STA ValueStackMSB3,Y         // Store byte 3 (MSB)
      INC ZP.SP                    // Advance stack pointer
  }
  
  Stacks.PopLong()     // Pop 4-byte value to LTOP0-3, verify LONG type
  {
      DEC ZP.SP                    // Move back to top item
      LDY ZP.SP
      LDA ValueStackLSB,Y          // Load byte 0 (LSB)
      STA ZP.LTOP0
      LDA ValueStackMSB,Y          // Load byte 1
      STA ZP.LTOP1  
      LDA ValueStackMSB2,Y         // Load byte 2
      STA ZP.LTOP2
      LDA ValueStackMSB3,Y         // Load byte 3 (MSB)
      STA ZP.LTOP3
      // Optional: verify type is BASICType.LONG
  }
  ```
- **Dependencies**: None (infrastructure ready)
- **Deliverable**: Working LONG stack operations

## Phase 2: Type System (Core LONG support)

### Task 2.1: LONG Type Definition  
- **Status**: ✅ Already exists - `BASICType.LONG = 0x05`
- **Dependencies**: None
- **Deliverable**: LONG type can be identified in type checks

### Task 2.2: TOLONG Opcode (Type Promotion)
- **File**: CPU instruction decoder
- **New Opcode**: `OpCode.TOLONG = 0x1D`
- **Implementation**:
  ```assembly
  TOLONG:
      Stacks.PopTop()           // Get value and type to TOPL/TOPH
      LDA typeFromStack
      AND #BASICType.TYPEMASK   // Mask VAR/ARRAY flags
      
      switch (A) {
          case BASICType.BYTE:
              // Zero-extend: 0x12 -> 0x00000012
              LDA ZP.TOPL
              STA ZP.LNEXT0
              STZ ZP.LNEXT1
              STZ ZP.LNEXT2  
              STZ ZP.LNEXT3
              break;
              
          case BASICType.WORD:
              // Zero-extend: 0x1234 -> 0x00001234  
              LDA ZP.TOPL
              STA ZP.LNEXT0
              LDA ZP.TOPH
              STA ZP.LNEXT1
              STZ ZP.LNEXT2
              STZ ZP.LNEXT3
              break;
              
          case BASICType.INT:
              // Sign-extend: check high bit, extend with 0x00 or 0xFF
              LDA ZP.TOPL
              STA ZP.LNEXT0
              LDA ZP.TOPH
              STA ZP.LNEXT1
              // Check sign bit in TOPH
              BPL positive
              LDA #0xFF    // Negative: extend with 0xFF
              JMP setHighBytes
          positive:
              LDA #0x00    // Positive: extend with 0x00
          setHighBytes:
              STA ZP.LNEXT2
              STA ZP.LNEXT3
              break;
              
          default:
              // Error: unsupported type
              break;
      }
      
      Stacks.PushLong()  // Push as BASICType.LONG
  ```
- **Dependencies**: Task 1.1
- **Deliverable**: Any numeric type can be promoted to LONG

### Task 2.3: Variable Storage Extension
- **Files**: Variable management system  
- **Changes**: Extend variable records from 2 to 4 bytes for LONG variables
- **Implementation**: 
  - Variables with `BASICType.LONG` use 4 bytes storage
  - Use field constants, not hardcoded offsets  
  - Update get/set variable functions
- **Dependencies**: Task 2.1
- **Deliverable**: LONG variables can be stored and retrieved

## Phase 3: Arithmetic Operations (Core functionality)

### Task 3.1: Type-Aware Addition
- **File**: Arithmetic opcodes
- **Changes**: Modify ADD opcode to detect LONG types
- **Implementation**:
  ```assembly
  ADD_Opcode:
      // Check if either operand is LONG
      LDA stackType1
      AND #BASICType.TYPEMASK
      CMP #BASICType.LONG
      BEQ checkSecondOperand
      LDA stackType2  
      AND #BASICType.TYPEMASK
      CMP #BASICType.LONG
      BEQ promoteFirstThenAdd
      
      // Neither is LONG: use existing 16-bit add
      JMP existing16BitAdd
      
  promoteFirstThenAdd:
      // Second operand is LONG, promote first
      // Stack: [LONG][other] -> [LONG][LONG]
      // Implementation details for promotion
      
  checkSecondOperand:
      LDA stackType2
      AND #BASICType.TYPEMASK  
      CMP #BASICType.LONG
      BEQ bothAreLong
      // First is LONG, promote second
      
  bothAreLong:
      JMP Long.Add    // Use existing library function
  ```
- **Dependencies**: Tasks 1.1, 2.2, existing `Long.Add()`
- **Deliverable**: `s=s+j` works with automatic type promotion

### Task 3.2: Type-Aware Subtraction  
- **File**: Arithmetic opcodes
- **Changes**: Modify SUB opcode to detect LONG types
- **Implementation**: Similar to Task 3.1 but call `Long.Sub()`
- **Dependencies**: Tasks 1.1, 2.2, existing `Long.Sub()`
- **Deliverable**: `MILLIS() - START` works with LONG operands

### Task 3.3: Type Promotion Utilities
- **File**: Type conversion utilities
- **New Functions**:
  ```hopper
  PromoteStackToLong()     // If stack top is not LONG, convert it
  PromoteBothToLong()      // Ensure both stack items are LONGs
  ```
- **Implementation**: Check types, call TOLONG logic when needed
- **Dependencies**: Tasks 1.1, 2.2
- **Deliverable**: Clean promotion utilities for arithmetic operations

## Phase 4: LONG Literals (Minimal for benchmark)

### Task 4.1: Simple LONG Literal Support
- **Scope**: Handle `s=0` case specifically  
- **Implementation**: 
  ```hopper
  // For LONG variable = small constant
  // Compiler emits: PUSHBYTE 0, TOLONG
  ```
- **Alternative approaches**:
  - **Zero optimization**: Special case for zero constants
  - **Small constant optimization**: PUSHBYTE + TOLONG for values 0-255
  - **Full literals**: Defer until after benchmark works
- **Dependencies**: Task 2.2 (TOLONG)
- **Deliverable**: Benchmark can compile `s=0` assignment

### Task 4.2: LONG Variable Assignment
- **File**: Assignment operations
- **Changes**: Handle assignment to LONG variables
- **Implementation**: 
  - Check variable type before assignment
  - If target is LONG and source isn't, promote source
  - Store 4 bytes instead of 2 for LONG variables
- **Dependencies**: Tasks 2.2, 2.3, 4.1
- **Deliverable**: `LONG S` and `s=0` work together

## Phase 5: System Integration (Complete the benchmark)

### Task 5.1: MILLIS() Returns LONG
- **File**: Time system functions  
- **Changes**: Modify `MILLIS()` to return 32-bit value as BASICType.LONG
- **Implementation**: 
  ```hopper
  MILLIS():
      // Read 32-bit timer value into LNEXT0-3
      // (existing timer reading logic, extended to 32-bit)
      Stacks.PushLong()    // Push as LONG instead of WORD
  ```
- **Dependencies**: Task 1.1
- **Deliverable**: `LONG START = MILLIS()` works

### Task 5.2: LONG to String Conversion
- **File**: Print/String utilities
- **New Function**: `LongToString()` - Convert 32-bit value to decimal string
- **Algorithm**: 
  ```hopper
  LongToString():
      // Use existing Long.DivMod() with divisor 10
      // Collect remainder digits in reverse order
      // Handle sign for negative values
      // Reverse digit string for final result
  ```
- **Dependencies**: Task 1.1, existing `Long.DivMod()`
- **Deliverable**: `PRINT s` and `PRINT MILLIS() - START` work

### Task 5.3: LONG Variable Declaration
- **File**: BASIC compiler  
- **Changes**: Parse `LONG variableName` declarations
- **Implementation**: 
  - Recognize `LONG` keyword in variable declarations
  - Set variable type to `BASICType.LONG` in symbol table
  - Allocate 4 bytes storage instead of 2
  - Update variable access code generation
- **Dependencies**: Tasks 2.1, 2.3
- **Deliverable**: `LONG S`, `LONG START` declarations work

## Phase 6: Testing & Validation

### Task 6.1: Unit Tests
- **Scope**: Test each component individually
- **Tests**:
  - **Stack Operations**: PushLong/PopLong with known values
  - **TOLONG Conversion**: 
    - BYTE→LONG: 0x42 → 0x00000042
    - WORD→LONG: 0x1234 → 0x00001234  
    - INT→LONG: 0x8000 → 0xFFFF8000 (negative), 0x7FFF → 0x00007FFF (positive)
  - **Arithmetic**: LONG+LONG, LONG+WORD with promotion
  - **String Conversion**: Positive/negative LONGs to decimal strings
- **Dependencies**: All previous tasks
- **Deliverable**: Confidence in individual components

### Task 6.2: Integration Test
- **Scope**: Run the complete benchmark
- **Expected Output**: 
  ```
  ..........50005000 [time] ms
  ```
- **Validation**:
  - Correct sum: 10 * (1+2+...+1000) = 10 * 500500 = 5005000
  - MILLIS() timing measurement works
  - No crashes or memory corruption
- **Dependencies**: All previous tasks
- **Deliverable**: Working LONG implementation

## Implementation Notes

### Memory Impact
- **Stack arrays**: ✅ Already allocated (+512 bytes: 0x0800, 0x0900)
- **Variable storage**: +2 bytes per LONG variable  
- **Zero page**: ✅ Already allocated (LNEXT0-3, LTOP0-3, LRESULT0-7)

### Performance Considerations
- **Type checking overhead**: Every arithmetic operation includes type detection
- **Promotion cost**: Converting smaller types to LONG takes additional cycles
- **32-bit arithmetic**: LONG operations inherently slower than 16-bit
- **Stack operations**: LONG push/pop takes longer due to 4-byte transfers

### Integration with Existing Code
- **Existing `long.asm`**: ✅ Ready to integrate (uses correct ZP variables)
- **Type system**: ✅ BASICType.LONG already defined
- **Stack infrastructure**: ✅ Memory layout ready
- **Backward compatibility**: Existing BYTE/WORD/INT operations unchanged

### Risk Mitigation
- **Incremental testing**: Test each phase independently
- **Type safety**: TOLONG handles all existing numeric types properly
- **Memory safety**: Stack operations respect existing boundaries
- **Fallback compatibility**: Non-LONG operations work exactly as before

## Success Criteria
✅ Benchmark compiles without errors  
✅ Benchmark runs and produces correct output (50005000)  
✅ Timing measurement works (MILLIS() subtraction shows reasonable time)  
✅ Existing BYTE/WORD/INT programs continue to work unchanged  
✅ Type promotion works seamlessly in mixed arithmetic (`s=s+j`)  
✅ Performance impact is acceptable for the added functionality  
✅ No memory corruption or stack overflow issues

## Future Enhancements (Post-Implementation)
- **Efficient LONG literals**: Direct 32-bit constants without TOLONG
- **Complete LONG arithmetic**: Multiplication, division, modulo
- **LONG bitwise operations**: AND, OR, XOR, shifts
- **LONG arrays**: Support for arrays of LONG values
- **LONG comparisons**: All comparison operators with mixed types
- **Optimization**: Faster type detection, better constant handling
- **LONG I/O**: Reading LONG values from input, formatted output