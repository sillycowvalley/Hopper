# External Hopper VM Function Contracts
Documented based on actual source code analysis from /Source/Runtime/6502/

//==============================================================================
// STACK OPERATIONS (/Source/Runtime/6502/Stacks)
//==============================================================================

Stacks.Initialize()
// Input: None
// Output: Value and type stacks initialized, ZP.SP = 0, ZP.BP = 0, ZP.CSP = 0
// Modifies: ZP.SP, ZP.BP, ZP.CSP, ZP.IDXL (set to 0), ZP.IDXH, stack memory cleared
// Modifies: A, X (via Utilities.ClearPages)

Stacks.PopTop()
// Input: Value stack must not be empty
// Output: ZP.TOP = value, ZP.TOPT = type, ZP.SP decremented
// Modifies: ZP.TOP, ZP.TOPT, ZP.SP, X (used as index)

Stacks.PopNext()
// Input: Value stack must not be empty
// Output: ZP.NEXT = value, ZP.NEXTT = type, ZP.SP decremented
// Modifies: ZP.NEXT, ZP.NEXTT, ZP.SP, X (used as index)

Stacks.PopTopNext()
// Input: Value stack must have at least 2 entries
// Output: ZP.TOP = top value, ZP.NEXT = next value, types in ZP.TOPT/ZP.NEXTT
// Modifies: ZP.TOP, ZP.TOPT, ZP.NEXT, ZP.NEXTT, ZP.SP (decremented by 2)
// Note: Uses X register as index, may preserve X in INLINE_EXPANSIONS version

Stacks.PushTop()
// Input: ZP.TOP = value (16-bit), A = type
// Output: Value and type pushed to stack, ZP.SP incremented
// Modifies: ZP.SP, value stack memory, type stack memory, Y (used as index)

Stacks.PushNext()
// Input: ZP.NEXT = value (16-bit), A = type
// Output: Value and type pushed to stack, ZP.SP incremented
// Modifies: ZP.SP, value stack memory, type stack memory, Y (used as index)

Stacks.PushX()
// Input: X = value to push (8-bit, 0 or 1)
// Output: Value pushed to stack as Types.Bool, ZP.SP incremented
// Modifies: ZP.NEXT (X->NEXTL, 0->NEXTH), ZP.NEXTT (set to Types.Bool), ZP.SP
// Note: Calls PushNext() internally with A = Types.Bool

Stacks.PushACC()
// Input: ZP.ACC = value (16-bit), ZP.ACCT = type
// Output: Value and type pushed to stack, ZP.SP incremented
// Modifies: ZP.SP, value stack memory, type stack memory, Y (used as index)

Stacks.PopACC()
// Input: Value stack must not be empty
// Output: ZP.ACC = value (16-bit), ZP.SP decremented
// Modifies: ZP.ACC, ZP.SP, Y (used as index)
// Note: Does NOT populate ZP.ACCT (type is ignored)

Stacks.PopIDX()
// Input: Value stack must not be empty
// Output: ZP.IDX = value (16-bit), ZP.SP decremented
// Modifies: ZP.IDX, ZP.SP, Y (used as index)

//==============================================================================
// MEMORY MANAGEMENT (/Source/Runtime/6502/Memory) - HOPPER_BASIC VERSION
//==============================================================================

Memory.InitializeHeapSize()
// Input: ZP.PROGSIZE = program size in pages
// Output: Heap initialized, ZP.HEAPSTART and ZP.HEAPSIZE set, heap memory cleared
// Modifies: ZP.HEAPSTART, ZP.HEAPSIZE, ZP.FREELISTL, ZP.FREELISTH
// Modifies: A, X, Y, ZP.IDXL, ZP.IDXH (via Utilities.ClearPages and probeRAM)
// Note: May call probeRAM() which modifies additional registers

Memory.Allocate()
// Input: ZP.ACC = requested size (16-bit)
// Output: ZP.IDX = allocated address (0x0000 if allocation failed)
// Modifies: ZP.M* scratch space (internal to memory management operations)
// Preserves: ZP.ACC, A, X, Y, processor flags (via stack)

Memory.Free()
// Input: ZP.IDX = address to free (must not be 0x0000)
// Output: C set (success)
// Modifies: ZP.M* scratch space (internal to memory management operations)
// Preserves: ZP.IDX, ZP.ACC, A, X, Y, processor flags (via stack)

Memory.Available()
// Input: None
// Output: Available memory (UInt) pushed to value stack
// Modifies: Value stack, ZP.SP, calls AvailableACC() then Stacks.PushACC()

Memory.AvailableACC()
// Input: None
// Output: ZP.ACC = available memory (16-bit)
// Modifies: ZP.ACC, ZP.IDX, ZP.IDY (working registers for free list traversal)

//==============================================================================
// INTEGER MATH (/Source/Runtime/6502/IntMath)
//==============================================================================

IntMath.MulShared()
// Input: ZP.NEXT = left operand, ZP.TOP = right operand (both positive)
// Output: ZP.TOP = product (16-bit), ZP.TOPT = Types.UInt
// Modifies: ZP.TOP (result), ZP.TOPT (set to Types.UInt), ZP.UWIDE0-UWIDE3 (scratch)
// Note: Optimized for common cases (powers of 2, small numbers, 8x8 multiplication)

IntMath.UtilityDiv()
// Input: ZP.NEXT = dividend, ZP.TOP = divisor (both positive, divisor != 0)
// Output: ZP.NEXT = quotient
// Modifies: ZP.NEXT (result), ZP.TOP, ZP.ACC (remainder), calls DivMod()
// Note: Optimized for common divisors (1, 2, 4, 10, 50, 100)

IntMath.DivMod()
// Input: ZP.NEXT = dividend, ZP.TOP = divisor (both positive, divisor != 0)
// Output: ZP.NEXT = quotient, ZP.ACC = remainder
// Modifies: ZP.NEXT (quotient), ZP.ACC (remainder), X (loop counter)
// Note: Classic 16-bit division algorithm

IntMath.NegateTop()
// Input: ZP.TOP = value to negate
// Output: ZP.TOP = -ZP.TOP (two's complement negation)
// Modifies: ZP.TOP only

IntMath.NegateNext()
// Input: ZP.NEXT = value to negate
// Output: ZP.NEXT = -ZP.NEXT (two's complement negation)
// Modifies: ZP.NEXT only

//==============================================================================
// SERIAL COMMUNICATION (/Source/Runtime/6502/Serial)
//==============================================================================

Serial.Initialize()
// Input: None
// Output: Serial hardware initialized, buffers cleared
// Modifies: ZP.SerialInWritePointer, ZP.SerialInReadPointer, ZP.SerialBreakFlag (all set to 0)
// Modifies: Hardware state via SerialDevice.initialize()

Serial.IsAvailable()
// Input: None
// Output: Z flag clear if character available, Z set if not
// Modifies: Processor flags only
// Note: Disables/enables interrupts briefly, may call SerialDevice.pollRead()

Serial.WaitForChar()
// Input: None (waits for user input)
// Output: A = received character
// Modifies: A register, X register temporarily (preserved via stack on 65C02S)
// Note: Uses ZP.W1 on non-65C02S systems as workspace

Serial.WriteChar()
// Input: A = character to transmit
// Output: Character transmitted to serial port
// Modifies: Hardware state only, may call SerialDevice.pollRead()

Serial.HexOut()
// Input: A = byte to output as hexadecimal
// Output: Two hex digits transmitted (e.g., A=0x42 outputs "42")
// Modifies: CPU stack (uses PHA/PLA), calls WriteChar() twice
// Preserves: A register (restored to original value)

Serial.HexIn()
// Input: None (waits for two hex characters)
// Output: A = assembled byte value
// Modifies: A register, ZP.W0 (WorkSpaceHexIn), calls WaitForChar() and Utilities.MakeNibble()

//==============================================================================
// UTILITIES (/Source/Runtime/6502/Utilities)
//==============================================================================

Utilities.ClearPages()
// Input: ZP.IDX = start address, X = number of 256-byte pages to clear
// Output: Memory cleared (filled with zeros)
// Modifies: A (used as value to store), X (decremented), Y (page offset), ZP.IDXH (incremented)
// Preserves: ZP.IDXL (documented behavior - does NOT modify)

Utilities.MakeNibble()
// Input: A = character ('0'-'9' or 'A'-'F')
// Output: A = 4-bit value (0x00-0x0F)
// Modifies: A register only

Utilities.IncIDX()
// Input: ZP.IDX = 16-bit value
// Output: ZP.IDX incremented by 1
// Modifies: ZP.IDX only

Utilities.IncIDY()
// Input: ZP.IDY = 16-bit value
// Output: ZP.IDY incremented by 1
// Modifies: ZP.IDY only

Utilities.DecACCx2()
// Input: ZP.ACC = 16-bit value
// Output: ZP.ACC decremented by 2
// Modifies: ZP.ACC, A register (preserved via stack)

Utilities.CopyBytes()
// Input: ZP.FSOURCEADDRESS = source, ZP.FDESTINATIONADDRESS = dest, ZP.LCOUNTL/H = count
// Output: Bytes copied from source to destination
// Modifies: ZP.LCOUNTL/H (decremented to 0), ZP.FSOURCEADDRESS (incremented), 
//          ZP.FDESTINATIONADDRESS (incremented), A, Y (used as offset)

//==============================================================================
// CONTRACT NOTES
//==============================================================================

// SCRATCH SPACE CONVENTIONS (observed from source):
// - ZP.M0-M15: Memory manager scratch space (Memory.Allocate/Free)
// - ZP.UWIDE0-UWIDE3: Math operation scratch space (IntMath functions)
// - ZP.W0-W7: General workspace (Serial uses W0, W1)
// - ZP.F*: File/string operation workspace (FSOURCEADDRESS, FDESTINATIONADDRESS, etc.)
// - ZP.L*: List/long operation workspace (LCOUNTL/H, etc.)

// REGISTER PRESERVATION PATTERNS:
// - HOPPER_BASIC versions of Memory functions preserve A, X, Y, ZP.ACC via stack
// - Serial functions generally preserve registers except documented outputs
// - Stack functions intentionally modify stack interface registers (ZP.TOP, etc.)
// - Math functions use dedicated scratch space (ZP.UWIDE*)

// ERROR HANDLING:
// - Memory.Allocate() returns 0x0000 on failure (check ZP.IDX)
// - Math functions assume valid inputs (caller must check division by zero)
// - Stack functions assume sufficient stack space (no underflow/overflow checks)

// PERFORMANCE CHARACTERISTICS:
// - All external functions are hand-tuned for 6502 performance
// - Math functions include optimizations for common cases
// - Memory functions use efficient free list management
// - Stack operations use direct array access with minimal overhead