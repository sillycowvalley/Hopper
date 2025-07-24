# HopperBASIC Symbol Table Design

## Overview

HopperBASIC uses a unified symbol table to store all program identifiers: variables, constants, and functions (including the main program).

## Design Decisions

### Unified Table Structure
- **Single table** stores all identifier types (variables, constants, functions)
- **Shared implementation** reduces code size and complexity
- **Generic operations** work across all identifier types

### Simple Data Structure
- **Linked list** implementation
- Linear search through entries
- No optimization for lookup speed

### Runtime Resolution
- **Identifiers stored as strings** during tokenization  
- **Lookup performed at runtime** when tokens execute
- **No caching** of resolved addresses

## Rationale

### Why Simple Linear Search?
- **Small scale**: Typical programs have 5-20 total identifiers
- **Rare operation**: Lookups only occur during statement execution
- **6502 performance**: Linear search of 20 items is microseconds
- **User perception**: No noticeable delay

### Why Runtime Resolution?
- **Forward references**: Functions can call functions defined later
- **Dynamic modification**: FORGET and redefinition change symbol meanings
- **Implementation simplicity**: No cache invalidation or dependency tracking needed

### Why Unified Table?
- **Code reuse**: Single implementation for all identifier types
- **FORGET command**: Single lookup covers variables, constants, and functions
- **Memory efficiency**: Shared allocation and management strategy
- **Debugging**: One table structure to understand and debug

## Table Operations

### Core Functions
- `TableLookup(name)` - Find entry by name across all types
- `TableAdd(name, type, dataType, value)` - Add new entry  
- `TableRemove(name)` - Remove entry (for FORGET)
- `TableClear()` - Clear all entries (for NEW)
- `TableIterate(entryType)` - Iterate by type (for VARS, FUNCS, CONSTS commands)

### Entry Types
- **VARIABLE** - Mutable values (INT, WORD, BIT, BYTE, STRING, ARRAY)
- **CONSTANT** - Immutable values (defined with CONST)
- **FUNCTION** - Executable code blocks (including main program from BEGIN/END)

## Implementation Benefits

1. **Minimal code footprint** - Single table implementation
2. **Predictable behavior** - Consistent operations across identifier types  
3. **Easy debugging** - One data structure to inspect and validate
4. **Flexible execution** - Supports forward references and dynamic redefinition
5. **Memory efficient** - No duplicate table management code

## Performance Characteristics

- **Lookup time**: O(n) where n ≈ 20 maximum
- **Memory overhead**: Minimal - simple linked list nodes
- **Runtime cost**: Negligible on target 6502 systems
- **Development cost**: Low complexity, easy to implement and maintain

---

## Two-Layer Design

### Layer 1: Table (Generic Linked List)
Pure linked list operations with no knowledge of record contents:

```hopper
unit Table
{
    // Get first node in list
    // Input: X = ZP address of list head pointer
    // Output: ZP.IDX = first node (0x0000 if empty list)
    // Preserves: A, Y, ZP.ACC, ZP.TOP, ZP.NEXT
    GetFirst();
    
    // Get next node in traversal
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next node (0x0000 if end of list)
    // Preserves: A, X, Y, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
    GetNext();
    
    // Add new node to list
    // Input: X = ZP address of list head pointer, ZP.ACC = node size (16-bit)
    // Output: ZP.IDX = new node address (0x0000 if allocation failed), C set if successful
    // Preserves: A, Y, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Add();
    
    // Delete specific node from list
    // Input: X = ZP address of list head pointer, ZP.IDX = node to delete
    // Output: C set if successful, NC if node not found
    // Preserves: A, Y, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Delete();
    
    // Clear entire list (free all nodes)
    // Input: X = ZP address of list head pointer
    // Output: None (list head set to 0x0000 in place)
    // Preserves: A, Y, ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Clear();
}
```

### Layer 2: Objects (Symbol Table Implementation)
Symbol-specific operations using Table foundation:

#### Node Layout
Symbol nodes now have a 7-byte fixed header plus variable-length name:
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   symbolType|dataType (packed byte)
            High nibble: SymbolType (VARIABLE=1, CONSTANT=2, FUNCTION=3)
            Low nibble:  BasicType (INT=2, BYTE=3, WORD=4, BIT=6, etc.)
Offset 3-4: value/address (16-bit current value)
Offset 5-6: tokens pointer (16-bit pointer to initialization token stream)
Offset 7+:  null-terminated name string
```

#### Constants
```hopper
const byte symbolOverhead = 7;       // Fixed fields before name
const byte typeOffset = 2;           // Offset to symbolType|dataType field
const byte valueOffset = 3;          // Offset to value field
const byte tokensOffset = 5;         // Offset to tokens pointer field
const byte nameOffset = 7;           // Offset to name field in node
```

#### Zero Page Storage
Dedicated zero page locations that persist across `Memory.Allocate()` calls:
- **ZP.SymbolType** (0x72): symbolType|dataType packed byte
- **ZP.SymbolValueL/H** (0x73-0x74): 16-bit symbol value
- **ZP.SymbolNameL/H** (0x75-0x76): Pointer to symbol name string

```hopper
unit Objects
{
    // Symbol types
    enum SymbolType
    {
        VARIABLE = 0x01,   // Mutable values
        CONSTANT = 0x02,   // Immutable values  
        FUNCTION = 0x03    // Executable code blocks
    }
    
    // Initialize empty symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: All registers and ZP variables
    Initialize();
    
    // Add new symbol to table
    // Input: ZP.ACC = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.TOP = value (16-bit), ZP.NEXT = tokens pointer (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Add call)
    // Uses: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName for parameter storage
    Add();
    
    // Find symbol by name
    // Input: ZP.ACC = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: Available slots in 0x77-0x7F range for temporary operations
    Find();
    
    // Remove symbol by node pointer
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: C set if successful, NC if node not found
    // Preserves: A, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Delete call)
    // Uses: ZP.Lxx variables as temporary workspace
    Remove();
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.TOP = value, ZP.NEXT = tokens pointer
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC
    GetData();
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.TOP = new value
    // Output: C set if successful, NC if not a variable
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    SetValue();
    
    // Start iteration for specific symbol type
    // Input: ZP.ACCL = symbol type filter (0 = all types)
    // Output: ZP.IDX = first matching symbol, C set if found, NC if none
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateStart();
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.ACCL = type filter
    // Output: ZP.IDX = next matching symbol, C set if found, NC if done
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateNext();
    
    // Destroy entire symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Clear call)
    Destroy();
}
```

### Layer 3: Variables (High-Level BASIC Interface)
BASIC-specific variable and constant management:

```hopper
unit Variables
{
    // Variable management using Objects foundation
    // Two-stage approach: Find name to address, then operate on address
    
    // Initialize variable system
    // Output: Symbol table cleared and ready
    // Preserves: All registers and ZP variables
    Initialize();
    
    // Declare new variable or constant
    // Input: ZP.ACC = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.TOP = initial value (16-bit), ZP.NEXT = tokens pointer (16-bit)
    // Output: C set if successful, NC if error (name exists, out of memory)
    // Uses: Objects.Add() internally with extended node layout
    Declare();
    
    // Find variable/constant name to address
    // Input: ZP.ACC = name pointer, ZP.ACCL = expected symbolType (VARIABLE or CONSTANT, 0 = any)
    // Output: ZP.IDX = symbol node address, C set if found and correct type, NC if not found or wrong type
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Error: Sets LastError if found but wrong type
    Find();
    
    // Get variable/constant value by address
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.TOP = value, ZP.TOPT = dataType, C set if successful, NC if error
    // Preserves: A, X, Y, ZP.ACC, ZP.NEXT
    // Error: Fails if node is not variable or constant
    GetValue();
    
    // Set variable value by address (variables only)
    // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new value
    // Output: C set if successful, NC if error (not a variable, type mismatch)
    // Preserves: A, X, Y, ZP.ACC, ZP.NEXT
    // Error: Fails if node is not a variable (constants rejected)
    SetValue();
    
    // Get type info by address
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), C set if successful, NC if error
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Error: Fails if node is not variable or constant
    GetType();
    
    // Get name from current node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACC = name pointer (points into node data)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    GetName();
    
    // Get initialization tokens from current node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.NEXT = tokens pointer (points to initialization token stream)
    // Preserves: A, X, Y, ZP.TOP, ZP.ACC
    GetTokens();
    
    // Remove variable or constant by name
    // Input: ZP.ACC = name pointer
    // Output: C set if successful, NC if not found
    // Uses: Objects.Remove() internally
    Remove();
    
    // Start iteration over variables only (for VARS command)
    // Output: ZP.IDX = first variable node, C set if found, NC if none
    // Sets up iteration state for IterateNext()
    IterateVariables();
    
    // Start iteration over constants only (for CONSTS command if added)
    // Output: ZP.IDX = first constant node, C set if found, NC if none
    IterateConstants();
    
    // Start iteration over all symbols (for general enumeration)
    // Output: ZP.IDX = first symbol node, C set if found, NC if none
    IterateAll();
    
    // Continue iteration (use after any Iterate* method)
    // Input: ZP.IDX = current node, ZP.ACCL = type filter from previous call
    // Output: ZP.IDX = next matching node, C set if found, NC if done
    IterateNext();
    
    // Clear all variables and constants (for NEW command)
    // Output: Empty symbol table
    // Uses: Objects.Destroy() internally
    Clear();
}
```

## Usage Examples

### Adding a Variable
```hopper
// Add INT variable "COUNT" = 42
LDA #(testName % 256)
STA ZP.ACCL
LDA #(testName / 256)
STA ZP.ACCH

// Pack symbolType|dataType: VARIABLE(1) in high nibble, INT(2) in low nibble
LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
STA ZP.ACCT

LDA #42
STA ZP.TOPL
STZ ZP.TOPH

// Tokens pointer to initialization expression
LDA #(tokenStream % 256)
STA ZP.NEXTL
LDA #(tokenStream / 256)
STA ZP.NEXTH

Objects.Add();
// C set if successful, ZP.IDX contains new node address
```

### Finding and Reading a Symbol
```hopper
// Find symbol "COUNT"
LDA #(testName % 256)
STA ZP.ACCL
LDA #(testName / 256)
STA ZP.ACCH

Objects.Find();
if (C)  // Found
{
    Objects.GetData();
    // ZP.ACCT contains packed type, ZP.TOP contains value, ZP.NEXT contains tokens pointer
    
    LDA ZP.ACCT
    AND #0x0F           // Extract data type (low nibble)
    CMP #BasicType.INT
    // ZP.TOPL/H contains the value (42)
    // ZP.NEXTL/H contains tokens pointer
}
```

### Iterating Variables Only
```hopper
LDA #SymbolType.VARIABLE
STA ZP.ACCL
Objects.IterateStart();

loop
{
    if (NC) { break; }  // No more variables
    
    // ZP.IDX points to current variable node
    Objects.GetData();
    // Process variable...
    
    Objects.IterateNext();
}
```

### Declaring a Variable with Tokens
```hopper
// Declare: INT COUNT = 10 * 5
// Assume tokens for "10 * 5" are already in token buffer at position tokensPtr

LDA #(variableName % 256)
STA ZP.ACCL
LDA #(variableName / 256)
STA ZP.ACCH

// Pack symbolType|dataType: VARIABLE(1) in high nibble, INT(2) in low nibble
LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
STA ZP.ACCT

LDA #50  // Evaluated result of 10 * 5
STA ZP.TOPL
STZ ZP.TOPH

LDA #(tokensPtr % 256)  // Pointer to "10 * 5" token stream
STA ZP.NEXTL
LDA #(tokensPtr / 256)
STA ZP.NEXTH

Variables.Declare();
// C set if successful, ZP.IDX contains new node address
```

### Finding and Accessing Symbol Data
```hopper
// Find variable "COUNT"
LDA #(variableName % 256)
STA ZP.ACCL
LDA #(variableName / 256)
STA ZP.ACCH

LDA #SymbolType.VARIABLE
STA ZP.ACCL

Variables.Find();
if (C)  // Found
{
    Variables.GetValue();
    // ZP.TOP contains current value, ZP.TOPT contains data type
    
    Variables.GetTokens();
    // ZP.NEXT now points to initialization token stream
    
    Variables.GetName();
    // ZP.ACC now points to name string within node
}
```

### Iterating and Displaying Variables
```hopper
Variables.IterateVariables();

loop
{
    if (NC) { break; }  // No more variables
    
    // ZP.IDX points to current variable node
    Variables.GetName();
    // Print name from ZP.ACC
    
    Variables.GetValue();
    // Print value from ZP.TOP
    
    Variables.GetType();
    // Print type info from ZP.ACCT
    
    Variables.IterateNext();
}
```

## Usage Patterns

### Variable lookup in expressions (two-stage):
```hopper
// Stage 1: Find name to address
Tokenizer.GetTokenString(); // Name in ZP.ACC
LDA #SymbolType.VARIABLE
STA ZP.ACCL
Variables.Find();
if (NC) { /* undefined variable error */ }

// Stage 2: Get value by address
Variables.GetValue(); // ZP.IDX already set from Find
// Value now in ZP.TOP, type in ZP.TOPT
```

### Assignment (two-stage):
```hopper
// Stage 1: Find variable name
Tokenizer.GetTokenString(); // Name in ZP.ACC
LDA #SymbolType.VARIABLE
STA ZP.ACCL
Variables.Find();
if (NC) { /* undefined variable error */ }

// Stage 2: Set value by address
// ... evaluate expression, result in ZP.TOP ...
Variables.SetValue(); // ZP.IDX from Find, ZP.TOP has new value
```

### Type checking (find accepts either):
```hopper
// For general identifier lookup (could be variable or constant)
Tokenizer.GetTokenString();
LDA #0  // Accept any symbol type
STA ZP.ACCL
Variables.Find();
if (C)
{
    Variables.GetType(); // Get actual type info
}
```

## Memory Management

The symbol table integrates with the Hopper VM memory system:

1. **Allocation**: Uses `Memory.Allocate()` through `Table.Add()`
2. **Deallocation**: Uses `Memory.Free()` through `Table.Delete()`
3. **Persistence**: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName survive memory operations
4. **Efficiency**: Only allocates space needed for each symbol (7 bytes + name length)

## Error Handling

All operations return success/failure status via carry flag:
- **C set**: Operation successful
- **C clear**: Operation failed (out of memory, symbol not found, etc.)

The symbol table does not set error messages - that's the responsibility of the calling code to check the carry flag and handle appropriately.

## Key Features

This design enables:
- **Rich VARS command**: Can show both current value and original initialization expression
- **Constant re-evaluation**: Support for constants that depend on other constants
- **Array size expressions**: Store and re-evaluate size expressions like `BIT FLAGS[LENGTH]`
- **Debugging support**: Always have access to original source expression
- **Forward references**: Functions can call functions defined later
- **Dynamic modification**: FORGET and redefinition change symbol meanings