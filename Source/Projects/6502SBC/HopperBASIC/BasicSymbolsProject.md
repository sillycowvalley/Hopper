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
Symbol nodes have a 5-byte fixed header plus variable-length name:
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   symbolType|dataType (packed byte)
            High nibble: SymbolType (VARIABLE=1, CONSTANT=2, FUNCTION=3)
            Low nibble:  BasicType (INT=2, BYTE=3, WORD=4, BIT=6, etc.)
Offset 3-4: value/address (16-bit)
Offset 5+:  null-terminated name string
```

#### Constants
```hopper
const byte symbolOverhead = 5;       // Fixed fields before name
const byte typeOffset = 2;           // Offset to symbolType|dataType field
const byte valueOffset = 3;          // Offset to value field
const byte nameOffset = 5;           // Offset to name field in node
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
    // Input: ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
    //        ZP.NEXT = value (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Add call)
    // Uses: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName for parameter storage
    Add();
    
    // Find symbol by name
    // Input: ZP.TOP = name pointer to search for
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
    // Output: ZP.ACC = symbolType|dataType (packed), ZP.NEXT = value
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP
    GetData();
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.NEXT = new value
    // Output: C set if successful, NC if not a variable
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.ACC
    SetValue();
    
    // Start iteration for specific symbol type
    // Input: ZP.ACC = symbol type filter (0 = all types)
    // Output: ZP.IDX = first matching symbol, C set if found, NC if none
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateStart();
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.ACC = type filter
    // Output: ZP.IDX = next matching symbol, C set if found, NC if done
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateNext();
    
    // Destroy entire symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT, ZP.ACC
    // Munts: ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Clear call)
    Destroy();
}
```

## Usage Examples

### Adding a Variable
```hopper
// Add INT variable "COUNT" = 42
LDA #(testName % 256)
STA ZP.TOPL
LDA #(testName / 256)
STA ZP.TOPH

// Pack symbolType|dataType: VARIABLE(1) in high nibble, INT(2) in low nibble
LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.INT)
STA ZP.ACCL
STZ ZP.ACCH

LDA #42
STA ZP.NEXTL
STZ ZP.NEXTH

Objects.Add();
// C set if successful, ZP.IDX contains new node address
```

### Finding and Reading a Symbol
```hopper
// Find symbol "COUNT"
LDA #(testName % 256)
STA ZP.TOPL
LDA #(testName / 256)
STA ZP.TOPH

Objects.Find();
if (C)  // Found
{
    Objects.GetData();
    // ZP.ACC contains packed type, ZP.NEXT contains value
    
    LDA ZP.ACCL
    AND #0x0F           // Extract data type (low nibble)
    CMP #BasicType.INT
    // ZP.NEXTL/H contains the value (42)
}
```

### Iterating Variables Only
```hopper
LDA #Objects.SymbolType.VARIABLE
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

## Memory Management

The symbol table integrates with the Hopper VM memory system:

1. **Allocation**: Uses `Memory.Allocate()` through `Table.Add()`
2. **Deallocation**: Uses `Memory.Free()` through `Table.Delete()`
3. **Persistence**: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName survive memory operations
4. **Efficiency**: Only allocates space needed for each symbol (5 bytes + name length)

## Error Handling

All operations return success/failure status via carry flag:
- **C set**: Operation successful
- **C clear**: Operation failed (out of memory, symbol not found, etc.)

The symbol table does not set error messages - that's the responsibility of the calling code to check the carry flag and handle appropriately.