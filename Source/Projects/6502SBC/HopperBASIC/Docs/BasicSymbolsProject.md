# HopperBASIC Symbol Table Design
**Document Type: Symbol Architecture**

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
- **ARGUMENT** - Function parameters (stored in separate arguments tables)

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
    // Output: ZP.IDX = first node (0x0000 if empty list), C set if found, NC if empty
    // Preserves: A, Y, ZP.ACC, ZP.TOP, ZP.NEXT
    GetFirst();
    
    // Get next node in traversal
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next node (0x0000 if end of list), C set if found, NC if end
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
    // Output: C set (always succeeds), list head set to 0x0000 in place
    // Preserves: A, Y, ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Clear();
}
```

### Layer 2: Objects (Symbol Table Implementation)
Symbol-specific operations using Table foundation:

#### Node Layout
Symbol nodes have a 7-byte fixed header plus variable-length name:
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   symbolType|dataType (packed byte)
            High nibble: SymbolType (VARIABLE=1, CONSTANT=2, FUNCTION=3, ARGUMENT=4)
            Low nibble:  BasicType (INT=2, BYTE=3, WORD=4, BIT=6, etc.)
Offset 3-4: tokens pointer (16-bit pointer to initialization/body token stream)
Offset 5-6: value/address (16-bit - value for variables/constants, args list head for functions)
Offset 7+:  null-terminated name string
```

#### Constants
```hopper
const byte symbolOverhead = 7;       // Fixed fields before name
const byte snNext = 0;               // Next pointer offset
const byte snType = 2;               // symbolType|dataType field offset
const byte snTokens = 3;             // Tokens pointer field offset
const byte snValue = 5;              // Value field offset
const byte snArguments = 5;          // Alias for value field when used for functions
const byte snName = 7;               // Name field offset
```

#### Zero Page Storage
Dedicated zero page locations that persist across `Memory.Allocate()` calls:
- **ZP.SymbolType**: symbolType|dataType packed byte
- **ZP.SymbolValueL/H**: 16-bit symbol value
- **ZP.SymbolNameL/H**: Pointer to symbol name string
- **ZP.SymbolLength**: Name length including null terminator
- **ZP.SymbolTokensL/H**: Tokens pointer
- **ZP.SymbolIteratorFilter**: Type filter for iteration

```hopper
unit Objects
{
    // Symbol types
    enum SymbolType
    {
        VARIABLE = 0x01,   // Mutable values
        CONSTANT = 0x02,   // Immutable values  
        FUNCTION = 0x03,   // Executable code blocks
        ARGUMENT = 0x04    // Function parameters
    }
    
    // Initialize empty symbol tables
    // Output: ZP.VariableListL/H = 0x0000, ZP.FunctionsListL/H = 0x0000
    // Preserves: All registers and ZP variables
    Initialize();
    
    // Add new symbol to table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.IDY = tokens pointer (16-bit), ZP.NEXT = value/args (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Add call)
    // Uses: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName, ZP.SymbolTokens for parameter storage
    Add();
    
    // Find symbol by name
    // Input: X = ZP address of table head, ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.LCURRENT for temporary operations
    Find();
    
    // Remove symbol by node pointer
    // Input: X = ZP address of table head, ZP.IDX = symbol node address (from Find)
    // Output: C set if successful, NC if node not found
    // Preserves: A, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Delete call)
    // Uses: ZP.Lxx variables as temporary workspace
    Remove();
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value/args
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC
    GetData();
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.IDY = new value
    // Output: C set if successful, NC if not a variable
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    SetValue();
    
    // Get tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address
    // Output: ZP.IDY = tokens pointer (16-bit)
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    GetTokens();
    
    // Set tokens pointer for symbol node
    // Input: ZP.IDX = symbol node address, ZP.IDY = new tokens pointer
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    SetTokens();
    
    // Start iteration with type filter
    // Input: X = ZP address of table head, ZP.SymbolIteratorFilter = filter (0 = all, or specific symbolType)
    // Output: ZP.IDX = first matching symbol, C set if found, NC if none
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.LCURRENT, ZP.LNEXT
    IterateStart();
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.SymbolIteratorFilter = type filter from previous call
    // Output: ZP.IDX = next matching symbol, C set if found, NC if done
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.LCURRENT, ZP.LNEXT
    IterateNext();
    
    // Destroy entire symbol table
    // Input: X = ZP address of table head
    // Output: C set (always succeeds)
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
    
    // Declare new variable or constant
    // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
    // Output: C set if successful, NC if error (name exists, out of memory)
    // Munts: ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    Declare();
    
    // Find variable/constant by name with optional type filtering
    // Input: ZP.TOP = name pointer, ZP.SymbolIteratorFilter = expected symbolType (VARIABLE or CONSTANT, 0 = any)
    // Output: ZP.IDX = symbol node address, C set if found and correct type, NC if not found or wrong type
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Error: Sets LastError if found but wrong type
    // Munts: ZP.LCURRENT, ZP.SymbolTemp0
    Find();
    
    // Get variable/constant value and type
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.TOP = value, ZP.TOPT = dataType, C set if successful, NC if error
    // Preserves: A, X, Y, ZP.ACC, ZP.NEXT
    // Error: Fails if node is not variable or constant
    GetValue();
    
    // Set variable value (variables only, constants are immutable)
    // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new value
    // Output: C set if successful, NC if error (not a variable)
    // Preserves: A, X, Y, ZP.ACC, ZP.NEXT
    // Error: Fails if node is not a variable (constants rejected)
    SetValue();
    
    // Get type information for variable/constant
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), C set if successful, NC if error
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Error: Fails if node is not variable or constant
    GetType();
    
    // Get variable/constant signature info
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value
    // Preserves: A, X, Y, ZP.IDX
    GetSignature();
    
    // Get name from current node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACC = name pointer (points into node data)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    GetName();
    
    // Get initialization tokens from current node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.NEXT = tokens pointer
    // Preserves: A, X, Y, ZP.TOP, ZP.ACC
    GetTokens();
    
    // Remove variable or constant by name with token cleanup
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Munts: ZP.IDY, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX, ZP.SymbolTemp0, ZP.SymbolTemp1
    Remove();
    
    // Start iteration over variables only (for VARS command)
    // Output: ZP.IDX = first variable node, C set if found, NC if none
    // Sets up iteration state for IterateNext()
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateVariables();
    
    // Start iteration over constants only (for CONSTS command)
    // Output: ZP.IDX = first constant node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateConstants();
    
    // Start iteration over all symbols (for LIST command)
    // Output: ZP.IDX = first symbol node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateAll();
    
    // Continue iteration (use after any Iterate* method)
    // Output: ZP.IDX = next matching node, C set if found, NC if done
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateNext();
    
    // Clear all variables and constants with token cleanup (for NEW command)
    // Output: Empty symbol table, C set (always succeeds)
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
    Clear();
}
```

### Layer 4: Functions (Function-Specific Interface)
Function management building on Objects foundation with separate Arguments unit:

#### Function Node Layout
Functions reuse the existing Objects node structure:
```
Offset 0-1: next pointer (managed by Table unit)
Offset 2:   unused in functions
Offset 3-4: function body tokens pointer
Offset 5-6: arguments list head pointer (points directly to first argument node)
Offset 7+:  null-terminated function name string
```

```hopper
unit Functions
{
    // Core function management - no argument handling
    
    // Declare new function
    // Input: ZP.TOP = name pointer, ZP.NEXT = arguments list head pointer, 
    //        ZP.IDY = function body tokens pointer
    // Output: ZP.IDX = function node address, C set if successful, NC if error
    // Note: Arguments list created separately via Arguments unit
    // Munts: ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    Declare();
    
    // Find function by name
    // Input: ZP.TOP = name pointer
    // Output: ZP.IDX = function node address, C set if found and is function, NC if not found or wrong type
    // Error: Sets LastError if found but wrong type
    // Munts: ZP.LCURRENT, ZP.SymbolTemp0
    Find();
    
    // Get function body tokens
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = function body tokens pointer, C set (always succeeds)
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    GetBody();
    
    // Get function name
    // Input: ZP.IDX = function node address
    // Output: ZP.TOP = name pointer (points into node data), C set (always succeeds)
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    GetName();
    
    // Set arguments list head pointer in function node
    // Input: ZP.IDX = function node address, ZP.IDY = arguments list head
    // Output: C set if successful
    // Munts: ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
    SetArguments();
    
    // Get arguments list head pointer from function node
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = arguments list head pointer, C set if has arguments, NC if no arguments
    // Preserves: A, X, Y, ZP.IDX
    GetArguments();
    
    // Remove function by name
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Note: Clears arguments list automatically before removing function
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX, ZP.SymbolTemp0, ZP.SymbolTemp1
    Remove();
    
    // Start iteration over functions only
    // Output: ZP.IDX = first function node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateFunctions();
    
    // Continue function iteration
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next function node, C set if found, NC if done
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateNext();
    
    // Clear all functions
    // Output: Empty function table, C set (always succeeds)
    // Note: Clears all arguments lists and frees function body tokens
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX, ZP.SymbolTemp0, ZP.SymbolTemp1
    Clear();
    
    // Set function body tokens
    // Input: ZP.IDX = function node address, ZP.IDY = new function body tokens pointer
    // Output: C set if successful
    // Note: Frees old tokens if non-zero
    // Munts: ZP.TOP, ZP.NEXT
    SetBody();
}
```

### Layer 5: Arguments (Argument-Specific Interface)
Independent argument list management with arguments stored directly in function nodes:

#### Argument Node Structure
Arguments use simplified node structure optimized for argument data:
```
Offset 0-1: next pointer
Offset 2+:  null-terminated argument name
```

```hopper
unit Arguments
{
    // Argument list management - arguments list head stored directly in function node
    // No separate "table head storage" - function node field points directly to first argument
    
    // Add argument to function's arguments list at the end for correct order
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: C set if successful, NC if allocation failed
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT, ZP.LPREVIOUS, 
    //        ZP.SymbolType, ZP.SymbolNameL/H, ZP.SymbolLength
    Add();
    
    // Find argument by name in function's arguments list
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: ZP.IDY = argument node address, ZP.ACCL = argument index, C set if found, NC if not found
    // Preserves: A, X, Y, ZP.IDX
    // Munts: ZP.LCURRENT, ZP.LNEXT
    Find();
    
    // Get argument name pointer from argument node
    // Input: ZP.IDY = argument node address
    // Output: ZP.TOP = argument name pointer, C set (always succeeds)
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.NEXT
    GetName();
    
    // Find argument by index for BP offset calculation
    // Input: ZP.IDX = function node address, ZP.ACCL = argument index
    // Output: ZP.IDY = argument node address, C set if found, NC if index out of range
    // Preserves: A, X, Y, ZP.IDX
    // Munts: ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0
    FindByIndex();
    
    // Get argument count in function's arguments list
    // Input: ZP.IDX = function node address
    // Output: ZP.ACCL = argument count, C set (always succeeds)
    // Preserves: A, X, Y, ZP.IDX
    // Munts: ZP.LCURRENT, ZP.LNEXT
    GetCount();
    
    // Start iteration over arguments in function's list
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = first argument node, C set if found, NC if no arguments
    // Preserves: A, X, Y, ZP.IDX
    IterateStart();
    
    // Continue argument iteration
    // Input: ZP.IDY = current argument node
    // Output: ZP.IDY = next argument node, C set if found, NC if end of arguments
    // Preserves: A, X, Y, ZP.IDX
    // Munts: ZP.LCURRENT
    IterateNext();
    
    // Clear all arguments in function's list with proper memory cleanup
    // Input: ZP.IDX = function node address
    // Output: Function's arguments field set to null, all argument nodes freed, C set (always succeeds)
    // Preserves: A, X, Y, ZP.IDX
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
    Clear();
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
LDA #((SymbolType.VARIABLE << 4) | BASICType.INT)
STA ZP.ACCT

LDA #42
STA ZP.NEXTL
STZ ZP.NEXTH

// Tokens pointer to initialization expression
LDA #(tokenStream % 256)
STA ZP.IDYL
LDA #(tokenStream / 256)
STA ZP.IDYH

LDX #ZP.VariablesList
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

LDX #ZP.VariablesList
Objects.Find();
if (C)  // Found
{
    Objects.GetData();
    // ZP.ACCT contains packed type, ZP.IDY contains value, ZP.NEXT contains tokens pointer
    
    LDA ZP.ACCT
    AND #0x0F           // Extract data type (low nibble)
    CMP #BASICType.INT
    // ZP.IDYL/H contains the value (42)
    // ZP.NEXTL/H contains tokens pointer
}
```

### Iterating Variables Only
```hopper
Variables.IterateVariables();

loop
{
    if (NC) { break; }  // No more variables
    
    // ZP.IDX points to current variable node
    Variables.GetValue();
    // Process variable...
    
    Variables.IterateNext();
}
```

### Declaring a Function with Arguments
```hopper
// FUNC Add(INT a, WORD b) -> INT

// Step 1: Add arguments to function after declaring it
LDA #(functionName % 256)
STA ZP.TOPL
LDA #(functionName / 256) 
STA ZP.TOPH

// No arguments initially
STZ ZP.NEXTL
STZ ZP.NEXTH

LDA #(functionBodyTokens % 256)  // Pointer to function body tokens
STA ZP.IDYL
LDA #(functionBodyTokens / 256)
STA ZP.IDYH

Functions.Declare();
// ZP.IDX now contains function node address

// Step 2: Add arguments to the function
LDA #(arg1Name % 256)  // "a"
STA ZP.TOPL
LDA #(arg1Name / 256)
STA ZP.TOPH
Arguments.Add();  // Uses ZP.IDX from Declare

LDA #(arg2Name % 256)  // "b"
STA ZP.TOPL
LDA #(arg2Name / 256)
STA ZP.TOPH
Arguments.Add();  // Uses ZP.IDX
```

### Function Call Validation
```hopper
// Calling Add(count, 100) where count is INT
LDA #(functionName % 256)
STA ZP.TOPL
LDA #(functionName / 256)
STA ZP.TOPH

Functions.Find();  // Find "Add" function
if (C)
{
    Functions.GetArguments();  // Get arguments list head in ZP.IDY
    
    Arguments.GetCount();  // Uses ZP.IDX (function node)
    // ZP.ACCL now contains argument count (2)
    
    // Validate each argument by iterating
    Arguments.IterateStart();  // Uses ZP.IDX
    
    // First argument name: "a"
    Arguments.GetName();  // Uses ZP.IDY
    // ZP.TOP points to "a"
    
    Arguments.IterateNext();
    // Second argument name: "b"
    Arguments.GetName();  // Uses ZP.IDY
    // ZP.TOP points to "b"
}
```

### Parameter Resolution During Function Execution
```hopper
// Inside function body, resolve "a" parameter to BP offset
Functions.Find();  // Current function
// ZP.IDX = function node

LDA #(paramName % 256)  // "a"
STA ZP.TOPL
LDA #(paramName / 256)
STA ZP.TOPH

Arguments.Find();  // Uses ZP.IDX (function node)
if (C)
{
    // ZP.ACCL = argument index (0 for "a")
    // ZP.IDY = argument node address
    
    Arguments.GetCount();  // Uses ZP.IDX (function node)
    // ZP.ACCL = argument count (2)
    
    // BP offset = argument count - argument index - 1
    // For "a": offset = 2 - 0 - 1 = 1
    // For "b": offset = 2 - 1 - 1 = 0
}
```

### Variable lookup in expressions (two-stage):
```hopper
// Stage 1: Find name to address
Tokenizer.GetTokenString(); // Name in ZP.TOP
LDA #SymbolType.VARIABLE
STA ZP.SymbolIteratorFilter
Variables.Find();
if (NC) { /* undefined variable error */ }

// Stage 2: Get value by address
Variables.GetValue(); // ZP.IDX already set from Find
// Value now in ZP.TOP, type in ZP.TOPT
```

### Assignment (two-stage):
```hopper
// Stage 1: Find variable name
Tokenizer.GetTokenString(); // Name in ZP.TOP
LDA #SymbolType.VARIABLE
STA ZP.SymbolIteratorFilter
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
STZ ZP.SymbolIteratorFilter  // Accept any symbol type
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
3. **Persistence**: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName, ZP.SymbolTokens survive memory operations
4. **Efficiency**: Only allocates space needed for each symbol (7 bytes + name length)
5. **Arguments Lists**: Each function has separate arguments list using simplified 2-byte + name nodes

## Error Handling

All operations return success/failure status via carry flag:
- **C set**: Operation successful
- **C clear**: Operation failed (out of memory, symbol not found, etc.)

The symbol table does not set error messages - that's the responsibility of the calling code to check the carry flag and handle appropriately.

## Token Stream Storage Rationale

The core requirement for storing initialization token streams alongside current values is **persistent program state management**:

### SAVE/LOAD Cycle Integrity
**SAVE**: Store original initialization expressions, not just current values
**LOAD**: Re-execute all stored token streams to recreate exact program state
```hopper
// Original program:
CONST BYTE WIDTH = 10
CONST BYTE HEIGHT = WIDTH * 2  
INT count = WIDTH + HEIGHT

// SAVE stores token streams: "10", "WIDTH * 2", "WIDTH + HEIGHT"
// LOAD re-evaluates them in order, recreating dependencies correctly
```

### CLEAR Command Implementation
**CLEAR**: Re-execute variable initialization expressions to reset to original values
```hopper
INT score = 100
// ... user changes score to 250 during execution ...
// CLEAR re-evaluates "100" token stream, resets score back to 100
```

### Dependency Chain Integrity
Storing only values breaks when constants have dependencies:
```hopper
CONST BYTE BASE = 10           // Storing value only: 10
CONST BYTE DERIVED = BASE * 2  // Storing value only: 20

// Later: FORGET BASE; CONST BYTE BASE = 5
// Problem: DERIVED still shows 20, but should be 10!
// Solution: Re-evaluate "BASE * 2" tokens → correctly gets 10
```

### RUN Command Prerequisites
**RUN**: Should call CLEAR first to ensure clean, repeatable variable state
- Constants evaluate once in declaration order
- Variables reset to their initialization expressions  
- Program starts with predictable, consistent state

## Key Features

This design enables:
- **Rich VARS command**: Can show both current value and original initialization expression
- **Constant re-evaluation**: Support for constants that depend on other constants
- **State persistence**: SAVE/LOAD maintains referential integrity across sessions
- **Variable reset**: CLEAR restores original initialization values
- **Debugging support**: Always have access to original source expression
- **Referential integrity**: Prevents "stale constant" problems in dependency chains
- **Forward references**: Functions can call functions defined later
- **Dynamic modification**: FORGET and redefinition change symbol meanings
- **Unified argument handling**: Arguments stored directly in function nodes with optimized structure
- **Memory efficiency**: No duplicate code for argument management, optimized 2-byte argument node overhead