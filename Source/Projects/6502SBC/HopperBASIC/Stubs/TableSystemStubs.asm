# Table and Objects System Reference Stubs

## Table.asm
```hopper
unit Table
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Uses ZP.L* scratch space for legitimate Table operations (does not leak beyond method boundaries)
    
    // Legitimate scratch space for Table operations:
    // ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX - Internal linked list traversal
    
    // Generic linked list operations using ZP.Lxx workspace
    // Lists are represented by a pointer stored at a ZP address
    // Empty list = 0x0000
    // Each node has next pointer at fixed offset 0-1 followed by arbitrary user data
    // Node layout: [0-1: next pointer] [user data]
    
    // Get first node in list
    // Input: X = ZP address of list head pointer
    // Output: ZP.IDX = first node (0x0000 if empty list), C set if found, NC if empty
    GetFirst()
    
    // Get next node in traversal
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next node (0x0000 if end of list), C set if found, NC if end
    GetNext()
    
    // Add new node to end of list
    // Input: X = ZP address of list head pointer, ZP.ACC = node size (16-bit)
    // Output: ZP.IDX = new node address, C set if successful, NC if allocation failed
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Add()
    
    // Delete specific node from list
    // Input: X = ZP address of list head pointer, ZP.IDX = node to delete
    // Output: C set if successful, NC if node not found
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Delete()
    
    // Clear entire list (free all nodes)
    // Input: X = ZP address of list head pointer
    // Output: C set (always succeeds), list head set to 0x0000
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Clear()
}
```

## Objects.asm
```hopper
unit Objects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "BasicTypes"
    uses "Table"
    uses "Tools"
    
    friend Variables, Functions, Arguments;
    
    // API Status: Clean
    // Symbol table implementation using Table foundation
    // Uses ZP.Symbol* scratch space for legitimate symbol operations
    // ZP.VariableListL/H stores the variables/constants table head pointer
    // ZP.FunctionsListL/H stores the functions table head pointer
    
    // Symbol types
    enum SymbolType
    {
        VARIABLE = 0x01,   // Mutable values
        CONSTANT = 0x02,   // Immutable values  
        FUNCTION = 0x03,   // Executable code blocks
        ARGUMENT = 0x04    // Function parameters
    }
    
    // Symbol node memory map:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   symbolType|dataType (packed byte)
    // Offset 3-4: tokens pointer (16-bit pointer to initialization/body token stream)
    // Offset 5-6: value/address (16-bit - value for variables/constants, args list for functions)
    // Offset 7+:  null-terminated name string
    
    const byte symbolOverhead = 7;       // Fixed fields before name (including Table's next pointer)
    const byte snNext = 0;               // Next pointer offset (2 bytes)
    const byte snType = 2;               // symbolType|dataType field offset
    const byte snTokens = 3;             // Tokens pointer field offset (2 bytes)
    const byte snValue = 5;              // Value/args field offset (2 bytes)
    const byte snArguments = 5;          // same slot as Values, better name for Function arguments
    const byte snName = 7;               // Name field offset (variable length)
    
    // Initialize empty symbol tables
    // Output: ZP.VariableListL/H = 0x0000, ZP.FunctionsListL/H = 0x0000
    Initialize()
    
    // Add new symbol to table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.IDY = tokens pointer (16-bit), ZP.NEXT = value/args (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Modifies: ZP.L* scratch space (internal to Table operations), ZP.Symbol* scratch space
    Add()
    
    // Find symbol by name
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Modifies: ZP.LCURRENT (internal traversal)
    Find()
    
    // Remove symbol by node pointer
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.IDX = symbol node address (from Find)
    // Output: C set if successful, NC if node not found
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Remove()
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value/args
    GetData()
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.IDY = new value/args
    // Output: C set if successful, NC if not a variable
    SetValue()
    
    // Get tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address
    // Output: ZP.IDY = tokens pointer (16-bit)
    GetTokens()
    
    // Set tokens pointer for symbol node
    // Input: ZP.IDX = symbol node address, ZP.IDY = new tokens pointer
    SetTokens()
    
    // Destroy entire symbol table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList)
    // Output: C set (always succeeds)
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Destroy()
    
    // Start iteration with type filter
    // Input: X = ZP address of table head, ZP.SymbolIteratorFilter = filter (0 = all, or specific symbolType)
    // Output: ZP.IDX = first matching symbol (0x0000 if none), C set if found
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateStart()
    
    // Continue iteration with filter
    // Input: None (uses saved filter from IterateStart)
    // Output: ZP.IDX = next matching symbol (0x0000 if no more), C set if found
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateNext()
}
```

## Variables.asm
```hopper
unit Variables
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "Objects"
    uses "BasicTypes"
    uses "Messages"
    uses "Table"
    
    // API Status: Clean
    // Variable management using Objects foundation
    // Two-stage approach: Find name to address, then operate on address
    
    // Declare new variable or constant
    // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
    // Output: C set if successful, NC if error (name exists, out of memory)
    // Modifies: ZP.L* and ZP.Symbol* scratch space (internal to symbol operations)
    Declare()
    
    // Find variable/constant by name with optional type filtering
    // Input: ZP.TOP = name pointer, ZP.SymbolIteratorFilter = expected symbolType (VARIABLE or CONSTANT, 0 = any)
    // Output: ZP.IDX = symbol node address, C set if found and correct type, NC if not found or wrong type
    // Modifies: ZP.LCURRENT, ZP.SymbolTemp0 (internal search operations)
    Find()
    
    // Get variable/constant value and type
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.TOP = value, ZP.TOPT = dataType, C set if successful, NC if error
    GetValue()
    
    // Set variable value (variables only, constants are immutable)
    // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new value
    // Output: C set if successful, NC if error (not a variable)
    SetValue()
    
    // Get type information for variable/constant
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), C set if successful, NC if error
    GetType()
    
    // Get variable/constant signature info
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value, C set if successful, NC if error
    GetSignature()
    
    // Get name pointer from symbol node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACC = name pointer (points into node data), C set (always succeeds)
    GetName()
    
    // Get initialization tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.NEXT = tokens pointer, C set (always succeeds)
    GetTokens()
    
    // Remove variable or constant by name with token cleanup
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Modifies: ZP.L* and ZP.Symbol* scratch space (internal operations)
    Remove()
    
    // Start iteration over variables only (for VARS command)
    // Output: ZP.IDX = first variable node, C set if found, NC if none
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateVariables()
    
    // Start iteration over constants only (for CONSTS command)
    // Output: ZP.IDX = first constant node, C set if found, NC if none
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateConstants()
    
    // Start iteration over all symbols (for LIST command)
    // Output: ZP.IDX = first symbol node, C set if found, NC if none
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateAll()
    
    // Continue iteration (use after any Iterate* method)
    // Output: ZP.IDX = next matching node, C set if found, NC if done
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateNext()
    
    // Clear all variables and constants with token cleanup (for NEW command)
    // Output: Empty symbol table, C set (always succeeds)
    // Modifies: ZP.L* scratch space (internal Table operations)
    Clear()
}
```

## Functions.asm
```hopper
unit Functions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Objects"
    uses "Arguments"
    uses "BasicTypes"
    uses "Messages"
    
    // API Status: Clean
    // Function management building on Objects foundation
    // Functions use the existing Objects node structure:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   unused in functions
    // Offset 3-4: function body tokens pointer
    // Offset 5-6: arguments list head pointer (points directly to first argument node)
    // Offset 7+:  null-terminated function name string
    
    // Declare new function
    // Input: ZP.TOP = name pointer
    //        ZP.NEXT = arguments list head pointer, ZP.IDY = function body tokens pointer
    // Output: ZP.IDX = function node address, C set if successful, NC if error
    // Modifies: ZP.L* and ZP.Symbol* scratch space (internal operations)
    Declare()
    
    // Find function by name
    // Input: ZP.TOP = name pointer
    // Output: ZP.IDX = function node address, C set if found and is function, NC if not found or wrong type
    // Modifies: ZP.LCURRENT, ZP.SymbolTemp0 (internal operations)
    Find()
    
    // Get function body tokens
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = function body tokens pointer, C set (always succeeds)
    GetBody()
    
    // Get function name
    // Input: ZP.IDX = function node address
    // Output: ZP.TOP = name pointer (points into node data), C set (always succeeds)
    GetName()
    
    // Set arguments list head pointer in function node
    // Input: ZP.IDX = function node address, ZP.IDY = arguments list head
    // Output: C set if successful
    // Modifies: ZP.Symbol* scratch space (internal operations)
    SetArguments()
    
    // Get arguments list head pointer from function node
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = arguments list head pointer, C set if has arguments, NC if no arguments
    GetArguments()
    
    // Remove function by name
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Modifies: ZP.L* and ZP.Symbol* scratch space (internal operations)
    Remove()
    
    // Start iteration over functions only
    // Output: ZP.IDX = first function node, C set if found, NC if none
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateFunctions()
    
    // Continue function iteration
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next function node, C set if found, NC if done
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    IterateNext()
    
    // Clear all functions
    // Output: Empty function table, C set (always succeeds)
    // Modifies: ZP.L* and ZP.Symbol* scratch space (internal operations)
    Clear()
    
    // Set function body tokens
    // Input: ZP.IDX = function node address, ZP.IDY = new function body tokens pointer
    // Output: C set if successful
    SetBody()
}
```

## Arguments.asm
```hopper
unit Arguments
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "BasicTypes"
    uses "Tools"
    
    // API Status: Clean
    // Argument table management - arguments list head stored directly in function node
    // No separate "table head storage" - function node field points directly to first argument
    
    // Argument Node Structure:
    // Offset 0-1: next pointer
    // Offset 2+:  null-terminated argument name
    
    const byte argOverhead = 2;     // Fixed fields before name
    const byte anNext = 0;          // Next pointer offset (2 bytes)
    const byte anName = 2;          // Name field offset (variable length)
    
    // Add argument to function's arguments list at the end for correct order
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: C set if successful, NC if allocation failed
    // Modifies: ZP.L* and ZP.Symbol* scratch space (internal operations)
    Add()
    
    // Find argument by name in function's arguments list
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: ZP.IDY = argument node address, ZP.ACCL = argument index, C set if found, NC if not found
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    Find()
    
    // Get argument name pointer from argument node
    // Input: ZP.IDY = argument node address
    // Output: ZP.TOP = argument name pointer, C set (always succeeds)
    GetName()
    
    // Find argument by index for BP offset calculation
    // Input: ZP.IDX = function node address, ZP.ACCL = argument index
    // Output: ZP.IDY = argument node address, C set if found, NC if index out of range
    // Modifies: ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0 (internal operations)
    FindByIndex()
    
    // Get argument count in function's arguments list
    // Input: ZP.IDX = function node address
    // Output: ZP.ACCL = argument count, C set (always succeeds)
    // Modifies: ZP.LCURRENT, ZP.LNEXT (internal traversal)
    GetCount()
    
    // Start iteration over arguments in function's list
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = first argument node, C set if found, NC if no arguments
    IterateStart()
    
    // Continue argument iteration
    // Input: ZP.IDY = current argument node
    // Output: ZP.IDY = next argument node, C set if found, NC if end of arguments
    // Modifies: ZP.LCURRENT (internal operation)
    IterateNext()
    
    // Clear all arguments in function's list with proper memory cleanup
    // Input: ZP.IDX = function node address
    // Output: Function's arguments field set to null, all argument nodes freed, C set (always succeeds)
    // Modifies: ZP.Symbol* scratch space (internal operations)
    Clear()
}
```