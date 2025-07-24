### Two-Layer Design

#### Layer 1: Table (Generic Linked List)
Pure linked list operations with no knowledge of record contents:

```hopper
unit Table
{
    // Get first node in list
    // Input: ZP.IDY = list head pointer
    // Output: ZP.LCURRENT = first node (0x0000 if empty list)
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT
    GetFirst();
    
    // Get next node in traversal
    // Input: ZP.LCURRENT = current node
    // Output: ZP.LCURRENT = next node (0x0000 if end of list)
    // Preserves: A, X, Y, ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
    GetNext();
    
    // Add new node to front of list
    // Input: ZP.IDY = list head pointer, ZP.ACC = node size (16-bit)
    // Output: ZP.IDX = new node address (0x0000 if failed), ZP.IDY = updated list head
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Add();
    
    // Delete specific node from list
    // Input: ZP.IDY = list head pointer, ZP.LCURRENT = node to delete
    // Output: ZP.IDY = updated list head
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Delete();
    
    // Clear entire list (free all nodes)
    // Input: ZP.IDY = list head pointer
    // Output: ZP.IDY = 0x0000 (empty list)
    // Preserves: A, X, Y, ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Clear();
}
```

#### Layer 2: Objects (Symbol Table Implementation)
Symbol-specific operations using Table foundation:

```hopper
unit Objects
{
    // Initialize empty symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: All registers and ZP variables
    Initialize();
    
    // Add new symbol to table
    // Input: ZP.TOP = name pointer, ZP.NEXT = symbolType|dataType (packed),
    //        ZP.ACC = value (16-bit)
    // Output: ZP.IDX = new symbol node address (0x0000 if failed)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Add();
    
    // Find symbol by name
    // Input: ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address (0x0000 if not found)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Find();
    
    // Remove symbol by name
    // Input: ZP.TOP = name pointer to remove
    // Output: Z set if removed, NZ if not found
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Remove();
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.NEXT = symbolType|dataType (packed), ZP.ACC = value
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP
    GetData();
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.ACC = new value
    // Output: Z set if successful, NZ if not a variable
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT
    SetValue();
    
    // Start iteration for specific symbol type
    // Input: ZP.NEXT = symbol type filter (0 = all types)
    // Output: ZP.IDX = first matching symbol (0x0000 if none)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateStart();
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.NEXT = type filter
    // Output: ZP.IDX = next matching symbol (0x0000 if done)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateNext();
    
    // Destroy entire symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT, ZP.ACC
    Destroy();
}