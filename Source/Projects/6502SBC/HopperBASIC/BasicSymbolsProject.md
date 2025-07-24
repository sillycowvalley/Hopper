### Two-Layer Design

#### Layer 1: Table (Generic Linked List)
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
    // Output: ZP.IDX = new node address (0x0000 if allocation failed), Z set if successful
    // Preserves: A, Y, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Add();
    
    // Delete specific node from list
    // Input: X = ZP address of list head pointer, ZP.IDX = node to delete
    // Output: None (list head updated in place)
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
    // Input: ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
    //        ZP.NEXT = value (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, X, Y, ZP.TOP, ZP.ACC, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Add();
    
    // Find symbol by name
    // Input: ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Find();
    
    // Remove symbol by node pointer
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: None (thin wrapper around Table.Delete)
    // Preserves: A, Y, ZP.TOP, ZP.NEXT, ZP.ACC
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
    Destroy();
}