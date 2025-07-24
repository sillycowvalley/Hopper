unit Objects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "BasicTypes"
    uses "Table"
    
    // Unified symbol table for variables, constants, and functions
    // Uses Table unit for underlying linked list operations
    // Persistent state: ZP.SymbolListL/SymbolListH (global symbol table head)
    
    // Object types
    enum ObjectType
    {
        VARIABLE = 0x01,   // Mutable values (INT, WORD, BIT, future: BYTE, STRING, ARRAY)
        CONSTANT = 0x02,   // Immutable values (defined with CONST)
        FUNCTION = 0x03    // Executable code blocks (including main program)
    }
    
    // Node layout (fixed field offsets from node start):
    // Offset 0: objectType (ObjectType enum)
    // Offset 1: dataType (BasicType enum, or 0 for functions)
    // Offset 2-3: value/address (value for primitives, heap address for objects)
    // Offset 4-5: next pointer (managed by Table unit)
    // Offset 6+: null-terminated name string
    
    const byte SYMBOL_OVERHEAD = 6;    // Fixed fields before name
    const byte NAME_OFFSET = 6;        // Offset to name field in node
    
    // Initialize empty symbol table
    Initialize()
    {
        STZ ZP.SymbolListL
        STZ ZP.SymbolListH
    }
    
    // Add new object to table
    // Input: ZP.FITEM = name pointer, ZP.LTYPE = objectType, ZP.LITYPE = dataType, 
    //        ZP.LCOUNT = value (16-bit)
    // Output: ZP.LCURRENT = new object node address (0x0000 if failed)
    // Uses: Table.Add() - caller must not use F slots until this returns
    Add()
    {
        // Calculate node size: overhead + name length + 1 (null term)
        calculateNodeSize(); // Returns size in ZP.LLENGTH
        
        // Set up for Table.Add()
        LDA ZP.SymbolListL
        STA ZP.LCURRENTL
        LDA ZP.SymbolListH
        STA ZP.LCURRENTH
        
        Table.Add(); // Allocates node, returns new head in LCURRENT
        
        // Check if allocation succeeded
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z) { return; } // Failed
        
        // Update global symbol table head
        LDA ZP.LCURRENTL
        STA ZP.SymbolListL
        LDA ZP.LCURRENTH
        STA ZP.SymbolListH
        
        // Initialize the new node fields
        initializeNode();
    }
    
    // Calculate required node size for current object
    // Input: ZP.FITEM = name pointer
    // Output: ZP.LLENGTH = total node size (16-bit)
    // Uses: Y register for string traversal
    calculateNodeSize()
    {
        // Start with fixed overhead (6 bytes: objectType + dataType + value + next ptr)
        LDA #SYMBOL_OVERHEAD
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        
        // Add name length + null terminator
        LDY #0
        loop
        {
            LDA [ZP.FITEM], Y
            if (Z) { break; } // Hit null terminator
            
            INC ZP.LLENGTHL
            if (Z) { INC ZP.LLENGTHH }
            
            INY
        }
        
        // Add 1 for null terminator
        INC ZP.LLENGTHL
        if (Z) { INC ZP.LLENGTHH }
    }
    
    // Initialize fields in newly allocated node
    // Input: ZP.LCURRENT = node address, ZP.LTYPE = objectType, 
    //        ZP.LITYPE = dataType, ZP.LCOUNT = value, ZP.FITEM = name
    // Uses: Y register for field access
    initializeNode()
    {
        // Set objectType (offset 0)
        LDY #0
        LDA ZP.LTYPE
        STA [ZP.LCURRENT], Y
        
        // Set dataType (offset 1)
        INY
        LDA ZP.LITYPE
        STA [ZP.LCURRENT], Y
        
        // Set value/address (offset 2-3)
        INY
        LDA ZP.LCOUNTL
        STA [ZP.LCURRENT], Y
        INY
        LDA ZP.LCOUNTH
        STA [ZP.LCURRENT], Y
        
        // Next pointer (offset 4-5) already set by Table.Add()
        
        // Copy name string starting at offset 6
        copyNameToNode();
    }
    
    // Copy name string to node
    // Input: ZP.LCURRENT = node address, ZP.FITEM = source name
    // Uses: Y register for copying, ZP.LPREVIOUS for source addressing
    copyNameToNode()
    {
        // Initialize source index counter
        STZ ZP.LPREVIOUSL
        STZ ZP.LPREVIOUSH
        
        LDY #NAME_OFFSET  // Destination index
        
        loop
        {
            // Calculate source address: FITEM + source index
            CLC
            LDA ZP.FITEML
            ADC ZP.LPREVIOUSL
            STA ZP.LNEXTL
            LDA ZP.FITEMH
            ADC ZP.LPREVIOUSH
            STA ZP.LNEXTH
            
            // Load character from calculated source address
            PHY  // Save destination index
            LDY #0
            LDA [ZP.LNEXT], Y
            PLY  // Restore destination index
            
            // Store character at destination
            STA [ZP.LCURRENT], Y
            if (Z) { break; } // Copied null terminator
            
            // Increment source index
            INC ZP.LPREVIOUSL
            if (Z) { INC ZP.LPREVIOUSH }
            
            // Increment destination index
            INY
        }
    }
    
    // Look up object by name
    // Input: ZP.FITEM = name to search for
    // Output: ZP.LCURRENT = object node address (0x0000 if not found)
    // Uses: Table.Find()
    Lookup()
    {
        // Set up for Table.Find()
        LDA ZP.SymbolListL
        STA ZP.LCURRENTL
        LDA ZP.SymbolListH
        STA ZP.LCURRENTH
        
        LDA #NAME_OFFSET
        STA ZP.LCOUNTL
        STZ ZP.LCOUNTH
        
        Table.Find(); // Returns result in LCURRENT
    }
    
    // Remove object by name
    // Input: ZP.FITEM = name to remove
    // Output: Z set if removed, NZ if not found
    Remove()
    {
        // First find the object
        Lookup(); // Returns node in LCURRENT
        
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z)
        {
            // Not found
            LDA #1  // Set NZ
            return;
        }
        
        // Save node address for Table.Remove()
        LDA ZP.LCURRENTL
        STA ZP.FITEML  // Use FITEM for node to remove
        LDA ZP.LCURRENTH
        STA ZP.FITEMH
        
        // Check if it's an object type that needs cleanup
        cleanupObjectData();
        
        // Set up for Table.Remove()
        LDA ZP.SymbolListL
        STA ZP.LCURRENTL
        LDA ZP.SymbolListH
        STA ZP.LCURRENTH
        
        Table.Remove(); // Remove node, returns new head in LCURRENT
        
        // Update global symbol table head
        LDA ZP.LCURRENTL
        STA ZP.SymbolListL
        LDA ZP.LCURRENTH
        STA ZP.SymbolListH
        
        LDA #0  // Set Z (success)
    }
    
    // Clean up object data for object types (strings, arrays, functions)
    // Input: ZP.FITEM = node address to check
    // For Phase 1 (primitives only), this is a no-op
    cleanupObjectData()
    {
        // Phase 1: Only INT, WORD, BIT - no cleanup needed
        // Phase 3: Check dataType and free referenced objects
        
        // Future implementation:
        // LDY #1  // dataType offset
        // LDA [ZP.FITEM], Y
        // CMP #BasicType.STRING
        // if (Z) { /* free string object */ }
        // CMP #BasicType.ARRAY  
        // if (Z) { /* free array object */ }
        // etc.
    }
    
    // Destroy entire object table
    Destroy()
    {
        // Clean up all object references first
        cleanupAllObjects();
        
        // Set up for Table.Clear()
        LDA ZP.SymbolListL
        STA ZP.LCURRENTL
        LDA ZP.SymbolListH
        STA ZP.LCURRENTH
        
        Table.Clear(); // Frees all nodes
        
        // Reset global head
        STZ ZP.SymbolListL
        STZ ZP.SymbolListH
    }
    
    // Clean up all object references before destroying table
    // For Phase 1, this is a no-op
    cleanupAllObjects()
    {
        // Phase 1: Only primitives - no cleanup needed
        // Phase 3: Walk table and free all referenced objects
    }
    
    // Get object data from found node
    // Input: ZP.LCURRENT = object node address (from Lookup)
    // Output: ZP.LTYPE = objectType, ZP.LITYPE = dataType, ZP.LCOUNT = value
    GetData()
    {
        // Get objectType (offset 0)
        LDY #0
        LDA [ZP.LCURRENT], Y
        STA ZP.LTYPE
        
        // Get dataType (offset 1)
        INY
        LDA [ZP.LCURRENT], Y
        STA ZP.LITYPE
        
        // Get value/address (offset 2-3)
        INY
        LDA [ZP.LCURRENT], Y
        STA ZP.LCOUNTL
        INY
        LDA [ZP.LCURRENT], Y
        STA ZP.LCOUNTH
    }
    
    // Set object value (for variables only)
    // Input: ZP.LCURRENT = object node address, ZP.LCOUNT = new value
    // Output: Z set if successful, NZ if not a variable
    SetValue()
    {
        // Check if it's a variable (not constant or function)
        LDY #0
        LDA [ZP.LCURRENT], Y
        CMP #ObjectType.VARIABLE
        if (NZ)
        {
            LDA #1  // Set NZ (not a variable)
            return;
        }
        
        // Update value (offset 2-3)
        LDY #2
        LDA ZP.LCOUNTL
        STA [ZP.LCURRENT], Y
        INY
        LDA ZP.LCOUNTH
        STA [ZP.LCURRENT], Y
        
        LDA #0  // Set Z (success)
    }
    
    // Iterator support for console commands (VARS, FUNCS, etc.)
    // Start iteration for specific object type
    // Input: ZP.LTYPE = object type to iterate (0 = all types)
    // Output: ZP.LCURRENT = first matching object (0x0000 if none)
    IterateStart()
    {
        LDA ZP.SymbolListL
        STA ZP.LCURRENTL
        LDA ZP.SymbolListH
        STA ZP.LCURRENTH
        
        // Find first matching object
        findNextMatch();
    }
    
    // Get next object in iteration
    // Input: ZP.LCURRENT = current object, ZP.LTYPE = type filter
    // Output: ZP.LCURRENT = next matching object (0x0000 if done)
    IterateNext()
    {
        // Check if current is valid
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z) { return; } // Already at end
        
        // Move to next node in list
        LDA ZP.LCURRENTL
        STA ZP.LPREVIOUSL
        LDA ZP.LCURRENTH
        STA ZP.LPREVIOUSH
        Table.getNextPointer(); // Returns next in LNEXT
        
        LDA ZP.LNEXTL
        STA ZP.LCURRENTL
        LDA ZP.LNEXTH
        STA ZP.LCURRENTH
        
        // Find next matching object
        findNextMatch();
    }
    
    // Find next object matching type filter
    // Input: ZP.LCURRENT = starting position, ZP.LTYPE = type filter (0 = all)
    // Output: ZP.LCURRENT = next matching object (0x0000 if none)
    findNextMatch()
    {
        loop
        {
            // Check if we've reached end
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z) { return; } // End of list
            
            // Check type filter
            LDA ZP.LTYPE
            if (Z) { return; } // No filter - return current
            
            // Get object type from current node
            LDY #0
            LDA [ZP.LCURRENT], Y
            CMP ZP.LTYPE
            if (Z) { return; } // Found matching type
            
            // Move to next node
            LDA ZP.LCURRENTL
            STA ZP.LPREVIOUSL
            LDA ZP.LCURRENTH
            STA ZP.LPREVIOUSH
            Table.getNextPointer();
            
            LDA ZP.LNEXTL
            STA ZP.LCURRENTL
            LDA ZP.LNEXTH
            STA ZP.LCURRENTH
        }
    }
}