unit AST
{
    friend Parser;
    
    // AST zero page allocation
    const byte astSlots = 0x80;
    
    const uint astRoot      = astSlots+0;  // Root of AST tree
    const byte astRootL     = astSlots+0;
    const byte astRootH     = astSlots+1;
    
    // Node types
    enum NodeType
    {
        Program      = 1,   // Root node
        Function     = 2,   // Function definition
        CompoundStmt = 3,   // Block { }
        ExprStmt     = 4,   // Expression statement
        CallExpr     = 5,   // Function call
        Identifier   = 6,   // Variable/function name
        StringLit    = 7,   // String literal
        IntLit       = 8,   // Integer literal (32 bits)
    }
    
    // Node structure (8 bytes):
    // [0] = NodeType
    // [1] = Reserved/flags
    // [2-3] = Data pointer (for strings) or value
    // [4-5] = First child pointer
    // [6-7] = Next sibling pointer
    
    const byte iNodeType = 0;
    const byte iData     = 2;
    const byte iChild    = 4;
    const byte iNext     = 6;
    
    const byte nodeSize = 8;
    
    Initialize()
    {
        // Create root Program node
        LDA #nodeSize
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            STZ astRootL
            STZ astRootH
            CLC
            return;
        }
        
        LDA ZP.IDXL
        STA astRootL
        LDA ZP.IDXH
        STA astRootH
        
        // Set it as Program type
        LDA # NodeType.Program
        STA [ZP.IDX]
        
        SEC
    }
    
    Dispose()
    {
        LDA astRootL
        ORA astRootH
        if (NZ)
        {
            // Recursively free the tree
            LDA astRootL
            STA ZP.IDXL
            LDA astRootH
            STA ZP.IDXH
            freeNode();
            
            STZ astRootL
            STZ astRootH
        }
    }
    
    // Create a new node
    // Input:  A = NodeType
    // Output: ZP.IDX = node pointer, C set on success
    CreateNode()
    {
        PHA  // Save node type
        
        LDA # nodeSize
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            PLA
            Parser.OutOfMemoryError();
            return;
        }
        
        // Set node type
        PLA
        STA [ZP.IDX]
        
        SEC
    }
    
    // Recursively free a node and its children
    // Input: ZP.IDX = node pointer
    freeNode()
    {
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) { return; }  // Null pointer
        
        PHY
        
        // Save current node pointer
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Free first child if exists
        LDY # iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        freeNode();
        
        // Restore node pointer
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Save again
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Free next sibling if exists
        LDY # iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        freeNode();
        
        // Restore and free this node
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.IDXH
        Memory.Free();
        
        PLY
    }
    
    GetRoot() // -> IDX
    {
        LDA AST.astRootL
        STA ZP.IDXL
        LDA AST.astRootH
        STA ZP.IDXH
    }   
    
    // Set first child of a node
    // Input: ZP.IDX = parent node, ZP.IDY = child node
    SetFirstChild()
    {
        LDY # iChild
        LDA ZP.IDYL
        STA [ZP.IDX], Y
        INY
        LDA ZP.IDYH
        STA [ZP.IDX], Y
    }
    
    // Set next sibling of a node  
    // Input: ZP.IDX = node, ZP.IDY = sibling node
    SetNextSibling()
    {
        LDY # iNext
        LDA ZP.IDYL
        STA [ZP.IDX], Y
        INY
        LDA ZP.IDYH
        STA [ZP.IDX], Y
    }
    
    // Set data pointer of a node
    // Input: ZP.IDX = node, ZP.ACC = data pointer
    SetData()
    {
        LDY # iData
        LDA ZP.ACCL
        STA [ZP.IDX], Y
        INY
        LDA ZP.ACCH
        STA [ZP.IDX], Y
    }
    
    // Walk to the end of sibling list
    // Input:  ZP.IDX = starting node
    // Output: ZP.IDX = last node in sibling chain
    walkToLastSibling() // -> IDX
    {
        PHY
        
        loop
        {
            LDY # iNext
            LDA [ZP.IDX], Y
            STA ZP.TEMP
            INY
            LDA [ZP.IDX], Y
            
            ORA ZP.TEMP  // Check if null
            if (Z) { break; }  // Found end of list
            
            // Move to next sibling
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            LDA ZP.TEMP
            STA ZP.IDXL
        }
        PLY
    }

    // Add a child to a node (handles existing children)
    // Input: ZP.IDX = parent node, ZP.IDY = new child node
    AddChild()
    {
        PHY
        
        // Check if parent already has a child
        LDY # iChild
        LDA [ZP.IDX], Y
        STA ZP.TEMP
        INY
        LDA [ZP.IDX], Y
        
        ORA ZP.TEMP  // Check if null
        if (Z)  // No existing child
        {
            // Just set as first child
            SetFirstChild();// IDX[iChild] = IDY
        }
        else
        {
            // Has existing child - load it
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            LDA ZP.TEMP
            STA ZP.IDXL  // IDX = first child
            
            // Walk to end of sibling list
            walkToLastSibling(); // -> IDX (last sibling)
            
            // IDX = last sibling, IDY = new node to add
            SetNextSibling();// IDX[iNext] = IDY
        }
        
        PLY
    }
    
    // Add a sibling to a node (walks to end of list)
    // Input: ZP.IDX = node, ZP.IDY = new sibling
    AddSibling()
    {
        walkToLastSibling(); // -> IDX (last in chain)
        SetNextSibling();// IDX[iNext] = IDY
    }
    
    
#if defined(DEBUG)

    // Debug strings
    const string nodeProg     = "PROG";
    const string nodeFunc     = "FUNC";
    const string nodeBlock    = "BLOCK";
    const string nodeExpr     = "EXPR";
    const string nodeCall     = "CALL";
    const string nodeId       = "ID ";
    const string nodeStr      = "STR ";
    const string nodeInt      = "INT ";
    const string nodeUnknown  = "??";
    
    // Print the AST tree with indentation
    // Input: ZP.IDX = node pointer, ZP.TEMP = indent level
    PrintNode()
    {
        // Check for null
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) { return; }
        
        PHY
        
        // Print indentation
        LDY ZP.TEMP
        if (NZ)
        {
            loop
            {
                Print.Space();
                Print.Space();
                DEY
                if (Z) { break; }
            }
        }
        
        // Print node type
        LDY # iNodeType
        LDA [ZP.IDX], Y
        switch (A)
        {
            case NodeType.Program:
            {
                LDA #(nodeProg % 256)
                STA ZP.STRL
                LDA #(nodeProg / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.Function:
            {
                LDA #(nodeFunc % 256)
                STA ZP.STRL
                LDA #(nodeFunc / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.CompoundStmt:
            {
                LDA #(nodeBlock % 256)
                STA ZP.STRL
                LDA #(nodeBlock / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.ExprStmt:
            {
                LDA #(nodeExpr % 256)
                STA ZP.STRL
                LDA #(nodeExpr / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.CallExpr:
            {
                LDA #(nodeCall % 256)
                STA ZP.STRL
                LDA #(nodeCall / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.Identifier:
            {
                LDA #(nodeId % 256)
                STA ZP.STRL
                LDA #(nodeId / 256)
                STA ZP.STRH
                Print.String();
                
                // Print the identifier string
                LDY # iData
                LDA [ZP.IDX], Y
                STA ZP.STRL
                INY
                LDA [ZP.IDX], Y
                STA ZP.STRH
                
                LDA ZP.STRL
                ORA ZP.STRH
                if (NZ)
                {
                    Print.String();
                }
            }
            case NodeType.StringLit:
            {
                LDA #(nodeStr % 256)
                STA ZP.STRL
                LDA #(nodeStr / 256)
                STA ZP.STRH
                Print.String();
                
                LDA #'"'
                Print.Char();
                
                // Print the string literal
                LDY # iData
                LDA [ZP.IDX], Y
                STA ZP.STRL
                INY
                LDA [ZP.IDX], Y
                STA ZP.STRH
                
                LDA ZP.STRL
                ORA ZP.STRH
                if (NZ)
                {
                    Print.String();
                }
                LDA #'"'
                Print.Char();
            }
            case NodeType.IntLit:
            {
                LDA #(nodeInt % 256)
                STA ZP.STRL
                LDA #(nodeInt / 256)
                STA ZP.STRH
                Print.String();
                
                // Get pointer to 32-bit value
                LDY # iData
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                
                // Print the integer value
                // Load 32-bit value from where IDY points
                LDY #0
                LDX #0
                loop
                {
                    LDA [ZP.IDY], Y
                    STA ZP.TOP, X
                    INX
                    INY
                    CPY #4
                    if (Z) { break; }
                }
                Long.Print();
            }
            default:
            {
                LDA #(nodeUnknown % 256)
                STA ZP.STRL
                LDA #(nodeUnknown / 256)
                STA ZP.STRH
                Print.String();
            }
        } // switch
        Print.Space(); LDA ZP.IDXH Print.Hex(); LDA ZP.IDXL Print.Hex();
        
        Print.NewLine();
        
        // Save current node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Process first child with increased indentation
        LDY # iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            INC ZP.TEMP  // Increase indent
            PrintNode();  // Recursive call for child
            DEC ZP.TEMP  // Restore indent
        }
        
        // Restore current node
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Process next sibling at same indentation
        LDY # iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            PrintNode();  // Recursive call for sibling (same indent)
        }
        
        PLY
    }
    
    // Print entire AST from root
    PrintTree()
    {
        LDA astRootL
        STA ZP.IDXL
        LDA astRootH
        STA ZP.IDXH
        
        STZ ZP.TEMP  // Start with no indentation
        Print.NewLine();
        PrintNode();
    }


#endif 
    
}
