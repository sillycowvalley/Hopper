unit AST
{
    uses "../System/Shared"
    
    friend Parser, CodeGen, Gen6502, Library, CC, Errors;
    
    // AST zero page allocation
    const byte astSlots = 0x80;
    
    const uint astRoot      = astSlots+0;  // Root of AST tree
    const byte astRootL     = astSlots+0;
    const byte astRootH     = astSlots+1;
    
    const uint astNode      = astSlots+2;
    const byte astNodeL     = astSlots+2;
    const byte astNodeH     = astSlots+3;
    
    const uint astTempNode   = astSlots+4;
    const byte astTempNodeL  = astSlots+4;
    const byte astTempNodeH  = astSlots+5;
    
    // Node types
    enum NodeType
    {
        Program      = 1,   // Root node
        Function     = 2,   // Function definition
        CompoundStmt = 3,   // Block { }
        ExprStmt     = 4,   // Expression statement
        CallExpr     = 5,   // Function call
        Identifier   = 6,   // Variable/function name
        StringLit    = 7,   // String literal  (Data is a pointer to a string)
        CharLit      = 8,   // Character literal (8 bits stored in Data)
        IntLit       = 9,   // Integer literal (16 bits stored in Data)
        LongLit      = 10,  // Long literal    (32 bits stored in Data+ )
        VarDecl      = 11,  // Variable declaration 
        Assign       = 12,  // Assignment expression
        BinOp        = 13,  // Binary operation
        PostfixOp    = 14,  // Postfix operation
        For          = 15,  // For loop 
        Return       = 16,  // Return statement 
        If           = 17,  // If statement
        While        = 18,  // While statement
        UnaryOp      = 19,  
        Empty        = 20,  // like ";"
        ConstDecl    = 21,  // Const declaration
        Break        = 22,
        Continue     = 23,
        
        AfterLast           // see freeNode()
    }
    
    enum BinOpType
    {
        None = 0,
        Add = 1,   // +
        Sub = 2,   // -
        Mul = 3,   // *
        Div = 4,   // /
        Mod = 5,   // %
        
        LT = 6,    // 
        GT = 7,    // >
        LE = 8,    // <=
        GE = 9,    // >=
        EQ = 10,   // ==
        NE = 11,   // !=
        
        LogicalOr  = 12,
        LogicalAnd = 13,
        BitwiseOr  = 14,
        BitwiseAnd = 15,
    }
    
    enum UnaryOpType
    {
        Minus       = 0x00,
        Plus        = 0x01,
        Not         = 0x02,  // For future ! operator
        BitNot      = 0x03,  // For future ~ operator
        Dereference = 0x04,  // *(pointer reference)
    }
    enum PostfixOpType
    {
        Increment,
        Decrement
    }
    enum VarScope
    {
        Local  = 0,
        Global = 1
    }
    enum StringType
    {
        Normal,
        Hex,
    }
    
    // Common node structure:
    //     [0] = NodeType
    //     [1-2] = Source line number
    //     [3-4] = First child pointer
    //     [5-6] = Next sibling pointer
    
    const byte nodeSize = 14; // minimum allocation is 16 bytes - 2 for the allocator
    
    const byte iNodeType   = 0;
    const byte iLineNumber = 1;
    const byte iChild      = 3;
    const byte iNext       = 5;
    
    // Program node (root):
    // ExprStmt node:
    // CompoundStmt node:
    // CallExpr node:
    //     No additional fields
    
    
    // Function node:
    //     [7]     Return type
    //     [8]     <unused>
    //     [9-10]  Code offset (where function starts in buffer)
    const byte iReturnType = 7;
    const byte iOffset     = 9; // Code offset index (where code ends up in the codegen buffer)
    
    // Identifier node:
    //     [7-8]  Data pointer -> string
    const byte iData     = 7; 
    
    // StringLit node:
    //     [7-8]   Data pointer -> string
    //     [9-10]  Code offset index (where string ends up in the codegen buffer)
    //     [11]    StringFlags: StringType.Normal for regular string, StringType.Hex for created from bytes
    // const byte iData     = 7;
    // const byte iOffset   = 9; 
    const byte iStrFlags    = 11;
    
    // VarDecl node:
    //     [7-8]  <unused>
    //     [9]    offset on stack relative to BP (signed single byte offset)
    //     [10]   varFlags: VarScope.Local or VarScope.Global
    //     [11]   Variable type (Token.Long/Int/Char)
    // const byte iOffset   = 9;
    const byte iVarScope    = 10;
    const byte iVarType     = 11;
    
    // ConstDecl node:
    //     [7-8]  <unused>
    //     [9]    <unused>
    //     [10]   <unused>
    //     [11]   Constant type (Token.Long/Int/Char)
    // const byte iOffset   = 9;
    const byte iConstType   = 11;
    
    // BinOp node:
    //     [7]    BinOpType
    const byte iBinOp = 7;
    
    // PostfixOp node:
    //     [7]    PostfixOpType
    const byte iPostfixOp = 7;
    
    // UnaryOp node:
    //     [7]    UnaryOpType
    const byte iUnaryOp = 7;
    
    
    // For node:
    //     [7-8]   Init expression (optional)
    //     [9-10]  Exit condition expression (optional)
    //     [11-12] Next/update expression (optional)
    //     Child:  Body statement
    const byte iForInit    = 7;
    const byte iForExit    = 9;
    const byte iForNext    = 11;
    
    // If node:
    //     [0]     NodeType (NodeType.If)
    //     [1-2]   Source line number
    //     [3-4]   Child pointer -> condition expression
    //     [5-6]   Next sibling pointer
    //
    // Child list structure:
    //   1. Condition expression (child)
    //   2. Then statement (condition->next)
    //   3. Else statement (then->next) - optional, can be null
    
    // While node:
    //     [0]     NodeType (NodeType.While)
    //     [1-2]   Source line number
    //     [3-4]   Child pointer -> condition expression
    //     [5-6]   Next sibling pointer
    //
    // Child list structure:
    //   1. Condition expression (child)
    //   2. Body statement
    
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
            FreeNode();
            
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
        
        LDA ZP.TEMP
        PHA
        
        LDA # nodeSize
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            PLA
            PLA
            Errors.OutOfMemory();
            return;
        }
        
        PLA
        STA ZP.TEMP
        
        // Set node type
        PLA
        STA [ZP.IDX]
        
        // Store current line number from lexer
        PHY
        
        Lexer.GetLineNumber(); // -> ACC
        
        LDY # iLineNumber
        LDA ZP.ACCL
        STA [ZP.IDX], Y
        INY
        LDA ZP.ACCH
        STA [ZP.IDX], Y
        PLY
        
        SEC
    }
    
    // Recursively free a node and its children
    // Input: ZP.IDX = node pointer
    FreeNode()
    {
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) { return; }  // Null pointer
        
        // Save current node pointer for recursion
        LDA astNodeL
        PHA
        LDA astNodeH
        PHA
        loop
        {
            LDA ZP.IDXL
            STA astNodeL
            LDA ZP.IDXH
            STA astNodeH
            
            // Check node type is valid before processing
            LDY #iNodeType
            LDA [astNode], Y
            CMP # NodeType.AfterLast
            if (C)  // >=  than max node type?
            {
                // Corrupted node - just free it without recursion
                LDA astNodeH 
                STA ZP.IDXH
                LDA astNodeL
                STA ZP.IDXL
                Memory.Free();
                
                break;
            }
        
        
            // Free literal data if applicable
            switch (A)
            {
                case NodeType.StringLit:
                case NodeType.Identifier:
                case NodeType.IntLit:
                case NodeType.LongLit:
                case NodeType.CharLit:
                {
                    LDY #iData
                    LDA [astNode], Y
                    STA ZP.IDXL
                    INY
                    LDA [astNode], Y
                    STA ZP.IDXH
                    
                    // Check if pointer looks valid
                    LDA ZP.IDXL
                    ORA ZP.IDXH
                    if (NZ)
                    {
                        Memory.Free();
                    }
                }
            }
            
            // Free first child if exists
            LDY # iChild
            LDA [astNode], Y
            STA ZP.IDXL
            INY
            LDA [astNode], Y
            STA ZP.IDXH
            FreeNode();
            
            // Free next sibling if exists
            LDY # iNext
            LDA [astNode], Y
            STA ZP.IDXL
            INY
            LDA [astNode], Y
            STA ZP.IDXH
            FreeNode();
            
            LDA astNodeH 
            STA ZP.IDXH
            LDA astNodeL
            STA ZP.IDXL
            Memory.Free();
            break;
        } // single exit
        
        PLA 
        STA astNodeH
        PLA
        STA astNodeL
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
    
    // Get first child of a node
    // Input: ZP.IDX = parent node
    // Output ZP.IDY = child node
    GetFirstChild()
    {
        LDY # iChild
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
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

    // Add a child node to a parent node
    // Input:  ZP.IDX = parent node (preserved)
    //         ZP.IDY = child node to add
    // Output: Child added to parent's child list
    //         If parent has no children, child becomes first child
    //         If parent has children, child is appended as last sibling
    // Note:   IDX is preserved (still points to parent after call)
    //         IDY is preserved
    AddChild()
    {
        PHY
        
        // Save parent node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
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
        
        // Restore parent node
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
    }
    
    // Add a sibling to a node (walks to end of list)
    // Input: ZP.IDX = node, ZP.IDY = new sibling
    AddSibling()
    {
        walkToLastSibling(); // -> IDX (last in chain)
        SetNextSibling();// IDX[iNext] = IDY
    }
    
    // Input:  STR = name
    // Output: IDX = ConstDecl node
    //         C set if found
    FindConstant()  
    {
        // Search program level for ConstDecl nodes
        LDA AST.astRootL
        STA ZP.IDXL
        LDA AST.astRootH
        STA ZP.IDXH
        
        AST.GetFirstChild(); // -> IDY
        
        loop
        {
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z) { CLC return; }  // Not found
            
            // Check if ConstDecl
            LDY #AST.iNodeType
            LDA [ZP.IDY], Y
            CMP #AST.NodeType.ConstDecl
            if (Z)
            {
                // Get identifier child and compare name
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                AST.GetFirstChild(); // -> IDY (identifier)
                
                LDY #AST.iData
                LDA [ZP.IDY], Y
                TAX
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                CompareStrings(); // STR vs IDY
                if (C)
                {
                    // Found it! Return ConstDecl in IDX
                    SEC
                    return;
                }
            }
            // Try next sibling
            LDY #AST.iNext
            LDA [ZP.IDY], Y
            TAX
            INY
            LDA [ZP.IDY], Y
            STA ZP.IDYH
            STX ZP.IDYL
        }
    }
    
    
    // Find VarDecl node for an identifier
    // Input: ZP.STR = identifier name
    // Output: IDX = VarDecl node, C set if found
    FindVariable()
    {
        // Start from current function node
        LDA CodeGen.functionNodeL
        STA ZP.IDXL
        LDA CodeGen.functionNodeH
        STA ZP.IDXH
        
        // Get first child (identifier)
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        // Walk siblings looking for parameters first
        loop
        {
            // Get next sibling
            LDY # AST.iNext
            LDA [ZP.IDY], Y
            TAX
            INY
            LDA [ZP.IDY], Y
            if (Z)
            {
                TXA
                if (Z) { break; }  // No more siblings
            }
            STA ZP.IDYH
            STX ZP.IDYL
            
            // Check node type
            LDY #AST.iNodeType
            LDA [ZP.IDY], Y
            CMP #AST.NodeType.VarDecl
            if (Z)
            {
                // It's a parameter VarDecl - get its identifier child
                LDA ZP.IDYL
                PHA
                LDA ZP.IDYH
                PHA
                
                // Get VarDecl's child (Identifier node)
                LDY #AST.iChild
                LDA [ZP.IDY], Y
                STA ZP.IDXL
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDXH
                
                // Get identifier's name from iData
                LDY #AST.iData
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                
                // Compare with target name
                CompareStrings();  // STR vs IDY
                
                PLA
                STA ZP.IDYH
                PLA
                STA ZP.IDYL
                
                if (C)
                {
                    // Found it! Return the VarDecl node
                    LDA ZP.IDYL
                    STA ZP.IDXL
                    LDA ZP.IDYH
                    STA ZP.IDXH
                    SEC
                    return;
                }
            }
            else
            {
                CMP #AST.NodeType.CompoundStmt
                if (Z)
                {
                    // Hit the compound statement - save it and stop parameter search
                    LDA ZP.IDYL
                    STA ZP.IDXL
                    LDA ZP.IDYH
                    STA ZP.IDXH
                    break;
                }
            }
        }

        
        // Now IDX = CompoundStmt, search its children for local VarDecls
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }  // Not found
            
            // Check if it's a VarDecl
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            CMP #AST.NodeType.VarDecl
            if (Z)
            {
                // Save VarDecl node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Get VarDecl's child (Identifier node)
                LDY #AST.iChild
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                
                // Get identifier's name from iData
                LDY #AST.iData
                LDA [ZP.IDY], Y
                TAX
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                // Compare with target name
                CompareStrings();  // STR vs IDY
                
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                
                if (C)
                {
                    // Found it! IDX already has VarDecl
                    SEC
                    return;
                }
            }
            
            // Move to next sibling
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
        }
        
        // Not found in locals, now search globals at root
        LDA AST.astRootL
        STA ZP.IDXL
        LDA AST.astRootH
        STA ZP.IDXH
        
        AST.GetFirstChild(); // -> IDY
        
        loop
        {
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z) { CLC return; }  // Not found anywhere
            
            LDY #AST.iNodeType
            LDA [ZP.IDY], Y
            CMP #AST.NodeType.VarDecl
            if (Z)
            {
                // Save VarDecl node
                LDA ZP.IDYL
                PHA
                LDA ZP.IDYH
                PHA
                
                // Get identifier child
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                AST.GetFirstChild(); // -> IDY (identifier)
                
                // Get name from identifier
                LDY #AST.iData
                LDA [ZP.IDY], Y
                TAX
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                // Compare with target name
                CompareStrings(); // STR vs IDY
                
                // Restore VarDecl
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                
                if (C)
                {
                    // Found global!
                    SEC
                    return;
                }
                
                // Restore IDY for next iteration
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
            }
            
            // Next sibling at root level
            LDY #AST.iNext
            LDA [ZP.IDY], Y
            TAX
            INY
            LDA [ZP.IDY], Y
            STA ZP.IDYH
            STX ZP.IDYL
        }
        
        CLC  // Not found
    }
    
    // Find a function node in the AST by name
    // Input: ZP.STR = function name to find
    // Output: IDX = Function node if found
    //         C set on success, clear if not found
    FindFunction()
    {
        // Get first child of Program
        AST.GetRoot();  // -> IDX
        LDA ZP.IDXL
        STA AST.astNodeL
        LDA ZP.IDXH
        STA AST.astNodeH
        
        LDY #AST.iChild
        LDA [AST.astNode], Y
        TAX
        INY
        LDA [AST.astNode], Y
        STA AST.astNodeH
        STX AST.astNodeL
        loop
        {
            // Check it's a Function
            LDY #AST.iNodeType
            LDA [AST.astNode], Y
            CMP #AST.NodeType.Function
            if (Z)
            {
                // compare STR to name
                // Get function's identifier child (assume it is the first child)
                LDY #AST.iChild
                LDA [AST.astNode], Y
                TAX
                INY
                LDA [AST.astNode], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                // Get identifier's string pointer
                LDY #AST.iData
                LDA [ZP.IDY], Y
                TAX
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                // Compare strings [STR] with [IDY]
                CompareStrings();
                if (C)
                {
                    // Found it! Return astNode as Function
                    LDA AST.astNodeL
                    STA ZP.IDXL
                    LDA AST.astNodeH
                    STA ZP.IDXH
                    SEC
                    break;
                }
            }
            
            // try next sibling
            LDY #AST.iNext
            LDA [AST.astNode], Y
            TAX
            INY
            LDA [AST.astNode], Y
            STA AST.astNodeH
            STX AST.astNodeL
            
            LDA AST.astNodeH
            ORA AST.astNodeL
            if (Z)
            {
                CLC
                break;
            }
        } // loop
    }
    
    // Clone a node (only supports Identifier for compound assignments)
    // Input: IDY = node to clone
    // Output: IDY = cloned node, C set on success, clear on failure
    CloneNode()
    {
        loop
        {
            // Get node type
            LDY #AST.iNodeType
            LDA [ZP.IDY], Y
            
             
            CMP #NodeType.Identifier
            if (NZ)
            {
                // Not an identifier - unsupported for cloning
                
        #ifdef DEBUG
                Print.NewLine();
                LDA #'C' 
                Print.Char();
                LDA #'?' 
                Print.Char();
                LDY #AST.iNodeType
                LDA [ZP.IDY], Y
                Print.Hex();
        #endif
                
                LDA # Error.UnsupportedLValue
                Errors.ShowIDY();
                break;
            }
        
            // Create new Identifier node
            LDA #AST.NodeType.Identifier
            AST.CreateNode(); // -> IDX
            if (NC) 
            { 
                break; 
            }
        
            // Get original identifier's string pointer
            
            LDY #AST.iData
            LDA [ZP.IDY], Y
            STA ZP.STRL
            INY
            LDA [ZP.IDY], Y
            STA ZP.STRH
            
            // Duplicate the string
            Utilities.DuplicateString(); // STR -> STR
            if (NC)
            {
                // Failed to clone string, free the node
                AST.FreeNode(); // IDX
                break;
            }
            
            // Store the new string pointer in ACC for SetData
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
        
            // Set the cloned string in new node
            AST.SetData(); // IDX[iData] = ACC
            
            // Return cloned node in IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
        
            SEC
            break;
        } // single exit
    }
    
    
    CountFunctionParameters() // Input: AST.astNode = Function node, Output: A = param count
    {
        LDX #0
        
        // First child is identifier
        LDY #AST.iChild
        LDA [AST.astNode], Y
        STA ZP.IDYL
        INY
        LDA [AST.astNode], Y
        STA ZP.IDYH
        
        // Move to first sibling (could be parameter or body)
        loop
        {
            // Get next sibling
            LDY #AST.iNext
            LDA [ZP.IDY], Y
            STA ZP.TEMP
            INY
            LDA [ZP.IDY], Y
            if (Z)
            {
                LDA ZP.TEMP
                if (Z) { break; }  // No more siblings
            }
            STA ZP.IDYH
            LDA ZP.TEMP
            STA ZP.IDYL
            
            // Check if it's CompoundStmt (the body)
            LDY #AST.iNodeType
            LDA [ZP.IDY], Y
            CMP #AST.NodeType.CompoundStmt
            if (Z) { break; }  // Found body, stop counting
            
            // It's a parameter
            INX
        }
        
        TXA
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
    const string nodeLong     = "LONG ";
    const string nodeChar     = "CHAR ";
    const string nodeCharPtr  = "CHARPTR ";
    const string nodeFilePtr  = "FILEPTR ";
    const string nodeVarDecl  = "VAR "; 
    const string nodeConstDecl= "CONST "; 
    const string nodeAssign   = "ASSIGN";
    const string nodeReturn   = "RETURN";
    const string nodeBreak    = "BREAK";
    const string nodeContinue = "CONTINUE";
    const string nodeBinOp    = "BINOP ";
    const string nodePostfixOp= "POSTFIX ";
    const string nodeFor      = "FOR";
    const string nodeWhile    = "WHILE";
    const string nodeUnary    = "UNARY";
    const string nodeIf       = "IF";
    const string nodeEmpty    = "EMPTY";
    const string nodeUnknown  = "??";
    
    const string typeVoid    = "void ";
    const string typeInt     = "int ";
    const string typeLong    = "long ";
    const string typeChar    = "char ";
    const string typeCharPtr = "char* ";
    const string typeFilePtr = "FILE* ";
    
    
    const string opAdd = "+";
    const string opSub = "-";
    const string opMul = "*";
    const string opDiv = "/";
    const string opMod = "%";
    
    const string opAnd = "&&";
    const string opOr  = "||";
    const string opBitAnd = "&";
    const string opBitOr  = "|";
    
    const string opEQ = "==";
    const string opNE = "!=";
    const string opLT = "<";
    const string opGT = ">";
    const string opLE = "<=";
    const string opGE = ">=";
    
    const string opIncrement = "++";
    const string opDecrement = "--";
    
    const string nodeBPOffset = "[BP";
    
    const string hexData = "<hex data>";
    
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
        
        // Print line number in brackets
        LDA #'['
        Print.Char();
        LDY #AST.iLineNumber
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.ACCH
        Shared.MoveAccToTop();
        Long.Print();
        LDA #']'
        Print.Char();
        Print.Space();
        
        // Print node type
        LDY # iNodeType
        LDA [ZP.IDX], Y
        switch (A)
        {
            case NodeType.Empty:
            {
                LDA #(nodeEmpty % 256)
                STA ZP.STRL
                LDA #(nodeEmpty / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.If:
            {
                LDA #(nodeIf % 256)
                STA ZP.STRL
                LDA #(nodeIf / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.While:
            {
                LDA #(nodeWhile % 256)
                STA ZP.STRL
                LDA #(nodeWhile / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.UnaryOp:
            {
                LDA #(nodeUnary % 256)
                STA ZP.STRL
                LDA #(nodeUnary / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.Return:
            {
                LDA #(nodeReturn % 256)
                STA ZP.STRL
                LDA #(nodeReturn / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.For:
            {
                LDA #(nodeFor % 256)
                STA ZP.STRL
                LDA #(nodeFor / 256)
                STA ZP.STRH
                Print.String();
                Print.Space(); LDA ZP.IDXH Print.Hex(); LDA ZP.IDXL Print.Hex();
                Print.NewLine();
                
                // Save current node for later
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Print init expression if present
                LDY #AST.iForInit
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
                    PrintNode();  // Recursive call for init
                    DEC ZP.TEMP  // Restore indent
                }
                
                // Restore node
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Print exit expression if present
                LDY #AST.iForExit
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
                    PrintNode();  // Recursive call for exit
                    DEC ZP.TEMP  // Restore indent
                }
                
                // Restore node
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Print next expression if present
                LDY #AST.iForNext
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
                    PrintNode();  // Recursive call for next
                    DEC ZP.TEMP  // Restore indent
                }
                
                // Restore node - don't pop this time, leave it for normal child processing
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
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
                // Print return type
                LDY #AST.iReturnType
                LDA [ZP.IDX], Y
                
                // Convert type token to string and print
                switch (A)
                {
                    case Token.Void:
                    {
                        LDA #(typeVoid % 256)
                        STA ZP.STRL
                        LDA #(typeVoid / 256)
                        STA ZP.STRH
                        Print.String();
                    }
                    case Token.Int:
                    {
                        LDA #(typeInt % 256)
                        STA ZP.STRL
                        LDA #(typeInt / 256)
                        STA ZP.STRH
                        Print.String();
                    }
                    case Token.Long:
                    {
                        LDA #(typeLong % 256)
                        STA ZP.STRL
                        LDA #(typeLong / 256)
                        STA ZP.STRH
                        Print.String();
                    }
                    case Token.Char:
                    {
                        LDA #(typeChar % 256)
                        STA ZP.STRL
                        LDA #(typeChar / 256)
                        STA ZP.STRH
                        Print.String();
                    }
                    case Token.CharPtr:
                    {
                        LDA #(typeCharPtr % 256)
                        STA ZP.STRL
                        LDA #(typeCharPtr / 256)
                        STA ZP.STRH
                        Print.String();
                    }
                    case Token.FilePtr:
                    {
                        LDA #(typeFilePtr % 256)
                        STA ZP.STRL
                        LDA #(typeFilePtr / 256)
                        STA ZP.STRH
                        Print.String();
                    }
                    
                }
                
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
            
            case NodeType.PostfixOp:
            {
                LDA #(nodePostfixOp % 256)
                STA ZP.STRL
                LDA #(nodePostfixOp / 256)
                STA ZP.STRH
                Print.String();
                
                // Print the operator
                LDY # iPostfixOp
                LDA [ZP.IDX], Y
                switch (A)
                {
                    case PostfixOpType.Increment:
                    {
                        LDA #(opIncrement % 256)
                        STA ZP.STRL
                        LDA #(opIncrement / 256)
                        STA ZP.STRH
                    }
                    case PostfixOpType.Decrement:
                    {
                        LDA #(opDecrement % 256)
                        STA ZP.STRL
                        LDA #(opDecrement / 256)
                        STA ZP.STRH
                    }
                }
                Print.String();
            }
            
            case NodeType.BinOp:
            {
                LDA #(nodeBinOp % 256)
                STA ZP.STRL
                LDA #(nodeBinOp / 256)
                STA ZP.STRH
                Print.String();
                
                // Print the operator
                LDY #iBinOp
                LDA [ZP.IDX], Y
                switch (A)
                {
                    case BinOpType.Add:
                    {
                        LDA #(opAdd % 256)
                        STA ZP.STRL
                        LDA #(opAdd / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.Sub:
                    {
                        LDA #(opSub % 256)
                        STA ZP.STRL
                        LDA #(opSub / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.Mul:
                    {
                        LDA #(opMul % 256)
                        STA ZP.STRL
                        LDA #(opMul / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.Div:
                    {
                        LDA #(opDiv % 256)
                        STA ZP.STRL
                        LDA #(opDiv / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.Mod:
                    {
                        LDA #(opMod % 256)
                        STA ZP.STRL
                        LDA #(opMod / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.LogicalAnd:
                    {
                        LDA #(opAnd % 256)
                        STA ZP.STRL
                        LDA #(opAnd / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.LogicalOr:
                    {
                        LDA #(opOr % 256)
                        STA ZP.STRL
                        LDA #(opOr / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.BitwiseAnd:
                    {
                        LDA #(opBitAnd % 256)
                        STA ZP.STRL
                        LDA #(opBitAnd / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.BitwiseOr:
                    {
                        LDA #(opBitOr % 256)
                        STA ZP.STRL
                        LDA #(opBitOr / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.EQ:
                    {
                        LDA #(opEQ % 256)
                        STA ZP.STRL
                        LDA #(opEQ / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.NE:
                    {
                        LDA #(opNE % 256)
                        STA ZP.STRL
                        LDA #(opNE / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.LT:
                    {
                        LDA #(opLT % 256)
                        STA ZP.STRL
                        LDA #(opLT / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.GT:
                    {
                        LDA #(opGT % 256)
                        STA ZP.STRL
                        LDA #(opGT / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.LE:
                    {
                        LDA #(opLE % 256)
                        STA ZP.STRL
                        LDA #(opLE / 256)
                        STA ZP.STRH
                    }
                    case BinOpType.GE:
                    {
                        LDA #(opGE % 256)
                        STA ZP.STRL
                        LDA #(opGE / 256)
                        STA ZP.STRH
                    }
                    default:
                    {
                        LDA #'?'
                        Print.Char();
                        return;
                    }
                }
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
                
                LDY # iStrFlags
                LDA [ZP.IDX], Y
                CMP # StringType.Hex
                if (Z)
                {
                    LDA #(hexData % 256)
                    STA ZP.STRL
                    LDA #(hexData / 256)
                    STA ZP.STRH
                    Print.String();
                }
                else
                {
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
                Print.Space(); LDA ZP.STRH Print.Hex();LDA ZP.STRL Print.Hex();
            }
            case NodeType.IntLit:
            case NodeType.LongLit:  // Handle both the same way (both are 32-bit)
            {
                // Print the type name
                LDY #iNodeType
                LDA [ZP.IDX], Y
                CMP #NodeType.LongLit
                if (Z)
                {
                    LDA #(nodeLong % 256)
                    STA ZP.STRL
                    LDA #(nodeLong / 256)
                    STA ZP.STRH
                }
                else
                {
                    LDA #(nodeInt % 256)
                    STA ZP.STRL
                    LDA #(nodeInt / 256)
                    STA ZP.STRH
                }
                Print.String();
                
                // Get pointer to 32-bit value
                LDY #iData
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                
                // Check for null pointer
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z)
                {
                    // Null pointer - print "null"
                    LDA #'n'
                    Print.Char();
                    LDA #'u'
                    Print.Char();
                    LDA #'l'
                    Print.Char();
                    LDA #'l'
                    Print.Char();
                }
                else
                {
                    // Load 32-bit value from where IDY points into TOP
                    LDY #0
                    LDA [ZP.IDY], Y
                    STA ZP.TOP0
                    INY
                    LDA [ZP.IDY], Y
                    STA ZP.TOP1
                    INY
                    LDA [ZP.IDY], Y
                    STA ZP.TOP2
                    INY
                    LDA [ZP.IDY], Y
                    STA ZP.TOP3
                    
                    // Print the value
                    Long.Print();
                }
            }
            
            case NodeType.CharLit:
            {
                LDA #(nodeChar % 256)
                STA ZP.STRL
                LDA #(nodeChar / 256)
                STA ZP.STRH
                Print.String();
                
                // Get pointer to character value
                LDY #iData
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                
                // Print the character value as hex
                LDA #'0'
                Print.Char();
                LDA #'x'
                Print.Char();
                LDA [ZP.IDY]
                Print.Hex();
            }
            
            case NodeType.ConstDecl:
            {
                LDA #(nodeConstDecl % 256)
                STA ZP.STRL
                LDA #(nodeConstDecl / 256)
                STA ZP.STRH
                Print.String();
                
                // Print the type
                LDY #iConstType
                LDA [ZP.IDX], Y
                switch (A)
                {
                    case Token.Long:    { LDA # (nodeLong % 256)    STA ZP.STRL LDA # (nodeLong / 256)    STA ZP.STRH }
                    case Token.Int:     { LDA # (nodeInt % 256)     STA ZP.STRL LDA # (nodeInt / 256)     STA ZP.STRH }
                    case Token.Char:    { LDA # (nodeChar % 256)    STA ZP.STRL LDA # (nodeChar / 256)    STA ZP.STRH }
                    case Token.CharPtr: { LDA # (nodeCharPtr % 256) STA ZP.STRL LDA # (nodeCharPtr / 256) STA ZP.STRH }
                    default:            { LDA # (nodeUnknown % 256) STA ZP.STRL LDA # (nodeUnknown / 256) STA ZP.STRH }
                }
                Print.String();
            }
            case NodeType.VarDecl:
            {
                LDA #(nodeVarDecl % 256)
                STA ZP.STRL
                LDA #(nodeVarDecl / 256)
                STA ZP.STRH
                Print.String();
                
                // Print the type
                LDY #iVarType
                LDA [ZP.IDX], Y
                switch (A)
                {
                    case Token.Long:    { LDA # (nodeLong % 256)    STA ZP.STRL LDA # (nodeLong / 256)    STA ZP.STRH }
                    case Token.Int:     { LDA # (nodeInt % 256)     STA ZP.STRL LDA # (nodeInt / 256)     STA ZP.STRH }
                    case Token.Char:    { LDA # (nodeChar % 256)    STA ZP.STRL LDA # (nodeChar / 256)    STA ZP.STRH }
                    case Token.CharPtr: { LDA # (nodeCharPtr % 256) STA ZP.STRL LDA # (nodeCharPtr / 256) STA ZP.STRH }
                    case Token.FilePtr: { LDA # (nodeFilePtr % 256) STA ZP.STRL LDA # (nodeFilePtr / 256) STA ZP.STRH }
                    default:            { LDA # (nodeUnknown % 256) STA ZP.STRL LDA # (nodeUnknown / 256) STA ZP.STRH }
                }
                Print.String();
                
                LDY #iVarScope
                LDA [ZP.IDX], Y
                
                CMP # VarScope.Local
                if (Z)
                {     
                    // Print the BP offset
                    LDA #(nodeBPOffset % 256)
                    STA ZP.STRL
                    LDA #(nodeBPOffset / 256)
                    STA ZP.STRH
                    Print.String();
                    
                    // Get and print the signed offset
                    LDY #iOffset
                    LDA [ZP.IDX], Y
                    
                    // Check if negative (bit 7 set)
                    if (MI)
                    {
                        // It's negative, print minus sign
                        PHA
                        LDA #'-'
                        Print.Char();
                        PLA
                        
                        // Negate to get absolute value
                        EOR #0xFF
                        CLC
                        ADC #1
                    }
                    else
                    {
                        PHA
                        LDA #'+'
                        Print.Char();
                        PLA
                    }
                    LDX # ZP.TOP
                    Shared.LoadByte();
                    Long.Print();
                    
                    LDA #']'
                    Print.Char();
                }
                else
                {
                    // Get and print the address
                    LDY #iOffset
                    LDA [ZP.IDX], Y
                    LDX # ZP.TOP
                    Shared.LoadByte();
                    Long.Print();
                }
            }
            case NodeType.Assign:
            {
                LDA #(nodeAssign % 256)
                STA ZP.STRL
                LDA #(nodeAssign / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.Break:
            {
                LDA #(nodeBreak % 256)
                STA ZP.STRL
                LDA #(nodeBreak / 256)
                STA ZP.STRH
                Print.String();
            }
            case NodeType.Continue:
            {
                LDA #(nodeContinue % 256)
                STA ZP.STRL
                LDA #(nodeContinue / 256)
                STA ZP.STRH
                Print.String();
            }
            
            default:
            {
                Print.Hex(); Print.Space();
                LDA #(nodeUnknown % 256) STA ZP.STRL LDA #(nodeUnknown / 256) STA ZP.STRH
                Print.String();
            }
        } // switch
        
        LDY # iNodeType
        LDA [ZP.IDX], Y
        switch (A)
        {
            case NodeType.For:
            {
            }
            default:
            {
                Print.Space(); LDA ZP.IDXH Print.Hex(); LDA ZP.IDXL Print.Hex();
                Print.NewLine();
            }
        }
        
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
