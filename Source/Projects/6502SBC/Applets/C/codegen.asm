unit CodeGen
{
    uses "AST"
    uses "Library"
    uses "Errors"
    
    friend Library;
    
    // Code generation state
    const byte cgSlots = 0x90;
    
    const byte codeBuffer     = cgSlots+0;   // Pointer to allocated buffer
    const byte codeBufferL    = cgSlots+0;
    const byte codeBufferH    = cgSlots+1;
    const byte codeOffset     = cgSlots+2;   // RELATIVE offset in buffer (0-based)
    const byte codeOffsetL    = cgSlots+2;
    const byte codeOffsetH    = cgSlots+3;
    const byte codeSize       = cgSlots+4;   // Current allocated size
    const byte codeSizeL      = cgSlots+4;   
    const byte codeSizeH      = cgSlots+5;   
    const byte stringBuffer   = cgSlots+6;  // String literals
    const byte stringBufferL  = cgSlots+6;  // String literals
    const byte stringBufferH  = cgSlots+7;  // String literals
    const byte stringOffset   = cgSlots+8;  // RELATIVE offset in string buffer
    const byte stringOffsetL  = cgSlots+8;
    const byte stringOffsetH  = cgSlots+9;
    
    const byte functionLocals = cgSlots+10; // Count of locals in current function
    const byte storeOp        = cgSlots+11;
    
    const byte forwardPatchL  = cgSlots+12;
    const byte forwardPatchH  = cgSlots+13;
    
    const byte elsePatchL     = forwardPatchL;
    const byte elsePatchH     = forwardPatchH;
    
    const byte backwardPatchL  = cgSlots+14;
    const byte backwardPatchH  = cgSlots+15;
    
    const byte endPatchL       = backwardPatchL;
    const byte endPatchH       = backwardPatchH;
    
    
    const byte functionNode    = cgSlots+16;   // Current function being compiled
    const byte functionNodeL   = cgSlots+16;   
    const byte functionNodeH   = cgSlots+17;
    
    const byte runtimeZeroPageSlots = 0x60;
    const byte runtimeBP            = runtimeZeroPageSlots+0; // Base pointer for stack frame
    
    const byte runtimeStack0        = runtimeZeroPageSlots+1;
    const byte runtimeStack0L       = runtimeZeroPageSlots+1;
    const byte runtimeStack0H       = runtimeZeroPageSlots+2;
    
    const byte runtimeStack1        = runtimeZeroPageSlots+3;
    const byte runtimeStack1L       = runtimeZeroPageSlots+3;
    const byte runtimeStack1H       = runtimeZeroPageSlots+4;
    
    const byte runtimeStack2        = runtimeZeroPageSlots+5;
    const byte runtimeStack2L       = runtimeZeroPageSlots+5;
    const byte runtimeStack2H       = runtimeZeroPageSlots+6;
    
    const byte runtimeStack3        = runtimeZeroPageSlots+7;
    const byte runtimeStack3L       = runtimeZeroPageSlots+7;
    const byte runtimeStack3H       = runtimeZeroPageSlots+8;
    
    
    // Success message
    const string msgSaved = "Saved ";
    const string msgBytes = " bytes\n";
    
    const string msgMain = "main";
    
    // 65C02S opcodes
    enum OpCode
    {
        ORA_ZP  = 0x05,
        CLC     = 0x18,
        INC_A   = 0x1A,
        JSR     = 0x20,
        SEC     = 0x38, 
        PHA     = 0x48,
        JMP_ABS = 0x4C,
        PHY     = 0x5A,
        RTS     = 0x60,
        STZ_ZP  = 0x64,
        PLA     = 0x68,
        ADC_IMM = 0x69,
        JMP_IND = 0x6C,
        PLY     = 0x7A, 
        BRA     = 0x80,
        STA_ZP  = 0x85,
        STX_ZP  = 0x86, 
        TXA     = 0x8A,
        STA_IND_Y = 0x91,
        TYA     = 0x98,
        TXS     = 0x9A,
        LDY_IMM = 0xA0,
        LDX_IMM = 0xA2,
        LDA_ZP  = 0xA5, 
        LDX_ZP  = 0xA6,
        TAY     = 0xA8,
        LDA_IMM = 0xA9,
        LDA_IND_Y = 0xB1,
        TSX     = 0xBA,
        CPY_IMM = 0xC0,
        CMP_IMM = 0xC9,
        INY     = 0xC8,
        DEX     = 0xCA,
        BNE     = 0xD0,
        PHX     = 0xDA, 
        INC_ZP  = 0xE6, 
        INX     = 0xE8,
        SBC_IMM = 0xE9, 
        NOP     = 0xEA, 
        BEQ     = 0xF0,
        PLX     = 0xFA,
    }    
    
#ifdef DEBUGSTACK

    debugPrintY() // Input: X = marker character to print first
    {
        PHX
        
        // Generates code to print Y register as hex
        // Preserves A, X, and Y
        
        // Save registers
        LDA # OpCode.PHX  
        EmitByte();            // Save X
        LDA # OpCode.PHY
        EmitByte();            // Save Y
        
        // Print a space for readability
        LDA # OpCode.LDA_IMM
        EmitByte();
        PLA
        EmitByte();
        LDA # OpCode.LDX_IMM
        EmitByte();
        LDA # SysCall.PrintChar
        EmitByte();
        // Emit: JSR dispatch
        Library.EmitDispatchCall();
        
        
        // Move Y to A for Print.Hex
        LDA # OpCode.TYA
        EmitByte();            // Y -> A
        
        // Call Print.Hex (assuming syscall 0x11 for PrintHex)
        LDA # OpCode.LDX_IMM
        EmitByte();
        LDA # SysCall.PrintHex
        EmitByte();
        // Emit: JSR dispatch
        Library.EmitDispatchCall();
        
        // Print a space for readability
        LDA # OpCode.LDA_IMM
        EmitByte();
        LDA # ' '
        EmitByte();
        LDA # OpCode.LDX_IMM
        EmitByte();
        LDA # SysCall.PrintChar
        EmitByte();
        // Emit: JSR dispatch
        Library.EmitDispatchCall();
        
        // Restore registers
        LDA # OpCode.PLY
        EmitByte();            // Restore Y
        LDA # OpCode.PLX
        EmitByte();            // Restore X
    }
#endif    
    
    
    
    // Initialize code generation buffers and state
    // Allocates initial 4KB code buffer
    // Output: C set on success, clear on failure
    Initialize()
    {
        // Start with 4KB
        LDA #0x00
        STA ZP.ACCL
        LDA #0x10  // 4KB = 0x1000
        STA ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        STA codeBufferL
        LDA ZP.IDXH
        STA codeBufferH
        
        STZ codeOffsetL   // Start at offset 0
        STZ codeOffsetH
        
        LDA #0x10
        STA codeSizeH     // 4KB size
        STZ codeSizeL
    }
    
    // Clean up and free allocated code generation buffers
    Dispose()
    {
        // Free code buffer
        LDA codeBufferL
        ORA codeBufferH
        if (NZ)
        {
            LDA codeBufferL
            STA ZP.IDXL
            LDA codeBufferH
            STA ZP.IDXH
            Memory.Free();
            
            STZ codeBufferL
            STZ codeBufferH
        }
    }
    
    // Emit a single byte to the code buffer
    // Input: A = byte to emit
    // Output: C set on success, clear on failure
    // Note: Automatically grows buffer if needed
    EmitByte()  // A = byte to emit
    {
        PHA
        
        TAX
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop
        {
            // Check if we need to grow buffer
            LDA codeOffsetH
            CMP codeSizeH
            if (Z)
            {
                LDA codeOffsetL
                CMP codeSizeL
                if (Z)
                {
                    PHX
                    growBuffer();  // Double the size
                    PLX
                    if (NC) { break; }
                }
            }
            
            // Add base to offset to get absolute address
            CLC
            LDA codeBufferL
            ADC codeOffsetL
            STA ZP.ACCL
            LDA codeBufferH
            ADC codeOffsetH
            STA ZP.ACCH
            
            // Store byte
            TXA
            STA [ZP.ACC]
            
            // Increment offset (relative!)
            INC codeOffsetL
            if (Z){ INC codeOffsetH}
            
            SEC
            break;
        } // single exit
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        
    }
    
    // Double the size of the code buffer when it fills up
    // Internal helper for EmitByte
    // Output: C set on success, clear on failure
    growBuffer()
    {
        PHY
        
        LDA ZP.TEMP
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // new size
        ASL codeSizeL
        ROL codeSizeH
        
        // Allocate new buffer (double size)
        LDA codeSizeL
        STA ZP.ACCL
        LDA codeSizeH
        STA ZP.ACCH
        
        Memory.Allocate();  // New buffer in IDX
        if (NC) 
        { 
            // Failed to allocate
            Errors.OutOfMemory();
            PLY
            return; 
        }  
        
        // Copy old buffer to new
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Setup for copy: IDY = old (source), IDX = new (dest)
        LDA codeBufferL
        STA ZP.IDYL
        LDA codeBufferH
        STA ZP.IDYH
        
        LDA codeOffsetL
        STA ZP.ACCL
        LDA codeOffsetH
        STA ZP.ACCH
        loop
        {
            LDA [ZP.IDY]
            STA [ZP.IDX]
            Shared.IncIDY();
            Shared.IncIDX();
            Shared.DecACC();
            LDA ZP.ACCL
            ORA ZP.ACCH
            if (Z) { break; }
        }
        
        // Free old buffer
        LDA codeBufferL
        STA ZP.IDXL
        LDA codeBufferH
        STA ZP.IDXH
        Memory.Free();
        
        // Update pointer to new buffer        
        PLA
        STA codeBufferH
        PLA
        STA codeBufferL
        SEC
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.TEMP
        
        PLY
    }
    
    // Recursively walk AST and emit all string literals
    // Input: IDX = AST node to start from
    // Output: C set on success, clear on failure
    // Note: Updates iOffset field of StringLit nodes with code offset
    emitStrings()  // Input: IDX = node
    {
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) { SEC return; }  // Null node
        
        // Check if this is a StringLit
        LDY # AST.iNodeType
        LDA [ZP.IDX], Y
        CMP # AST.NodeType.StringLit
        if (Z)
        {
            // Store current offset in the node
            LDY # AST.iOffset
            LDA codeOffsetL
            STA [ZP.IDX], Y
            INY
            LDA codeOffsetH
            STA [ZP.IDX], Y
            
            // Get string pointer
            LDY # AST.iData
            LDA [ZP.IDX], Y
            STA ZP.STRL
            INY
            LDA [ZP.IDX], Y
            STA ZP.STRH
            
            // Emit the string
            LDY #0
            loop
            {
                LDA [ZP.STR], Y
                EmitByte();
                if (NC) { return; }
                if (Z) { break; }  // Including null
                INY
            }
        }
        
        // Save current node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Recurse on first child
        LDY # AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        emitStrings();
        
        // Restore and recurse on sibling
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        if (NC) { return; } 
        
        LDY #AST.iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        emitStrings();
    }
    
    // Compare two null-terminated strings
    // Input: ZP.STR = first string pointer
    //        ZP.IDY = second string pointer
    // Output: C set if strings are equal, clear if different
    CompareStrings()
    {
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            CMP [ZP.IDY], Y
            if (NZ)
            {
                CLC  // Not equal
                return;
            }
            // Check if we hit null terminator
            CMP #0
            if (Z)
            {
                SEC  // Equal (both null)
                return;
            }
            INY
        }
    }
    
    // Find VarDecl node for an identifier
    // Input: ZP.STR = identifier name
    // Output: IDX = VarDecl node, C set if found
    findVariable()
    {
        // Start from current function node
        LDA functionNodeL
        STA ZP.IDXL
        LDA functionNodeH
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
                CodeGen.CompareStrings();  // STR vs IDY
                
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
            if (Z) { CLC return; }  // Not found
            
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
                CodeGen.CompareStrings();  // STR vs IDY
                
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
        
        CLC  // Not found
    }
    
    // Find a function node in the AST by name
    // Input: ZP.STR = function name to find
    // Output: IDX = Function node if found
    //         C set on success, clear if not found
    findFunction()
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
                CodeGen.CompareStrings();
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
    
    // Generate code to load an identifier's value and push to stack
    // Input: IDX = Identifier node
    // Output: C set on success, clear on failure
    //         Generated code pushes value to stack
    generateLoadIdentifier()
    {
        // Get identifier name from node
        LDY #AST.iData
        LDA [ZP.IDX], Y
        STA ZP.STRL
        INY
        LDA [ZP.IDX], Y
        STA ZP.STRH
        
        LDA ZP.IDXH
        PHA
        LDA ZP.IDXL
        PHA
    
        // Find the variable declaration
        findVariable();
        if (NC)
        {
#ifdef DEBUG
LDA #'x' Print.Char(); Print.Space(); Print.String(); Print.Space();
#endif
            PLA
            STA ZP.IDXL
            PLA
            STA ZP.IDXH
            LDA # Error.UndefinedIdentifier
            Errors.ShowIDX();
            return;
        }

        PLA
        PLA
        
        // IDX now points to VarDecl node
        // Get the BP offset (stored during declaration processing)
        LDY #AST.iOffset
        LDA [ZP.IDX], Y
        
        // Generate code to load from BP+offset into ZP.NEXT
#ifdef DEBUGSTACK
        LDX #'f'
#endif
        getNEXT(); if (NC) { return; }
        
        // Generate code to push ZP.NEXT onto stack
#ifdef DEBUGSTACK
        LDX #'m'
#endif
        pushNEXT();
    }
    
    // Add the BIOS entry point address (0x0800) to value in ACC
    // Input: ZP.ACC = relative offset
    // Output: ZP.ACC = absolute address (offset + 0x0800)
    AddEntryPoint()
    {
        CLC
        LDA ZP.ACCL
        ADC # (BIOSInterface.EntryPoint % 256)
        STA ZP.ACCL
        LDA ZP.ACCH
        ADC # (BIOSInterface.EntryPoint / 256)
        STA ZP.ACCH
    }
    
    countFunctionParameters() // Input: AST.astNode = Function node, Output: A = param count
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
    
    
    // Generate code for calling a user-defined function
    generateUserFunctionCall() // Input: IDX = CallExpr node
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        LDA AST.astTempNodeL
        PHA
        LDA AST.astTempNodeH
        PHA
        
        LDA ZP.IDXL
        STA AST.astTempNodeL
        LDA ZP.IDXH
        STA AST.astTempNodeH
        
        loop
        {
            // Get function name from first child (identifier)
            LDY #AST.iChild
            LDA [AST.astTempNode], Y
            STA ZP.IDYL
            INY
            LDA [AST.astTempNode], Y
            STA ZP.IDYH
            
            // Get function name string
            LDY #AST.iData
            LDA [ZP.IDY], Y
            STA ZP.STRL
            INY
            LDA [ZP.IDY], Y
            STA ZP.STRH
            
            LDA ZP.IDYH
            PHA
            LDA ZP.IDYL
            PHA
            
            // Find the function in the AST
            findFunction(); // STR = name -> IDX = Function node, C = found
            if (NC)
            {
#ifdef DEBUG
    Print.NewLine(); LDA #'b' Print.Char(); Print.String();
#endif            
                PLA
                STA ZP.IDYL
                PLA
                STA ZP.IDYH
                LDA # Error.UndefinedIdentifier
                Errors.ShowIDY();
                break;
            }
            PLA
            PLA
            
            // Save function node
            LDA ZP.IDXL
            STA AST.astNodeL
            LDA ZP.IDXH
            STA AST.astNodeH
            
            // Get function's code offset
            LDY #AST.iOffset
            LDA [AST.astNode], Y
            STA ZP.ACCL
            INY
            LDA [AST.astNode], Y
            STA ZP.ACCH
            
            // Count parameters
            countFunctionParameters(); // [AST.astNode] -> A = count
            STA ZP.TEMP
            
            // Get first argument (skip identifier, get its sibling)
            LDY #AST.iChild
            LDA [AST.astTempNode], Y
            STA ZP.IDYL
            INY
            LDA [AST.astTempNode], Y
            STA ZP.IDYH
            
            // Move to first argument (sibling of identifier)
            LDY #AST.iNext
            LDA [ZP.IDY], Y
            STA ZP.IDXL
            INY
            LDA [ZP.IDY], Y
            STA ZP.IDXH
            
            // Evaluate and push each argument
            loop
            {
                LDA ZP.IDXL
                ORA ZP.IDXH
                if (Z) { break; }  // No more arguments
                
                LDA ZP.IDXH
                PHA
                LDA ZP.IDXL
                PHA
                LDA ZP.TEMP
                PHA
                
                // Generate code to evaluate this argument
                
                generateExpression(); // Result pushed on stack
                
                PLA
                STA ZP.TEMP
                PLA
                STA ZP.IDXL
                PLA
                STA ZP.IDXH
                
                if (NC) { break; }
                
                DEC ZP.TEMP
                
                // Move to next argument
                LDY #AST.iNext
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDXH
                STX ZP.IDXL
            }
            if (NC) { break; }
            
            // Check argument count matches (optional but good)
            LDA ZP.TEMP
            if (NZ)
            {
                LDA # Error.TooFewArguments
                Errors.Show();
                break;
            }
            
            // Generate JSR to function
            LDA #OpCode.JSR
            EmitByte(); if (NC) { break; }
            
            // Get function offset again and convert to absolute
            LDY #AST.iOffset
            LDA [AST.astNode], Y
            STA ZP.ACCL
            INY
            LDA [AST.astNode], Y
            STA ZP.ACCH
            
            AddEntryPoint(); // Convert offset to absolute address
            
            LDA ZP.ACCL
            EmitByte(); if (NC) { break; }
            LDA ZP.ACCH
            EmitByte(); if (NC) { break; }
            
            // Clean up arguments from stack
            countFunctionParameters(); // [AST.astNode] -> A = count
            STA ZP.TEMP
            if (NZ)
            {
                loop
                {
                    LDA #OpCode.PLA
                    EmitByte(); if (NC) { break; }
                    
                    DEC ZP.TEMP
                    if (Z) { break; }
                }
                if (NC) { break; }
            }
            
            // Result (if any) is now at top of stack
            SEC
            break;
        } // single exit
        
        PLA
        STA AST.astTempNodeH
        PLA
        STA AST.astTempNodeL
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    }
    
       
    // Generate code for a function call expression
    // Input: IDX = CallExpr node
    // Output: C set on success, clear on failure
    // Note: Dispatches to system or user function generation
    generateCallExpr()  // Input: IDX = CallExpr node
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        
        loop
        {
            // ALWAYS reserve return slot for ANY function call
            LDA # OpCode.PHA
            EmitByte(); if (NC) { break; }
            
            // First child is function identifier, second child (sibling) is first argument
            LDY #AST.iChild
            LDA [ZP.IDX], Y
            STA AST.astNodeL
            INY
            LDA [ZP.IDX], Y
            STA AST.astNodeH
            
            // Get function name string
            LDY #AST.iData
            LDA [AST.astNode], Y
            STA ZP.STRL
            INY
            LDA [AST.astNode], Y
            STA ZP.STRH
            
            // Check if it's a system function
            IsSystemFunction();  // -> A = syscall#, C = is system
            if (C)
            {
                switch (A)
                {
                    case SysCall.PrintString:
                    {
                        Library.PrintfCall();
                        if (NC) { break; }
                    }
                    case SysCall.TimeMillis:
                    {
                        Library.MillisCall();
                        if (NC) { break; }
                    }
                    case SysCall.TimeSeconds:
                    {
                        Library.SecondsCall();
                        if (NC) { break; }
                    }
                    case SysCall.PrintChar:
                    {
                        Library.PutcharCall();
                        if (NC) { break; }
                    }
                    default:
                    {
#ifdef DEBUG
Print.Hex(); LDA #'s' Print.Char();LDA #'f' Print.Char();
#endif     
                        LDA # Error.NotImplemented
                        Errors.ShowIDX();
                        break;
                    }
                }
            }
            else
            {
                IsFileFunction();
                if (C)
                {
                    switch (A)
                    {
                        case FileFunction.FOpen:
                        case FileFunction.FClose:
                        case FileFunction.FGetC:
                        case FileFunction.FPutC:
                        case FileFunction.FGetS:
                        case FileFunction.FPutS:
                        case FileFunction.FEof:
                        case FileFunction.FRead:
                        case FileFunction.FWrite:
                        {
#ifdef DEBUG
Print.Hex(); LDA #'f' Print.Char();LDA #'f' Print.Char();
#endif     
                            LDA # Error.NotImplemented
                            Errors.ShowIDX();
                            break;
                        }
                        default:
                        {
#ifdef DEBUG
Print.Hex(); LDA #'f' Print.Char();LDA #'f' Print.Char();
#endif     
                            LDA # Error.NotImplemented
                            Errors.ShowIDX();
                            break;
                        }
                    }
                }
                else
                {
                    generateUserFunctionCall();  // Future: JSR to user function
                    if (NC) { break; }
                }
            }
            SEC
            break;
        } // single exit
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL 
    }
    
    // Generate code for a variable declaration
    // Input: IDX = VarDecl node
    // Output: C set on success, clear on failure
    // Note: Allocates stack space and records BP offset in node
    generateVarDecl()
    {
        loop
        {
            // Allocate stack space for the variable - just push a dummy value
            LDA #OpCode.PHA
            EmitByte(); if (NC) { return; }
        
            // Store the BP offset in the VarDecl node for later reference
            // Locals are at negative offsets: first local at BP+0, second at BP-1, etc.
            LDY # AST.iOffset  // Reuse the offset field for BP offset
            LDA functionLocals
            if (NZ)
            {
                EOR #0xFF      // Negate to get -1, -2, etc.
            }
            STA [ZP.IDX], Y   // Store as BP-relative offset (0, -1, -2, etc.)
            
            // Update the local count
            INC functionLocals
            
            LDY # AST.iInitializer
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)  // Has initializer
            {
                // TODO: Generate code for initializer expression
                // For now, just error
#ifdef DEBUG
Print.Hex(); LDA #'v' Print.Char();
#endif
                LDA # Error.NotImplemented
                Errors.ShowIDY();
                break;
            }
                    
            SEC
            break;
        }
    }
    
    pushC()
    {
        // SP -> X -> Y
        LDA #OpCode.TSX  
        EmitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        EmitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        EmitByte(); if (NC) { return; }
        
        // Convert carry flag to 0 or 1
        // LDA #0
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // ADC #0 - adds carry flag to 0, giving 0 or 1
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // Store result (0 or 1) to stack via pointer
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // Store 0 to stack via pointer
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1
        EmitByte(); if (NC) { return; }
        
        // Store 0 to stack via pointer
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2
        EmitByte(); if (NC) { return; }
        
        // Store 0 to stack via pointer
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3
        EmitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to push 32-bit value from ZP.NEXT onto runtime stack
    pushNEXT() // X -> marker
    {
#ifdef DEBUGSTACK        
        PHX
#endif
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        EmitByte(); 
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        LDA #OpCode.TXA
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        LDA #OpCode.TAY
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        
#ifdef DEBUGSTACK  
PLX      
CodeGen.debugPrintY();         
#endif
        
        // Store NEXT0 to stack via pointer
        // LDA ZP.NEXT0
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0
        EmitByte(); if (NC) { return; }
        
        // Store NEXT1 to stack via pointer
        // LDA ZP.NEXT1
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1
        EmitByte(); if (NC) { return; }
        
        // Store NEXT2 to stack via pointer
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2
        EmitByte(); if (NC) { return; }
        
        // Store NEXT3 to stack via pointer
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3
        EmitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to push 32-bit value from ZP.TOP onto runtime stack
    pushTOP() // X -> marker
    {
#ifdef DEBUGSTACK
        PHX
#endif
        // SP -> X -> Y
        LDA #OpCode.TSX  
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        LDA #OpCode.TXA
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        LDA #OpCode.TAY
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        
#ifdef DEBUGSTACK 
PLX      
CodeGen.debugPrintY();        
#endif
        
        // Store TOP0 to stack via pointer
        // LDA ZP.TOP0
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP0
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0
        EmitByte(); if (NC) { return; }
        
        // Store TOP1 to stack via pointer
        // LDA ZP.TOP1
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1
        EmitByte(); if (NC) { return; }
        
        // Store TOP2 to stack via pointer
        // LDA ZP.TOP2
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2
        EmitByte(); if (NC) { return; }
        
        // Store TOP3 to stack via pointer
        // LDA ZP.TOP3
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3
        EmitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to pop 32-bit value from stack into ZP.NEXT
    popNEXT() // X -> marker
    {
#ifdef DEBUGSTACK
        PHX
#endif
        
        LDA #OpCode.PLA
        EmitByte(); 
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX 
#endif
            return; 
        }
        
        // SP points one past to the slot we are interested in
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX 
#endif
            return; 
        }
        LDA #OpCode.TXA
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX 
#endif
            return; 
        }
        LDA #OpCode.TAY
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX 
#endif
            return; 
        }
        
#ifdef DEBUGSTACK
PLX      
CodeGen.debugPrintY();         
#endif
        
        // Load NEXT0 from stack via pointer
        // LDA [runtimeStack0],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0
        EmitByte(); if (NC) { return; }
        // STA ZP.NEXT0
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0 
        EmitByte(); if (NC) { return; }
        
        // Load NEXT1 from stack via pointer
        // LDA [runtimeStack1],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1
        EmitByte(); if (NC) { return; }
        // STA ZP.NEXT1
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
                
        // Load NEXT2 from stack via pointer
        // LDA [runtimeStack2],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2
        EmitByte(); if (NC) { return; }
        // STA ZP.NEXT2
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        // Load NEXT3 from stack via pointer
        // LDA [runtimeStack3],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3
        EmitByte(); if (NC) { return; }
        // STA ZP.NEXT3
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    
    // Generate code to pop 32-bit value from stack into ZP.TOP
    popTOP() // X -> marker
    {
#ifdef DEBUGSTACK
        PHX
#endif
        LDA #OpCode.PLA
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        
        
        // SP points one past to the slot we are interested in
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        
        LDA #OpCode.TXA
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        
        LDA #OpCode.TAY
        EmitByte();
        if (NC) 
        {
#ifdef DEBUGSTACK
            PLX
#endif
            return;
        }
        
#ifdef DEBUGSTACK
PLX      
CodeGen.debugPrintY();        
#endif
        
        // Load TOP0 from stack via pointer
        // LDA [runtimeStack0],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP0
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP0 
        EmitByte(); if (NC) { return; }
        
        // Load TOP1 from stack via pointer
        // LDA [runtimeStack1],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP1
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        EmitByte(); if (NC) { return; }
                
        // Load TOP2 from stack via pointer
        // LDA [runtimeStack2],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP2
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        EmitByte(); if (NC) { return; }
        
        // Load TOP3 from stack via pointer
        // LDA [runtimeStack3],Y
        LDA #OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP3
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    
    // Generate code to calculate effective Y offset from BP
    // Input: A = logical offset (signed)
    // Output: Generated code leaves effective offset in Y register
    //    
    //    Higher addresses (0x01FF)
    //    ...
    //    [Parameters]        ; BP+6, BP+7, etc (in caller's frame)
    //    [Return Address Hi] ; BP+2
    //    [Return Address Lo] ; BP+1
    //    [Old BP]            ; BP+0 <- BP points here
    //    [Local var 0-3]     ; BP-4 to BP-1 (first long)
    //    [Local var 4-7]     ; BP-8 to BP-5 (second long)
    //    ...
    //    Lower addresses (grows down)
    calculateBPOffset()
    {
        STA ZP.TEMP  // Save logical offset
        
        // Load BP into A
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeBP
        EmitByte(); if (NC) { return; }
        
        // Add the offset
        LDA #OpCode.CLC
        EmitByte(); if (NC) { return; }
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        
        // Calculate and emit the adjusted offset value
        LDA ZP.TEMP
        // Now A contains either the original negative offset OR the adjusted positive offset
        EmitByte(); if (NC) { return; }
        
        // Transfer result to Y
        LDA #OpCode.TAY
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    
    // Generate code to store ZP.NEXT at BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    putNEXT()  // X -> marker
    {
#ifdef DEBUGSTACK        
        PHX
#endif        
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset();
            if (NC) 
            {
#ifdef DEBUGSTACK
                PLX
#endif
                break;
            }
#ifdef DEBUGSTACK        
PLX            
debugPrintY(); 
#endif           
            
            // Store NEXT0 through pointer
            LDA #OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack0
            EmitByte(); if (NC) { break; }
            
            // Store NEXT1 through pointer
            LDA #OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y  // 0x91
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack1
            EmitByte(); if (NC) { break; }
            
            // Store NEXT2 through pointer
            LDA #OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack2
            EmitByte(); if (NC) { break; }
            
            // Store NEXT3 through pointer
            LDA #OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack3
            EmitByte(); if (NC) { break; }
            
            SEC
            break;
        } // single exit
    }
    
    // Generate code to load ZP.NEXT from BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    getNEXT() // X->marker
    {
#ifdef DEBUGSTACK
        PHX
#endif
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset();
            if (NC) 
            {
#ifdef DEBUGSTACK
                PLX
#endif
                break;
            }
#ifdef DEBUGSTACK            
PLX            
debugPrintY(); 
#endif        
                       
            // Load NEXT0 through pointer
            LDA #OpCode.LDA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack0
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { break; }
            
            // Load NEXT1 through pointer
            LDA #OpCode.LDA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack1
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { break; }
            
            // Load NEXT2 through pointer
            LDA #OpCode.LDA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack2
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { break; }
            
            // Load NEXT3 through pointer
            LDA #OpCode.LDA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA #runtimeStack3
            EmitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { break; }
            
            SEC
            break;
        } // single exit
    }


    // Generate code for an integer literal
    // Input: IDX = IntLit or LongLit node
    // Output: Code emitted to load value into ZP.NEXT0-3 at runtime
    //         C set on success, clear on failure
    generateIntLiteral()  // Input: IDX = literal node
    {
        // Get pointer to 32-bit value in heap
        LDY #AST.iData
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        // Read the 4 bytes from heap into temp storage
        LDY #0
        LDA [ZP.IDY], Y
        STA ZP.NEXT0
        INY
        LDA [ZP.IDY], Y
        STA ZP.NEXT1
        INY
        LDA [ZP.IDY], Y
        STA ZP.NEXT2
        INY
        LDA [ZP.IDY], Y
        STA ZP.NEXT3
        
        // Now emit code to load these values at runtime
        
        // Emit code for NEXT0
        LDA ZP.NEXT0
        if (Z)  // Optimize: use STZ for zero
        {
            LDA #OpCode.STZ_ZP  // 0x64
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM  // 0xA9
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT0
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP  // 0x85
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { return; }
        }
        
        // Emit code for NEXT1
        LDA ZP.NEXT1
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT1
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { return; }
        }
        
        // Emit code for NEXT2
        LDA ZP.NEXT2
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT2
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { return; }
        }
        
        // Emit code for NEXT3
        LDA ZP.NEXT3
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT3
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { return; }
        }
#ifdef DEBUGSTACK        
        LDX #'l'
#endif
        pushNEXT();
        SEC
    }  
    
    // Generate code to increment 32-bit value in NEXT0-3
    generateIncNEXT()
    {
        // CLC (clear carry)
        LDA #OpCode.CLC
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT0
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        // ADC #1
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #1
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT0
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT1
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        // ADC #0 (adds carry if any)
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT1
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        // ADC #0
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT2
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        // ADC #0
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT3
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to decrement 32-bit value in NEXT0-3
    generateDecNEXT()
    {
        // SEC (set carry for subtraction)
        LDA # OpCode.SEC
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT0
        LDA # OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        // SBC #1
        LDA # OpCode.SBC_IMM
        EmitByte(); if (NC) { return; }
        LDA #1
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT0
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT1
        LDA # OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        // SBC #0 (subtracts borrow if any)
        LDA #OpCode.SBC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT1
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        // SBC #0
        LDA #OpCode.SBC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT2
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        // SBC #0
        LDA #OpCode.SBC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA ZP.NEXT3
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    generatePostfixOp()
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        LDA storeOp
        PHA
        
        // Save the PostfixOp node pointer
        LDA ZP.IDXH
        STA AST.astNodeH
        LDA ZP.IDXL
        STA AST.astNodeL
        
        loop
        {
            // Get operator type (++ or --)
            LDY #AST.iPostfixOp
            LDA [AST.astNode], Y
            STA storeOp  // Save operator type
            
            // Get the identifier (child)
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            // Get identifier's name
            LDY #AST.iData
            LDA [ZP.IDX], Y
            STA ZP.STRL
            INY
            LDA [ZP.IDX], Y
            STA ZP.STRH
            
            // Find the VarDecl for this identifier
            findVariable();  // -> IDX
            if (NC)
            {
#ifdef DEBUG
LDA #'y' Print.Char(); Print.Space(); Print.String(); Print.Space();
#endif
                LDA AST.astNodeH
                STA ZP.IDXH
                LDA AST.astNodeL
                STA ZP.IDXL
                LDA # Error.UndefinedIdentifier
                Errors.ShowIDX();
                break;
            }
            
            // Get the BP offset from VarDecl
            LDY #AST.iOffset
            LDA [ZP.IDX], Y  // This is the signed offset
            
            // Load current value from BP+offset into NEXT
#ifdef DEBUGSTACK
            LDX #'g'
#endif
            getNEXT(); if (NC) { break; }
            
            // Push the ORIGINAL value (this is what postfix returns)
#ifdef DEBUGSTACK
            LDX #'k'
#endif
            pushNEXT(); if (NC) { break; }
            
            // Save offset for store later
            LDY #AST.iOffset
            LDA [ZP.IDX], Y
            PHA  // We need to save offset across the syscall
            
            // Perform increment or decrement based on operator
            LDA storeOp  // Get operator type
            switch (A)
            {
                case PostfixOpType.Increment:
                {
                    generateIncNEXT();
                    if (NC) { PLA break; }
                }
                case PostfixOpType.Decrement:
                {
                    generateDecNEXT();
                    if (NC) { PLA break; }
                }
            }
            
            // Result is in NEXT, store it back
            PLA  // Get offset
#ifdef DEBUGSTACK
            LDX #'e'
#endif
            putNEXT();  // A = offset
            
            SEC
            break;
        } // single exit
        
        PLA
        STA storeOp
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    } 
    
    
    generateBinOp()
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        LDA storeOp
        PHA
        
        // Save the BinOp node pointer
        LDA ZP.IDXH
        STA AST.astNodeH
        LDA ZP.IDXL
        STA AST.astNodeL
        
        loop
        {
            // Get operator type
            LDY #AST.iBinOp
            LDA [AST.astNode], Y
            STA storeOp  // Save operator
            
            // Generate left operand
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            generateExpression(); if (NC) { break; }
            
            // Left operand is now on stack
            
            // Generate right operand
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            generateExpression(); if (NC) { break; }
            
            // Right operand is now on stack
            
            // Pop both operands
#ifdef DEBUGSTACK
            LDX #'u'
#endif
            CodeGen.popTOP();    // Right operand -> TOP
#ifdef DEBUGSTACK
            LDX #'t'
#endif
            CodeGen.popNEXT();   // Left operand -> NEXT
            
            // Perform operation based on saved operator
            LDA storeOp  // Get operator type
            switch (A)
            {
                case BinOpType.Add:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA #BIOSInterface.SysCall.LongAdd
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.Sub:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongSub
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.Mul:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongMul
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.Div:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongDiv
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.Mod:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongMod
                    EmitByte(); if (NC) { break; }
                }
                
                case BinOpType.EQ:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA #BIOSInterface.SysCall.LongEQ
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.NE:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongNE
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.LT:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA #BIOSInterface.SysCall.LongLT
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.GT:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongGT
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.LE:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA #BIOSInterface.SysCall.LongLE
                    EmitByte(); if (NC) { break; }
                }
                case BinOpType.GE:
                {
                    LDA #OpCode.LDX_IMM
                    EmitByte(); if (NC) { break; }
                    LDA # BIOSInterface.SysCall.LongGE
                    EmitByte(); if (NC) { break; }
                }
            } // switch
            
            // Emit: JSR dispatch
            Library.EmitDispatchCall(); if (NC) { break; }
            
            LDA storeOp
            switch (A)
            {
                case BinOpType.Add:
                case BinOpType.Sub:
                case BinOpType.Mul:
                case BinOpType.Div:
                case BinOpType.Mod:
                {
                    // Result is in NEXT, push it
#ifdef DEBUGSTACK
                    LDX #'j'
#endif
                    CodeGen.pushNEXT();
                }
                default:
                {
                    // result is C or NC
                    CodeGen.pushC();
                }
            }
            SEC
            break;
        } // single exit
        
        PLA
        STA storeOp
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    }
    
    // Generate code for an expression
    // Input: IDX = expression node
    // Output: Result in ZP.TOP0-3 or on stack
    //         C set on success, clear on failure
    generateExpression()
    {
        LDY # AST.iNodeType
        LDA [ZP.IDX], Y
        
        switch (A)
        {
            case NodeType.CharLit:
            case NodeType.IntLit:
            case NodeType.LongLit:
            {
                generateIntLiteral();
                if (NC) { return; }
            } 
            case NodeType.Identifier:
            {
                generateLoadIdentifier();
                if (NC) { return; }
            }
            case NodeType.CallExpr:
            {
                generateCallExpr();
                if (NC) { return; }
            }
            case NodeType.BinOp:
            {
                generateBinOp();
                if (NC) { return; }
            }            
            case NodeType.Assign:
            {
                generateAssignment();
                if (NC) { return; }
            }
            case NodeType.PostfixOp:
            {
                generatePostfixOp();
                if (NC) { return; }
            }
            
            default:
            {
#ifdef DEBUG
Print.Hex(); LDA #'e' Print.Char();
#endif
                LDA #Error.NotImplemented
                Errors.ShowIDX();
                if (NC) { return; }
            }
        }
        SEC
    }
    
    // Generate code for an assignment expression
    // Input: IDX = Assign node
    // Output: C set on success, clear on failure
    // Note: Currently incomplete - needs variable lookup
    generateAssignment()
    {
        // Save Assign node
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        
        LDA ZP.IDXL
        STA AST.astNodeL
        LDA ZP.IDXH
        STA AST.astNodeH
        
        loop
        {
            // First get to first child (identifier) and push it
            LDY #AST.iChild
            LDA [ZP.IDX], Y
            PHA
            TAX
            INY
            LDA [ZP.IDX], Y
            PHA
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Now get its sibling (second child -> RHS expression)
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Generate code for RHS expression (puts value in ZP.TOP)
            generateExpression();
            if (C)
            {
#ifdef DEBUGSTACK            
                LDX #'s'
#endif
                popNEXT();
            }
            
            // restore the identifer node
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            if (NC) { break; }
            
            // Get identifier's name
            LDY #AST.iData
            LDA [ZP.IDX], Y
            STA ZP.STRL
            INY
            LDA [ZP.IDX], Y
            STA ZP.STRH
            
            // Find the VarDecl for this identifier
            findVariable();  // -> IDX
            if (NC)
            {
#ifdef DEBUG
LDA #'z' Print.Char(); Print.Space(); Print.String(); Print.Space();
#endif
                LDA AST.astNodeH
                STA ZP.IDXH
                LDA AST.astNodeL
                STA ZP.IDXL
                LDA # Error.UndefinedIdentifier
                Errors.ShowIDX();
                break; // Variable not found!
            }
            
            // Get the BP offset from VarDecl
            LDY #AST.iOffset
            LDA [ZP.IDX], Y  // This is the signed offset
            
            // Store NEXT at BP+offset
#ifdef DEBUGSTACK
            LDX #'d'
#endif
            putNEXT();  // A = offset
            
            // Assignment expressions return the assigned value
#ifdef DEBUGSTACK
            LDX #'i'
#endif
            pushNEXT();
                
            SEC
            break;
        } // single exit
        
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    }
    
    // Generate code for a for loop
    // Input: IDX = For node
    // Output: C set on success, clear on failure
    generateFor()
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        
        LDA backwardPatchL
        PHA
        LDA backwardPatchH
        PHA
        LDA forwardPatchL
        PHA
        LDA forwardPatchH
        PHA
        
        // Save the For node pointer
        LDA ZP.IDXH
        STA AST.astNodeH
        LDA ZP.IDXL
        STA AST.astNodeL
        
        loop
        {
            // Generate init expression if present
            LDY #AST.iForInit
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (NZ)
            {
                generateExpression(); if (NC) { break; }
                // Init result is not used, pop it
                LDA #OpCode.PLA
                EmitByte(); if (NC) { break; }
            }
            
            // Record loop start position (codeBuffer + codeOffset)
            LDA codeOffsetL
            STA ZP.ACCL
            LDA codeOffsetH
            STA ZP.ACCH
            AddEntryPoint(); // Convert to runtime address -> ACC
            LDA ZP.ACCL
            STA backwardPatchL
            LDA ZP.ACCH
            STA backwardPatchH
            
            // Generate exit condition if present
            LDY #AST.iForExit
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (NZ)
            {
                generateExpression(); if (NC) { break; }
                
                // Pop result to check it
#ifdef DEBUGSTACK
                LDX #'r'
#endif
                popNEXT(); if (NC) { break; }
                
                // Test if NEXT is zero (false)
                LDA #OpCode.LDA_ZP
                EmitByte(); if (NC) { break; }
                LDA #ZP.NEXT0
                EmitByte(); if (NC) { break; }
                
                LDA # OpCode.ORA_ZP
                EmitByte(); if (NC) { break; }
                LDA #ZP.NEXT1
                EmitByte(); if (NC) { break; }
                
                LDA # OpCode.ORA_ZP
                EmitByte(); if (NC) { break; }
                LDA #ZP.NEXT2
                EmitByte(); if (NC) { break; }
                
                LDA # OpCode.ORA_ZP
                EmitByte(); if (NC) { break; }
                LDA #ZP.NEXT3
                EmitByte(); if (NC) { break; }
                
                // BNE skip_exit (if true, continue loop)
                LDA # OpCode.BNE
                EmitByte(); if (NC) { break; }
                LDA #3  // Skip over JMP instruction (3 bytes)
                EmitByte(); if (NC) { break; }
                
                // JMP to exit (will patch address later)
                LDA #OpCode.JMP_ABS
                EmitByte(); if (NC) { break; }
                
                // Save current offset for patching
                LDA codeOffsetL
                STA forwardPatchL
                LDA codeOffsetH
                STA forwardPatchH
                
                // Emit placeholder address
                LDA #0
                EmitByte(); if (NC) { break; }
                EmitByte(); if (NC) { break; }
            }
            else
            {
                // No exit condition - mark as no patch needed
                STZ forwardPatchL
                STZ forwardPatchH
            }
            
            // Generate body (child of For node)
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            generateBlock(); if (NC) { break; }
            
            // Generate next expression if present
            LDY #AST.iForNext
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (NZ)
            {
                generateExpression(); if (NC) { break; }
                // Next result is not used, pop it
                LDA #OpCode.PLA
                EmitByte(); if (NC) { break; }
            }
            
            // JMP back to loop start
            LDA #OpCode.JMP_ABS
            EmitByte(); if (NC) { break; }
            
            // Emit the backward jump address
            LDA backwardPatchL
            EmitByte(); if (NC) { break; }
            LDA backwardPatchH
            EmitByte(); if (NC) { break; }
            
            // Patch forward jump if needed
            LDA forwardPatchL
            ORA forwardPatchH
            if (NZ)
            {
                // Calculate current absolute position
                LDA codeOffsetL
                STA ZP.ACCL
                LDA codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + forwardPatch offset)
                LDA codeBufferL
                CLC
                ADC forwardPatchL
                STA ZP.IDXL
                LDA codeBufferH
                ADC forwardPatchH
                STA ZP.IDXH
                
                // Write exit address at patch location
                LDY #0
                LDA ZP.ACCL
                STA [ZP.IDX], Y
                INY
                LDA ZP.ACCH
                STA [ZP.IDX], Y
            }
            
            SEC
            break;
        } // single exit
        
        PLA
        STA forwardPatchH
        PLA
        STA forwardPatchL
        PLA
        STA backwardPatchH
        PLA
        STA backwardPatchL
        
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    }
    
    
            
    
    
    
    
    
    // Generate code for a statement
    // Input: IDX = statement node
    // Output: C set on success, clear on failure
    // Note: Dispatches to appropriate statement generator
    generateStatement()  // Input: IDX = statement node
    {
        // Check statement type
        LDY #AST.iNodeType
        LDA [ZP.IDX], Y
        
        switch (A)
        {
            case NodeType.CompoundStmt:
            {
                generateBlock();
                if (NC) { return; }
            }
            case NodeType.VarDecl:
            {
                generateVarDecl();
                if (NC) { return; }
            }
            case NodeType.For:
            {
                generateFor();
                if (NC) { return; }
            }
            case NodeType.Return:
            {
                generateReturn();
                if (NC) { return; }
            }
            case NodeType.If:
            {
                generateIf();
                if (NC) { return; }
            }
            case NodeType.ExprStmt:
            {
                // Get the expression (child)
                LDY # AST.iChild
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDXH
                STX ZP.IDXL
                
                // Generate the expression (any type)
                generateExpression();
                if (NC) { return; }
                
                // ALL expression statements discard the value
                LDA # OpCode.PLA
                EmitByte();
            }
            default:
            {
                // Future: case AST.NodeType.If, For, Return, etc.
#ifdef DEBUG                
Print.Hex(); LDA #'s' Print.Char();
#endif                
                LDA # Error.NotImplemented
                Errors.ShowIDX();
                return;
            }
        }
        SEC
    }
    
    // Generate code for a compound statement (block)
    // Input: IDX = CompoundStmt node
    // Output: C set on success, clear on failure
    // Note: Processes all child statements sequentially
    generateBlock()  // Input: IDX = CompoundStmt
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        
        LDA ZP.IDXL
        STA AST.astNodeL
        LDA ZP.IDXH
        STA AST.astNodeH
        
        
        // Get first child (first statement)
        LDY #AST.iChild
        LDA [AST.astNode], Y
        TAX
        INY
        LDA [AST.astNode], Y
        STA AST.astNodeH
        STX AST.astNodeL
                
        // Process all statements (they're siblings)
        loop
        {
            LDA AST.astNodeH
            ORA AST.astNodeL
            if (Z) { SEC break; }  // No more statements
            
            LDA AST.astNodeL
            STA ZP.IDXL
            LDA AST.astNodeH
            STA ZP.IDXH
            generateStatement();  // Uses IDX
            if (NC) { break; }
            
            // Move to next statement
            LDY #AST.iNext
            LDA [AST.astNode], Y
            TAX
            INY
            LDA [AST.astNode], Y
            STA AST.astNodeH
            STX AST.astNodeL
        }
        
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    }
    
    // Generate code for if statement
    // Input: IDX = If node
    // Output: C set on success, clear on failure
    generateIf()  // Input: IDX = If node
    {
        // Save AST.astNode for recursion
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        
        // Save current node in AST.astNode
        LDA ZP.IDXL
        STA AST.astNodeL
        LDA ZP.IDXH
        STA AST.astNodeH
        
        // Save patch addresses for recursion
        LDA elsePatchL
        PHA
        LDA elsePatchH
        PHA
        LDA endPatchL
        PHA
        LDA endPatchH
        PHA
        
        loop
        {
            // Get condition (first child)
            LDY # AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            

            
            // Generate condition expression
            generateExpression(); if (NC) { break; }
            
            // Pop result to check it
#ifdef DEBUGSTACK
LDX #'v'
#endif
            popNEXT(); if (NC) { break; }

            // Test if NEXT is zero (false)
            LDA #OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { break; }
            
            LDA #OpCode.ORA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { break; }
            
            LDA #OpCode.ORA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { break; }
            
            LDA #OpCode.ORA_ZP
            EmitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { break; }
            
            // BNE to_then (if true, execute then clause)
            LDA #OpCode.BNE
            EmitByte(); if (NC) { break; }
            LDA #3  // Skip over JMP instruction (3 bytes)
            EmitByte(); if (NC) { break; }
            
            // JMP to else/end (will patch address later)
            LDA #OpCode.JMP_ABS
            EmitByte(); if (NC) { break; }
            
            // Save patch location for jump to else/end
            LDA codeOffsetL
            STA elsePatchL
            LDA codeOffsetH
            STA elsePatchH
            
            // Emit placeholder address
            LDA #0
            EmitByte(); if (NC) { break; }
            EmitByte(); if (NC) { break; }
            
            // to_then: Generate then statement
            // Get then statement (next sibling of condition)
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            generateStatement(); if (NC) { break; }
            
            // Check if there's an else clause
            // Get else statement (next sibling of then)
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            LDY #AST.iNext
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
                // There's an else clause - need to jump over it
                // JMP to end (skip else)
                LDA #OpCode.JMP_ABS
                EmitByte(); if (NC) { break; }
                
                // Save patch location for jump to end
                LDA codeOffsetL
                STA endPatchL
                LDA codeOffsetH
                STA endPatchH
                
                // Emit placeholder address
                LDA #0
                EmitByte(); if (NC) { break; }
                EmitByte(); if (NC) { break; }
                
                // Patch the elsePatch to jump here (start of else)
                // Calculate current absolute position
                LDA codeOffsetL
                STA ZP.ACCL
                LDA codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + elsePatch offset)
                LDA codeBufferL
                CLC
                ADC elsePatchL
                STA ZP.IDXL
                LDA codeBufferH
                ADC elsePatchH
                STA ZP.IDXH
                
                // Write else address at patch location
                LDY #0
                LDA ZP.ACCL
                STA [ZP.IDX], Y
                INY
                LDA ZP.ACCH
                STA [ZP.IDX], Y
                
                // Generate else statement
                // Restore the else node pointer
                LDY #AST.iChild
                LDA [AST.astNode], Y
                STA ZP.IDXL
                INY
                LDA [AST.astNode], Y
                STA ZP.IDXH
                
                LDY #AST.iNext
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDXH
                STX ZP.IDXL
                
                LDY #AST.iNext
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDXH
                STX ZP.IDXL
                
                generateStatement(); if (NC) { break; }
                
                // Patch the endPatch to jump here (after else)
                // Calculate current absolute position
                LDA codeOffsetL
                STA ZP.ACCL
                LDA codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + endPatch offset)
                LDA codeBufferL
                CLC
                ADC endPatchL
                STA ZP.IDXL
                LDA codeBufferH
                ADC endPatchH
                STA ZP.IDXH
                
                // Write end address at patch location
                LDY #0
                LDA ZP.ACCL
                STA [ZP.IDX], Y
                INY
                LDA ZP.ACCH
                STA [ZP.IDX], Y
            }
            else
            {
                // No else clause - patch elsePatch to jump here
                // Calculate current absolute position
                LDA codeOffsetL
                STA ZP.ACCL
                LDA codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + elsePatch offset)
                LDA codeBufferL
                CLC
                ADC elsePatchL
                STA ZP.IDXL
                LDA codeBufferH
                ADC elsePatchH
                STA ZP.IDXH
                
                // Write end address at patch location
                LDY #0
                LDA ZP.ACCL
                STA [ZP.IDX], Y
                INY
                LDA ZP.ACCH
                STA [ZP.IDX], Y
            }
            
            SEC
            break;
        } // single exit
        
        // Restore patch addresses
        PLA
        STA endPatchH
        PLA
        STA endPatchL
        PLA
        STA elsePatchH
        PLA
        STA elsePatchL
        
        // Restore AST.astNode
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
    }
    
    
    
    
    generateReturn() // Input: IDX = Return node
    {
        loop
        {
            // Check if Return node has an expression (child)
            LDY #AST.iChild
            LDA [ZP.IDX], Y
            STA ZP.ACCL
            INY
            LDA [ZP.IDX], Y
            STA ZP.ACCH
            
            // Get function's return type
            LDY #AST.iReturnType
            LDA [functionNode], Y
            
            // Check for type mismatch
            CMP #Token.Void
            if (Z)
            {
                // Function returns void
                LDA ZP.ACCL
                ORA ZP.ACCH
                if (NZ)  // Has return expression but shouldn't
                {
                    LDA # Error.VoidFunction
                    Errors.ShowIDX();
                    break;
                }
            }
            else
            {
                // Function returns non-void
                LDA ZP.ACCL
                ORA ZP.ACCH
                if (Z)  // No return expression but needs one
                {
                    LDA # Error.ExpressionExpected
                    Errors.ShowIDX();
                    break;
                }
                
                // Generate code for return expression
                LDA ZP.ACCL
                STA ZP.IDXL
                LDA ZP.ACCH
                STA ZP.IDXH
                
                generateExpression();
                if (NC) { break; }
                
                // Pop result into NEXT
#ifdef DEBUGSTACK
                LDX #'q'
#endif
                popNEXT();
                if (NC) { break; }
                
                LDA AST.astNodeL
                PHA 
                LDA AST.astNodeH
                PHA 
                
                LDA functionNodeL
                STA AST.astNodeL
                LDA functionNodeH
                STA AST.astNodeH
                
                // Store result in return slot (BP + param_count + 4)
                countFunctionParameters();  // [AST.astNode] -> A = count
                
                CLC
                ADC #4  // Skip saved BP and return address, and one more to get to actual return slot
                
#ifdef DEBUGSTACK
                LDX #'c'
#endif
                putNEXT(); // A = BP offset
                
                PLA
                STA AST.astNodeH
                PLA
                STA AST.astNodeL
                
                if (NC) { break; }
            }
            
            // Generate function epilogue (same as in generateFunctionBody)
            
            // LDX runtimeBP
            LDA #OpCode.LDX_ZP
            EmitByte(); if (NC) { break; }
            LDA #runtimeBP
            EmitByte(); if (NC) { break; }
            
            // TXS - restore stack pointer
            LDA #OpCode.TXS
            EmitByte(); if (NC) { break; }
            
            // PLA - pop saved BP
            LDA #OpCode.PLA
            EmitByte(); if (NC) { break; }
            
            // STA runtimeBP - restore old BP
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA #runtimeBP
            EmitByte(); if (NC) { break; }
            
#ifdef DEBUGSTACK
    LDA # OpCode.TAY
    EmitByte(); if (NC) { break; }
            
    LDX #'b'
    debugPrintY();
#endif            
            
            
            // RTS
            LDA #OpCode.RTS
            EmitByte(); if (NC) { break; }
            
            SEC
            break;
        } // single exit
    }
    
    // Generate code for a function body
    // Input: IDX = Function node
    // Output: C set on success, clear on failure
    // Note: Generates prologue, body statements, and epilogue
    generateFunctionBody()  // Input: functionNode = Function node
    {
        // Function's children are: identifier, parameters (optional), then block (as siblings)
        // Skip to the block
        LDY #AST.iChild
        LDA [functionNode], Y
        STA ZP.IDXL
        INY
        LDA [functionNode], Y
        STA ZP.IDXH
        
        // IDX = identifier, walk siblings to find CompoundStmt
        loop
        {
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            if (Z)
            {
                TXA
                if (Z) 
                { 
                    // No CompoundStmt found - error!
                    LDA # Error.UnexpectedFailure
                    Errors.Show();
                    return;
                }
            }
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Check if this sibling is CompoundStmt
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            CMP #AST.NodeType.CompoundStmt
            if (Z) { break; }  // Found it!
            
            // Not CompoundStmt, keep looking (skip parameters)
        }
        
        // Initialize local count
        STZ functionLocals
        
        // Generate function prologue
        //    LDA #runtimeBP
        //    PHA
        //    TSX            // Get current stack pointer
        //    STX runtimeBP  // Save as base pointer
        loop
        {
            // push BP
            LDA # OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            LDA #OpCode.PHA
            EmitByte(); if (NC) { break; }
            
            // BP = SP
            LDA # OpCode.TSX
            EmitByte(); if (NC) { break; }
            LDA # OpCode.STX_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            
#ifdef DEBUGSTACK
    LDA # OpCode.TXA
    EmitByte(); if (NC) { break; }
    LDA # OpCode.TAY
    EmitByte(); if (NC) { break; }
            
    LDX #'B'
    debugPrintY();
#endif
            
            // Now IDX = CompoundStmt
            generateBlock();  // Process all statements in block
            if (NC) { break; }
            
            // Generate function epilogue
            // Restore stack pointer
            //
            //    LDX #runtimeBP
            //    TXS
            //    PLA
            //    STA #runtimeBP
            // 
                   
            // SP = BP     
            LDA # OpCode.LDX_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            LDA #OpCode.TXS
            EmitByte(); if (NC) { break; }
            
            // pop BP
            LDA # OpCode.PLA
            EmitByte(); if (NC) { break; }
            LDA # OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            
#ifdef DEBUGSTACK
    LDA # OpCode.TAY
    EmitByte(); if (NC) { break; }
            
    LDX #'b'
    debugPrintY();
#endif                        
            
            LDA # OpCode.RTS
            EmitByte(); if (NC) { break; }
            break;
        }
    }
    
    // New function to emit all functions
    emitAllFunctions()
    {
        AST.GetRoot(); // -> IDX (Program node)
        
        // Get first child
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        // Walk all children of Program
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }  // No more functions
            
            // Check if it's a Function node
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            CMP #AST.NodeType.Function
            if (Z)
            {
                LDA ZP.IDXH
                STA functionNodeH   
                LDA ZP.IDXL
                STA functionNodeL
                
                
                // Check if this is main
                LDY #AST.iChild
                LDA [functionNode], Y
                STA ZP.IDYL
                INY
                LDA [functionNode], Y
                STA ZP.IDYH
                
                // Get function name
                LDY #AST.iData
                LDA [ZP.IDY], Y
                STA ZP.STRL
                INY
                LDA [ZP.IDY], Y
                STA ZP.STRH
                
                // Compare with "main"
                LDA #(msgMain % 256)
                STA ZP.IDYL
                LDA #(msgMain / 256)
                STA ZP.IDYH
                CompareStrings(); // Compare [STR] with [IDY]
                if (C)
                {
                    // This is main - emit stack initialization first
                    createStack();     // Emit stack init code
                    if (NC) { return; }
                }
                
                // Store current code offset in Function node
                LDY #AST.iOffset
                LDA codeOffsetL
                STA [functionNode], Y
                INY
                LDA codeOffsetH
                STA [functionNode], Y
                
                // Generate this function's body
                generateFunctionBody();  // Uses IDX = Function node
                if (NC) { return; }
            }// function node
            
            // Move to next sibling
            LDY #AST.iNext
            LDA [functionNode], Y
            STA ZP.IDXL
            INY
            LDA [functionNode], Y
            STA ZP.IDXH
        } // loop
        
        SEC
    }
    
        
    // Patch the initial JMP instruction to point to main
    // Note: Called after main's address is known
    patchEntryJump()
    {
        // Calculate absolute address
        LDA codeOffsetL
        STA ZP.ACCL
        LDA codeOffsetH
        STA ZP.ACCH
        AddEntryPoint();
        
        // Write to offset 1-2
        LDY #1
        LDA ZP.ACCL
        STA [codeBuffer], Y
        INY
        LDA ZP.ACCH
        STA [codeBuffer], Y
    }
    
    // Generate code to allocate the runtime stack
    // Output: C set on success, clear on failure
    // Note: Allocates 1K parallel stack, sets up stack pointers
    createStack()
    {
        // Patch JMP to main
        patchEntryJump();
    
        // Allocate 1022 bytes (0x0400) for the parallel stack (1022 + 2 allocator size bytes = 1024)
        LDA #0xFE
        STA ZP.ACCL
        LDA #0x03
        STA ZP.ACCH
        
        // Call Memory.Allocate via BIOS dispatch
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.MemAllocate
        EmitByte(); if (NC) { return; }
        Library.EmitDispatchCall();
        
        // Assume since this is the first allocation, Memory.Allocate returns page-aligned for 1K allocation
        // After 2-byte header, base address will be xx00
        
        // zero the LSB's since we are page aligned
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0L
        EmitByte(); if (NC) { return; }
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1L
        EmitByte(); if (NC) { return; }
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2L
        EmitByte(); if (NC) { return; }
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3L
        EmitByte(); if (NC) { return; }
        
        // now the MSB's
        LDA # OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA # ZP.IDXH
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0H
        EmitByte(); if (NC) { return; }
        LDA # OpCode.INC_A
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1H
        EmitByte(); if (NC) { return; }
        LDA # OpCode.INC_A
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2H
        EmitByte(); if (NC) { return; }
        LDA # OpCode.INC_A
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3H
        EmitByte(); if (NC) { return; }
    }
    
    // Main compile function
    // Output: C set on success, clear on failure
    // Note: Coordinates all code generation phases
    Compile()
    {
        // 1. Reserve 3 bytes for JMP to main
        LDA # OpCode.JMP_ABS
        EmitByte(); if (NC) { return; } // JMP absolute
        LDA #0
        EmitByte(); if (NC) { return; } // Placeholder low
        LDA #0
        EmitByte(); if (NC) { return; } // Placeholder high
        
        // 2. Emit our generic BIOS dispatch function (needs to be callable with JSR)
        LDA # OpCode.JMP_IND
        EmitByte(); if (NC) { return; }
        LDA # ZP.BIOSDISPATCH
        EmitByte(); if (NC) { return; }
        LDA #0x00
        EmitByte(); if (NC) { return; }
        LDA # OpCode.RTS
        EmitByte(); if (NC) { return; } // JMP absolute
        
        // 3. Walk AST and emit all string literals
        AST.GetRoot();  // -> IDX
        emitStrings();
        if (NC) { return; }
        
        // 4. Generate ALL functions
        emitAllFunctions();
        if (NC) { return; }
        
        SEC
    }

    // Save generated code to an executable file
    // Input: ZP.STR = output filename
    // Output: C set on success, clear on failure
    // Note: Deletes existing file, marks output as executable
    Save()
    {
        LDA # FileType.Any // all files
        File.Exists();
        if (C)
        {
            File.Delete();
        }
        
        // Open file for writing
        File.StartSave();
        if (NC)
        {
            LDA #Error.FileSaveError
            Errors.Show();
            return;
        }
        
        // Set source to our code buffer
        LDA codeBufferL
        STA File.SectorSourceL
        LDA codeBufferH
        STA File.SectorSourceH
        
        // Set transfer length to amount of code generated
        LDA codeOffsetL
        STA File.TransferLengthL
        LDA codeOffsetH
        STA File.TransferLengthH
        
        // Write the code buffer
        File.AppendStream();
        if (NC)
        {
            LDA # Error.FileSaveError
            Errors.Show();
            return;
        }
        
        // Mark as executable and close
        LDA #0x80  // Executable flag
        File.EndSave();
        if (NC)
        {
            LDA # Error.FileSaveError
            Errors.Show();
            return;
        }
        
        LDA #(msgSaved % 256)
        STA ZP.STRL
        LDA #(msgSaved / 256)
        STA ZP.STRH
        Print.String();
        
        // Print byte count
        LDA codeOffsetL
        STA ZP.ACCL
        LDA codeOffsetH
        STA ZP.ACCH
        Shared.MoveAccToTop();
        Long.Print();
        
        LDA #(msgBytes % 256)
        STA ZP.STRL
        LDA #(msgBytes / 256)
        STA ZP.STRH
        Print.String();
        
#if defined(DEBUG)        
        // Add diagnostic hex dump
        Print.NewLine();
        dumpCodeBuffer();
        Print.NewLine();
#endif        
        
        SEC
    }
    
    // Diagnostic hex dump of generated code
    // Shows 16 bytes per row with addresses starting at 0x0800
    // Note: Debug function only, included when DEBUG defined
    dumpCodeBuffer()
    {
        PHY
        PHX
        
        // Start at beginning of code buffer
        LDA codeBufferL
        STA ZP.IDYL
        LDA codeBufferH
        STA ZP.IDYH
        
        // Track virtual address (starts at 0x0800)
        LDA #0x00
        STA ZP.ACCL
        LDA #0x08
        STA ZP.ACCH
        
        // Track bytes remaining
        LDA codeOffsetL
        STA ZP.IDXL
        LDA codeOffsetH
        STA ZP.IDXH
        
        loop
        {
            // Check if we have bytes left
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Print address
            LDA ZP.ACCH
            Print.Hex();
            LDA ZP.ACCL
            Print.Hex();
            LDA #':'
            Print.Char();
            Print.Space();
            
            // Print up to 32 bytes on this row
            LDX #32
            loop
            {
                // Check if we have bytes left
                LDA ZP.IDXL
                ORA ZP.IDXH
                if (Z) 
                { 
                    // Pad with spaces if less than 16 bytes on last row
                    loop
                    {
                        PHX
                        Print.Space();
                        Print.Space();
                        Print.Space();
                        PLX
                        DEX
                        if (Z) { break; }
                    }
                    break; 
                }
                
                PHX
                // Print byte
                LDA [ZP.IDY]
                Print.Hex();
                Print.Space();
                PLX
                
                // Advance buffer pointer
                INC ZP.IDYL
                if (Z) { INC ZP.IDYH }
                
                // Decrement bytes remaining
                LDA ZP.IDXL
                if (Z)
                {
                    DEC ZP.IDXH
                }
                DEC ZP.IDXL
                
                // Decrement column counter
                DEX
                if (Z) { break; }
            }
            
            Print.NewLine();
            
            // Advance virtual address by 16
            CLC
            LDA ZP.ACCL
            ADC #32
            STA ZP.ACCL
            LDA ZP.ACCH
            ADC #0
            STA ZP.ACCH
        }
        
        PLX
        PLY
    }
}
