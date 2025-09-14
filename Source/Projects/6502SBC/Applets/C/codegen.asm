unit CodeGen
{
    uses "AST"
    
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
    
    const string sysprintf = "printf";
    
    // 6502 opcodes
    enum OpCode
    {
        CLC     = 0x18,
        INC_A   = 0x1A,
        JSR     = 0x20,
        PHA     = 0x48,
        JMP_ABS = 0x4C,
        RTS     = 0x60,
        STZ_ZP  = 0x64,
        PLA     = 0x68,
        ADC_IMM = 0x69,
        JMP_IND = 0x6C,
        STA_ZP  = 0x85,
        STX_ZP  = 0x86, 
        TXA     = 0x8A,
        STA_IND_Y = 0x91,
        TXS     = 0x9A,
        LDX_IMM = 0xA2,
        LDA_ZP  = 0xA5, 
        LDX_ZP  = 0xA6,
        TAY     = 0xA8,
        LDA_IMM = 0xA9,
        LDA_IND_Y = 0xB1,
        TSX     = 0xBA,
        DEX     = 0xCA,
        INC_ZP  = 0xE6, 
        INX = 0xE8,
    }    
    
    // Check if a function name corresponds to a system function
    // Input: ZP.STR = function name to check
    // Output: A = syscall number if system function
    //         C set if system function, clear if user function
    isSystemFunction()
    {
        // Check for "printf"
        LDA #(sysprintf % 256)
        STA ZP.IDYL
        LDA #(sysprintf / 256)
        STA ZP.IDYH
        compareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.PrintString
            SEC
            return;
        }
             
        // TODO : add more system functions here...
        
        CLC  // Not a system function
    }
    
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
        LDA ZP.TEMP
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
        STA ZP.TEMP
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
    compareStrings()
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
        // For now, search the compound statement for matching VarDecl
        // This is simplified - real implementation would handle scoping
        
        // Start from function's compound statement
        AST.GetRoot();  // Get program root
        
        // Get first child (should be function)
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        // Skip to compound statement (second child of function)
        LDY #AST.iChild
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
        
        // Now search children of compound for VarDecl nodes
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
            if (Z) { CLC break; }  // Not found
            
            // Check if it's a VarDecl
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            CMP #AST.NodeType.VarDecl
            if (Z)
            {
                // Get its identifier child
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                LDY #AST.iChild
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                // Get identifier's name
                LDY #AST.iData
                LDA [ZP.IDY], Y
                TAX
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDYH
                STX ZP.IDYL
                
                // Compare with target name
                CodeGen.compareStrings();  // STR vs IDY
                if (C)
                {
                    // Found it!
                    PLA
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    SEC
                    break;
                }
                
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
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
                compareStrings();
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
    
    // Add the BIOS entry point address (0x0800) to value in ACC
    // Input: ZP.ACC = relative offset
    // Output: ZP.ACC = absolute address (offset + 0x0800)
    addEntryPoint()
    {
        CLC
        LDA ZP.ACCL
        ADC # (BIOSInterface.EntryPoint % 256)
        STA ZP.ACCL
        LDA ZP.ACCH
        ADC # (BIOSInterface.EntryPoint / 256)
        STA ZP.ACCH
    }
    
    // Generate code for a printf system call
    // Input: IDX = CallExpr node for printf
    // Output: C set on success, clear on failure
    // Note: Currently only supports string literal as first argument
    generatePrintfCall()
    {
        loop
        {
            // first child is identifier (which we already know is "printf")
            LDY #AST.iChild
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Move to first argument (sibling of identifier)
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Should be a StringLit node
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            CMP #AST.NodeType.StringLit
            if (NZ) 
            {
                LDA # Token.StringLiteral
                Errors.Expected();
                break;
            }
            
            // Get string's offset (stored during emitStrings)
            // TODO : should be an expression -> STR
            LDY #AST.iOffset
            LDA [ZP.IDX], Y
            STA ZP.ACCL
            INY
            LDA [ZP.IDX], Y
            STA ZP.ACCH
            
            addEntryPoint();
            
            // Generate: LDA #low(string)
            LDA # OpCode.LDA_IMM
            EmitByte(); if (NC) { break;}
            LDA ZP.ACCL
            EmitByte();if (NC) { break;}
            
            // Generate: STA ZP.STRL
            LDA #OpCode.STA_ZP
            EmitByte();if (NC) { break;}
            LDA #ZP.STRL
            EmitByte();if (NC) { break;}
            
            // Generate: LDA #high(string)
            LDA #OpCode.LDA_IMM
            EmitByte();if (NC) { break;}
            LDA ZP.ACCH
            EmitByte();if (NC) { break;}
            
            // Generate: STA ZP.STRH
            LDA # OpCode.STA_ZP
            EmitByte();if (NC) { break;}
            LDA # ZP.STRH
            EmitByte();if (NC) { break;}
            
            // Generate: LDX #SysCall.PrintString
            LDA #OpCode.LDX_IMM
            EmitByte();if (NC) { break;}
            LDA #BIOSInterface.SysCall.PrintString
            EmitByte();if (NC) { break;}
            
            emitDispatchCall();
            break;
        } // single exit
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
            isSystemFunction();  // -> A = syscall#, C = is system
            if (C)
            {
                switch (A)
                {
                    case SysCall.PrintString:
                    {
                        generatePrintfCall();
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
                // generateUserFunctionCall();  // Future: JSR to user function
#ifdef DEBUG
Print.Hex(); LDA #'u' Print.Char();LDA #'f' Print.Char();
#endif
                LDA # Error.NotImplemented
                Errors.ShowIDX();
                break;
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
            // Locals are at negative offsets: first local at BP-1, second at BP-2, etc.
            LDY # AST.iOffset  // Reuse the offset field for BP offset
            LDA functionLocals
            EOR #0xFF         // Negate (two's complement without the +1)
            STA [ZP.IDX], Y   // Store as BP-relative offset (-1, -2, etc.)
            
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
    
    // Generate code to push 32-bit value from ZP.NEXT onto runtime stack
    pushNEXT()
    {
        // TSX - get current stack pointer
        LDA #OpCode.TSX
        EmitByte(); if (NC) { return; }
        
        // Transfer X to Y for indirect indexed addressing
        LDA #OpCode.TXA
        EmitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        EmitByte(); if (NC) { return; }
        
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
        
        // DEX - point to new top (the value we just pushed)
        LDA #OpCode.DEX
        EmitByte(); if (NC) { return; }
        
        // TXS - update stack pointer
        LDA #OpCode.TXS
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to pop 32-bit value from stack into ZP.NEXT
    popNEXT()
    {
        // TSX - get current stack pointer
        LDA #OpCode.TSX
        EmitByte(); if (NC) { return; }
        
        // INX - point to top value (SP points one past)
        LDA #OpCode.INX  
        EmitByte(); if (NC) { return; }
        
        // Transfer X to Y for indirect indexed addressing
        LDA #OpCode.TXA
        EmitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        EmitByte(); if (NC) { return; }
        
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
                                              
        // TXS - update stack pointer (now points to next free)
        LDA #OpCode.TXS
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
        if (PL)  // Positive offset (parameters)
        {
            // Calculate adjusted offset at compile time
            CLC
            ADC #3  // Add 3 to skip frame overhead
        }
        // Now A contains either the original negative offset OR the adjusted positive offset
        EmitByte(); if (NC) { return; }
        
        // Transfer result to Y
        LDA #OpCode.TAY
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
    
    // Generate code to store ZP.NEXT at BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    putNEXT()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset(); if (NC) { break; }
            
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
    getNEXT()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset(); if (NC) { break; }
                       
            // Transfer A to Y
            LDA #OpCode.TAY
            EmitByte(); if (NC) { break; }
            
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
    // Output: Value loaded into ZP.TOP0-3
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
        
        // Read the 4 bytes from heap into NEXT (temp storage)
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
        
        pushNEXT();
              
        SEC
    }
    
    // Generate code for an expression
    // Input: IDX = expression node
    // Output: Result in ZP.TOP0-3 or on stack
    //         C set on success, clear on failure
    generateExpression()
    {
        LDY #AST.iNodeType
        LDA [ZP.IDX], Y
        
        switch (A)
        {
            case NodeType.IntLit:
            case NodeType.LongLit:
            {
                generateIntLiteral();
            }
            default:
            {
#ifdef DEBUG
Print.Hex(); LDA #'e' Print.Char();
#endif
                LDA #Error.NotImplemented
                Errors.ShowIDX();
            }
        }
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
            if (NC) { break; } // Variable not found!
            
            // Get the BP offset from VarDecl
            LDY #AST.iOffset
            LDA [ZP.IDX], Y  // This is the signed offset
            
            // Store NEXT at BP+offset
            putNEXT();  // A = offset
            
                
            SEC
            break;
        } // single exit
        
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
            case NodeType.VarDecl:
            {
                generateVarDecl();
            }
            case NodeType.ExprStmt:
            {
                // Get the expression (child)
                LDY #AST.iChild
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDXH
                STX ZP.IDXL
                
                // Check expression type
                LDY #AST.iNodeType
                LDA [ZP.IDX], Y
                switch (A)
                {
                    case NodeType.CallExpr:
                    {
                        generateCallExpr();
                    }
                    case NodeType.Assign:
                    {
                        generateAssignment();
                    }
                    default:
                    {
                        // Future: BinOp, Assign, etc.
#ifdef DEBUG
Print.Hex(); LDA #'e' Print.Char();LDA #'s' Print.Char();
#endif
                        LDA #Error.NotImplemented
                        Errors.ShowIDX();
                    }
                }
            }
            default:
            {
                // Future: case AST.NodeType.If, For, Return, etc.
#ifdef DEBUG                
Print.Hex(); LDA #'s' Print.Char();
#endif                
                LDA # Error.NotImplemented
                Errors.ShowIDX();
            }
        }
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
            if (Z) { break; }  // No more statements
            
            LDA AST.astNodeL
            STA ZP.IDXL
            LDA AST.astNodeH
            STA ZP.IDXH
            generateStatement();  // Uses IDX
            
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
    
    // Generate code for a function body
    // Input: IDX = Function node
    // Output: C set on success, clear on failure
    // Note: Generates prologue, body statements, and epilogue
    generateFunctionBody()  // Input: IDX = Function node
    {
        LDA ZP.IDXL
        STA AST.astNodeL
        LDA ZP.IDXH
        STA AST.astNodeH
        
        // Function's children are: identifier, then block (as siblings)
        // Skip to the block
        LDY #AST.iChild
        LDA [AST.astNode], Y
        STA ZP.IDXL
        INY
        LDA [AST.astNode], Y
        STA ZP.IDXH
        
        // IDX = identifier, get its sibling (block)
        LDY #AST.iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
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
            
            // Now IDX = CompoundStmt
            generateBlock();  // Process all statements in block
            
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
            
            LDA # OpCode.RTS
            EmitByte(); if (NC) { break; }
            break;
        }
    }
    
    
    // Generate code for the main function
    // Output: C set on success, clear on failure
    // Note: Finds main in AST and generates its body
    emitMain()
    {
         // Find main function in AST
        LDA # (msgMain % 256)
        STA ZP.STRL
        LDA # (msgMain / 256)
        STA ZP.STRH
        findFunction();      // -> IDX = main Function node
        if (NC)
        {
            LDA # Error.NoEntryPoint
            Errors.Show();
            return; 
        }
        
        // Store code offset in Function node
        LDY #AST.iOffset
        LDA codeOffsetL
        STA [ZP.IDX], Y
        INY
        LDA codeOffsetH
        STA [ZP.IDX], Y
        
        // Generate code for function body
        generateFunctionBody();  // Uses IDX = Function node
        
        SEC
    }
    
    // Emit a JSR to the BIOS dispatch function
    // Output: C set on success, clear on failure
    // Note: Assumes X register contains syscall number
    emitDispatchCall()
    {
        LDA # OpCode.JSR
        EmitByte(); if (NC) { return; }
        
        // Add base to offset to get absolute address (4th byte into our code after the entrypoint JMP)
        CLC
        LDA # (BIOSInterface.EntryPoint % 256)
        ADC # 3
        EmitByte(); if (NC) { return; }
        LDA # (BIOSInterface.EntryPoint / 256)
        EmitByte(); 
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
        addEntryPoint();
        
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
        emitDispatchCall();
        
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
        
        // 4. create the runtime stack
        createStack();
        if (NC) { return; }
        
        // 5. Generate main function
        emitMain();
        if (NC) { return; }
        
        // TODO 
        
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
            
            // Print up to 16 bytes on this row
            LDX #16
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
            ADC #16
            STA ZP.ACCL
            LDA ZP.ACCH
            ADC #0
            STA ZP.ACCH
        }
        
        PLX
        PLY
    }
}
