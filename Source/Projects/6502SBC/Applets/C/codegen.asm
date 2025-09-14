unit CodeGen
{
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
        INC_A   = 0x1A,
        JSR     = 0x20,
        PHA     = 0x48,
        JMP_ABS = 0x4C,
        RTS     = 0x60,
        STZ_ZP  = 0x64,
        PLA     = 0x68,
        JMP_IND = 0x6C,
        STA_ZP  = 0x85,
        STX_ZP  = 0x86, 
        TXS     = 0x9A,
        LDX_IMM = 0xA2,
        LDA_ZP  = 0xA5, 
        LDX_ZP  = 0xA6,
        LDA_IMM = 0xA9,
        TSX     = 0xBA,
        DEX     = 0xCA,
        INC_ZP  = 0xE6, 
    }    
    
    // Check if function name is a system function
    // Input: ZP.STR = function name
    // Output: A = syscall number, C set if system function
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
    
    // Recursively walk AST and emit string literals
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
    
    // Compare [STR] with [IDY], return C set if equal
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
    
    // Find a function (for now, just returns first function)
    // Input:  STR = function name
    // Output: IDX = Function node, C set on success
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
    
    // ZP.ACC + 0x0800 -> ZP.ACC
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
    
    // Generate code for function call
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
LDA #'d' Print.Char();                        
                        LDA # Error.NotImplemented
                        Errors.ShowIDX();
                        break;
                    }
                }
            }
            else
            {
                // generateUserFunctionCall();  // Future: JSR to user function
LDA #'e' Print.Char();
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
    
    // VarDecl node in IDX
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
                Print.Hex(); LDA 'v' Print.Char();
#endif
                LDA # Error.NotImplemented
                Errors.ShowIDY();
                break;
            }
                    
            SEC
            break;
        }
    }
    
    // Generate code for an integer literal
    // Input: IDX = IntLit/LongLit node
    // Output: Value in ZP.TOP0-3
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
        
        // Generate inline code to load these values
        // LDA #byte0
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA ZP.NEXT0
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP0
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP0
        EmitByte(); if (NC) { return; }
        
        // LDA #byte1
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA ZP.NEXT1
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP1
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        EmitByte(); if (NC) { return; }
        
        // LDA #byte2
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA ZP.NEXT2
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP2
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        EmitByte(); if (NC) { return; }
        
        // LDA #byte3
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA ZP.NEXT3
        EmitByte(); if (NC) { return; }
        // STA ZP.TOP3
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
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
                Print.Hex(); LDA 'e' Print.Char();
#endif
                LDA #Error.NotImplemented
                Errors.ShowIDX();
            }
        }
    }
    
    // Generate code for assignment
    // Input: IDX = Assign node
    generateAssignment()
    {
        // Save Assign node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Get right-hand side (second child - the value)
        // First get to first child
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        // Now get its sibling (second child)
        LDY #AST.iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        // Generate code for RHS expression (puts value in ZP.TOP)
        generateExpression();
        if (NC) { PLA PLA return; }
        
        // Now we have value in ZP.TOP0-3
        // Need to store it to the variable (first child of Assign)
        
        // Restore Assign node
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Get first child (the variable identifier)
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        // For now, assume it's the first local variable (BP-relative offset 0)
        // Generate: store TOP to stack at BP offset
        
        // TODO: Look up variable's actual offset
        // For now, 's' is first local, so X = 0
        
        // Store each byte to its stack page
        // LDA ZP.TOP0
        // TODO
        
        
        SEC
    }
    
    // Generate code for a statement
    generateStatement()  // Input: IDX = statement node
    {
        // Check statement type
        LDY #AST.iNodeType
        LDA [ZP.IDX], Y
        switch (A)
        {
            case NodeType.Assign:
            {
                generateAssignment();
            }
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
                    default:
                    {
                        // Future: BinOp, Assign, etc.
#ifdef DEBUG
                        Print.Hex(); LDA 'e' Print.Char();LDA 's' Print.Char();
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
                Print.Hex(); LDA 's' Print.Char();
#endif                
                LDA # Error.NotImplemented
                Errors.ShowIDX();
            }
        }
    }
    
    // Generate code for compound statement
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
    
    // Generate code for function body
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
    
    
    // Generate code for main function
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
    
    // Patch JMP to main
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

    // Save generated code to file
    // Input: ZP.STR = output filename (e.g., "HELLOX")
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
    
    
    // Add this function to CodeGen unit (codegen.asm)

    // Diagnostic hex dump of code buffer
    // Shows 16 bytes per row with addresses starting at 0x0800
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

