unit CodeGen
{
    uses "AST"
    uses "Library"
    uses "Errors"
    uses "Gen6502"
    uses "Utilities"
    
    friend Library, AST, Gen6502;
    
    // Code generation state
    const byte cgSlots = 0xA0;
    
    const byte functionLocals = cgSlots+0; // Count of locals in current function
    const byte storeOp        = cgSlots+1;
    
    const byte forwardPatchL  = cgSlots+2;
    const byte forwardPatchH  = cgSlots+3;
    
    const byte elsePatchL     = forwardPatchL;
    const byte elsePatchH     = forwardPatchH;
    
    const byte backwardPatchL  = cgSlots+4;
    const byte backwardPatchH  = cgSlots+5;
    
    const byte endPatchL       = backwardPatchL;
    const byte endPatchH       = backwardPatchH;
    
    const byte functionNode    = cgSlots+6;   // Current function being compiled
    const byte functionNodeL   = cgSlots+6;   
    const byte functionNodeH   = cgSlots+7;
    
    const byte functionFlags   = cgSlots+8;
    // Bit 0 - we're in "main"
    // Bit 1 - "main" had arguments
    
    
    const string msgMain = "main";
    
    // Initialize code generation buffers and state
    Initialize()
    {
        Gen6502.Initialize();
    }
    
    // Clean up and free allocated code generation buffers
    Dispose()
    {
        Gen6502.Dispose();
    }
    
       
    // Input: IDX = StringLit node
    // Output: C set on success, clear on failure
    //         Generated code pushes value to stack
    generateLoadString()
    {
        // Get string's offset (stored during emitStrings)
        LDY #AST.iOffset
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.ACCH
        CodeGen.AddEntryPoint();
        
        // A = LSB, X = MSB
        LDA ZP.ACCL
        LDX ZP.ACCH
        VCode.PushWORD();
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
        FindVariable();
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
        GetNEXT(); if (NC) { return; }
        
        // Generate code to push ZP.NEXT onto stack
        PushNEXT();
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
            FindFunction(); // STR = name -> IDX = Function node, C = found
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
            AST.CountFunctionParameters(); // [AST.astNode] -> A = count
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
            AST.CountFunctionParameters(); // [AST.astNode] -> A = count
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
            VCode.Reserve(); if (NC) { break; }
            VCode.Flush();   if (NC) { break; }
            
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
                    case SysCall.MemAllocate:
                    {
                        Library.AllocCall();
                        if (NC) { break; }
                    }
                    case SysCall.MemFree:
                    {
                        Library.FreeCall();
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
                        {
                            Library.FOpenCall();
                            if (NC) { break; }
                        }
                        case FileFunction.FGetC:
                        {
                            Library.FGetCCall();
                            if (NC) { break; }
                        }
                        case FileFunction.FRead:
                        {
                            Library.FReadCall();
                            if (NC) { break; }
                        }
                        case FileFunction.FClose:
                        {
                            Library.FCloseCall();
                            if (NC) { break; }
                        }
                        case FileFunction.FPutC:
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
            VCode.Reserve(); if (NC) { return; }
            VCode.Flush();   if (NC) { return; }
        
            // Store the BP offset in the VarDecl node for later reference
            // Locals are at negative offsets: first local at BP+0, second at BP-1, etc.
            LDY # AST.iOffset  // Reuse the offset field for BP offset
            LDA functionLocals
            if (NZ)
            {
                EOR #0xFF      // Negate to get -1, -2, etc.
                CLC
                ADC #1
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
    
    generateNegateLong()
    {
        LDA #0
        VCode.PushCHAR(); if (NC) { return; } 
        VCode.PopNEXT();  if (NC) { return; } 
        
        // Original value -> TOP
        VCode.PopTOP();  if (NC) { return; }    
        
        // NEXT = NEXT - TOP (0 - value)
        LDA #OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA #BIOSInterface.SysCall.LongSub
        EmitByte(); if (NC) { return; }
        Library.EmitDispatchCall(); if (NC) { return; }
        
        PushNEXT(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code for reading through a pointer: value = *ptr
    // Stack IN:  pointer address (32-bit)
    // Stack OUT: value that was at that address (32-bit)
    // Example: x = *ptr  (we're reading FROM memory)
    generateDereferenceRead()
    {
        // Pop pointer from stack into TOP
        VCode.PopTOP();
        if (NC) { return; }
        
        // Read byte FROM memory at [TOP]
        LDA #OpCode.LDA_IND
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP
        EmitByte(); if (NC) { return; }
        
        // Put value in NEXT and zero-extend
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        LDA # OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        // Push value onto stack
        VCode.PushNEXT();
    }
    
    // Generate code for writing through a pointer: *ptr = value
    // Stack IN:  pointer address (32-bit)
    // NEXT IN:   value to write
    // Stack OUT: nothing
    // Example: *ptr = 5  (we're writing TO memory)
    generateDereferenceWrite()
    {
        // Pop pointer from stack into TOP
        VCode.PopTOP();
        if (NC) { return; }
        VCode.PopNEXT(); // RHS
        if (NC) { return; }
        
        // Write byte TO memory at [IDY]
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STA_IND
        EmitByte(); if (NC) { return; }
        LDA #ZP.TOP
        EmitByte(); if (NC) { return; }
        
        SEC
    }

    // Generate code for unary operators
    generateUnaryOp()
    {
        LDA AST.astNodeL
        PHA
        LDA AST.astNodeH
        PHA
        LDA storeOp
        PHA
        
        // Get operator type
        LDY #AST.iUnaryOp
        LDA [ZP.IDX], Y
        STA storeOp  // Save operator type
       
        // Get child pointer (the operand)
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL

        loop
        {
            // Generate code for the operand
            generateExpression();
            if (NC) { break; }

            // Check operator type
            LDA storeOp
            switch (A)
            {
                case UnaryOpType.Minus:
                {
                    generateNegateLong();
                    if (NC) { break; }
                }
                case UnaryOpType.Dereference:
                {
                    generateDereferenceRead();
                    if (NC) { break; }
                }
                default:
                {
#ifdef DEBUG
Print.Hex(); LDA #'u' Print.Char();
#endif
                    // Could add other unary operators here (!, ~, etc.)
                    LDA #Error.NotImplemented
                    Errors.ShowIDX();
                    break;
                }
            }
            SEC
            break;
        }
        PLA
        STA storeOp
        PLA
        STA AST.astNodeH
        PLA
        STA AST.astNodeL
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
            FindVariable();  // -> IDX
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
            GetNEXT(); if (NC) { break; }
            
            // Push the ORIGINAL value (this is what postfix returns)
            PushNEXT(); if (NC) { break; }
            
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
                    IncNEXT();
                    if (NC) { PLA break; }
                }
                case PostfixOpType.Decrement:
                {
                    DecNEXT();
                    if (NC) { PLA break; }
                }
            }
            
            // Result is in NEXT, store it back
            PLA  // Get offset
            PutNEXT();  // A = offset
            
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
            
            generateExpression(); if (NC) { break; } // AST.astNode first child
            
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
            PopTOP();    // Right operand -> TOP
            PopNEXT();   // Left operand -> NEXT
            
            // Perform operation based on saved operator
            LDA storeOp  // Get operator type
            switch (A)
            {
                case BinOpType.Add:
                {
                    LongADD();  if (NC) { break; }
                    PushNEXT(); if (NC) { break; }
                    SEC
                    break;
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
                    PushNEXT();
                }
                default:
                {
                    // result is C or NC
                    PushC();
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
                LongPushNEXT();
                if (NC) { return; }
            } 
            case NodeType.StringLit:
            {
                generateLoadString();
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
            case NodeType.UnaryOp:
            {
                generateUnaryOp();
                if (NC) { return; }
            }
            
            default:
            {
#ifdef DEBUG
Print.Hex(); LDA #'e' Print.Char();
Print.Space(); LDA ZP.IDXH Print.Hex();LDA ZP.IDXL Print.Hex();
Print.Space();
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
        
        LDA storeOp
        PHA
        
        loop
        {
            // Get first child (left side)
            LDY #AST.iChild
            LDA [ZP.IDX], Y
            PHA
            TAX
            INY
            LDA [ZP.IDX], Y
            PHA
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Save left side type
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            STA storeOp
            
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
            
            // restore the LHS node
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            if (NC) { break; }
            
            LDA storeOp
            CMP #AST.NodeType.UnaryOp
            if (Z)
            {
                // Check if it's a dereference
                LDY #AST.iUnaryOp
                LDA [ZP.IDX], Y
                CMP #AST.UnaryOpType.Dereference
                if (NZ)
                {
                    // Not dereference - invalid lvalue
                    LDA AST.astNodeH
                    STA ZP.IDXH
                    LDA AST.astNodeL
                    STA ZP.IDXL
                    LDA # Error.SyntaxError
                    Errors.ShowIDX();
                    break;
                }
                
                // Get pointer expression (child of UnaryOp)
                LDY #AST.iChild
                LDA [ZP.IDX], Y
                TAX
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDXH
                STX ZP.IDXL
                
                // Generate code to get pointer
                generateExpression();
                if (NC) { break; }
                
                generateDereferenceWrite();
                if (NC) { break; }
            }
            else 
            {

                CMP # AST.NodeType.Identifier
                if (NZ)
                {
                    // Not identifier - invalid lvalue
                    LDA AST.astNodeH
                    STA ZP.IDXH
                    LDA AST.astNodeL
                    STA ZP.IDXL
                    LDA # Error.UnexpectedFailure
                    Errors.ShowIDX();
                    break;
                }
            
                // Get identifier's name
                LDY #AST.iData
                LDA [ZP.IDX], Y
                STA ZP.STRL
                INY
                LDA [ZP.IDX], Y
                STA ZP.STRH
                
                // Find the VarDecl for this identifier
                FindVariable();  // -> IDX
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
                
                VCode.PopNEXT(); // RHS
            
                // Get the BP offset from VarDecl
                LDY #AST.iOffset
                LDA [ZP.IDX], Y  // This is the signed offset
                
                // Store NEXT at BP+offset
                VCode.PutNEXT();  // A = offset
            }
                
            // Assignment expressions return the assigned value
            VCode.PushNEXT();
                
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
                VCode.Discard();  if (NC) { break; }
            }
            VCode.Flush();
            
            // Record loop start position (codeBuffer + codeOffset)
            LDA Gen6502.codeOffsetL
            STA ZP.ACCL
            LDA Gen6502.codeOffsetH
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
                PopNEXT();  if (NC) { break; }
                // Test if NEXT is zero (false)
                NEXTZero(); if (NC) { break; }
                
                // BNE skip_exit (if true, continue loop)
                LDA # OpCode.BNE
                EmitByte(); if (NC) { break; }
                LDA #3  // Skip over JMP instruction (3 bytes)
                EmitByte(); if (NC) { break; }
                
                // JMP to exit (will patch address later)
                LDA #OpCode.JMP_ABS
                EmitByte(); if (NC) { break; }
                
                // Save current offset for patching
                LDA Gen6502.codeOffsetL
                STA forwardPatchL
                LDA Gen6502.codeOffsetH
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
                VCode.Discard();  if (NC) { break; }
            }
            VCode.Flush();
            
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
                LDA Gen6502.codeOffsetL
                STA ZP.ACCL
                LDA Gen6502.codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + forwardPatch offset)
                CLC
                LDA Gen6502.codeBufferL
                ADC forwardPatchL
                STA ZP.IDXL
                LDA Gen6502.codeBufferH
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
    
    
    // Generate code for a while loop
    // Input: IDX = While node
    // Output: C set on success, clear on failure
    generateWhile()
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
        
        // Save the While node pointer
        LDA ZP.IDXH
        STA AST.astNodeH
        LDA ZP.IDXL
        STA AST.astNodeL
        
        loop
        {
            VCode.Flush(); if (NC) { break; }
            
            // Record loop start position (codeBuffer + codeOffset)
            LDA Gen6502.codeOffsetL
            STA ZP.ACCL
            LDA Gen6502.codeOffsetH
            STA ZP.ACCH
            AddEntryPoint(); // Convert to runtime address -> ACC
            LDA ZP.ACCL
            STA backwardPatchL
            LDA ZP.ACCH
            STA backwardPatchH
            
            // Generate condition (first child of While node)
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            generateExpression(); if (NC) { break; } // AST.astNode first child
            
            // Pop result to check it
            PopNEXT(); if (NC) { break; }
            // Test if NEXT is zero (false)
            NEXTZero(); if (NC) { break; }
            
            // BNE skip_exit (if true, continue loop)
            LDA #OpCode.BNE
            EmitByte(); if (NC) { break; }
            LDA #3  // Skip over JMP instruction (3 bytes)
            EmitByte(); if (NC) { break; }
            
            // JMP to exit (will patch address later)
            LDA #OpCode.JMP_ABS
            EmitByte(); if (NC) { break; }
            
            // Save current offset for patching
            LDA Gen6502.codeOffsetL
            STA forwardPatchL
            LDA Gen6502.codeOffsetH
            STA forwardPatchH
            
            // Emit placeholder address
            LDA #0
            EmitByte(); if (NC) { break; }
            EmitByte(); if (NC) { break; }
            
            // Get body (second child - sibling of condition)
            LDY #AST.iChild
            LDA [AST.astNode], Y
            STA ZP.IDXL
            INY
            LDA [AST.astNode], Y
            STA ZP.IDXH
            
            // Move to sibling (body)
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            generateStatement(); if (NC) { break; }
            
            // JMP back to loop start
            LDA #OpCode.JMP_ABS
            EmitByte(); if (NC) { break; }
            
            // Emit the backward jump address
            LDA backwardPatchL
            EmitByte(); if (NC) { break; }
            LDA backwardPatchH
            EmitByte(); if (NC) { break; }
            
            // Patch forward jump
            // Calculate current absolute position
            LDA Gen6502.codeOffsetL
            STA ZP.ACCL
            LDA Gen6502.codeOffsetH
            STA ZP.ACCH
            AddEntryPoint(); // Convert to runtime address -> ACC
            
            // Calculate patch location (codeBuffer + forwardPatch offset)
            CLC
            LDA Gen6502.codeBufferL
            ADC forwardPatchL
            STA ZP.IDXL
            LDA Gen6502.codeBufferH
            ADC forwardPatchH
            STA ZP.IDXH
            
            // Write exit address at patch location
            LDY #0
            LDA ZP.ACCL
            STA [ZP.IDX], Y
            INY
            LDA ZP.ACCH
            STA [ZP.IDX], Y
            
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
            case NodeType.While:
            {
                generateWhile();
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
                VCode.Discard();  if (NC) { return; }
            }
            case NodeType.Empty:
            {
                // Empty statement - generate nothing (fall through to SEC)
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
            generateExpression(); if (NC) { break; } // AST.astNode first child
            
            // Pop result to check it
            PopNEXT();  if (NC) { break; }
            
            // Test if NEXT is zero (false)
            NEXTZero(); if (NC) { break; }

            // BNE to_then (if true, execute then clause)
            LDA #OpCode.BNE
            EmitByte(); if (NC) { break; }
            LDA #3  // Skip over JMP instruction (3 bytes)
            EmitByte(); if (NC) { break; }
            
            // JMP to else/end (will patch address later)
            LDA #OpCode.JMP_ABS
            EmitByte(); if (NC) { break; }
            
            // Save patch location for jump to else/end
            LDA Gen6502.codeOffsetL
            STA elsePatchL
            LDA Gen6502.codeOffsetH
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
                LDA Gen6502.codeOffsetL
                STA endPatchL
                LDA Gen6502.codeOffsetH
                STA endPatchH
                
                // Emit placeholder address
                LDA #0
                EmitByte(); if (NC) { break; }
                EmitByte(); if (NC) { break; }
                
                // Patch the elsePatch to jump here (start of else)
                // Calculate current absolute position
                LDA Gen6502.codeOffsetL
                STA ZP.ACCL
                LDA Gen6502.codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + elsePatch offset)
                LDA Gen6502.codeBufferL
                CLC
                ADC elsePatchL
                STA ZP.IDXL
                LDA Gen6502.codeBufferH
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
                LDA Gen6502.codeOffsetL
                STA ZP.ACCL
                LDA Gen6502.codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + endPatch offset)
                LDA Gen6502.codeBufferL
                CLC
                ADC endPatchL
                STA ZP.IDXL
                LDA Gen6502.codeBufferH
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
                LDA Gen6502.codeOffsetL
                STA ZP.ACCL
                LDA Gen6502.codeOffsetH
                STA ZP.ACCH
                AddEntryPoint(); // Convert to runtime address -> ACC
                
                // Calculate patch location (codeBuffer + elsePatch offset)
                LDA Gen6502.codeBufferL
                CLC
                ADC elsePatchL
                STA ZP.IDXL
                LDA Gen6502.codeBufferH
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
                PopNEXT();
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
                AST.CountFunctionParameters();  // [AST.astNode] -> A = count (generateReturn)
                
                CLC
                ADC #4  // Skip saved BP and return address, and one more to get to actual return slot
                
                PutNEXT(); // A = BP offset
                
                PLA
                STA AST.astNodeH
                PLA
                STA AST.astNodeL
                
                if (NC) { break; }
            }
            
            // Generate function epilogue (same as in generateFunctionBody)
            Gen6502.Epilogue();
            break;
        } // single exit
    }
    
    // Generate code for a function body
    // Input: functionNode = Function node
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
        
        loop
        {
            Gen6502.Prologue();
            // Now IDX = CompoundStmt
            generateBlock();  // Process all statements in block
            if (NC) { break; }
            Gen6502.Epilogue();
            break;
        }
    }
    
    // New function to emit all functions
    emitAllFunctions()
    {
        STZ functionFlags
        
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
                    SMB0 functionFlags // Bit 0 - we're in "main"
                    
                    // This is main - emit stack initialization first
                    Gen6502.CreateStack();     // Emit stack init code
                    if (NC) { return; }
                    
                    LDA functionNodeH
                    STA AST.astNodeH
                    LDA functionNodeL
                    STA AST.astNodeL
                    AST.CountFunctionParameters(); // [AST.astNode] -> A = count
                    if (NZ)
                    {
                        SMB1 functionFlags // Bit 1 - "main" has arguments
                        Gen6502.CreateCLIArguments();
                    }
                    if (NC) { return; }
                }
                else
                {
                    STZ functionFlags // not in "main"
                }
                
                // Store current code offset in Function node
                LDY #AST.iOffset
                LDA Gen6502.codeOffsetL
                STA [functionNode], Y
                INY
                LDA Gen6502.codeOffsetH
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
        Gen6502.EmitStrings();
        if (NC) { return; }
        
        // 4. Generate ALL functions
        emitAllFunctions();
        if (NC) { return; }
        
        SEC
    }

}
