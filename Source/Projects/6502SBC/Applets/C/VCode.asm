unit VCode
{
    const byte vzSlots = 0xD0;
    
    const byte vcodeBuffer     = vzSlots+0;
    const byte vcodeBufferL    = vzSlots+0;
    const byte vcodeBufferH    = vzSlots+1;
    
    const byte vcodeOffset     = vzSlots+2;
    
    const byte vcodeSize       = vzSlots+4;
    const byte vcodeSizeL      = vzSlots+4;
    const byte vcodeSizeH      = vzSlots+5;
    
    enum VOpCode
    {
        PushNEXT,
        PushTOP,
        PopNEXT,
        PopTOP,
        PutNEXT,
        GetNEXT,
        IncNEXT,
        DecNEXT,
        PushC,
        LongADD,
    }
    
    Initialize()
    {
        // 256 bytes should be plenty
        LDA #0x00
        STA ZP.ACCL
        LDA #0x01
        STA ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        STA vcodeBufferL
        LDA ZP.IDXH
        STA vcodeBufferH
        
        STZ vcodeOffset   // Start at offset 0
        
        LDA #0x01
        STA vcodeSizeH
        STZ vcodeSizeL
    }
    Dispose()
    {
        // Free code buffer
        LDA vcodeBufferL
        ORA vcodeBufferH
        if (NZ)
        {
            LDA vcodeBufferL
            STA ZP.IDXL
            LDA vcodeBufferH
            STA ZP.IDXH
            Memory.Free();
            
            STZ vcodeBufferL
            STZ vcodeBufferH
        }
    }
    
    IsEmpty()
    {
        LDA vcodeOffset
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
    }
    
    Flush()
    {
//LDA #'<' Print.Char(); LDA vcodeOffset Print.Hex(); LDA #':' Print.Char(); Print.Space();
        LDY #0
        loop
        {
            LDA [vcodeBuffer], Y
            INY
//PHA PHY Print.Hex(); Print.Space(); PLY PLA           
            
            switch (A)
            {
                case VOpCode.PushNEXT:
                {
                    PHY pushNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.PushTOP:
                {
                    PHY pushTOP(); PLY if (NC) { return; }
                }
                case VOpCode.PopNEXT:
                {
                    PHY popNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.PopTOP:
                {
                    PHY popTOP(); PLY if (NC) { return; }
                }
                case VOpCode.PushC:
                {
                    PHY pushC(); PLY if (NC) { return; }
                }
                
                case VOpCode.IncNEXT:
                {
                    PHY incNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.DecNEXT:
                {
                    PHY decNEXT(); PLY if (NC) { return; }
                }
                
                case VOpCode.LongADD:
                {
                    PHY longADD(); PLY if (NC) { return; }
                }
                
                case VOpCode.GetNEXT:
                {
                    LDA [vcodeBuffer], Y // BP offset
                    INY
                    PHY getNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.PutNEXT:
                {
                    LDA [vcodeBuffer], Y // BP offset
                    INY
                    PHY putNEXT(); PLY if (NC) { return; }
                }
                default:
                {
Print.Hex();                    
                    LDA #Error.NotImplemented
                    Errors.ShowIDX();
loop { }                    
                    if (NC) { return; }
                }
            }

            CPY vcodeOffset
            if (Z) { break; }
        }
        STZ vcodeOffset
//LDA #'>' Print.Char();
        SEC
    }
    
    addVCode()
    {
        PHA
        PHX
        
        LDY vcodeOffset
        CPY #250
        if (C) // >= 250
        {
            Flush(); if (NC) { PLX PLA return; }
        }
        
        LDY vcodeOffset
        PLX
        TXA
        STA [vcodeBuffer], Y
        INC vcodeOffset
        
        PLA
        switch (X)
        {
            case VOpCode.GetNEXT:
            case VOpCode.PutNEXT:
            {
                // BP offset argument
                LDY vcodeOffset
                STA [vcodeBuffer], Y
                INC vcodeOffset
            }
        }
        
        SEC
    }
    
    PushNEXT()
    {
        LDX # VOpCode.PushNEXT
        addVCode();
    }
    PopNEXT()
    {
        LDX # VOpCode.PopNEXT
        addVCode();
    }
    PushTOP()
    {
        LDX # VOpCode.PushTOP
        addVCode();
    }
    PopTOP()
    {
        LDX # VOpCode.PopTOP
        addVCode();
    }
    PushC()
    {
        LDX # VOpCode.PushC
        addVCode();
    }
    
    IncNEXT()
    {
        LDX # VOpCode.IncNEXT
        addVCode();
    }
    DecNEXT()
    {
        LDX # VOpCode.DecNEXT
        addVCode();
    }
    LongADD()
    {
        LDX # VOpCode.LongADD
        addVCode();
    }
    
    GetNEXT() // A = BP offset
    {
        LDX # VOpCode.GetNEXT
        addVCode();
    }
    PutNEXT() // A = BP offset
    {
        LDX # VOpCode.PutNEXT
        addVCode();
    }
    
    
    // Generate code to push 32-bit value from ZP.NEXT onto runtime stack
    pushNEXT()
    {
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store NEXT0 to stack via pointer
        // LDA ZP.NEXT0
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store NEXT1 to stack via pointer
        // LDA ZP.NEXT1
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store NEXT2 to stack via pointer
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store NEXT3 to stack via pointer
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        Gen6502.emitByte(); if (NC) { return; }
        
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
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeBP
        Gen6502.emitByte(); if (NC) { return; }
        
        // Add the offset
        LDA #OpCode.CLC
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.ADC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        
        // Calculate and emit the adjusted offset value
        LDA ZP.TEMP
        // Now A contains either the original negative offset OR the adjusted positive offset
        Gen6502.emitByte(); if (NC) { return; }
        
        // Transfer result to Y
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to load ZP.NEXT from BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    getNEXT()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset();
            if (NC) 
            {
                break;
            }
                       
            // Load NEXT0 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            Gen6502.emitByte(); if (NC) { break; }
            
            // Load NEXT1 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            Gen6502.emitByte(); if (NC) { break; }
            
            // Load NEXT2 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack2
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            Gen6502.emitByte(); if (NC) { break; }
            
            // Load NEXT3 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack3
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            Gen6502.emitByte(); if (NC) { break; }
            
            SEC
            break;
        } // single exit
    }
    
    
    
    
    // Generate code to store ZP.NEXT at BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    putNEXT()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset();
            if (NC) 
            {
                break;
            }
            
            // Store NEXT0 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack0
            Gen6502.emitByte(); if (NC) { break; }
            
            // Store NEXT1 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y  // 0x91
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack1
            Gen6502.emitByte(); if (NC) { break; }
            
            // Store NEXT2 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack2
            Gen6502.emitByte(); if (NC) { break; }
            
            // Store NEXT3 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack3
            Gen6502.emitByte(); if (NC) { break; }
            
            SEC
            break;
        } // single exit
    }
    
    // Generate code to pop 32-bit value from stack into ZP.TOP
    popTOP()
    {
        LDA #OpCode.PLA
        Gen6502.emitByte(); if (NC) { return; }
        
        // SP points one past to the slot we are interested in
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        
        // Load TOP0 from stack via pointer
        // LDA [runtimeStack0],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.TOP0
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP0 
        Gen6502.emitByte(); if (NC) { return; }
        
        // Load TOP1 from stack via pointer
        // LDA [runtimeStack1],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.TOP1
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        Gen6502.emitByte(); if (NC) { return; }
                
        // Load TOP2 from stack via pointer
        // LDA [runtimeStack2],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.TOP2
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        Gen6502.emitByte(); if (NC) { return; }
        
        // Load TOP3 from stack via pointer
        // LDA [runtimeStack3],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.TOP3
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    
    // Generate code to push 32-bit value from ZP.TOP onto runtime stack
    pushTOP()
    {
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store TOP0 to stack via pointer
        // LDA ZP.TOP0
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP0
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store TOP1 to stack via pointer
        // LDA ZP.TOP1
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store TOP2 to stack via pointer
        // LDA ZP.TOP2
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store TOP3 to stack via pointer
        // LDA ZP.TOP3
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to pop 32-bit value from stack into ZP.NEXT
    popNEXT()
    {
        
        LDA #OpCode.PLA
        Gen6502.emitByte(); if (NC) { return; } 
        
        // SP points one past to the slot we are interested in
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        // Load NEXT0 from stack via pointer
        // LDA [runtimeStack0],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.NEXT0
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0 
        Gen6502.emitByte(); if (NC) { return; }
        
        // Load NEXT1 from stack via pointer
        // LDA [runtimeStack1],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.NEXT1
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
                
        // Load NEXT2 from stack via pointer
        // LDA [runtimeStack2],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.NEXT2
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        // Load NEXT3 from stack via pointer
        // LDA [runtimeStack3],Y
        LDA #OpCode.LDA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        // STA ZP.NEXT3
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to increment 32-bit value in NEXT0-3
    incNEXT()
    {
        // CLC (clear carry)
        LDA #OpCode.CLC
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT0
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        // ADC #1
        LDA #OpCode.ADC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #1
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT0
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT1
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        // ADC #0 (adds carry if any)
        LDA #OpCode.ADC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT1
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        // ADC #0
        LDA #OpCode.ADC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT2
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        
        // ADC #0
        LDA #OpCode.ADC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT3
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    // Generate code to decrement 32-bit value in NEXT0-3
    decNEXT()
    {
        // SEC (set carry for subtraction)
        LDA # OpCode.SEC
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT0
        LDA # OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        // SBC #1
        LDA # OpCode.SBC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #1
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT0
        LDA # OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT1
        LDA # OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        // SBC #0 (subtracts borrow if any)
        LDA #OpCode.SBC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT1
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        // SBC #0
        LDA #OpCode.SBC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT2
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        
        // SBC #0
        LDA #OpCode.SBC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // STA ZP.NEXT3
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    pushC()
    {
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        // Convert carry flag to 0 or 1
        // LDA #0
        LDA #OpCode.LDA_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // ADC #0 - adds carry flag to 0, giving 0 or 1
        LDA #OpCode.ADC_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store result (0 or 1) to stack via pointer
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #0
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store 0 to stack via pointer
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store 0 to stack via pointer
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        
        // Store 0 to stack via pointer
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    longADD()
    {
        loop
        {
            LDA #OpCode.LDX_IMM
            Gen6502.emitByte(); if (NC) { break; }
            LDA #BIOSInterface.SysCall.LongAdd
            Gen6502.emitByte(); if (NC) { break; }
            break;
        }
    }
    
}
