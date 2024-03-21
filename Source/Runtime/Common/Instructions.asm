unit Instruction
{
    enum Instructions
    {
        ENTER     = 0x49,
        
        CALL      = 0x34,
        CALLI     = 0x6A,
        
        RET       = 0x35,
        RETRES    = 0x36,
        
        JZ        = 0x31,
        JNZ       = 0x32,
        
        PUSHI     = 0x37,
        POPLOCAL  = 0x38,
        PUSHLOCAL = 0x39,
        
        ADD       = 0x80,
        
        EQ        = 0x92,
    }
    
    enum Types
    {
        Undefined,
        Char,   // char (for now)
        Int,    // 16 bit signed

        Byte,   // unsigned char
        UInt,   // internal type for unsigned 16 bit int (tFlags and tEnum)

        Reference,  // internal type for "ref" addresses (tUInt)

        Bool,
    }
    
    // return the width of the operand of the current opCode (in A), in A
    GetOperandWidth()
    {
        switch (A)
        {
            case Instructions.ENTER:
            case Instructions.ADD:
            case Instructions.EQ:
            {
                LDA #0
            }
        
            case Instructions.CALL:
            case Instructions.CALLI:

            case Instructions.RET:
            case Instructions.RETRES:

            case Instructions.JZ:
            case Instructions.JNZ:

            case Instructions.PUSHI:
            case Instructions.POPLOCAL:
            case Instructions.PUSHLOCAL:
            {
                LDA #2
            }
            default:
            {
                BRK // Not Implemented!
            }
        }
    }
    
    // Z set if next instruction is CALL, Z clear if not
    IsNextCALL()
    {
        // ACC = PC + CODESTART + instruction length
        GetNextAddress();
        
        // load next instruction into A
        LDA [ZP.ACC]
        
        CMP # Instructions.CALL
        if (NZ)
        {
            CMP # Instructions.CALLI
        }    
    }
    // load the current PC address into ACC (PC + CODESTART)
    GetCurrentAddress()
    {
        // add CODESTART offset
        CLC
        LDA ZP.PCL
        ADC ZP.CODESTARTL
        STA ZP.ACCL
        LDA ZP.PCH
        ADC ZP.CODESTARTH
        STA ZP.ACCH
    }
    
    // load the current PC address plus instruction length into ACC (PC + instruction length + CODESTART)
    GetNextAddress()
    {  
        // ACC = PC + CODESTART
        GetCurrentAddress();
        
        // load current instruction into A
        LDA [ZP.ACC]
        
        // get the width of the operand of the current opCode (in A), in A
        GetOperandWidth();
        
        INC A // add the opCode size to get instruction length
        
        // add current instruction length to current address
        CLC
        ADC ZP.ACCL
        STA ZP.ACCL
        LDA # 0
        ADC ZP.ACCH
        STA ZP.ACCH
    }
    
    // Resolve method index (IDX) to method address (IDX)
    //    munts: IDY, A and Y
    lookupMethod()
    {
        
        CLC
        LDA # (Address.HopperData & 0xFF)
        ADC # 6
        STA IDYL
        LDA # (Address.HopperData >> 8)
        ADC # 0
        STA IDYH
        
        loop
        {
            LDA [IDY]
            CMP IDXL
            if (Z)
            {
                LDY #1
                LDA [IDY], Y
                CMP IDXH
                if (Z)
                {
                    // we have a winner
                    INY
                    LDA [IDY], Y
                    STA IDXL
                    INY
                    LDA [IDY], Y
                    STA IDXH
                    RTS
                }
            }
        
            CLC
            LDA IDYL
            ADC #4
            STA IDXL
            if (NC) { continue; }
            INC IDXH
        }
    }
    
    // load the operand into IDX, increment the PC by 2
    consumeOperand()
    {
        Utilities.IncACC();
        
        LDY #1
        LDA [ZP.ACC]
        STA ZP.IDXL
        LDA [ZP.ACC], Y
        STA ZP.IDXH
        
        Utilities.IncPC();
        Utilities.IncPC();
    }
    
       
    enter()
    {
        Stacks.PushBP();
        LDA ZP.SP
        STA ZP.BP
    }
    
    add()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        CLC
        LDA ZP.NEXTL
        ADC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ADC ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    
    eq()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        LDA ZP.NEXTL
        CMP ZP.TOPL
        if (Z)
        {
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDA # 1
                Stacks.PushBool();
                return;
            }
        }
        LDA # 0
        Stacks.PushBool();
    }
    
    ret()
    {
        consumeOperand();
        
        // SP -= IDX
        SEC
        LDA ZP.SP
        SBC ZP.IDXL
        STA ZP.SP
        
        Stacks.PopBP();
        LDA ZP.CSP
        if (Z)
        {
            // PC = Address.InvalidAddress; // exit program
            LDA #(Address.InvalidAddress & 0xFF)
            STA ZP.PCL
            LDA #(Address.InvalidAddress >> 8)
            STA ZP.PCH
        }
        else
        {
            Stacks.PopPC();
        }
    }
    
    retRes()
    {
        consumeOperand();
        Stacks.PopTop();
        
        // SP -= IDX
        SEC
        LDA ZP.SP
        SBC ZP.IDXL
        STA ZP.SP
        
        Stacks.PushTop();
        
        Stacks.PopBP();
        
        LDA ZP.CSP
        if (Z)
        {
            // PC = Address.InvalidAddress; // exit program
            LDA #(Address.InvalidAddress & 0xFF)
            STA ZP.PCL
            LDA #(Address.InvalidAddress >> 8)
            STA ZP.PCH
        }
        else
        {
            Stacks.PopPC();
        }
    }
    
       
    jz()
    {
        Stacks.PopTop();
        LDA ZP.TOPL
        if (Z)
        {
            LDA ZP.TOPH
            if (Z)
            {
                consumeOperand();
                // PC += offset - 3
                CLC
                LDA ZP.PCL
                ADC ZP.IDXL
                STA ZP.PCL
                LDA ZP.PCH
                ADC ZP.IDXH
                STA ZP.PCH
                SEC
                LDA ZP.PCL
                SBC # 3
                STA ZP.PCL
                LDA ZP.PCH
                SBC # 0
                STA ZP.PCH
                return;
            }
        }
        // skip operand
        Utilities.IncPC();
        Utilities.IncPC();
    }
    jnz()
    {
        Stacks.PopTop();
        LDA ZP.TOPL
        if (Z)
        {
            LDA ZP.TOPH
            if (NZ)
            {
                consumeOperand();
                // PC += offset - 3
                CLC
                LDA ZP.PCL
                ADC ZP.IDXL
                STA ZP.PCL
                LDA ZP.PCH
                ADC ZP.IDXH
                STA ZP.PCH
                SEC
                LDA ZP.PCL
                SBC # 3
                STA ZP.PCL
                LDA ZP.PCH
                SBC # 0
                STA ZP.PCH
                return;
            }
        }
        // skip operand
        Utilities.IncPC();
        Utilities.IncPC();
    }
    
    pushI()
    {
        consumeOperand();
        LDA ZP.IDXL
        STA ZP.TOPL
        LDA ZP.IDXH
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    pushLocal()
    {
        consumeOperand();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        LDA Address.ValueStackLSB, Y
        STA ZP.TOPL
        LDA Address.ValueStackMSB, Y
        STA ZP.TOPH
        LDA Address.TypeStackLSB, Y
        STA ZP.TOPT
     
        PushTop();   
    }
    popLocal()
    {
        consumeOperand();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
                
        PopTop();
        
        LDA ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA ZP.TOPH
        STA Address.ValueStackMSB, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
    }
    
    call()
    {
        // change CALL to CALLI
        LDA #Instructions.CALLI
        STA [ZP.ACC]
        
        consumeOperand();
        PushPC();
        
        // resolve index (IDX) to address (IDX)
        lookupMethod();
        
        // address store back to [ACC] and PC
        LDY #1
        LDA ZP.IDXL
        STA [ZP.ACC]
        STA ZP.PCL
        LDA ZP.IDXH
        STA [ZP.ACC], Y
        STA ZP.PCH
    }
    
    callI()
    {
        consumeOperand();
        Stacks.PushPC();
        
        // IDX -> PC
        LDA ZP.IDXL
        STA ZP.PCL
        LDA ZP.IDXH
        STA ZP.PCH
    }
      
    Execute()
    {
        LDA # (InvalidAddress & 0xFF) // assume that MSB and LSB of InvalidAddress are the same
        CMP PCH
        if (Z)
        {
            CMP PCL
            if (Z)
            {
                return; // end of program run
            }
        }
        
        // ACC = PC + CODESTART
        GetCurrentAddress();
        
        // load current instruction into A
        LDA [ZP.ACC]
        
        Utilities.IncPC();
        
        switch (A)
        {
            case Instructions.ENTER:
            {
                enter();
            }
            case Instructions.PUSHI:
            {
                pushI();
            }
            case Instructions.PUSHLOCAL:
            {
                pushLocal();
            }
            case Instructions.POPLOCAL:
            {
                popLocal();
            }
            case Instructions.ADD:
            {
                add();
            }
            case Instructions.EQ:
            {
                eq();
            }
            case Instructions.JZ:
            {
                jz();
            }
            case Instructions.JNZ:
            {
                jnz();
            }
            case Instructions.CALL:
            {
                call();
            }
            case Instructions.CALLI:
            {
                callI();
            }
            case Instructions.RET:
            {
                ret();
            }
            case Instructions.RETRES:
            {
                retRes();
            }
            default:
            {
                BRK // Not Implemented!
            }
        }
    }
}
