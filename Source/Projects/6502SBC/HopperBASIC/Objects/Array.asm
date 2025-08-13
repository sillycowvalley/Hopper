unit BASICArray
{
    // Array memory map:
    //   0000 number of elements
    //   xx   type = WORD|INT|BYTE|CHAR|BIT
    //   0000 first element in array
    //   ..
    //   <nn>  last element in array
    
    const uint aiCount    = 0;
    const uint aiType     = 2;
    const uint aiElements = 3;
    
    const byte[] bitMasks = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
                              0b00010000, 0b00100000, 0b01000000, 0b10000000 };
                              
    // element type in ACCT, number of elements ACC, array in IDX
    New()
    {
        loop
        {
            // preserve element count
            LDA ZP.ACCL   
            STA ZP.FLENGTHL
            LDA ZP.ACCH  
            STA ZP.FLENGTHH
            
            LDA ZP.ACCT
            switch (A)
            {
                case BASICType.BIT:
                {
                    // size = number of elements / 8 + 1
                    LDA # 0
                    STA ACARRY
                    LDA ZP.ACCL
                    AND # 0x07
                    if (NZ)
                    {
                        INC ACARRY
                    }
                    
                    LSR ZP.ACCH
                    ROR ZP.ACCL
                    LSR ZP.ACCH
                    ROR ZP.ACCL
                    LSR ZP.ACCH
                    ROR ZP.ACCL
                    CLC
                    LDA ZP.ACCL
                    ADC ACARRY
                    STA ZP.ACCL
                    if (C)
                    {
                        INC ZP.ACCH
                    }
                }
                case BASICType.CHAR:
                case BASICType.BYTE:
                {
                    // size = number of elements == size
                }
                default:
                {
                    // size = number of elements x 2
                    ASL ZP.ACCL
                    ROL ZP.ACCH
                }
            }
            
            // add 2 bytes for number of elements field and 1 byte for type of element field
            CLC
            LDA ZP.ACCL  // LSB
            ADC #3
            STA ACCL
            LDA ZP.ACCH  // MSB
            ADC #0
            STA ACCH
            
            // size is in ZP.ACC
            // return address in IDX
            Memory.Allocate();
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // Allocation failed
                Error.OutOfMemory(); BIT ZP.EmulatorPCL
                break;
            }
            
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            // zero initialize
            loop
            {
                LDA ZP.ACCL
                if (Z)
                {
                    LDA ZP.ACCH
                    if (Z)
                    {
                        break;
                    }
                }
                
                LDA # 0
                STA [ZP.FDESTINATIONADDRESS], Y
                IncDESTINATIONADDRESS;
                
                LDA ZP.ACCL
                if (Z)
                {
                    DEC ZP.ACCH
                }
                DEC ZP.ACCL
            }
            
            LDY # aiCount
            LDA ZP.FLENGTHK
            STA [IDX], Y
            INY
            LDA ZP.FLENGTHH
            STA [IDX], Y
            
            LDY # aiType
            LDA ZP.ACCT
            STA [IDX], Y
            SEC
            break;
        } // single exit
    }
       
    // Input:  array ptr in IDX
    // Output: element count in ACC
    GetCount()
    {
        LDY # aiCount
        LDA [IDX], Y
        STA ZP.ACCL
        INY
        LDA [IDX], Y
        STA ZP.ACCH
    }
    
    // Input:  array ptr in IDX
    // Output: element type in ACCT
    GetItemType()
    {
        Stacks.PopIDX(); // this
        
        LDY # aiType
        LDA [IDX], Y
        STA ZP.ACCT
    }
    
    // Input: array ptr in IDX, index in IDY, element type in ACCT
    // Output: element slot ptr in IDY and bit mask in X
    getIndexAndMask()
    {
        LDA ZP.ACCT
        switch (A)
        {
            case BASICType.BIT:
            {
                // capture the bit
                LDA IDYL
                AND # 0x07
                TAX
                
                // divide offset by 8
                LSR IDYH
                ROR IDYL
                LSR IDYH
                ROR IDYL
                LSR IDYH
                ROR IDYL
            }
            case BASICType.CHAR:
            case BASICType.BYTE:
            {
            }
            default:
            {
                // two byte elements : IDY << 1
                ASL IDYL
                ROL IDYH
            }
        }
        
        CLC
        LDA IDXL  // LSB
        ADC IDYL
        STA IDYL
        LDA IDXH  // MSB
        ADC IDYH
        STA IDYH
    }
    
    // Input: array ptr in IDX, index in IDY, element type in ACCT
    // Output: element int TOP
    GetItem()
    {
        PHA
        PHX
        PHY
        loop
        {
            // index < aiCount?
            LDY # aiCount+1
            LDA ZP.IDYH        // index MSB
            CMP [IDX], Y       // aiCount MSB
            if (Z)
            {
                DEY
                LDA ZP.IDYL    // index LSB
                CMP [IDX], Y   // aiCount LSB
            }
            if (C) // index < aiCount?
            {
                // index >= aiCount
                Error.BadIndex(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            LDY # aiType
            LDA [IDX], Y
            STA ZP.ACCT
            
            getIndexAndMask(); // returns index in IDY and bit # in X
                    
            LDY # aiElements
            LDA # 0
            STA ZP.TOPH
                          
            LDA ZP.ACCT
            switch (A)
            {
                case BASICType.BIT:
                {
                    LDA [IDY], Y           
                    AND bitMasks, X
                    if (Z)
                    {
                        STA ZP.TOPL
                    }
                    else
                    {
                        LDA # 1
                        STA ZP.TOPL   
                    }
                }
                case BASICType.CHAR:
                case BASICType.BYTE:
                {
                    LDA [IDY], Y
                    STA ZP.TOPL
                }
                default:
                {
                    LDA [IDY], Y
                    STA ZP.TOPL
                    INY
                    LDA [IDY], Y
                    STA ZP.TOPH
                }
            }      
            LDA ZP.ACCT
            SEC
            break;
        } // single exit
        PLY
        PLX
        PLA
    }
    // Input: array ptr in IDX, index in IDY, element in TOP
    // Output: element int TOP
    SetItem()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // index < aiCount?
            LDY # aiCount+1
            LDA ZP.IDYH        // index MSB
            CMP [IDX], Y       // aiCount MSB
            if (Z)
            {
                DEY
                LDA ZP.IDYL    // index LSB
                CMP [IDX], Y   // aiCount LSB
            }
            if (C) // index < aiCount?
            {
                Error.BadIndex(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            LDY # aiType
            LDA [IDX], Y
            STA ZP.ACCT
            
            getIndexAndMask(); // returns index in IDY and bit # in X
                    
            LDY # aiElements
            STZ ZP.NEXTH
                          
            LDA ZP.ACCT
            switch (A)
            {
                case BASICType.BIT:
                {
                    LDA ZP.TOPL
                    if (NZ)
                    {
                        // set the bit
                        LDA bitMasks, X
                        ORA [IDY], Y    
                        STA [IDY], Y
                    }
                    else
                    {
                        // clear the bit
                        LDA bitMasks, X
                        EOR # 0xFF
                        AND [IDY], Y    
                        STA [IDY], Y       
                    }
                }
                case BASICType.CHAR:
                case BASICType.BYTE:
                {
                    LDA ZP.TOPL
                    STA [IDY], Y
                }
                default:
                {
                    LDA ZP.TOPL
                    STA [IDY], Y
                    INY
                    LDA ZP.TOPH
                    STA [IDY], Y
                }
            } 
            SEC
            break;
        } // single exit     
        PLY
        PLX
        PLA
    }
}
    
