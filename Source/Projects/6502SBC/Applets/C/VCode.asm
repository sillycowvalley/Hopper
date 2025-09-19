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
    
    const byte peep0           = vzSlots+6;
    const byte peep1           = vzSlots+7;
    const byte peep2           = vzSlots+8;
    const byte peep3           = vzSlots+9;
    const byte peep4           = vzSlots+10;
    
    const byte vzOffset        = vzSlots+11;
    const byte vzArgument      = vzOffset;
    const byte vzArgument1     = vzSlots+12;
    
    enum VOpCode
    {
        None            = 0x00,
        PushNEXT        = 0x01,
        PushTOP         = 0x02,
        PopNEXT         = 0x03,
        PopTOP          = 0x04,
        PutNEXT         = 0x05,
        GetNEXT         = 0x06,
        IncNEXT         = 0x07,
        DecNEXT         = 0x08,
        PushC           = 0x09,
        LongADD         = 0x0A,
        
        Discard         = 0x0B,
        
        Inc             = 0x0C,               // GetNEXT[BP+offset] + PushNEXT + IncNEXT + PutNEXT[BP+offset] + Discard -> Inc[BP+offset]
        
        NEXTtoTOP       = 0x0D,
        TOPtoNEXT       = 0x0E,
        CtoNEXT         = 0x0F,
        PushCHAR        = 0x10,
        CHARtoNEXT      = 0x11,
        
        GetTOP          = 0x12,
        PutTOP          = 0x13,
        Reserve         = 0x14,
        NEXTZero        = 0x15,
        PushWORD        = 0x16,
        CtoONE          = 0x17,
        WORDtoTOP       = 0x18,
        LoadNEXT        = 0x19,
        StoreNEXT       = 0x1A,

    }
    const string strNone = "Undefined";
    const string strPushNEXT = "pshN";
    const string strPushTOP = "pshT";
    const string strPopNEXT = "popN";
    const string strPopTOP = "popT";
    const string strPutNEXT = "putN";
    const string strPutTOP = "putT";
    const string strGetNEXT = "getN";
    const string strGetTOP = "getT";
    const string strIncNEXT = "incN";
    const string strDecNEXT = "decN";
    const string strPushC = "pshC";
    const string strLongADD = "lAdd";
    const string strDiscard = "Disc";
    const string strReserve = "Rsrv";
    const string strInc = "Inc";
    const string strNEXTtoTOP = "NtoT";
    const string strTOPtoNEXT = "TtoN";
    const string strCtoNEXT = "CtoN";
    const string strPushCHAR = "pshCHR";
    const string strPushWORD = "pshW";
    const string strCHARtoNEXT = "CHRtoN";
    const string strNEXTZero = "Nzer";
    const string strCtoONE   = "Cto1";
    const string strWORDtoTOP = "WtoT";
    const string strLoadNEXT = "ldN";
    const string strStoreNEXT = "stN";
    
    printPeep()
    {
        switch (A)
        {
            case VOpCode.LoadNEXT:
            {
                LDA #(strLoadNEXT % 256)
                STA ZP.STRL
                LDA #(strLoadNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.StoreNEXT:
            {
                LDA #(strStoreNEXT % 256)
                STA ZP.STRL
                LDA #(strStoreNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.WORDtoTOP:
            {
                LDA #(strWORDtoTOP % 256)
                STA ZP.STRL
                LDA #(strWORDtoTOP / 256)
                STA ZP.STRH
            }
            case VOpCode.CtoONE:
            {
                LDA #(strCtoONE % 256)
                STA ZP.STRL
                LDA #(strCtoONE / 256)
                STA ZP.STRH
            }
            case VOpCode.PushNEXT:
            {
                LDA #(strPushNEXT % 256)
                STA ZP.STRL
                LDA #(strPushNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.PushTOP:
            {
                LDA #(strPushTOP % 256)
                STA ZP.STRL
                LDA #(strPushTOP / 256)
                STA ZP.STRH
            }
            case VOpCode.PopNEXT:
            {
                LDA #(strPopNEXT % 256)
                STA ZP.STRL
                LDA #(strPopNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.PopTOP:
            {
                LDA #(strPopTOP % 256)
                STA ZP.STRL
                LDA #(strPopTOP / 256)
                STA ZP.STRH
            }
            case VOpCode.PutNEXT:
            {
                LDA #(strPutNEXT % 256)
                STA ZP.STRL
                LDA #(strPutNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.PutTOP:
            {
                LDA #(strPutTOP % 256)
                STA ZP.STRL
                LDA #(strPutTOP / 256)
                STA ZP.STRH
            }
            case VOpCode.GetNEXT:
            {
                LDA #(strGetNEXT % 256)
                STA ZP.STRL
                LDA #(strGetNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.GetTOP:
            {
                LDA #(strGetTOP % 256)
                STA ZP.STRL
                LDA #(strGetTOP / 256)
                STA ZP.STRH
            }
            case VOpCode.IncNEXT:
            {
                LDA #(strIncNEXT % 256)
                STA ZP.STRL
                LDA #(strIncNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.DecNEXT:
            {
                LDA #(strDecNEXT % 256)
                STA ZP.STRL
                LDA #(strDecNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.PushC:
            {
                LDA #(strPushC % 256)
                STA ZP.STRL
                LDA #(strPushC / 256)
                STA ZP.STRH
            }
            case VOpCode.LongADD:
            {
                LDA #(strLongADD % 256)
                STA ZP.STRL
                LDA #(strLongADD / 256)
                STA ZP.STRH
            }
            case VOpCode.Discard:
            {
                LDA #(strDiscard % 256)
                STA ZP.STRL
                LDA #(strDiscard / 256)
                STA ZP.STRH
            }
            case VOpCode.Reserve:
            {
                LDA #(strReserve % 256)
                STA ZP.STRL
                LDA #(strReserve / 256)
                STA ZP.STRH
            }
            case VOpCode.NEXTZero:
            {
                LDA #(strNEXTZero % 256)
                STA ZP.STRL
                LDA #(strNEXTZero / 256)
                STA ZP.STRH
            }
            case VOpCode.Inc:
            {
                LDA #(strInc % 256)
                STA ZP.STRL
                LDA #(strInc / 256)
                STA ZP.STRH
            }
            case VOpCode.NEXTtoTOP:
            {
                LDA #(strNEXTtoTOP % 256)
                STA ZP.STRL
                LDA #(strNEXTtoTOP / 256)
                STA ZP.STRH
            }
            case VOpCode.TOPtoNEXT:
            {
                LDA #(strTOPtoNEXT % 256)
                STA ZP.STRL
                LDA #(strTOPtoNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.CtoNEXT:
            {
                LDA #(strCtoNEXT % 256)
                STA ZP.STRL
                LDA #(strCtoNEXT / 256)
                STA ZP.STRH
            }
            case VOpCode.PushWORD:
            {
                LDA #(strPushWORD % 256)
                STA ZP.STRL
                LDA #(strPushWORD / 256)
                STA ZP.STRH
            }
            case VOpCode.PushCHAR:
            {
                LDA #(strPushCHAR % 256)
                STA ZP.STRL
                LDA #(strPushCHAR / 256)
                STA ZP.STRH
            }
            case VOpCode.CHARtoNEXT:
            {
                LDA #(strCHARtoNEXT % 256)
                STA ZP.STRL
                LDA #(strCHARtoNEXT / 256)
                STA ZP.STRH
            }
            default:
            {
                LDA #(strNone % 256)
                STA ZP.STRL
                LDA #(strNone / 256)
                STA ZP.STRH
            }
        }
        Print.String();
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
        
        clearPeeps();
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
        PHA
        LDA vcodeOffset
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    Flush()
    {
        IsEmpty();
        if (C) { return; }
        
        LDY #0
        loop
        {
   
#ifdef DEBUG
PHY
LDA # OpCode.NOP  
Gen6502.emitByte(); SEC PLY
#endif
            
            LDA [vcodeBuffer], Y
            INY
            
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
                case VOpCode.Discard:
                {
                    PHY discard(); PLY if (NC) { return; }
                }
                case VOpCode.Reserve:
                {
                    PHY reserveSlot(); PLY if (NC) { return; }
                }
                case VOpCode.NEXTZero:
                {
                    PHY nextZero(); PLY if (NC) { return; }
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
                case VOpCode.GetTOP:
                {
                    LDA [vcodeBuffer], Y // BP offset
                    INY
                    PHY getTOP(); PLY if (NC) { return; }
                }
                case VOpCode.PutNEXT:
                {
                    LDA [vcodeBuffer], Y // BP offset
                    INY
                    PHY putNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.PutTOP:
                {
                    LDA [vcodeBuffer], Y // BP offset
                    INY
                    PHY putTOP(); PLY if (NC) { return; }
                }
                case VOpCode.LoadNEXT:
                {
                    LDA [vcodeBuffer], Y // offset
                    INY
                    PHY loadNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.StoreNEXT:
                {
                    LDA [vcodeBuffer], Y // offset
                    INY
                    PHY storeNEXT(); PLY if (NC) { return; }
                }
                
                case VOpCode.PushCHAR:
                {
                    LDA [vcodeBuffer], Y // char argument
                    INY
                    PHY pushCHAR(); PLY if (NC) { return; }
                }
                case VOpCode.CHARtoNEXT:
                {
                    LDA [vcodeBuffer], Y // char argument
                    INY
                    PHY CHARtoNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.Inc:
                {
                    LDA [vcodeBuffer], Y // BP offset
                    INY
                    PHY inc(); PLY if (NC) { return; }
                }
                
                case VOpCode.PushWORD:
                {
                    LDA [vcodeBuffer], Y // word argument LSB -> A
                    PHA
                    INY
                    LDA [vcodeBuffer], Y // word argument MSB -> X                 
                    INY
                    TAX
                    PLA
                    PHY pushWORD(); PLY if (NC) { return; }
                }
                case VOpCode.WORDtoTOP:
                {
                    LDA [vcodeBuffer], Y // word argument LSB -> A
                    PHA
                    INY
                    LDA [vcodeBuffer], Y // word argument MSB -> X                 
                    INY
                    TAX
                    PLA
                    PHY WORDtoTOP(); PLY if (NC) { return; }
                }
                
                
                
                case VOpCode.NEXTtoTOP:
                {
                    PHY NEXTtoTOP(); PLY if (NC) { return; }
                }
                case VOpCode.TOPtoNEXT:
                {
                    PHY TOPtoNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.CtoNEXT:
                {
                    PHY CtoNEXT(); PLY if (NC) { return; }
                }
                case VOpCode.CtoONE:
                {
                    PHY CtoONE(); PLY if (NC) { return; }
                }
                
                default:
                {
#ifdef DEBUG
Print.Hex(); LDA #'v' Print.Char();LDA #'c' Print.Char();
#endif
                    LDA #Error.NotImplemented
                    Errors.ShowIDX();
loop { }                    
                    if (NC) { return; }
                }
            }
#ifdef DEBUG
PHY
LDA #OpCode.NOP  
Gen6502.emitByte(); SEC PLY
#endif

            CPY vcodeOffset
            if (Z) { break; }
        }
        STZ vcodeOffset
        clearPeeps();
        SEC
    }
    clearPeeps()
    {
        STZ peep0
        STZ peep1
        STZ peep2
        STZ peep3
        STZ peep4
    }
    
    pushPeep() // A
    {
        PHA
        LDA peep3
        STA peep4
        LDA peep2
        STA peep3
        LDA peep1
        STA peep2
        LDA peep0
        STA peep1
        PLA
        STA peep0
    }
    
    popPeep()
    {
        LDA peep1
        STA peep0
        LDA peep2
        STA peep1
        LDA peep3
        STA peep2
        LDA peep4
        STA peep3
        STZ peep4
    }
    
    
    dumpPeeps()
    {
        CPX #0
        if (NZ)
        {
            Print.Space(); LDA #'>' Print.Char(); Print.Space();
        }
        
        LDA peep0
        if (NZ)
        {
            LDA peep1
            if (NZ) // at least two
            {
                CPX #0
                if (Z)
                {
                    Print.NewLine();
                }
                
                LDA peep4
                if (NZ)
                {
                    printPeep(); Print.Space();
                }
                
                LDA peep3
                if (NZ)
                {
                    printPeep(); Print.Space();
                }
                
                LDA peep2
                if (NZ)
                {
                    printPeep(); Print.Space();
                }
                
                LDA peep1
                if (NZ)
                {
                    printPeep(); Print.Space();
                }
                
                LDA peep0
                printPeep(); Print.Space();
            }
        }
    }
    
    // X = number of laps
    tryPeeps()
    {
#ifdef DEBUG
        dumpPeeps();
#endif  
        loop
        {
            LDA peep0
            switch (A)
            {
                case VOpCode.Discard:
                {
                    LDA peep1
                    switch (A)
                    {
                        case VOpCode.PushNEXT:
                        {
                            // PushNEXT, Discard -> NOP
                            DEC vcodeOffset
                            DEC vcodeOffset
                            popPeep();
                            popPeep();
#ifdef DEBUG
Print.Space(); LDA #'A' Print.Char();
#endif                            
                            SEC
                            break;
                        }
                        case VOpCode.PutNEXT:
                        {
                            LDA peep2
                            switch (A)
                            {
                                case VOpCode.IncNEXT:
                                {
                                    LDA peep3
                                    switch (A)
                                    {
                                        case VOpCode.PushNEXT:
                                        {
                                            LDA peep4
                                            switch (A)
                                            {
                                                case VOpCode.GetNEXT:
                                                {
#ifdef PROBLEMPEEPS
                                                    // TODO: GetNEXT side effect lost (NEXT no longer has value from original GetNEXT)
                                                    // GetNEXT[BP+offset] + PushNEXT + IncNEXT + PutNEXT[BP+offset] + Discard -> Inc[BP+offset]                                                    
                                                    //                      1 byte     1 byte    2 bytes              1 byte     = 5 bytes to remove
                                                    //
                                                    // decrement vcodeOffset by 5
                                                    // then modify [vcodeOffset-2]
                                                    
                                                    clearPeeps();
                                                    SEC
                                                    LDA vcodeOffset
                                                    SBC #5
                                                    STA vcodeOffset
                                                    LDY vcodeOffset
                                                    DEY
                                                    DEY
                                                    LDA # VOpCode.Inc
                                                    STA [vcodeBuffer], Y
                                                    pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'G' Print.Char(); LDA #'!' Print.Char();
#endif                                               
                                                    SEC
                                                    break;   
#endif                                                       
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                case VOpCode.PopNEXT:
                {
                    LDA peep1
                    switch (A)
                    {
                        case VOpCode.GetTOP:
                        {
                            LDA peep2
                            switch (A)
                            {
                                case VOpCode.PushNEXT:
                                {     
                                    // PushNEXT, GetTOP, PopNEXT -> PutTOP
                                    popPeep();
                                    popPeep();
                                    popPeep();
                                    DEC vcodeOffset
                                    DEC vcodeOffset
                                    LDY vcodeOffset
                                    LDA  [vcodeBuffer], Y // PutTOP offset
                                    DEY
                                    STA  [vcodeBuffer], Y
                                    DEY
                                    LDA # VOpCode.GetTOP
                                    STA  [vcodeBuffer], Y
                                    pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'L' Print.Char();
#endif                            
                                    SEC
                                    break;
                                }
                            }
                        }
                        case VOpCode.PushNEXT:
                        {
                            // PushNEXT, PopNEXT -> NOP
                            DEC vcodeOffset
                            DEC vcodeOffset
                            popPeep();
                            popPeep();
#ifdef DEBUG
Print.Space(); LDA #'B' Print.Char();
#endif                            
                            SEC
                            break;
                        }
                        case VOpCode.PushC:
                        {
                            // PushC, PopNEXT -> CtoNEXT
                            popPeep();
                            popPeep();
                            DEC vcodeOffset // PopNEXT
                            LDY vcodeOffset
                            DEY
                            LDA # VOpCode.CtoNEXT
                            STA [vcodeBuffer], Y
                            pushPeep();

#ifdef DEBUG
Print.Space(); LDA #'C' Print.Char();
#endif                            
                            SEC
                            break;
                        }
                        case VOpCode.PushCHAR:
                        {
                            // PushCHAR, PopNEXT -> CHARtoNEXT
                            popPeep();
                            popPeep();
                            DEC vcodeOffset // PopNEXT
                            LDY vcodeOffset
                            DEY // char argument
                            DEY 
                            LDA # VOpCode.CHARtoNEXT
                            STA [vcodeBuffer], Y
                            pushPeep();

#ifdef DEBUG
Print.Space(); LDA #'D' Print.Char();
#endif                            
                            SEC
                            break;
                        }
                        case VOpCode.PushTOP:
                        {
                            // PushTOP, PopNEXT -> TOPtoNEXT
                            popPeep();
                            popPeep();
                            DEC vcodeOffset
                            LDY vcodeOffset
                            DEY
                            LDA # VOpCode.TOPtoNEXT
                            STA [vcodeBuffer], Y
                            pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'E' Print.Char();
#endif
                            SEC
                            break;
                        }
                    }
                }
                case VOpCode.NEXTtoTOP:
                {
                    LDA peep1
                    switch (A)
                    {
                        case VOpCode.GetNEXT:
                        {
#ifdef PROBLEMPEEPS
                            // TODO: GetNEXT side effect lost (NEXT no longer has value from GetNEXT)
                            // GetNEXT, NEXTtoTOP -> GetTOP
                            popPeep();
                            popPeep();
                            DEC vcodeOffset
                            LDY vcodeOffset
                            DEY // BP argument
                            DEY
                            LDA # VOpCode.GetTOP
                            STA [vcodeBuffer], Y
                            pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'H' Print.Char();LDA #'!' Print.Char();
#endif                            
                            SEC
                            break;
#endif                  
                        }
                    }
                }
                case VOpCode.PutNEXT:
                {
                    LDA peep1
                    switch (A)
                    {
                        case VOpCode.TOPtoNEXT:
                        {
/*                            
                            // TODO: TOPtoNEXT side effect lost (NEXT no longer has value from TOP)
                            // DEFECT: see (c = fgetc(fp)) != -1
                            // TOPtoNEXT, PutNEXT -> PutTOP
                            popPeep();
                            popPeep();
                            DEC vcodeOffset
                            LDY vcodeOffset
                            LDA [vcodeBuffer], Y // BP argument 
                            DEY 
                            STA [vcodeBuffer], Y // BP argument 
                            DEY
                            LDA # VOpCode.PutTOP
                            STA [vcodeBuffer], Y
                            pushPeep();
*/                            
#ifdef DEBUG
Print.Space(); LDA #'I' Print.Char();LDA #'x' Print.Char(); // missed opportunity?
#endif                            
/*
                            SEC
                            break;
*/                            
                        }
                    }
                }
                case VOpCode.PopTOP:
                {
                    LDA peep1
                    switch (A)
                    {
                        case VOpCode.PushNEXT:
                        {
                            // PushNEXT, PopTOP -> NEXTtoTOP
                            popPeep();
                            popPeep();
                            DEC vcodeOffset
                            LDY vcodeOffset
                            DEY
                            LDA # VOpCode.NEXTtoTOP
                            STA [vcodeBuffer], Y
                            pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'F' Print.Char();
#endif                            
                            SEC
                            break;
                        }
                        case VOpCode.PushWORD:
                        {
                            // PushWORD, PopTOP -> WORDtoTOP
                            popPeep();
                            popPeep();
                            DEC vcodeOffset // PopTOP
                            LDY vcodeOffset
                            DEY // MSB argument
                            DEY // LSB argument
                            DEY 
                            LDA # VOpCode.WORDtoTOP
                            STA [vcodeBuffer], Y
                            pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'K' Print.Char();
#endif                            
                            SEC
                            break;
                        }
                    }
                    
                }
                case VOpCode.NEXTZero:
                {
                    LDA peep1
                    switch (A)
                    {
                        case VOpCode.CtoNEXT:
                        {
#ifdef PROBLEMPEEPS
                            // TODO: CtoNEXT side effect lost (NEXT no longer has value from CtoNEXT)
                            // CtoNEXT, NEXTZero -> CtoONE
                            popPeep();
                            popPeep();
                            DEC vcodeOffset
                            LDY vcodeOffset
                            DEY
                            LDA # VOpCode.CtoONE
                            STA [vcodeBuffer], Y
                            pushPeep();
#ifdef DEBUG
Print.Space(); LDA #'J' Print.Char();LDA #'!' Print.Char();
#endif                            
                            SEC
                            break;
#endif                            
                        }
                    }
                }
            }
            
            CLC
            break;
        } // single exit
    }
    
    addVCode()
    {
        PHY // possible MSB
        PHA // BP offset or LSB
        PHX // VOpCode
        
        LDY vcodeOffset
        CPY #250
        if (C) // >= 250
        {
            Flush(); if (NC) { PLX PLA PLY return; }
        }
        
        LDY vcodeOffset
        PLX // VOpCode
        
        LDY vcodeOffset
        TXA
        STA [vcodeBuffer], Y
        pushPeep(); // A -> peep0
        INC vcodeOffset
        
        PLA // BP offset or LSB
        PLY // possible MSB
        switch (X)
        {
            case VOpCode.GetNEXT:
            case VOpCode.GetTOP:
            case VOpCode.PutNEXT:
            case VOpCode.PutTOP:
            case VOpCode.LoadNEXT:
            case VOpCode.StoreNEXT:
            {
                // offset argument
                LDY vcodeOffset
                STA [vcodeBuffer], Y
                INC vcodeOffset
            }
            case VOpCode.PushCHAR:
            {
                // char argument
                LDY vcodeOffset
                STA [vcodeBuffer], Y
                INC vcodeOffset
            }
            case VOpCode.PushWORD:
            {
                // word argument
                PHY // MSB
                LDY vcodeOffset
                STA [vcodeBuffer], Y // LSB
                INC vcodeOffset
                PLA // MSB
                LDY vcodeOffset
                STA [vcodeBuffer], Y // MSB
                INC vcodeOffset
            }
        }
#ifdef PEEPHOLE        
        LDX #0
        loop
        {  
            PHX
            tryPeeps();
            PLX
            if (NC)
            {
                break; // no more optimizations
            }
            INX
        }
#endif        
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
    Discard()
    {
        LDX # VOpCode.Discard
        addVCode();
    }
    Reserve()
    {
        LDX # VOpCode.Reserve
        addVCode();
    }
    NEXTZero()
    {
        LDX # VOpCode.NEXTZero
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
    GetTOP() // A = BP offset
    {
        LDX # VOpCode.GetTOP
        addVCode();
    }
    PutNEXT() // A = BP offset
    {
        LDX # VOpCode.PutNEXT
        addVCode();
    }
    PutTOP() // A = BP offset
    {
        LDX # VOpCode.PutTOP
        addVCode();
    }
    PushCHAR() // A = char
    {
        LDX # VOpCode.PushCHAR
        addVCode();
    }
    LoadNEXT() // A = offset
    {
        LDX # VOpCode.LoadNEXT
        addVCode();
    }
    StoreNEXT() // A = offset
    {
        LDX # VOpCode.StoreNEXT
        addVCode();
    }
    
    
    PushWORD() // A = LSB, X = MSB
    {
        PHX
        PLY

        LDX # VOpCode.PushWORD
        addVCode(); // A = LSB, Y = MSB
    }
    CtoONE()
    {
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
    }
    CtoNEXT()
    {
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
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
    }
    
    NEXTtoTOP()
    {
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        Gen6502.emitByte(); if (NC) { return; }
    }
    TOPtoNEXT()
    {
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP0
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP1
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP2
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.LDA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.TOP3
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
    }
    
    CHARtoNEXT() // A = char argument
    {
        STA vzArgument
        
        LDA vzArgument
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            Gen6502.emitByte(); if (NC) { return; }    
        }
        else
        {
            LDA #OpCode.LDA_IMM
            Gen6502.emitByte(); if (NC) { return; }
            LDA vzArgument
            Gen6502.emitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { return; }
        }
        LDA # ZP.NEXT0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT1
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT2
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT3
        Gen6502.emitByte(); if (NC) { return; }
    }
    
    WORDtoTOP() // A = LSB, X = MSB
    {
        STA vzArgument
        STX vzArgument1
               
        LDA vzArgument
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            Gen6502.emitByte(); if (NC) { return; }    
        }
        else
        {
            LDA #OpCode.LDA_IMM
            Gen6502.emitByte(); if (NC) { return; }
            LDA vzArgument
            Gen6502.emitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { return; }
        }
        LDA # ZP.TOP0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA vzArgument1
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            Gen6502.emitByte(); if (NC) { return; }    
        }
        else
        {
            LDA #OpCode.LDA_IMM
            Gen6502.emitByte(); if (NC) { return; }
            LDA vzArgument1
            Gen6502.emitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { return; }
        }
        LDA # ZP.TOP1
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.TOP2
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.TOP3
        Gen6502.emitByte(); if (NC) { return; }
    }
    
    pushWORD() // A = LSB, X = MSB
    {
        STA vzArgument
        STX vzArgument1
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        
        LDA #OpCode.LDA_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA vzArgument
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA vzArgument
        CMP vzArgument1
        if (NZ)
        {
            LDA #OpCode.LDA_IMM
            Gen6502.emitByte(); if (NC) { return; }
            LDA vzArgument1
            Gen6502.emitByte(); if (NC) { return; }
        }
        
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA vzArgument1
        if (NZ)
        {
            LDA #OpCode.LDA_IMM
            Gen6502.emitByte(); if (NC) { return; }
            LDA #0
            Gen6502.emitByte(); if (NC) { return; }
        }
        
        // STZ [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        
        // STZ [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
    }
    
    pushCHAR() // A = char argument
    {
        STA vzArgument
        
        // SP -> X -> Y
        LDA #OpCode.TSX  
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        Gen6502.emitByte(); if (NC) { return; }
        
        
        LDA #OpCode.LDA_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA vzArgument
        Gen6502.emitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack0
        Gen6502.emitByte(); if (NC) { return; }
        
        LDA vzArgument
        if (NZ)
        {
            LDA #OpCode.LDA_IMM
            Gen6502.emitByte(); if (NC) { return; }
            LDA #0
            Gen6502.emitByte(); if (NC) { return; }
        }
        
        
        // STZ [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack1
        Gen6502.emitByte(); if (NC) { return; }
        
        // STZ [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack2
        Gen6502.emitByte(); if (NC) { return; }
        
        // STZ [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        Gen6502.emitByte(); if (NC) { return; }
        LDA # Gen6502.runtimeStack3
        Gen6502.emitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        Gen6502.emitByte(); if (NC) { return; }
        
        SEC
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
        STA vzOffset  // Save logical offset
        
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
        LDA vzOffset
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
    
    // Generate code to load ZP.TOP from BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    getTOP()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset();
            if (NC) 
            {
                break;
            }
                       
            // Load TOP0 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP0
            Gen6502.emitByte(); if (NC) { break; }
            
            // Load TOP1 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP1
            Gen6502.emitByte(); if (NC) { break; }
            
            // Load TOP2 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack2
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP2
            Gen6502.emitByte(); if (NC) { break; }
            
            // Load TOP3 through pointer
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack3
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP3
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
    
    // Generate code to store ZP.TOP at BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    putTOP()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset();
            if (NC) 
            {
                break;
            }
            
            // Store TOP0 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack0
            Gen6502.emitByte(); if (NC) { break; }
            
            // Store TOP1 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y  // 0x91
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack1
            Gen6502.emitByte(); if (NC) { break; }
            
            // Store TOP2 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP2
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack2
            Gen6502.emitByte(); if (NC) { break; }
            
            // Store TOP3 through pointer
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.TOP3
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
    
    discard()
    {
        LDA #OpCode.PLA  
        Gen6502.emitByte(); if (NC) { return; }
        SEC
    }
    reserveSlot()
    {
        // Allocate stack space for the variable - just push a dummy value
        LDA #OpCode.PHA  
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
        
        // NEXT = NEXT + TOP
        LDA # OpCode.CLC    Gen6502.emitByte(); if (NC) { return; }
        LDA # OpCode.LDA_ZP Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT0      Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.ADC_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.TOP0       Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT0      Gen6502.emitByte(); if (NC) { return; }
        LDA # OpCode.LDA_ZP Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT1      Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.ADC_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.TOP1       Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT1      Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.LDA_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT2      Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.ADC_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.TOP2       Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT2      Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.LDA_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT3      Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.ADC_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.TOP3       Gen6502.emitByte(); if (NC) { return; }
        LDA #OpCode.STA_ZP  Gen6502.emitByte(); if (NC) { return; }
        LDA # ZP.NEXT3      Gen6502.emitByte(); if (NC) { return; }
        
        SEC
        /*
        
        LDA #OpCode.LDX_IMM
        Gen6502.emitByte(); if (NC) { return; }
        LDA #BIOSInterface.SysCall.LongAdd
        Gen6502.emitByte(); if (NC) { return; }
    
        // Emit: JSR dispatch
        //Library.EmitDispatchCall();
        
        LDA # OpCode.JSR
        Gen6502.emitByte(); if (NC) { return; }
        
        // Add base to offset to get absolute address (4th byte into our code after the entrypoint JMP)
        CLC
        LDA # (BIOSInterface.EntryPoint % 256)
        ADC # 3
        Gen6502.emitByte(); if (NC) { return; }
        LDA # (BIOSInterface.EntryPoint / 256)
        Gen6502.emitByte();
        */

    }
    
    // Generate code to increment 32 bit value at BP+offset
    // Input: A = signed BP offset (e.g., 0xFF for -1)
    inc()
    {
        loop
        {
            // Calculate effective offset into Y
            calculateBPOffset(); if (NC) { break; }
            
            // CLC (clear carry)
            LDA #OpCode.CLC
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA # OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.ADC_IMM
            Gen6502.emitByte(); if (NC) { break; }
            LDA #1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack0
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.ADC_IMM
            Gen6502.emitByte(); if (NC) { break; }
            LDA #0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack1
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack2
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.ADC_IMM
            Gen6502.emitByte(); if (NC) { break; }
            LDA #0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack2
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA #OpCode.LDA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack3
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.ADC_IMM
            Gen6502.emitByte(); if (NC) { break; }
            LDA #0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_IND_Y
            Gen6502.emitByte(); if (NC) { break; }
            LDA # Gen6502.runtimeStack3
            Gen6502.emitByte(); if (NC) { break; }
                     
            SEC
            break;
        } // single exit
    }
    
    // Test if NEXT is zero (false)
    nextZero()
    {
        loop
        {
            // Test if NEXT is zero (false)
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA #OpCode.ORA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA #OpCode.ORA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            Gen6502.emitByte(); if (NC) { break; }
            
            LDA #OpCode.ORA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            Gen6502.emitByte(); if (NC) { break; }
            
            SEC
            break;
        }
    }
    
    // Generate code to load from zero page into ZP.NEXT
    loadNEXT()
    {
        STA ZP.TEMP
        loop
        {
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            Gen6502.emitByte(); if (NC) { break; }
            
            INC ZP.TEMP
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            Gen6502.emitByte(); if (NC) { break; }
            
            INC ZP.TEMP
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            Gen6502.emitByte(); if (NC) { break; }
            
            INC ZP.TEMP
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            Gen6502.emitByte(); if (NC) { break; }
            
            break;
        } // single exit
    }
    
    // Generate code to store from ZP.NEXT to zero page
    storeNEXT()  // Input: A = ZP base address
    {
        STA ZP.TEMP
        
        loop
        {
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT0
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            
            INC ZP.TEMP
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT1
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            
            INC ZP.TEMP
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT2
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            
            INC ZP.TEMP
            LDA #OpCode.LDA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA #ZP.NEXT3
            Gen6502.emitByte(); if (NC) { break; }
            LDA #OpCode.STA_ZP
            Gen6502.emitByte(); if (NC) { break; }
            LDA ZP.TEMP
            Gen6502.emitByte(); if (NC) { break; }
            
            break;
        } // single exit
    }
}
