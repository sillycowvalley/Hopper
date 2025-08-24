unit Tools // Tools.asm
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Production utilities only - debug functionality moved to Debug.asm
        
    
    // Input: A - character to test
    // Output: C for yes, NC for no
    IsPrintable()
    {
        loop
        {
            CMP #32
            if (C)  // >= 32
            {
                CMP #127
                if (NC)  // <= 126
                {
                    SEC
                    break;
                }
            }
            CLC
            break;
        } // exit loop
    }
        
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    // Preserves: Everything (saves/restores ZP.TOP)
    StringLength()
    {
        PHX
        PHY
        
        // Use stack to preserve caller's ZP.TOP instead of corrupting it
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH  
        PHA
        
        STX ZP.TOPL
        STY ZP.TOPH
        
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y
            if (Z) { break; }
            INY
#ifdef DEBUG
            if (Z) // wrapped around from 0xFF to 0x00
            {
                LDA # 0x04  Debug.Crash(); // runaway StringLength() calculation
            }
#endif
        }
        
        // Restore caller's ZP.TOP
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        TYA  // Length in A
        
        PLY
        PLX
    }
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    StringCompare()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.TOPL
            CMP ZP.NEXTL
            if (Z)
            {
                LDA ZP.TOPH
                CMP ZP.NEXTH
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.TOP], Y
            CMP [ZP.NEXT], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.TOP], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            
            INY
        }
        
        PLY
        PLA
    } 
    
    
    
    
    Seconds() inline
    {
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA NEXT3
        LDA ZP.TICK2
        STA NEXT2
        LDA ZP.TICK1
        STA NEXT1
        LDA ZP.TICK0 
        STA NEXT0
        
        LDA # 0xE8 // 1000 = 0x03E8
        STA TOP0
        LDA # 0x03
        STA TOP1
        STZ TOP2
        STZ TOP3
        
        Long.DivMod(); // Seconds = Millis / 1000   
        
        LDA NEXT0
        STA TOPL
        LDA NEXT1
        STA TOPH
        LDA # Types.UInt
        Stacks.PushTop();
    }
    
    
    // Create token stream from tokenizer buffer slice
    // Input: ZP.FSOURCEADDRESS = start position in BasicTokenizerBuffer
    //        ZP.FLENGTH = length of token stream to copy
    // Output: ZP.FDESTINATIONADDRESS = pointer to allocated token stream copy
    // Munts: A, ZP.IDY, ZP.ACC, ZP.FLENGTH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS
    // Error: Sets ZP.LastError if memory allocation fails
    const string createTokenStreamTrace = "CreateTokStr";
    CreateTokenStream()
    {
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
#ifdef TRACE
        LDA #(createTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(createTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
            // Allocate memory for token stream
            LDA ZP.FLENGTHL
            STA ZP.ACCL
            LDA ZP.FLENGTHH
            STA ZP.ACCH
            IncACC(); // for the EOF
            Memory.Allocate();  // Tools.CreateTokenStream(): Returns address in ZP.IDX
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // Allocation failed
                Error.OutOfMemory(); BIT ZP.EmulatorPCL
                CLC
                break;
            }
            
            // Set up copy source = TokenizerBuffer + saved position
            CLC
            LDA ZP.TokenBufferL
            ADC ZP.FSOURCEADDRESSL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenBufferH
            ADC ZP.FSOURCEADDRESSH
            STA ZP.FSOURCEADDRESSH
            
            // Return result and copy destination = allocated memory
            LDA ZP.IDXL
            STA ZP.IDYL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.IDYH
            STA ZP.FDESTINATIONADDRESSH
            
            // Copy the token stream
            Memory.Copy(); // Munts: ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, ZP.FLENGTH
            
            // Side Effect: ZP.FDESTINATIONADDRESS points one byte beyond end of Memory.Copy()
            LDA #Token.EOF
            STA [ZP.FDESTINATIONADDRESS] // Write EOF token
            
            SEC
            
            break;
        } // loop    
        
#ifdef TRACE
        LDA #(createTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(createTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLA
        STA ZP.IDXH    
        PLA
        STA ZP.IDXL
    }
}
