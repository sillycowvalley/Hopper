unit Memory // Memory.asm
{
    uses "Allocate.asm"
    uses "Free.asm"
    
    probeTest()
    {
        // set the carry flag if RAM is found at IDX
        loop
        {
            LDA #0xAA
            STA [IDX]
            LDA [IDX]
            CMP #0xAA
            if (Z)
            {
                LDA #0x55
                STA [IDX]
                LDA [IDX]
                CMP #0x55
                if (Z)
                {
                    SEC // RAM found
                    break;
                }
            }
            CLC // not RAM
            break;
        }
    }
    
    probeRAM()
    {
        loop
        {
            // probe to discover RAM size:
            LDA # 0xFF
            STA ZP.IDXL
            LDA # 0xDF
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0xE0 for 56K
                LDA # 0xE0
                break;
            }
            LDA # 0xBF
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0xC0 for 48K
                LDA # 0xC0
                break;
            }
            LDA # 0x7F
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0x80 for 32K
                LDA # 0x80
                break;
            }
            LDA # 0x3F
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0x40 for 16K
                LDA # 0x40
                break;
            }
            
            LDA # 0x02 Debug.Crash(); // failed to find at least 16K of RAM
            
            break;
        } // loop
    }        
    
    Initialize()
    {
        // Assumes that:
        // - entire program was loaded at HopperData (typically $0800)
        // - size in pages of loaded program is in PROGSIZE

        LDA # (Address.HeapStart >> 8)
        STA ZP.HEAPSTART
        
        // probe to discover RAM size:
        // - A = 0x40 for 16K    
        // - A = 0x80 for 32K    
        // - A = 0xC0 for 48K
        // - A = 0xE0 for 56K
        probeRAM(); // munts Y, sets A
    
        SEC
        SBC ZP.HEAPSTART
        STA ZP.HEAPSIZE
        
        // Zero initialize
        LDA #0
        PHA PHA
        STA IDXL
        LDA ZP.HEAPSTART
        STA IDXH
        LDX ZP.HEAPSIZE // number of 256 byte pages is same as MSB of size
        ClearPages(); // munts A, X, Y
        
        // FreeList = Hopper heap start
        LDA ZP.HEAPSTART
        STA ZP.FREELISTH
        PLA // 0 -> A
        TAY // 0 -> Y
        STA ZP.FREELISTL
        
        // all memory is in this single free list record
        STA [ZP.FREELIST], Y
        LDA ZP.HEAPSIZE
        INY
        STA [ZP.FREELIST], Y
        
        // next = null
        PLA // 0 -> A
        INY
        STA [ZP.FREELIST], Y
        INY
        STA [ZP.FREELIST], Y
        
        // prev = null
        INY
        STA [ZP.FREELIST], Y
        INY
        STA [ZP.FREELIST], Y
    }
    
    // Allocate memory block
    // Input: ZP.ACC = requested size (16-bit) 
    // Output: ZP.IDX = allocated address (0x0000 if allocation failed)
    // Munts: ZP.M*, ZP.FREELIST, ZP.ACCL(size), -> ZP.IDX
    const string memoryAllocate = "Allocate";
    Allocate()
    {
        PHA
        PHY

        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        Allocate.Allocate(); // Munts: A, Y, ZP.FREELIST, ZP.ACCL(size), -> ZP.IDX
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            // Allocation failed
            Error.OutOfMemory(); BIT ZP.EmulatorPCL
            CLC
        }
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        SEC
        PLY
        PLA
    }
    
    // Free memory block
    // Input: ZP.IDX = address to free (must not be 0x0000)
    // Output: C set (success)
    // Munts:  A, ZP.M* scratch space (internal to memory management operations), C on success
    Free()
    {   
        PHY

#if defined(DEBUG)
        LDA IDXL
        ORA IDXH
        if (Z)
        {
            LDA # 0x01 Debug.Crash(); // this is a bug (to try to free nullptr)
        }
#endif
        
        Free.Free(); // Munts: ZP.IDX, ZP.FREELIST 
        
        
        SEC // success
        
        PLY
    }
    
    Available()
    {
        // uses IDX and ACC
        STZ ZP.ACCL
        STZ ZP.ACCH
        LDA ZP.FREELISTL
        STA ZP.IDXL
        LDA ZP.FREELISTH
        STA ZP.IDXH
        
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                break;
            }
        
            LDY # 0
            CLC
            LDA [ZP.IDX], Y
            ADC ZP.ACCL
            STA ZP.ACCL
            INY
            LDA [ZP.IDX], Y
            ADC ZP.ACCH
            STA ZP.ACCH
            
            // 2 byte cost for each allocated block:
            DecACCx2();
            
            INY
            LDA [IDX], Y
            PHA
            INY
            LDA [IDX], Y
            STA IDXH
            PLA
            STA IDXL
        } // loop
    }
    
    Maximum()
    {
        // uses ACC, IDX and IDY
               
        // available = 0
        STZ ACCL
        STZ ACCH
        
        // current = FREELIST
        LDA FREELISTL
        STA IDXL
        LDA FREELISTH
        STA IDXH
        
        loop
        {
            LDA IDXL
            ORA IDXH
            if (Z)
            {
                // current == 0
                LDA ACCL
                ORA ACCH
                if (Z)
                {
                    // available== 0
                    break; 
                }
                
                // 2 byte cost for the block
                DecACCx2();
                break;
            }
    
            // size = ReadWord(current + 0);
            LDY # 0
            LDA [IDX], Y
            STA IDYL
            INY
            LDA [IDX], Y
            STA IDYH
            
            // size <= available?
            LDA IDYH
            CMP ACCH
            if (Z)
            {
                LDA IDYL
                CMP ACCL
            }
    
            if (NZ)    // size == available (not >)
            {
                if (C) // size <  available (not >)
                {
                    // size > available
                    
                    // size > available
                    //   so available = size;
                    LDA IDYL
                    STA ACCL
                    LDA IDYH
                    STA ACCH
                }
            }
    
            // current = ReadWord(current + 2);
            INY
            LDA [IDX], Y
            PHA
            INY
            LDA [IDX], Y
            STA IDXH
            PLA
            STA IDXL
        } // loop
    }
    
    // Copy bytes from source to destination
    // Input: ZP.FSOURCEADDRESS = source pointer
    //        ZP.FDESTINATIONADDRESS = destination pointer  
    //        ZP.FLENGTH = number of bytes to copy (16-bit)
    // Output: Data copied from source to destination
    // Munts: A, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, ZP.FLENGTH
    // Side Effect: ZP.FDESTINATIONADDRESS points one byte beyond last byte (used by CreateTokenStream)
    Copy()
    {
        loop
        {
            // Check if FLENGTH == 0
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Nothing left to copy
            
            // Copy one byte: *FDESTINATIONADDRESS = *FSOURCEADDRESS
            LDA [ZP.FSOURCEADDRESS]
            STA [ZP.FDESTINATIONADDRESS]
            
            // Increment FSOURCEADDRESS
            INC ZP.FSOURCEADDRESSL
            if (Z)
            {
                INC ZP.FSOURCEADDRESSH
            }
            
            // Increment FDESTINATIONADDRESS  
            INC ZP.FDESTINATIONADDRESSL
            if (Z)
            {
                INC ZP.FDESTINATIONADDRESSH
            }
            
            // Decrement FLENGTH
            LDA ZP.FLENGTHL
            if (Z)
            {
                DEC ZP.FLENGTHH
            }
            DEC ZP.FLENGTHL
        }
    }
    
    // Zero bytes at destination address
    // Input: ZP.FDESTINATIONADDRESS = destination pointer  
    //        ZP.FLENGTH = number of bytes to zero (16-bit)
    // Output: Memory zeroed at destination
    Clear()
    {
        loop
        {
            // Check if FLENGTH == 0
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Nothing left to zero
            
            // Write zero: *FDESTINATIONADDRESS = 0
            LDA #0
            STA [ZP.FDESTINATIONADDRESS]
            
            // Increment FDESTINATIONADDRESS  
            INC ZP.FDESTINATIONADDRESSL
            if (Z)
            {
                INC ZP.FDESTINATIONADDRESSH
            }
            
            // Decrement FLENGTH
            LDA ZP.FLENGTHL
            if (Z)
            {
                DEC ZP.FLENGTHH
            }
            DEC ZP.FLENGTHL
        }
    }
    
    // Clear entire 256-byte page to zero
    // Input: A = page MSB (e.g., 0x14 for page 0x1400-0x14FF)
    // Output: Entire page filled with 0x00
    // Modifies: ZP.IDX, A, Y
    ClearPage()
    {
        STA ZP.IDXH
        STZ ZP.IDXL
        
        LDY #0
        LDA #0
        loop
        {
            STA [IDX], Y
            DEY
            if (Z) { break; }
        }
    }
    
    // IDX (memory location) and X (number of pages):
    //    inspired by: https://forums.atariage.com/topic/186656-clearing-a-section-of-memory/
    //    munts A, X, Y And IDXH but doesn't modify IDXL
    ClearPages()
    {
        LDA #0
        loop
        {
            TAY // 0 -> Y
            loop
            {
                STA [IDX], Y
                DEY
                if (Z) { break; }
            }
            // next page ..
            INC IDXH
            DEX
            if (Z) { break; }
        }
    }
}
