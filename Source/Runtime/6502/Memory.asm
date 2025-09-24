unit Memory // Memory.asm
{
    uses "Allocate.asm"
    uses "Free.asm"
    
    probeTest()
    {
#ifdef MEMORY_CHECK
        LDA # (ProbingTest / 256) STA ACCH LDA # (ProbingTest % 256) STA ACCL PrintIndentACC(); 
        LDA ZP.IDXH
        Serial.HexOut();
        LDA ZP.IDXL
        Serial.HexOut();
#endif       
        
        // set the carry flag if RAM is found at IDX
        loop
        {
            LDA #0xAA
#ifdef CPU_65C02S
            STA [IDX]
            LDA [IDX]
#else
            LDY # 0
            STA [IDX], Y
            LDA [IDX], Y
#endif
            CMP #0xAA
            if (Z)
            {
                LDA #0x55
#ifdef CPU_65C02S
                STA [IDX]
                LDA [IDX]
#else
                STA [IDX], Y
                LDA [IDX], Y
#endif
                CMP #0x55
                if (Z)
                {
#ifdef MEMORY_CHECK
                    LDA # (ProbingRW / 256) STA ACCH LDA # (ProbingRW % 256) STA ACCL PrintACC(); 
#endif                    
                    SEC // RAM found
                    break;
                }
            }
#ifdef MEMORY_CHECK
            LDA # (ProbingRO / 256) STA ACCH LDA # (ProbingRO % 256) STA ACCL PrintACC(); 
#endif            
            CLC // not RAM
            break;
        }
    }
    
    probeRAM()
    {
        loop
        {
            // probe to discover RAM size:
#ifdef MEMORY_CHECK
            LDA # (ProbingRAM / 256) STA ACCH LDA # (ProbingRAM % 256) STA ACCL PrintIndentACC();
            INC Indent
#endif            
            
            
            LDA # 0xFF
            STA ZP.IDXL
            LDA # 0xDF
            STA ZP.IDXH
            probeTest();
            if (C)
            {
#ifdef MEMORY_CHECK
                LDA # (ProbingSuccess56K / 256) STA ACCH LDA # (ProbingSuccess56K % 256) STA ACCL PrintIndentACC();
#endif                
                // A = 0xE0 for 56K
                LDA # 0xE0
                break;
            }
            LDA # 0xBF
            STA ZP.IDXH
            probeTest();
            if (C)
            {
#ifdef MEMORY_CHECK
                LDA # (ProbingSuccess48K / 256) STA ACCH LDA # (ProbingSuccess48K % 256) STA ACCL PrintIndentACC();
#endif                
                // A = 0xC0 for 48K
                LDA # 0xC0
                break;
            }
            LDA # 0x7F
            STA ZP.IDXH
            probeTest();
            if (C)
            {
#ifdef MEMORY_CHECK
                LDA # (ProbingSuccess32K / 256) STA ACCH LDA # (ProbingSuccess32K % 256) STA ACCL PrintIndentACC();
#endif
                // A = 0x80 for 32K
                LDA # 0x80
                break;
            }
            LDA # 0x3F
            STA ZP.IDXH
            probeTest();
            if (C)
            {
#ifdef MEMORY_CHECK
                LDA # (ProbingSuccess16K / 256) STA ACCH LDA # (ProbingSuccess16K % 256) STA ACCL PrintIndentACC();
#endif                            
                // A = 0x40 for 16K
                LDA # 0x40
                break;
            }
            
#ifdef MEMORY_CHECK
            LDA # (ProbingFailed/ 256) STA ACCH LDA # (ProbingFailed % 256) STA ACCL PrintIndentACC();
#endif            
            
#if defined(HOPPER_BASIC)
            LDA # 0x02 Debug.Crash(); // failed to find at least 16K of RAM
#else
            LDA # 0x0B Die(); // failed to find at least 16K of RAM
#endif
            
            break;
        } // loop
        
#ifdef MEMORY_CHECK
        DEC Indent
#endif            
    }        
    
    InitializeHeapSize()
    {
        // Assumes that:
        // - entire program was loaded at HopperData (typically $0800)
        // - size in pages of loaded program is in PROGSIZE

#ifdef HOPPER_BASIC
        LDA # (Address.HopperData >> 8)
#else
        CLC
        LDA # (Address.HopperData >> 8)
        ADC ZP.PROGSIZE  // program size in pages (rounded up to the nearest page)
#endif
        STA ZP.HEAPSTART
        
        // if RAM does not end at 0x80 (0x7FFF) then respect the value of RamSize (like 0x5000 for Ben Eater 6502)
        LDA # (Address.RamSize >> 8)
        CMP # 0x80 
        if (Z)
        {
            // probe to discover RAM size:
            // - A = 0x40 for 16K    
            // - A = 0x80 for 32K    
            // - A = 0xC0 for 48K
            // - A = 0xE0 for 56K
            probeRAM(); // munts Y, sets A
        }
        SEC
        SBC ZP.HEAPSTART
        STA ZP.HEAPSIZE
        
#ifdef MEMORY_CHECK
        LDA # (ClearPages / 256) STA ACCH LDA # (ClearPages % 256) STA ACCL PrintIndentACC();
#endif
        
        // Zero initialize
        LDA #0
        PHA PHA
        STA IDXL
        LDA ZP.HEAPSTART
        STA IDXH
#ifdef MEMORY_CHECK
        Serial.HexOut();
#endif        
        LDX ZP.HEAPSIZE // number of 256 byte pages is same as MSB of size
#ifdef MEMORY_CHECK
        PrintCount();
#endif
        Utilities.ClearPages(); // munts A, X, Y
#ifdef MEMORY_CHECK    
        LDA # (Cleared / 256) STA ACCH LDA # (Cleared % 256) STA ACCL PrintACC();
#endif
        
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
    
#ifdef HOPPER_BASIC
    // don't use the stack versions by mistake
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Uses ZP.M* scratch space for memory management (internal operations only)

    // Allocate memory block
    // Input: ZP.ACC = requested size (16-bit) 
    // Output: ZP.IDX = allocated address (0x0000 if allocation failed)
    // Munts: ZP.M*, ZP.FREELIST, ZP.ACCL(size), -> ZP.IDX
    const string memoryAllocate = "Allocate";
    Allocate()
    {
        PHA
        PHY

#ifdef TRACE
        //LDA #(memoryAllocate % 256) STA ZP.TraceMessageL LDA #(memoryAllocate / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

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
        
#ifdef TRACE
        //LDA #(memoryAllocate % 256) STA ZP.TraceMessageL LDA #(memoryAllocate / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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

#if defined(DEBUG) || defined(TRACE)
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
    FreeIDY()
    {
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        LDA ZP.IDYL
        STA ZP.IDXL
        LDA ZP.IDYH
        STA ZP.IDXH
        Memory.Free();  // Input: ZP.IDX, Munts: ZP.IDX, ZP.M* -> C on success
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
    }
    
#else
    Allocate()
    {
        Stacks.PopACC();      // only care about ACCL and ACCH (not ACCT)
        Allocate.Allocate();
        
        // Push IDX:
        LDY ZP.SP
        LDA ZP.IDXL
        STA Address.ValueStackLSB, Y
        LDA ZP.IDXH
        STA Address.ValueStackMSB, Y
        LDA # Types.UInt
        STA Address.TypeStackLSB, Y
        INC ZP.SP
    }
    Free()
    {
        Stacks.PopIDX();
        Free.Free();
    }

    ReadByte()
    {
        Stacks.PopIDX();
        
#ifdef CPU_65C02S
        STZ ZP.NEXTH
        LDA [IDX]
#else
        LDY # 0
        STY ZP.NEXTH
        LDA [IDX], Y
#endif
        STA ZP.NEXTL
        
        LDA # Types.Byte
        STA ZP.NEXTT
        PushNext();
    }
    WriteByte()
    {
        Stacks.PopACC(); // only care about ACCL (not ACCT or ACCH)
        Stacks.PopIDX();
        
        LDA ACCL
#ifdef CPU_65C02S
        STA [IDX]
#else
        LDY # 0
        STA [IDX], Y
#endif
    }
#endif // not HOPPER_BASIC

    AvailableACC()
    {
        // uses IDXand ACC
        // pushes result to [top]
#ifdef CPU_65C02S
        STZ ZP.ACCL
        STZ ZP.ACCH
#else
        LDA # 0
        STA ZP.ACCL
        STA ZP.ACCH
#endif
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
    Available()
    {
        AvailableACC();
        LDA # Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();  // munts Y, A
    }
    
    MaximumACC()
    {
        // uses ACC, IDX and IDY
        // pushes result to [top]
               
        // available = 0
#ifdef CPU_65C02S
        STZ ACCL
        STZ ACCH
#else        
        LDA #0
        STA ACCL
        STA ACCH
#endif
        
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
    Maximum()
    {
        MaximumACC();
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();
    }
    
#ifdef HOPPER_BASIC
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
    
#endif
}
