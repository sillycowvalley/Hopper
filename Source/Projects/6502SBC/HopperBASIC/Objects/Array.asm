unit BASICArray
{
    // Array memory management for BASIC dynamic arrays
    // Arrays are dynamically allocated like strings with Variables owning the memory
    // Supports multiple element types: BIT, BYTE, CHAR, INT, WORD
    
    // Array object memory layout:
    // Offset 0-1: element count (16-bit)
    // Offset 2:   element type (BASICType enum value)
    // Offset 3+:  element data (format depends on type)
    //   - BIT:  packed 8 bits per byte
    //   - BYTE/CHAR: one byte per element  
    //   - INT/WORD: two bytes per element (LSB first)
    
    friend Debug, Commands, Variables, BASICSysCalls;
    
    const uint aiCount    = 0;  // Offset to element count field
    const uint aiType     = 2;  // Offset to element type field
    const uint aiOwner    = 3;  // Owner variable node pointer (2 bytes) NEW!
    const uint aiElements = 5;  // First element (was 3, now 5)
    
    // Lookup table for bit masking operations
    const byte[] BitMasks = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
                              0b00010000, 0b00100000, 0b01000000, 0b10000000 };
                              
    const byte ACARRY = ZP.FSIGN;
    
    // Input:  ACCL for elements, ACCT for element type
    // Output: ACCL for bytes required          
    elementsToBytes()
    {
        PHA
        
        // Calculate allocation size based on element type
        LDA ZP.ACCT
        switch (A)
        {
            case BASICType.BIT:
            {
                // BIT arrays: size = (elements + 7) / 8 (round up)
                LDA # 0
                STA ACARRY
                
                // Check if we need to round up
                LDA ZP.ACCL
                AND # 0x07
                if (NZ)
                {
                    INC ACARRY  // Will add 1 for partial byte
                }
                
                // Divide by 8 (shift right 3 times)
                LSR ZP.ACCH
                ROR ZP.ACCL
                LSR ZP.ACCH
                ROR ZP.ACCL
                LSR ZP.ACCH
                ROR ZP.ACCL
                
                // Add rounding adjustment
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
                // BYTE/CHAR arrays: size = number of elements (no change)
            }
            default:
            {
                // INT/WORD arrays: size = elements * 2
                ASL ZP.ACCL
                ROL ZP.ACCH
            }
        }
        PLA
    }
                                                            
    // Create new array object
    // Input: ZP.ACC = number of elements (16-bit), ZP.ACCT = element type (BASICType enum)
    // Output: ZP.IDX = allocated array pointer, C set if successful, NC if allocation failed
    // Modifies: ZP.FLENGTH, ZP.FDESTINATIONADDRESS, ACARRY
    // Preserves: Element count preserved in array header
    New()
    {
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        loop
        {
            // Save element count for array header
            // ACC --> FLENGTH
            LDA ZP.ACCL   
            STA ZP.FLENGTHL
            LDA ZP.ACCH  
            STA ZP.FLENGTHH
            
            // Calculate allocation size based on element type
            // Input:  ACCL for elements, ACCT for element type
            // Output: ACCL for bytes required          
            elementsToBytes();
                       
            // Add header overhead: 2 bytes count + 1 byte type + 2 byte owner = 5 bytes total (same as aiElements offset)
            CLC
            LDA ZP.ACCL
            ADC # aiElements
            STA ACCL
            LDA ZP.ACCH
            ADC #0
            STA ACCH
            
            // Allocate memory for array
            Memory.Allocate();  // Array.New(): Input: ZP.ACC = size, Munts: ZP.M*, ZP.FREELIST, ZP.ACCL, -> ZP.IDX
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            // Initialize allocated memory to zero
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            loop
            {
                // Check if we've zeroed all bytes
                LDA ZP.ACCL
                if (Z)
                {
                    LDA ZP.ACCH
                    if (Z)
                    {
                        break;
                    }
                }
                
                // Zero current byte
                LDA # 0
                STA [ZP.FDESTINATIONADDRESS]
                IncDESTINATIONADDRESS();
                
                // Decrement remaining count
                LDA ZP.ACCL
                if (Z)
                {
                    DEC ZP.ACCH
                }
                DEC ZP.ACCL
            }
            
            // Write array header - element count
            LDY # aiCount
            LDA ZP.FLENGTHL
            STA [ZP.IDX], Y
            INY
            LDA ZP.FLENGTHH
            STA [ZP.IDX], Y
            
            // Write array header - element type
            LDY # aiType
            LDA ZP.ACCT
            STA [ZP.IDX], Y
            
            // TODO : write owner?
            
            SEC  // Success
            break;
        } // single exit
        
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
    }
       
    // Get array element count
    // Input:  ZP.IDX = array pointer
    // Output: ZP.ACC = element count (16-bit)
    GetCount()
    {
        PHY
        LDY # aiCount
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.ACCH
        PLY
    }

    
    // Get array element type
    // Input:  ZP.IDX = array pointer (popped from stack)
    // Output: ZP.ACCT = element type (BASICType enum value)
    GetItemType()
    {
        PHY
        LDY # aiType
        LDA [ZP.IDX], Y
        STA ZP.ACCT
        PLY
    }
    
    // Calculate element address and bit mask for array access
    // Input: ZP.IDX = array pointer, ZP.IDY = element index, ZP.ACCT = element type
    // Output: ZP.IDY = element address in memory, X = bit number (0-7) for BIT arrays
    // Modifies: A register
    // Private helper method
    getIndexAndMask()
    {
        LDA ZP.ACCT
        switch (A)
        {
            case BASICType.BIT:
            {
                // For BIT arrays: save bit position and convert to byte offset
                LDA IDYL
                AND # 0x07      // Extract bit position (0-7)
                TAX             // Save in X for later masking
                
                // Convert bit index to byte offset (divide by 8)
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
                // BYTE/CHAR arrays: offset = index (no conversion needed)
            }
            default:
            {
                // INT/WORD arrays: offset = index * 2
                ASL IDYL
                ROL IDYH
            }
        }
        
        // Add array base address to offset
        CLC
        LDA IDXL
        ADC IDYL
        STA IDYL
        LDA IDXH
        ADC IDYH
        STA IDYH
    }
    
    // Get array element value
    // Input: ZP.IDX = array pointer, ZP.IDY = element index
    // Output: ZP.TOP = element value and type, C set if successful, NC if index out of bounds
    // Modifies: ZP.TOP
    // Preserves: A, X, Y registers
    GetItem()
    {
        PHA
        PHX
        PHY
        loop
        {
            // Bounds check: index < element count?
            LDY # aiCount+1
            LDA ZP.IDYH        // Index MSB
            CMP [ZP.IDX], Y       // Count MSB
            if (Z)
            {
                DEY
                LDA ZP.IDYL       // Index LSB
                CMP [ZP.IDX], Y   // Count LSB
            }
            if (C) // Set C if index >= count (out of bounds)
            {
                Error.RangeError(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            // Get element type from array header
            LDY # aiType
            LDA [ZP.IDX], Y
            STA ZP.ACCT
            
            // Calculate element address
            getIndexAndMask(); // Returns address in IDY, bit # in X
                    
            // Read element value based on type
            LDY # aiElements
            LDA # 0
            STA ZP.TOPH        // Default high byte = 0

            LDA ZP.ACCT
            STA ZP.TOPT
            switch (A)
            {
                case BASICType.BIT:
                {
                    // Extract bit value using mask
                    LDA [IDY], Y           
                    AND BitMasks, X
                    if (Z)
                    {
                        STA ZP.TOP0    // Bit is 0
                    }
                    else
                    {
                        LDA # 1
                        STA ZP.TOP0    // Bit is 1
                    }
                }
                case BASICType.CHAR:
                {
                    // Read single byte
                    LDA [IDY], Y
                    Long.LoadTopByte();  // A = byte value
                }
                default:
                {
                   // WORD | INT | BYTE 
                   LDA [IDY], Y
                   STA ZP.TOP0
                   LDA ZP.ACCT
                   CMP # BASICType.BYTE
                   if (NZ)
                   {
                       INY
                       LDA [IDY], Y
                       STA ZP.TOP1
                   }
                   BASICTypes.Promote(); // -> LONG
                }
            }      
            SEC                // Success
            break;
        } // single exit
        PLY
        PLX
        PLA
    }
    
    
    
    // Set array element value  
    // Input: ZP.IDX = array pointer, ZP.IDY = element index, ZP.TOP = new value and type
    // Output: C set if successful, NC if index out of bounds
    // Modifies: Element at specified index
    // Preserves: A, X, Y registers
    SetItem()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Bounds check: index < element count?
            LDY # aiCount+1
            LDA ZP.IDYH        // Index MSB
            CMP [ZP.IDX], Y       // Count MSB
            if (Z)
            {
                DEY
                LDA ZP.IDYL    // Index LSB
                CMP [ZP.IDX], Y   // Count LSB
            }
            if (C) // Set C if index >= count (out of bounds)
            {
                Error.RangeError(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            // Get element type from array header
            LDY # aiType
            LDA [ZP.IDX], Y
            STA ZP.ACCT
            
            // Calculate element address
            getIndexAndMask(); // Returns address in IDY, bit # in X
                    
            // Write element value based on type
            LDY # aiElements
            STZ ZP.NEXTH       // Clear for safety
            // TODO TYPE DEMOTION
            LDA ZP.ACCT
            switch (A)
            {
                case BASICType.BIT:
                {
                    LDA ZP.TOPL
                    if (NZ)
                    {
                        // Set the bit to 1
                        LDA BitMasks, X
                        ORA [IDY], Y    
                        STA [IDY], Y
                    }
                    else
                    {
                        // Clear the bit to 0
                        LDA BitMasks, X
                        EOR # 0xFF     // Invert mask
                        AND [IDY], Y    
                        STA [IDY], Y       
                    }
                }
                case BASICType.CHAR:
                case BASICType.BYTE:
                {
                    LDA ZP.TOP1
                    ORA ZP.TOP2
                    ORA ZP.TOP3 // x3
                    if (NZ)
                    {
                        Error.NumericOverflow(); BIT ZP.EmulatorPCL
                    }
                    // Write single byte
                    LDA ZP.TOP0
                    STA [IDY], Y
                }
                case BASICType.INT:
                {
                    LDA ZP.TOP2
                    ORA ZP.TOP3 // x2
                    if (NZ)
                    {
                        LDA ZP.TOP2
                        CMP #0xFF
                        if (NZ)
                        {
                            Error.NumericOverflow(); BIT ZP.EmulatorPCL
                        }
                        LDA ZP.TOP3
                        CMP #0xFF
                        if (NZ)
                        {
                            Error.NumericOverflow(); BIT ZP.EmulatorPCL
                        }
                    }
                    // Write two-byte value (LSB first)
                    LDA ZP.TOP0
                    STA [IDY], Y
                    INY
                    LDA ZP.TOP1
                    STA [IDY], Y
                }
                default:
                {
                    LDA ZP.TOP2
                    ORA ZP.TOP3 // x2
                    if (NZ)
                    {
                        Error.NumericOverflow(); BIT ZP.EmulatorPCL
                    }
                    
                    // Write two-byte value (LSB first)
                    LDA ZP.TOP0
                    STA [IDY], Y
                    INY
                    LDA ZP.TOP1
                    STA [IDY], Y
                }
            } 
            
            SEC                // Success
            break;
        } // single exit     
        PLY
        PLX
        PLA
    }
    
    
    
    // Input:  BASICArray = TOP, number of elements = NEXT
    // Output: BASICArray = TOP (may be the same, may be new)
    //         C = success, NC = failure
    Redimension()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop
        {   
            LDY # aiCount
            LDA [ZP.TOP], Y
            STA ZP.ACCL
            INY
            LDA [ZP.TOP], Y
            STA ZP.ACCH
            INY
            LDA [ZP.TOP], Y
            STA ZP.ACCT

            LDA ZP.ACCH
            CMP ZP.NEXTH
            if (Z)
            {
                LDA ZP.ACCL
                CMP ZP.NEXTL
                if (Z)
                {
                    // Calculate memory size based on element type
                    // Input:  ACCL for elements, ACCT for element type
                    // Output: ACCL for bytes required          
                    elementsToBytes();
                    
                    CLC
                    LDA ZP.TOPL
                    ADC # aiElements
                    STA ZP.FDESTINATIONADDRESSL
                    LDA ZP.TOPH
                    ADC #0
                    STA ZP.FDESTINATIONADDRESSH

                    loop
                    {
                        // Check if we've zeroed all bytes
                        LDA ZP.ACCL
                        if (Z)
                        {
                            LDA ZP.ACCH
                            if (Z)
                            {
                                break;
                            }
                        }
                        
                        // Zero current byte
                        LDA # 0x00
                        STA [ZP.FDESTINATIONADDRESS]
                        IncDESTINATIONADDRESS();
                        
                        // Decrement remaining count
                        LDA ZP.ACCL
                        if (Z)
                        {
                            DEC ZP.ACCH
                        }
                        DEC ZP.ACCL
                    }
                  
                    SEC
                    break;
                }
            }
            // desired new size (since ACC is preserved over Free)
            // NEXT --> ACC
            LDA ZP.NEXTL
            STA ZP.ACCL
            LDA ZP.NEXTH
            STA ZP.ACCH
            
            // TOP --> IDX
            LDA ZP.TOPL
            STA ZP.IDXL
            LDA ZP.TOPH
            STA ZP.IDXH
            Memory.Free(); // Input: ZP.IDX, Munts: A, ZP.IDX, ZP.M* -> C on success
            
            // Input: ZP.ACC = number of elements (16-bit), ZP.ACCT = element type (BASICType enum)
            // Output: ZP.IDX = allocated array pointer, C set if successful, NC if allocation failed
            BASICArray.New();
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            LDA ZP.IDXL
            STA ZP.TOPL
            LDA ZP.IDXH
            STA ZP.TOPH
            SEC                // Success
            break;
        } // single exit  
        
        PLA
        STA ZP.IDXH
        PLA 
        STA ZP.IDXL
              
        PLY
        PLX
        PLA
    }
    
    // Setup pointers for exporting array data
    // Input: ZP.NEXT contains array pointer
    // Output: File.SectorSource = pointer to array data (past header)
    //         File.TransferLength = number of bytes to export
    // Munts: ZP.IDX, ZP.ACC, ZP.ACCT, A
    GetExportPointers()
    {
        LDY # aiCount
        LDA [ZP.NEXT], Y
        STA ZP.ACCL
        INY
        LDA [ZP.NEXT], Y
        STA ZP.ACCH
        INY               // taking advantage that aiType follows aiCount
        LDA [ZP.NEXT], Y
        STA ZP.ACCT
        
        elementsToBytes();    // Convert count to bytes -> ZP.ACC
        
        // Set transfer length
        LDA ZP.ACCL
        STA File.TransferLengthL
        LDA ZP.ACCH
        STA File.TransferLengthH
        
        // Set source pointer to array data (skip 3-byte header)
        CLC
        LDA ZP.NEXTL
        ADC # aiElements  // #3
        STA File.SectorSourceL
        LDA ZP.NEXTH
        ADC # 0
        STA File.SectorSourceH
    }
    
}
