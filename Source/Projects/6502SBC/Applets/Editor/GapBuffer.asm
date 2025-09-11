unit GapBuffer
{
    
    // Zero page allocation
    const byte gbSlots = 0x80;
    
    // Private zero page state
    const byte gbBuffer = gbSlots+0;    // Start of allocated buffer
    const byte gbBufferL = gbSlots+0;
    const byte gbBufferH = gbSlots+1;
    
    const byte gbGapStart = gbSlots+2;  // Current gap position
    const byte gbGapStartL = gbSlots+2;
    const byte gbGapStartH = gbSlots+3;
    
    const byte gbGapEnd = gbSlots+4;    // End of gap
    const byte gbGapEndL = gbSlots+4;
    const byte gbGapEndH = gbSlots+5;
    
    const byte gbBufferSize = gbSlots+6; // Total allocated size
    const byte gbBufferSizeL = gbSlots+6;
    const byte gbBufferSizeH = gbSlots+7;
    
    // Public value for arguments and return values
    const byte GapValue = gbSlots+8;     // Common return value and argument for GapBuffer methods
    const byte GapValueL = gbSlots+8;
    const byte GapValueH = gbSlots+9;
    
    // Private temporary workspace (doesn't need to survive between method calls)
    const byte gbTempSize = gbSlots+10;   // Temp storage for growBuffer
    const byte gbTempSizeL = gbSlots+10;
    const byte gbTempSizeH = gbSlots+11;
    
    // GetCharAtFast
    const byte gbGapSizeL  = gbSlots+12;
    const byte gbGapSizeH  = gbSlots+13;
    const byte FastLengthL   = gbSlots+14;
    const byte FastLengthH   = gbSlots+15;
    
    enum UndoOp
    {
        None     = 0b00000000,
        Inserted = 0b00000001,
        Deleted  = 0b00000010,
    }
    
    // Undo state (part of GapBuffer!)
    const byte undoOp       = gbSlots+16;
    
    const byte undoGapStart = gbSlots+17;
    const byte undoGapStartL = gbSlots+17;
    const byte undoGapStartH = gbSlots+18;
    
    const byte undoGapEnd   = gbSlots+19;
    const byte undoGapEndL  = gbSlots+19;
    const byte undoGapEndH  = gbSlots+20;
    
    // Leaf workspace for calculations (don't survive function calls)
    const byte mgbTemp   = ZP.M0;     // Temporary 16-bit value
    const byte mgbTempL  = ZP.M0;
    const byte mgbTempH  = ZP.M1;
    const byte mgbCount  = ZP.M2;    // Copy count
    const byte mgbCountL = ZP.M2;
    const byte mgbCountH = ZP.M3;
    const byte mgbSrc    = ZP.M4;      // Source pointer
    const byte mgbSrcL   = ZP.M4;
    const byte mgbSrcH   = ZP.M5;
    const byte mgbDst    = ZP.M6;      // Destination pointer
    const byte mgbDstL   = ZP.M6;
    const byte mgbDstH   = ZP.M7;
    
    // Initialize gap buffer
    // Input: A,Y = size to allocate
    // Output: C set on success, clear on failure
    Initialize()
    {
        // Save requested size for Memory.Allocate (uses ZP.ACC)
        STA ZP.ACCL
        STY ZP.ACCH
        
        // Save size in our workspace too
        STA gbBufferSizeL
        STY gbBufferSizeH
        
        // Allocate buffer
        Memory.Allocate();
        
        // Check for allocation failure
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            // Allocation failed
            STZ gbBufferL
            STZ gbBufferH
            STZ gbBufferSizeL
            STZ gbBufferSizeH
            CLC
            return;
        }
        
        // Store buffer pointer
        LDA ZP.IDXL
        STA gbBufferL
        LDA ZP.IDXH
        STA gbBufferH
        
        // Gap starts at beginning and spans entire buffer
        STZ gbGapStartL
        STZ gbGapStartH
        
        LDA gbBufferSizeL
        STA gbGapEndL
        LDA gbBufferSizeH
        STA gbGapEndH
        
        STZ undoOp
        
        SEC  // Success
    }
    
    // Free allocated memory
    Dispose()
    {
        // Check if buffer allocated
        LDA gbBufferL
        ORA gbBufferH
        if (NZ)
        {
            // Free the buffer
            LDA gbBufferL
            STA ZP.IDXL
            LDA gbBufferH
            STA ZP.IDXH
            Memory.Free();
            
            // Clear all pointers
            STZ gbBufferL
            STZ gbBufferH
            STZ gbBufferSizeL
            STZ gbBufferSizeH
            STZ gbGapStartL
            STZ gbGapStartH
            STZ gbGapEndL
            STZ gbGapEndH
            STZ GapValueL
            STZ GapValueH
        }
    }
    
    CanUndo()  // Public
    {
        LDA undoOp
        if (Z) 
        { 
            CLC 
        }
        else 
        { 
            SEC 
        }
    }
    
    resetUndoState()
    {
        // Clear any existing undo
        STZ undoOp
        
        // Save current gap boundaries
        LDA gbGapStartL
        STA undoGapStartL
        LDA gbGapStartH
        STA undoGapStartH
        LDA gbGapEndL
        STA undoGapEndL
        LDA gbGapEndH
        STA undoGapEndH
    }
    
    ToggleUndo()  // Public - the magic method
    {
        LDA undoOp
        if (Z) 
        { 
            CLC  // Nothing to undo
            return;
        }
        
        // Save original operation type
        PHA
        
        PHX
        
        // Swap gbGapStart with undoGapStart
        LDX gbGapStartL
        LDA undoGapStartL
        STA gbGapStartL
        STX undoGapStartL
        
        LDX gbGapStartH
        LDA undoGapStartH
        STA gbGapStartH
        STX undoGapStartH
            
        // Swap gbGapEnd with undoGapEnd
        LDX gbGapEndL
        LDA undoGapEndL
        STA gbGapEndL
        STX undoGapEndL
        
        LDX gbGapEndH
        LDA undoGapEndH
        STA gbGapEndH
        STX undoGapEndH
        
        PLX
        
        
        // Flip the operation type
        LDA undoOp
        EOR #0b00000011  // Toggles between 1 and 2
        STA undoOp
        
        // Determine cursor position based on what we just undid
        PLA  // Get original operation type
        CMP #UndoOp.Deleted
        if (Z)  // Just undid a delete (restored text)
        {
            // Cursor goes to start of restored text (the old gap position)
            LDA undoGapStartL  // This now has the old "deleted" position
            STA GapValueL
            LDA undoGapStartH
            STA GapValueH
        }
        else  // Just undid an insert (removed text)
        {
            // Cursor goes to current gap (where text was removed)
            LDA gbGapStartL
            STA GapValueL
            LDA gbGapStartH
            STA GapValueH
        }
        
        SEC  // Success
    }
    
    // Clear buffer (make it empty)
    Clear()
    {
        // Reset gap to span entire buffer
        STZ gbGapStartL
        STZ gbGapStartH
        
        LDA gbBufferSizeL
        STA gbGapEndL
        LDA gbBufferSizeH
        STA gbGapEndH
        
        resetUndoState();
    }
    
    // Get gap start position
    // Output: GapValue = gap start position
    GetGapStart()
    {
        LDA gbGapStartL
        STA GapValueL
        LDA gbGapStartH
        STA GapValueH
    }
    
    // Get gap end position
    // Output: GapValue = gap end position
    GetGapEnd()
    {
        LDA gbGapEndL
        STA GapValueL
        LDA gbGapEndH
        STA GapValueH
    }
    
    // Get text length
    // Output: GapValue = text length
    GetTextLength()
    {
        // TextLength = BufferSize - (GapEnd - GapStart)
        SEC
        LDA gbGapEndL
        SBC gbGapStartL
        STA GapValueL
        LDA gbGapEndH
        SBC gbGapStartH
        STA GapValueH
        
        SEC
        LDA gbBufferSizeL
        SBC GapValueL
        STA GapValueL
        LDA gbBufferSizeH
        SBC GapValueH
        STA GapValueH
    }
    
    // Check if gap is at position
    // Input: GapValue = position
    // Output: C set if gap is at position
    IsGapAtPosition()
    {
        LDA GapValueL
        CMP gbGapStartL
        if (NZ)
        {
            CLC
            return;
        }
        LDA GapValueH
        CMP gbGapStartH
        if (NZ)
        {
            CLC
            return;
        }
        SEC
    }
    
    // Move gap to specified position
    // Input: GapValue = target position
    //        Modifies IDX and IDY (via copyBytes)
    MoveGapTo()
    {
        // this is the transaction boundary
        resetUndoState();
        
        // Save target position
        LDA GapValueL
        STA mgbTempL
        LDA GapValueH
        STA mgbTempH
        
        // Check if already at position
        LDA mgbTempL
        CMP gbGapStartL
        if (Z)
        {
            LDA mgbTempH
            CMP gbGapStartH
            if (Z) { return; }  // Already there
        }
        
        // Determine direction and move gap
        loop  // Single iteration for structure
        {
            LDA mgbTempH
            CMP gbGapStartH
            if (Z)
            {
                LDA mgbTempL
                CMP gbGapStartL
            }
            if (NC)  // target < gap start (move gap left)
            {
                // Calculate bytes to move: gbGapStart - target
                SEC
                LDA gbGapStartL
                SBC mgbTempL
                STA mgbCountL
                LDA gbGapStartH
                SBC mgbTempH
                STA mgbCountH
                
                // Source: buffer + target
                CLC
                LDA gbBufferL
                ADC mgbTempL
                STA mgbSrcL
                LDA gbBufferH
                ADC mgbTempH
                STA mgbSrcH
                
                // Destination: buffer + gbGapEnd - count
                SEC
                LDA gbGapEndL
                SBC mgbCountL
                STA mgbDstL
                LDA gbGapEndH
                SBC mgbCountH
                STA mgbDstH
                CLC
                LDA mgbDstL
                ADC gbBufferL
                STA mgbDstL
                LDA mgbDstH
                ADC gbBufferH
                STA mgbDstH
                
                // Copy bytes
                copyBytes();
                
                // Update gap position
                SEC
                LDA gbGapEndL
                SBC mgbCountL
                STA gbGapEndL
                LDA gbGapEndH
                SBC mgbCountH
                STA gbGapEndH
            }
            else  // target > gap start (move gap right)
            {
                // Calculate bytes to move: target - gbGapStart
                SEC
                LDA mgbTempL
                SBC gbGapStartL
                STA mgbCountL
                LDA mgbTempH
                SBC gbGapStartH
                STA mgbCountH
                
                // Source: buffer + gbGapEnd
                CLC
                LDA gbBufferL
                ADC gbGapEndL
                STA mgbSrcL
                LDA gbBufferH
                ADC gbGapEndH
                STA mgbSrcH
                
                // Destination: buffer + gbGapStart
                CLC
                LDA gbBufferL
                ADC gbGapStartL
                STA mgbDstL
                LDA gbBufferH
                ADC gbGapStartH
                STA mgbDstH
                
                // Copy bytes
                copyBytes();
    
                // Update gap position
                CLC
                LDA gbGapEndL
                ADC mgbCountL
                STA gbGapEndL
                LDA gbGapEndH
                ADC mgbCountH
                STA gbGapEndH
            }
            break;
        }
        
        // Set new gap start
        LDA mgbTempL
        STA gbGapStartL
        LDA mgbTempH
        STA gbGapStartH
    }
    
    // Helper: Copy bytes from src to dst
    // Helper: Copy bytes from src to dst
    copyBytes()
    {
        // Uses mgbSrc, mgbDst, mgbCount (preserves mgbCount)
        LDA mgbCountL
        ORA mgbCountH
        if (Z)
        {
            return;  // Nothing to copy
        }
        
        // Check if src == dst
        LDA mgbSrcL
        CMP mgbDstL
        if (Z)
        {
            LDA mgbSrcH
            CMP mgbDstH
            if (Z)
            {
                return;  // Same address, nothing to do
            }
        }
        
        LDA mgbCountL
        PHA
        LDA mgbCountH
        PHA
        LDA mgbDstL
        PHA
        LDA mgbDstH
        PHA
        LDA mgbSrcL
        PHA
        LDA mgbSrcH
        PHA
        
        // Simple rule: if dst > src, copy backward
        LDA mgbDstH
        CMP mgbSrcH
        if (Z)  // High bytes equal, check low
        {
            LDA mgbDstL
            CMP mgbSrcL
            if (C)  // dst.L >= src.L
            {
                if (NZ)  // Not equal, so dst.L > src.L
                {
                    copyBackward();
                }
                else
                {
                    copyForward();  // Equal (but we already handled this case earlier)
                }
            }
            else  // dst.L < src.L
            {
                copyForward();
            }
        }
        else  // High bytes not equal
        {
            if (C)  // dst.H > src.H (truly greater since not equal)
            {
                copyBackward();
            }
            else  // dst.H < src.H
            {
                copyForward();
            }
        }
        
        PLA
        STA mgbSrcH
        PLA
        STA mgbSrcL
        PLA
        STA mgbDstH
        PLA
        STA mgbDstL
        PLA
        STA mgbCountH
        PLA
        STA mgbCountL
    }
    
    
    copyForward()
    {
        loop
        {
            LDA [mgbSrc]
            STA [mgbDst]
            
            // Increment pointers
            INC mgbSrcL
            if (Z) { INC mgbSrcH }
            INC mgbDstL
            if (Z) { INC mgbDstH }
            
            // Decrement count
            LDA mgbCountL
            if (Z)
            {
                DEC mgbCountH
            }
            DEC mgbCountL
            
            LDA mgbCountH
            ORA mgbCountL
            if (Z) { break; }
        }
    }
    
    copyBackward()
    {
        // Point to last byte (src + count - 1)
        CLC
        LDA mgbSrcL
        ADC mgbCountL
        STA mgbSrcL
        LDA mgbSrcH
        ADC mgbCountH
        STA mgbSrcH
        
        LDA mgbSrcL
        if (Z) { DEC mgbSrcH }
        DEC mgbSrcL
        
        // Same for dst
        CLC
        LDA mgbDstL
        ADC mgbCountL
        STA mgbDstL
        LDA mgbDstH
        ADC mgbCountH
        STA mgbDstH
        
        LDA mgbDstL
        if (Z) { DEC mgbDstH }
        DEC mgbDstL
        
        loop
        {
            LDA [mgbSrc]
            STA [mgbDst]
            
            // Decrement pointers
            LDA mgbSrcL
            if (Z) { DEC mgbSrcH }
            DEC mgbSrcL
            
            LDA mgbDstL
            if (Z) { DEC mgbDstH }
            DEC mgbDstL
            
            LDA mgbCountL
            if (Z) { DEC mgbCountH }
            DEC mgbCountL
            
            LDA mgbCountH
            ORA mgbCountL
            if (Z) { break; }
        }
    }
    
     
    // Insert character at gap position
    // Input: A = character to insert
    // Note: Assumes gap is at correct position
    //       May call Memory.Allocate and Memory.Free (via growBuffer if gap is full)
    //       Modifies IDX directly, and if buffer grows: IDX and IDY (via growBuffer?copyBytes)
    InsertChar()
    {
        PHA  // Save character
        
        loop  // Single iteration for structure
        {
            // Check if gap is full
            LDA gbGapStartL
            CMP gbGapEndL
            if (Z)
            {
                LDA gbGapStartH
                CMP gbGapEndH
                if (Z)
                {
                    // Gap is full - need to grow buffer
                    growBuffer();
                    if (NC) 
                    { 
                        PLA  // Clean up stack
                        CLC
                        break;  // Growth failed
                    }
                }
            }
            
            // Store character at gap start
            CLC
            LDA gbBufferL
            ADC gbGapStartL
            STA ZP.IDXL
            LDA gbBufferH
            ADC gbGapStartH
            STA ZP.IDXH
            
            PLA  // Restore character
            STA [ZP.IDX]
            
            // Advance gap start
            INC gbGapStartL
            if (Z) { INC gbGapStartH }
            
            // Record as an insert
            LDA undoOp
            if (Z)
            {
                LDA #UndoOp.Inserted
                STA undoOp
            }
            else
            {
                CMP #UndoOp.Inserted
                if (NZ)
                {
                    BRK  // Mixed transaction - crash!
                }
            }
            
            SEC  // Success
            break;
        }
    }
    
    // Delete character before gap (backspace)
    // Note: Assumes gap is at correct position
    Backspace()
    {
        // Check if at beginning
        LDA gbGapStartL
        ORA gbGapStartH
        if (Z)
        {
            CLC  // Can't delete before position 0
            return;
        }
        
        // Move gap start back
        LDA gbGapStartL
        if (Z) { DEC gbGapStartH }
        DEC gbGapStartL
        
        LDA undoOp
        if (Z)
        {
            LDA #UndoOp.Deleted
            STA undoOp
        }
        else
        {
            CMP #UndoOp.Deleted
            if (NZ)
            {
                BRK  // Mixed transaction - crash!
            }
        }
        
        SEC  // Success
    }
    
    // returns deleted character in A, or 0
    Delete()
    {
        // Check if at end
        LDA gbGapEndL
        CMP gbBufferSizeL
        if (Z)
        {
            LDA gbGapEndH
            CMP gbBufferSizeH
            if (Z)
            {
                LDA #0
                CLC  // At end of buffer
                return;
            }
        }
        
        // Zero the position about to be deleted (current gap end)
        CLC
        LDA gbBufferL
        ADC gbGapEndL
        STA ZP.IDXL
        LDA gbBufferH
        ADC gbGapEndH
        STA ZP.IDXH
        LDA [ZP.IDX] // retrieve old character
        PHA
        
        // Move gap end forward
        INC gbGapEndL
        if (Z) { INC gbGapEndH }
        
        // Record as a delete
        LDA undoOp
        if (Z)
        {
            LDA #UndoOp.Deleted
            STA undoOp
        }
        else
        {
            CMP #UndoOp.Deleted
            if (NZ)
            {
                BRK  // Mixed transaction - crash!
            }
        }
        
        PLA  // Restore deleted character to A
        SEC  // Success
    }    
    
    // Helper: Calculate gap size
    // Output: mgbCount = gap size (gbGapEnd - gbGapStart)
    getGapSize()
    {
        SEC
        LDA gbGapEndL
        SBC gbGapStartL
        STA mgbCountL
        LDA gbGapEndH
        SBC gbGapStartH
        STA mgbCountH
    }
    
    GetCharAtFastPrep()
    {
        getGapSize();
        LDA mgbCountL
        STA gbGapSizeL
        LDA mgbCountH
        STA gbGapSizeH
        
        GetTextLength();
        LDA GapValueL
        STA FastLengthL
        LDA GapValueH
        STA FastLengthH
    }
    
    // Get character at logical position
    // Input: GapValue = position
    // Output: A = character (0 if out of bounds)
    GetCharAtFast()
    {
        // Save position
        LDA GapValueL
        STA mgbTempL
        LDA GapValueH
        STA mgbTempH
        
        loop  // Single iteration for structure
        {
            // Check if position >= text length (out of bounds)
            LDA mgbTempH
            CMP FastLengthH
            if (C)  // position.H >= length.H
            {
                if (NZ)  // position.H > length.H, definitely out of bounds
                {
                    LDA #0
                    break;
                }
                // High bytes equal, check low bytes
                LDA mgbTempL
                CMP FastLengthL
                if (C)  // position.L >= length.L, out of bounds
                {
                    LDA #0
                    break;
                }
                // position.L < length.L, so position < length (valid)
            }
            // else position.H < length.H, so position < length (valid)
            
            // Convert logical to physical position
            // Check if position >= gap start
            LDA mgbTempH
            CMP gbGapStartH
            if (C)  // position.H >= gap_start.H
            {
                if (NZ)  // position.H > gap_start.H, definitely after gap
                {
                    // Add gap size to position
                    CLC
                    LDA mgbTempL
                    ADC gbGapSizeL
                    STA mgbTempL
                    LDA mgbTempH
                    ADC gbGapSizeH
                    STA mgbTempH
                }
                else
                {
                    // High bytes equal, check low bytes
                    LDA mgbTempL
                    CMP gbGapStartL
                    if (C)  // position.L >= gap_start.L, at or after gap
                    {
                        // Add gap size to position
                        CLC
                        LDA mgbTempL
                        ADC gbGapSizeL
                        STA mgbTempL
                        LDA mgbTempH
                        ADC gbGapSizeH
                        STA mgbTempH
                    }
                    // else position < gap start, no adjustment
                }
            }
            // else position.H < gap_start.H, before gap, no adjustment
            
            // Read from physical position
            CLC
            LDA gbBufferL
            ADC mgbTempL
            STA mgbTempL
            LDA gbBufferH
            ADC mgbTempH
            STA mgbTempH
            
            
            LDA [mgbTemp]
            
            // If we got 0, return 0xFD to distinguish from actual 0
            if (Z)
            {
                LDA #0
            }
            break;
        }
    }
    
    // Get character at logical position
    // Input: GapValue = position
    // Output: A = character (0 if out of bounds)
    GetCharAt()
    {
        // Save position
        LDA GapValueL
        STA mgbTempL
        LDA GapValueH
        STA mgbTempH
        
        loop  // Single iteration for structure
        {
            // Get text length for bounds check
            GetTextLength();  // Returns in GapValue
            
            // Check if position >= text length (out of bounds)
            LDA mgbTempH
            CMP GapValueH
            if (C)  // position.H >= length.H
            {
                if (NZ)  // position.H > length.H, definitely out of bounds
                {
                    LDA #0
                    break;
                }
                // High bytes equal, check low bytes
                LDA mgbTempL
                CMP GapValueL
                if (C)  // position.L >= length.L, out of bounds
                {
                    LDA #0
                    break;
                }
                // position.L < length.L, so position < length (valid)
            }
            // else position.H < length.H, so position < length (valid)
            
            // Convert logical to physical position
            // Check if position >= gap start
            LDA mgbTempH
            CMP gbGapStartH
            if (C)  // position.H >= gap_start.H
            {
                if (NZ)  // position.H > gap_start.H, definitely after gap
                {
                    // Calculate gap size
                    getGapSize();
                    
                    // Add gap size to position
                    CLC
                    LDA mgbTempL
                    ADC mgbCountL
                    STA mgbTempL
                    LDA mgbTempH
                    ADC mgbCountH
                    STA mgbTempH
                }
                else
                {
                    // High bytes equal, check low bytes
                    LDA mgbTempL
                    CMP gbGapStartL
                    if (C)  // position.L >= gap_start.L, at or after gap
                    {
                        // Calculate gap size
                        getGapSize();
                        
                        // Add gap size to position
                        CLC
                        LDA mgbTempL
                        ADC mgbCountL
                        STA mgbTempL
                        LDA mgbTempH
                        ADC mgbCountH
                        STA mgbTempH
                    }
                    // else position < gap start, no adjustment
                }
            }
            // else position.H < gap_start.H, before gap, no adjustment
            
            // Read from physical position
            CLC
            LDA gbBufferL
            ADC mgbTempL
            STA mgbTempL
            LDA gbBufferH
            ADC mgbTempH
            STA mgbTempH
            
            
            LDA [mgbTemp]
            
            // If we got 0, return 0xFD to distinguish from actual 0
            if (Z)
            {
                LDA #0
            }
            break;
        }
    }
    
    // Grow buffer when gap is full
    growBuffer()
    {
        // Calculate new size (double current size)
        LDA gbBufferSizeL
        STA gbTempSizeL
        LDA gbBufferSizeH
        STA gbTempSizeH
        
        ASL gbTempSizeL
        ROL gbTempSizeH
        
        // Allocate new buffer (uses ZP.ACC for BIOS call)
        LDA gbTempSizeL
        STA ZP.ACCL
        LDA gbTempSizeH
        STA ZP.ACCH
        Memory.Allocate();
        
        // Check allocation
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            CLC  // Allocation failed
            return;
        }
        
        // Swap old and new buffer pointers
        // Old buffer goes to IDX (for Memory.Free later)
        // New buffer goes to gbBuffer
        LDA ZP.IDXL
        PHA
        LDA gbBufferL
        STA ZP.IDXL
        PLA
        STA gbBufferL
        
        LDA ZP.IDXH
        PHA
        LDA gbBufferH
        STA ZP.IDXH
        PLA
        STA gbBufferH
        
        // Copy text before gap
        // Source: old buffer (now in IDX)
        LDA ZP.IDXL
        STA mgbSrcL
        LDA ZP.IDXH
        STA mgbSrcH
        // Destination: new buffer (now in gbBuffer)
        LDA gbBufferL
        STA mgbDstL
        LDA gbBufferH
        STA mgbDstH
        LDA gbGapStartL
        STA mgbCountL
        LDA gbGapStartH
        STA mgbCountH
        copyBytes();
        
        // Calculate text after gap size
        SEC
        LDA gbBufferSizeL
        SBC gbGapEndL
        STA mgbCountL
        LDA gbBufferSizeH
        SBC gbGapEndH
        STA mgbCountH
        
        // Copy text after gap (if any)
        LDA mgbCountL
        ORA mgbCountH
        if (NZ)
        {
            // Source: old buffer + old gap end
            CLC
            LDA ZP.IDXL
            ADC gbGapEndL
            STA mgbSrcL
            LDA ZP.IDXH
            ADC gbGapEndH
            STA mgbSrcH
            
            // Calculate new gap end
            SEC
            LDA gbTempSizeL
            SBC mgbCountL
            STA gbGapEndL
            LDA gbTempSizeH
            SBC mgbCountH
            STA gbGapEndH
            
            // Destination: new buffer + new gap end
            CLC
            LDA gbBufferL
            ADC gbGapEndL
            STA mgbDstL
            LDA gbBufferH
            ADC gbGapEndH
            STA mgbDstH
            
            copyBytes();
        }
        else
        {
            // No text after gap, gap extends to end
            LDA gbTempSizeL
            STA gbGapEndL
            LDA gbTempSizeH
            STA gbGapEndH
        }
        
        // Free old buffer (already in IDX)
        Memory.Free();
        
        // Update buffer size (gbBuffer already has new buffer pointer)
        LDA gbTempSizeL
        STA gbBufferSizeL
        LDA gbTempSizeH
        STA gbBufferSizeH
        
        SEC  // Success
    }
    
#ifdef DEBUG
    // Debug helper: Dump gap buffer state
    const string dumpLabel = "= GAP BUFFER DUMP =";
    const string bufLabel = "Buf@";
    const string gapValueLabel = "GapV:";
    const string gapStartLabel = "GapS:";
    const string gapEndLabel = "GapE:";
    const string sizeLabel = "Size:";
    
    // Undo state labels
    const string undoOpLabel = "UndoOp:";
    const string undoStartLabel = "UndoS:";
    const string undoEndLabel = "UndoE:";
    
    const string rawLabel = "Raw:";
    const string logLabel = "Log:";
    
    Dump()
    {
        PHX
        PHY
        
        // Save ZP.IDX
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        Debug.Clear();
        
        // Header
        LDA #(dumpLabel % 256)
        STA ZP.STRL
        LDA #(dumpLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        // Buffer address
        LDA #(bufLabel % 256)
        STA ZP.STRL
        LDA #(bufLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbBufferL
        STA ZP.ACCL
        LDA GapBuffer.gbBufferH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Gap value
        LDA #(gapValueLabel % 256)
        STA ZP.STRL
        LDA #(gapValueLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.GapValueL
        STA ZP.ACCL
        LDA GapBuffer.GapValueH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Gap start
        LDA #(gapStartLabel % 256)
        STA ZP.STRL
        LDA #(gapStartLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbGapStartL
        STA ZP.ACCL
        LDA GapBuffer.gbGapStartH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Gap end
        LDA #(gapEndLabel % 256)
        STA ZP.STRL
        LDA #(gapEndLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbGapEndL
        STA ZP.ACCL
        LDA GapBuffer.gbGapEndH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Buffer size
        LDA #(sizeLabel % 256)
        STA ZP.STRL
        LDA #(sizeLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbBufferSizeL
        STA ZP.ACCL
        LDA GapBuffer.gbBufferSizeH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Undo operation type
        LDA #(undoOpLabel % 256)
        STA ZP.STRL
        LDA #(undoOpLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.undoOp
        Debug.LabeledByte();  // Shows 0=None, 1=Inserted, 2=Deleted
        
        // Undo gap start
        LDA #(undoStartLabel % 256)
        STA ZP.STRL
        LDA #(undoStartLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.undoGapStartL
        STA ZP.ACCL
        LDA GapBuffer.undoGapStartH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Undo gap end
        LDA #(undoEndLabel % 256)
        STA ZP.STRL
        LDA #(undoEndLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.undoGapEndL
        STA ZP.ACCL
        LDA GapBuffer.undoGapEndH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Dump first 64 raw bytes from buffer
        LDA #(rawLabel % 256)
        STA ZP.STRL
        LDA #(rawLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        LDA GapBuffer.gbBufferL
        STA ZP.IDXL
        LDA GapBuffer.gbBufferH
        STA ZP.IDXH
        LDA #48
        Debug.DumpMemory();
        
        // Show what GetCharAt returns for first 48 positions
        LDA #(logLabel % 256)
        STA ZP.STRL
        LDA #(logLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        LDX #32
        STZ ZP.IDYL  // Position counter
        loop
        {
            PHX
            
            // Get character at position IDYL
            LDA ZP.IDYL
            STA GapBuffer.GapValueL
            STZ GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Show it
            Debug.ByteSpace();
            
            INC ZP.IDYL
            PLX
            DEX
            if (Z) { break; }
        }
        
        // Restore ZP.IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
    }
#endif    
}
