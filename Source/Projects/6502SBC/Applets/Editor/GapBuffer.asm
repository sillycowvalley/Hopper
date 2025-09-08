unit GapBuffer
{
    uses "System/Definitions"
    uses "System/Memory"
    
    // Zero page allocation
    const byte gbSlots = 0x58;  // Base address - easy to relocate
    
    // Private zero page state
    const uint gbBuffer = gbSlots+0;    // Start of allocated buffer
    const uint gbBufferL = gbSlots+0;
    const uint gbBufferH = gbSlots+1;
    
    const uint gbGapStart = gbSlots+2;  // Current gap position
    const uint gbGapStartL = gbSlots+2;
    const uint gbGapStartH = gbSlots+3;
    
    const uint gbGapEnd = gbSlots+4;    // End of gap
    const uint gbGapEndL = gbSlots+4;
    const uint gbGapEndH = gbSlots+5;
    
    const uint gbBufferSize = gbSlots+6; // Total allocated size
    const uint gbBufferSizeL = gbSlots+6;
    const uint gbBufferSizeH = gbSlots+7;
    
    // Leaf workspace for calculations (don't survive function calls)
    const uint gbTemp = ZP.M0;     // Temporary 16-bit value
    const uint gbTempL = ZP.M0;
    const uint gbTempH = ZP.M1;
    const uint gbCount = ZP.M2;    // Copy count
    const uint gbCountL = ZP.M2;
    const uint gbCountH = ZP.M3;
    const uint gbSrc = ZP.M4;      // Source pointer
    const uint gbSrcL = ZP.M4;
    const uint gbSrcH = ZP.M5;
    const uint gbDst = ZP.M6;      // Destination pointer
    const uint gbDstL = ZP.M6;
    const uint gbDstH = ZP.M7;
    
    // Initialize gap buffer
    // Input: A,Y = size to allocate
    // Output: C set on success, clear on failure
    Initialize()
    {
        // Save requested size
        STA ZP.ACCL
        STY ZP.ACCH
        
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
        
        // Store buffer size
        LDA ZP.ACCL
        STA gbBufferSizeL
        LDA ZP.ACCH
        STA gbBufferSizeH
        
        // Gap starts at beginning and spans entire buffer
        STZ gbGapStartL
        STZ gbGapStartH
        
        LDA gbBufferSizeL
        STA gbGapEndL
        LDA gbBufferSizeH
        STA gbGapEndH
        
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
        }
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
    }
    
    // Public read-only accessors
    GetGapStart()
    {
        LDA gbGapStartL
        STA ZP.ACCL
        LDA gbGapStartH
        STA ZP.ACCH
    }
    
    GetGapEnd()
    {
        LDA gbGapEndL
        STA ZP.ACCL
        LDA gbGapEndH
        STA ZP.ACCH
    }
    
    GetTextLength()
    {
        // TextLength = BufferSize - (GapEnd - GapStart)
        SEC
        LDA gbGapEndL
        SBC gbGapStartL
        STA gbTempL
        LDA gbGapEndH
        SBC gbGapStartH
        STA gbTempH
        
        SEC
        LDA gbBufferSizeL
        SBC gbTempL
        STA ZP.ACCL
        LDA gbBufferSizeH
        SBC gbTempH
        STA ZP.ACCH
    }
    
    IsGapAtPosition()
    {
        // Input: ACC = position
        // Output: C set if gap is at position
        LDA ZP.ACCL
        CMP gbGapStartL
        if (NZ)
        {
            CLC
            return;
        }
        LDA ZP.ACCH
        CMP gbGapStartH
        if (NZ)
        {
            CLC
            return;
        }
        SEC
    }
    
    // Move gap to specified position
    // Input: ACC = target position
    MoveGapTo()
    {
        // Save target position
        LDA ZP.ACCL
        STA gbTempL
        LDA ZP.ACCH
        STA gbTempH
        
        // Check if already at position
        LDA gbTempL
        CMP gbGapStartL
        if (Z)
        {
            LDA gbTempH
            CMP gbGapStartH
            if (Z) { return; }  // Already there
        }
        
        // Determine direction and move gap
        loop  // Single iteration for structure
        {
            LDA gbTempH
            CMP gbGapStartH
            if (Z)
            {
                LDA gbTempL
                CMP gbGapStartL
            }
            if (C)  // target < gap start (move gap left)
            {
                // Calculate bytes to move: gbGapStart - target
                SEC
                LDA gbGapStartL
                SBC gbTempL
                STA gbCountL
                LDA gbGapStartH
                SBC gbTempH
                STA gbCountH
                
                // Source: buffer + target
                CLC
                LDA gbBufferL
                ADC gbTempL
                STA gbSrcL
                LDA gbBufferH
                ADC gbTempH
                STA gbSrcH
                
                // Destination: buffer + gbGapEnd - count
                SEC
                LDA gbGapEndL
                SBC gbCountL
                STA gbDstL
                LDA gbGapEndH
                SBC gbCountH
                STA gbDstH
                CLC
                LDA gbDstL
                ADC gbBufferL
                STA gbDstL
                LDA gbDstH
                ADC gbBufferH
                STA gbDstH
                
                // Copy bytes
                copyBytes();
                
                // Update gap position
                SEC
                LDA gbGapEndL
                SBC gbCountL
                STA gbGapEndL
                LDA gbGapEndH
                SBC gbCountH
                STA gbGapEndH
            }
            else  // target > gap start (move gap right)
            {
                // Calculate bytes to move: target - gbGapStart
                SEC
                LDA gbTempL
                SBC gbGapStartL
                STA gbCountL
                LDA gbTempH
                SBC gbGapStartH
                STA gbCountH
                
                // Source: buffer + gbGapEnd
                CLC
                LDA gbBufferL
                ADC gbGapEndL
                STA gbSrcL
                LDA gbBufferH
                ADC gbGapEndH
                STA gbSrcH
                
                // Destination: buffer + gbGapStart
                CLC
                LDA gbBufferL
                ADC gbGapStartL
                STA gbDstL
                LDA gbBufferH
                ADC gbGapStartH
                STA gbDstH
                
                // Copy bytes
                copyBytes();
                
                // Update gap position
                CLC
                LDA gbGapEndL
                ADC gbCountL
                STA gbGapEndL
                LDA gbGapEndH
                ADC gbCountH
                STA gbGapEndH
            }
            break;
        }
        
        // Set new gap start
        LDA gbTempL
        STA gbGapStartL
        LDA gbTempH
        STA gbGapStartH
    }
    
    // Helper: Copy bytes from src to dst
    copyBytes()
    {
        // Uses gbSrc, gbDst, gbCount
        LDA gbCountL
        ORA gbCountH
        if (Z) { return; }  // Nothing to copy
        
        // Set up pointers
        LDA gbSrcL
        STA ZP.IDXL
        LDA gbSrcH
        STA ZP.IDXH
        
        LDA gbDstL
        STA ZP.IDYL
        LDA gbDstH
        STA ZP.IDYH
        
        LDY #0
        loop
        {
            LDA [ZP.IDX], Y
            STA [ZP.IDY], Y
            
            // Increment pointers
            INC ZP.IDXL
            if (Z) { INC ZP.IDXH }
            INC ZP.IDYL
            if (Z) { INC ZP.IDYH }
            
            // Decrement count
            SEC
            LDA gbCountL
            SBC #1
            STA gbCountL
            LDA gbCountH
            SBC #0
            STA gbCountH
            
            // Check if done
            ORA gbCountL
            if (Z) { break; }
        }
    }
    
    // Insert character at gap position
    // Input: A = character to insert
    // Note: Assumes gap is at correct position
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
            LDY #0
            STA [ZP.IDX], Y
            
            // Advance gap start
            INC gbGapStartL
            if (Z) { INC gbGapStartH }
            
            SEC  // Success
            break;
        }
    }
    
    // Delete character before gap (backspace)
    // Note: Assumes gap is at correct position
    DeleteChar()
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
        
        SEC  // Success
    }
    
    // Delete character after gap (delete key)
    // Note: Assumes gap is at correct position
    DeleteForward()
    {
        loop  // Single iteration for structure
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
                    CLC  // At end of buffer
                    break;
                }
            }
            
            // Move gap end forward
            INC gbGapEndL
            if (Z) { INC gbGapEndH }
            
            SEC  // Success
            break;
        }
    }
    
    // Get character at logical position
    // Input: ACC = position
    // Output: A = character (0 if out of bounds)
    GetCharAt()
    {
        // Save position
        LDA ZP.ACCL
        STA gbTempL
        LDA ZP.ACCH
        STA gbTempH
        
        loop  // Single iteration for structure
        {
            // Check bounds
            GetTextLength();
            
            // Compare position against text length
            LDA ZP.ACCH
            CMP gbTempH
            if (C)
            {
                // text length > position, valid
            }
            else
            {
                if (Z)
                {
                    LDA ZP.ACCL
                    CMP gbTempL
                    if (NC)
                    {
                        // Out of bounds (position >= length)
                        LDA #0
                        break;
                    }
                }
                else
                {
                    // Out of bounds (position > length)
                    LDA #0
                    break;
                }
            }
            
            // Convert logical to physical position
            // Check if position is before or after gap
            LDA gbTempH
            CMP gbGapStartH
            if (C)
            {
                // Position is before gap, use as-is
            }
            else
            {
                if (Z)
                {
                    LDA gbTempL
                    CMP gbGapStartL
                    if (C)
                    {
                        // Position is before gap, use as-is
                    }
                    else
                    {
                        // Position is at or after gap, adjust
                        SEC
                        LDA gbGapEndL
                        SBC gbGapStartL
                        CLC
                        ADC gbTempL
                        STA gbTempL
                        LDA gbGapEndH
                        SBC gbGapStartH
                        ADC gbTempH
                        STA gbTempH
                    }
                }
                else
                {
                    // Position is after gap, adjust
                    SEC
                    LDA gbGapEndL
                    SBC gbGapStartL
                    CLC
                    ADC gbTempL
                    STA gbTempL
                    LDA gbGapEndH
                    SBC gbGapStartH
                    ADC gbTempH
                    STA gbTempH
                }
            }
            
            // Read from physical position
            CLC
            LDA gbBufferL
            ADC gbTempL
            STA ZP.IDXL
            LDA gbBufferH
            ADC gbTempH
            STA ZP.IDXH
            
            LDY #0
            LDA [ZP.IDX], Y
            break;
        }
    }
    
    // Grow buffer when gap is full
    growBuffer()
    {
        // Calculate new size (double current size)
        LDA gbBufferSizeL
        STA gbTempL
        LDA gbBufferSizeH
        STA gbTempH
        
        ASL gbTempL
        ROL gbTempH
        
        // Allocate new buffer
        LDA gbTempL
        STA ZP.ACCL
        LDA gbTempH
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
        
        // Save new buffer pointer in M8/M9 temporarily
        LDA ZP.IDXL
        STA ZP.M8
        LDA ZP.IDXH
        STA ZP.M9
        
        // Copy text before gap
        LDA gbBufferL
        STA gbSrcL
        LDA gbBufferH
        STA gbSrcH
        LDA ZP.M8
        STA gbDstL
        LDA ZP.M9
        STA gbDstH
        LDA gbGapStartL
        STA gbCountL
        LDA gbGapStartH
        STA gbCountH
        copyBytes();
        
        // Calculate text after gap size
        SEC
        LDA gbBufferSizeL
        SBC gbGapEndL
        STA gbCountL
        LDA gbBufferSizeH
        SBC gbGapEndH
        STA gbCountH
        
        // Copy text after gap (if any)
        LDA gbCountL
        ORA gbCountH
        if (NZ)
        {
            // Source: old buffer + old gap end
            CLC
            LDA gbBufferL
            ADC gbGapEndL
            STA gbSrcL
            LDA gbBufferH
            ADC gbGapEndH
            STA gbSrcH
            
            // Calculate new gap end
            SEC
            LDA gbTempL
            SBC gbCountL
            STA gbGapEndL
            LDA gbTempH
            SBC gbCountH
            STA gbGapEndH
            
            // Destination: new buffer + new gap end
            CLC
            LDA ZP.M8
            ADC gbGapEndL
            STA gbDstL
            LDA ZP.M9
            ADC gbGapEndH
            STA gbDstH
            
            copyBytes();
        }
        else
        {
            // No text after gap, gap extends to end
            LDA gbTempL
            STA gbGapEndL
            LDA gbTempH
            STA gbGapEndH
        }
        
        // Free old buffer
        LDA gbBufferL
        STA ZP.IDXL
        LDA gbBufferH
        STA ZP.IDXH
        Memory.Free();
        
        // Update buffer pointer and size
        LDA ZP.M8
        STA gbBufferL
        LDA ZP.M9
        STA gbBufferH
        LDA gbTempL
        STA gbBufferSizeL
        LDA gbTempH
        STA gbBufferSizeH
        
        SEC  // Success
    }
}