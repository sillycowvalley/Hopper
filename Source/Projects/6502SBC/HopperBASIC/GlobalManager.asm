unit GlobalManager
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/Utilities"
    
    friend Interpreter, BytecodeExecutor;
    
    // Global entry types
    enum GlobalTypes
    {
        VarInt    = 0x01,
        VarWord   = 0x02, 
        VarByte   = 0x03,
        VarBit    = 0x04,
        VarString = 0x05,
        
        ConstInt    = 0x81,  // High bit = constant flag
        ConstWord   = 0x82,
        ConstByte   = 0x83,
        ConstBit    = 0x84,
        ConstString = 0x85,
    }
    
    // Global memory layout (same for vars and constants):
    //   [nextPtr:2] [name:8] [type:1] [value:2] [flags:1]
    //   Total: 14 bytes per entry
    
    const uint ghNext = 0;      // 2 bytes: next global pointer
    const uint ghName = 2;      // 8 bytes: name (space-padded)
    const uint ghType = 10;     // 1 byte: type (with constant flag)
    const uint ghValue = 11;    // 2 bytes: value
    const uint ghFlags = 13;    // 1 byte: flags (future use)
    const uint ghSize = 14;     // Total size
    
    Initialize()
    {
        STZ ZP.VarListHead
        STZ ZP.VarListHeadHi
    }
    
    // Check if type represents a constant
    IsConstant()
    {
        // Type in A, returns C=1 if constant, C=0 if variable
        AND #0x80
        if (NZ) { SEC } else { CLC }
    }
    
    // Reset all variables to their declaration defaults (keep constants unchanged)
    ClearVariables()
    {
        // Walk the global list
        LDA ZP.VarListHead
        STA ZP.IDXL
        LDA ZP.VarListHeadHi
        STA ZP.IDXH
        
        loop
        {
            // Check for end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Check if this is a variable (not constant)
            LDY #ghType
            LDA [ZP.IDX], Y
            IsConstant();  // Returns C=1 if constant
            if (NC)  // It's a variable, reset to default (0)
            {
                LDY #ghValue
                LDA #0
                STA [ZP.IDX], Y
                INY
                STA [ZP.IDX], Y
            }
            
            // Move to next global
            LDY #ghNext
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
    }
    
    // Find global by name
    // Input: 8-byte space-padded name at ZP.FSOURCEADDRESS
    // Output: Address in ZP.IDX if found, Z=1 if found, Z=0 if not found
    FindGlobal()
    {
        // Walk the global list
        LDA ZP.VarListHead
        STA ZP.IDXL
        LDA ZP.VarListHeadHi
        STA ZP.IDXH
        
        loop
        {
            // Check for end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                LDA #0  // Not found, set Z=0
                break;
            }
            
            // Compare names (8 bytes)
            LDX #0
            loop
            {
                CPX #8
                if (Z)
                {
                    LDA #1  // Found - all 8 bytes matched
                    CMP #1  // Set Z=1
                    return;
                }
                
                LDY #ghName
                STX ZP.U0  // Save X
                TXA
                CLC
                ADC ghName
                TAY
                
                LDA [ZP.IDX], Y
                LDX ZP.U0  // Restore X
                CMP [ZP.FSOURCEADDRESS], X
                if (NZ)
                {
                    break;  // Names don't match
                }
                INX
            }
            
            // Check if we found a match
            CPX #8
            if (Z)
            {
                LDA #1  // Found
                CMP #1  // Set Z=1
                return;
            }
            
            // Move to next global
            LDY #ghNext
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
        // Not found
        LDA #0
        CMP #1  // Set Z=0
    }
    
    // Add or update global (with automatic redefinition)
    // Input: 8-byte space-padded name at ZP.FSOURCEADDRESS
    //        Type in ZP.FTYPE, Value in ZP.TOP
    AddGlobal()
    {
        // First check if it already exists and remove it
        FindGlobal();
        if (Z)  // Found existing - remove it first
        {
            // TODO: Remove existing global
            // For now, we'll allow duplicates
        }
        
        // Allocate memory for new global
        LDA #ghSize
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();  // Returns address in IDX
        
        // Link at head of list
        LDY #ghNext
        LDA ZP.VarListHead
        STA [ZP.IDX], Y
        INY
        LDA ZP.VarListHeadHi
        STA [ZP.IDX], Y
        
        // Update list head
        LDA ZP.IDXL
        STA ZP.VarListHead
        LDA ZP.IDXH
        STA ZP.VarListHeadHi
        
        // Copy name (8 bytes from FSOURCEADDRESS)
        LDY #ghName
        LDX #0
        loop
        {
            CPX #8
            if (Z) { break; }
            
            LDA [ZP.FSOURCEADDRESS], X
            STA [ZP.IDX], Y
            INX
            INY
        }
        
        // Store type, value, and flags
        LDY #ghType
        LDA ZP.FTYPE
        STA [ZP.IDX], Y
        
        LDY #ghValue
        LDA ZP.TOPL
        STA [ZP.IDX], Y
        INY
        LDA ZP.TOPH
        STA [ZP.IDX], Y
        
        LDY #ghFlags
        LDA #0
        STA [ZP.IDX], Y
    }
    
    // Get global value
    // Input: Address in ZP.IDX
    // Output: Value in ZP.TOP, Type in ZP.FTYPE
    GetGlobalValue()
    {
        LDY #ghType
        LDA [ZP.IDX], Y
        STA ZP.FTYPE
        
        LDY #ghValue
        LDA [ZP.IDX], Y
        STA ZP.TOPL
        INY
        LDA [ZP.IDX], Y
        STA ZP.TOPH
    }
}
