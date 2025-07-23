unit GlobalManager
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/Utilities"
    
    friend Interpreter, BytecodeExecutor, Tools;
    
    const string typeInt = "INT";
    const string typeWord = "WORD"; 
    const string typeByte = "BYTE";
    const string typeBit = "BIT";
    const string typeString = "STRING";
    const string typeUnknown = "?";    
    
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
    
    // Global memory layout (variable length):
    //   [nextPtr:2] [name:null-terminated] [type:1] [value:2] [flags:1]
    //   Total: 6 + strlen(name) bytes per entry
    
    const uint ghNext = 0;      // 2 bytes: next global pointer
    const uint ghName = 2;      // Variable bytes: null-terminated name
    // ghType, ghValue, ghFlags offsets are dynamic based on name length
    
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
            
            // Find the type offset (skip past null-terminated name)
            LDY # ghName  // ghName = 2 (after next pointer)
            loop
            {
                LDA [ZP.IDX], Y
                if (Z)
                { 
                    INY
                    break; // Found null terminator
                }  
                INY
            }
            // Y now points to type byte
            
            // Check if this is a variable (not constant)
            LDA [ZP.IDX], Y
            IsConstant();  // Returns C=1 if constant
            if (NC)  // It's a variable, reset to default (0)
            {
                INY  // Move to value
                LDA #0
                STA [ZP.IDX], Y
                INY
                STA [ZP.IDX], Y
            }
            
            // Move to next global - follow the next pointer at offset 0
            LDY # ghNext  // ghNext = 0
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
    }
    
    // Find global by name at address IDY (null-terminated)
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
                CMP #1  // Set Z=0
                break;
            }
            
            // Compare null-terminated name at IDY with stored name
            PHX                         // Save X
            LDX #0                      // Index into name at IDY
            LDY #ghName                 // Stored name position
            
            // Compare character by character
            loop
            {
                // Get character from name at IDY
                LDA [ZP.IDY], X
                PHA                     // Save name char
                
                // Get stored character and convert to uppercase
                LDA [ZP.IDX], Y
                
                // Convert name char to uppercase
                PLA                     // Get name char back
                CMP #'a'
                if (C)                  // >= 'a'
                {
                    CMP #('z'+1)
                    if (NC)             // <= 'z'
                    {
                        SBC #('a'-'A'-1)  // Convert to uppercase
                    }
                }
                
                // Compare characters
                CMP [ZP.IDX], Y
                if (NZ) { break; }      // Characters don't match
                
                // Check if both are null (end of both strings)
                if (Z)                  // Both chars are null - match found!
                {
                    PLX                 // Restore X
                    LDA #1
                    CMP #1              // Set Z=1
                    return;
                }
                
                INX                     // Next name character
                INY                     // Next stored character
            }
            
            PLX                         // Restore X
            
            // No match - skip to next global
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
    // Input: Token name at TokenPtr (null-terminated)
    //        Type in ZP.FTYPE, Value in ZP.TOP
    AddGlobal()
    {
        // First check if this global already exists
        LDA ZP.TokenPtr
        STA ZP.IDYL
        LDA ZP.TokenPtrHi
        STA ZP.IDYH
        
        FindGlobal();  // Sets IDX and Z flag
        if (Z)  // Found existing global
        {
            // Save the new value before GetGlobalValue overwrites TOP
            LDA ZP.TOPL
            PHA
            LDA ZP.TOPH
            PHA
            
            // Update existing global's value (but only if it's a variable, not a constant)
            GetGlobalValue();  // Returns type in FTYPE (overwrites TOP with old value)
            LDA ZP.FTYPE
            IsConstant();
            if (NC)  // It's a variable, not a constant - we can update it
            {
                // Find the value offset (skip past null-terminated name)
                LDY #ghName
                loop
                {
                    LDA [ZP.IDX], Y
                    if (Z) 
                    { 
                        INY  // Move past null terminator
                        INY  // Skip type byte to get to value
                        break; 
                    }
                    INY
                }
                // Y points to the value LOW BYTE position
                
                // Restore the new value and update
                PLA              // Get TOPH back
                INY              // Move to high byte position  
                STA [ZP.IDX], Y  // Store high byte
                DEY              // Back to low byte position
                PLA              // Get TOPL back
                STA [ZP.IDX], Y  // Store low byte
                
                return; // Done - updated existing variable
            }
            else
            {
                // It's a constant - restore stack and create new entry anyway
                PLA
                PLA
                // Fall through to create new entry (constants can be redefined)
            }
        }
        
        // Calculate name length from null-terminated string at TokenPtr
        LDX #0
        loop
        {
            LDA [ZP.TokenPtr], X
            if (Z) { break; }
            INX
        }
        // X now contains the length
        
        // Calculate total size: 2 (next) + X + 1 (null) + 1 (type) + 2 (value) + 1 (flags)
        TXA            // Transfer X to A
        CLC
        ADC #7         // Add the fixed overhead
        STA ZP.ACCL
        STZ ZP.ACCH
        
        // Allocate memory for new global
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
        
        // Copy name directly from TokenPtr (points to Address.BasicWorkBuffer)
        LDY #ghName
        LDX #0
        loop
        {
            LDA [ZP.TokenPtr], X
            STA [ZP.IDX], Y
            if (Z) 
            { 
                INY  // Move past the null terminator we just stored
                break; 
            }
            INX
            INY
        }
        // Y now points to the position after the null terminator
        
        // Store type, value, flags
        LDA ZP.FTYPE
        STA [ZP.IDX], Y
        INY
        LDA ZP.TOPL
        STA [ZP.IDX], Y
        INY
        LDA ZP.TOPH
        STA [ZP.IDX], Y
        INY
        LDA #0  // flags
        STA [ZP.IDX], Y
    }
    
    // Get global value
    // Input: Address in ZP.IDX
    // Output: Value in ZP.TOP, Type in ZP.FTYPE
    GetGlobalValue()
    {
        // Find the type offset (skip past null-terminated name)
        LDY #ghName  // Start at offset 2
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) 
            { 
                INY  // Move past null terminator to type byte
                break; 
            }
            INY
        }
        // Y now points to type byte
        
        LDA [ZP.IDX], Y
        STA ZP.FTYPE
        
        INY  // Move to value
        LDA [ZP.IDX], Y
        STA ZP.TOPL
        INY
        LDA [ZP.IDX], Y
        STA ZP.TOPH
    }
    
    // Debug function to list globals (variables and constants)
    // If bit 0 of BasicFlags clear, show variables; if set, show constants
    ListGlobals()
    {
        // Use GlobalManager's actual list
        LDA ZP.VarListHead
        STA ZP.IDXL
        LDA ZP.VarListHeadHi
        STA ZP.IDXH
        
        // Check if we have any globals at all
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            // For now, just return (no globals)
            return;
        }
        
        loop
        {
            // Check for end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Get type and value - but save IDX first
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            // Get type and value
            GlobalManager.GetGlobalValue();  // Returns type in FTYPE, value in TOP
            
            // Restore IDX
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Check if this matches what we want to show
            LDA ZP.FTYPE
            GlobalManager.IsConstant();  // Returns C=1 if constant
            
            if (BBR0, ZP.BasicFlags) // Showing variables
            {
                if (C) // This is a constant, skip it
                {
                    // Move to next global
                    LDY #GlobalManager.ghNext
                    LDA [ZP.IDX], Y
                    PHA
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    continue;
                }
            }
            else // Showing constants
            {
                if (NC) // This is a variable, skip it
                {
                    // Move to next global
                    LDY #GlobalManager.ghNext
                    LDA [ZP.IDX], Y
                    PHA
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    continue;
                }
            }
            // Print the global: TYPE NAME = VALUE
            printGlobalTypePrefix();
            
            LDA #' '
            Serial.WriteChar();
            
            // Print name (null-terminated)
            LDY #GlobalManager.ghName
            loop
            {
                LDA [ZP.IDX], Y
                if (Z) { break; }    // Stop at null terminator
                
                Serial.WriteChar();
                INY
            }
            
            // Print " = "
            LDA #' '
            Serial.WriteChar();
            LDA #'='
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Print value (already in TOP from GetGlobalValue)
            Tools.PrintDecimalWord();
            
            // Print newline
            LDA #'\n'
            Serial.WriteChar();
            
            // Move to next global
            LDY #GlobalManager.ghNext
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
#ifdef DEBUG        
        DumpHeap();
#endif
    }
    
    // Helper to print type prefix for globals
    printGlobalTypePrefix()
    {
        // Save IDX on the stack since Tools.PrintString() will overwrite it
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        LDA ZP.FTYPE
        AND #0x7F  // Clear constant flag to get base type
        switch (A)
        {
            case GlobalTypes.VarInt:
            {
                LDA #(typeInt % 256)
                STA ZP.IDXL
                LDA #(typeInt / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            case GlobalTypes.VarWord:
            {
                LDA #(typeWord % 256)
                STA ZP.IDXL
                LDA #(typeWord / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            case GlobalTypes.VarByte:
            {
                LDA #(typeByte % 256)
                STA ZP.IDXL
                LDA #(typeByte / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            case GlobalTypes.VarBit:
            {
                LDA #(typeBit % 256)
                STA ZP.IDXL
                LDA #(typeBit / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            case GlobalTypes.VarString:
            {
                LDA #(typeString % 256)
                STA ZP.IDXL
                LDA #(typeString / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            default:
            {
                LDA #(typeUnknown % 256)
                STA ZP.IDXL
                LDA #(typeUnknown / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
        }
        
        // Restore IDX from the stack
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
    }
}