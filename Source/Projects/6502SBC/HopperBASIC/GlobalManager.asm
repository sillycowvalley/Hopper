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
            LDY #ghName  // ghName = 2 (after next pointer)
            loop
            {
                LDA [ZP.IDX], Y
                INY
                if (Z) { break; }  // Found null terminator
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
            LDY #0  // ghNext should be 0, not 16!
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
    }
    
    // Find global by token name
    // Input: Token name from tokenizer (ZP.TokenStart, ZP.TokenLen)
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
            
            // Compare token with stored name
            PHX                         // Save X
            LDX ZP.TokenStart          // Token position in input buffer
            LDY #ghName                // Stored name position
            
            // Compare character by character
            loop
            {
                // Get token character (or 0 if past end)
                TXA
                SEC
                SBC ZP.TokenStart      // A = current position - start = offset
                CMP ZP.TokenLen
                if (C)                 // offset >= TokenLen
                {
                    LDA #0             // Past end of token, use null
                }
                else
                {
                    LDA Address.BasicInputBuffer, X
                    // Convert to uppercase
                    CMP #'a'
                    if (C)             // >= 'a'
                    {
                        CMP #('z'+1)
                        if (NC)        // <= 'z'
                        {
                            SBC #('a'-'A'-1)  // Convert to uppercase
                        }
                    }
                }
                PHA                    // Save token char
                
                // Get stored character
                LDA [ZP.IDX], Y
                
                // Compare characters
                PLA                    // Get token char back
                CMP [ZP.IDX], Y
                if (NZ) { break; }     // Characters don't match
                
                // Check if both are null (end of both strings)
                if (Z)                 // Both chars are null - match found!
                {
                    PLX                // Restore X
                    LDA #1
                    CMP #1             // Set Z=1
                    return;
                }
                
                INX                    // Next token character
                INY                    // Next stored character
            }
            
            PLX                        // Restore X
            
            // No match - skip to next global
            // First skip past the variable-length name
            LDY #ghName
            loop
            {
                LDA [ZP.IDX], Y
                INY
                if (Z) { break; }      // Found null terminator
            }
            // Skip past type (1), value (2), flags (1) = 4 more bytes
            INY
            INY
            INY
            INY
            
            // Actually, easier to just follow the next pointer
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
    // Input: Token name from tokenizer (ZP.TokenStart, ZP.TokenLen)
    //        Type in ZP.FTYPE, Value in ZP.TOP
    AddGlobal()
    {
        // Calculate total size: 2 (next) + TokenLen + 1 (null) + 1 (type) + 2 (value) + 1 (flags)
        CLC
        LDA #7
        ADC ZP.TokenLen
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
        
        // Copy name (TokenLen bytes + null terminator)
        LDY #ghName
        PHX                            // Save X
        LDX ZP.TokenStart
        LDA ZP.TokenLen
        STA ZP.ACCL                    // Save original TokenLen
        
        loop
        {
            LDA ZP.TokenLen
            if (Z) { break; }          // Copied all characters
            
            LDA Address.BasicInputBuffer, X
            // Convert to uppercase
            CMP #'a'
            if (C)                     // >= 'a'
            {
                CMP #('z'+1)
                if (NC)                // <= 'z'
                {
                    SBC #('a'-'A'-1)   // Convert to uppercase
                }
            }
            STA [ZP.IDX], Y
            
            INX
            INY
            DEC ZP.TokenLen            // Count down characters
        }
        
        PLX                            // Restore X
        
        // Add null terminator
        LDA #0
        STA [ZP.IDX], Y
        INY
        
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
        
        // Restore original TokenLen
        LDA ZP.ACCL
        STA ZP.TokenLen
    }
    
    // Get global value
    // Input: Address in ZP.IDX
    // Output: Value in ZP.TOP, Type in ZP.FTYPE
    GetGlobalValue()
    {
        // Find the type offset (skip past null-terminated name)
        LDY #ghName
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
           
        DumpVariables();
    }
    
    // Debug function to list globals (variables and constants)
    // If ZP.BasicFlags = 0, show variables; if ZP.BasicFlags = 1, show constants
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
            
            LDA ZP.BasicFlags  // What are we showing? 0=vars, 1=consts
            if (Z)     // Showing variables
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
            else       // Showing constants
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
    }
    
    // Helper to print type prefix for globals
    printGlobalTypePrefix()
    {
        LDA ZP.FTYPE
        AND #0x7F  // Clear constant flag to get base type
        switch (A)
        {
            case GlobalTypes.VarInt:
            {
                LDA #(typeInt % 256)
                STA ZP.IDYL
                LDA #(typeInt / 256)
                STA ZP.IDYH
                // Save current IDX
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                // Use IDY for PrintString
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Tools.PrintString();
                // Restore IDX
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            case GlobalTypes.VarWord:
            {
                LDA #(typeWord % 256)
                STA ZP.IDYL
                LDA #(typeWord / 256)
                STA ZP.IDYH
                // Save/restore IDX pattern as above
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Tools.PrintString();
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            case GlobalTypes.VarByte:
            {
                LDA #(typeByte % 256)
                STA ZP.IDYL
                LDA #(typeByte / 256)
                STA ZP.IDYH
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Tools.PrintString();
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            case GlobalTypes.VarBit:
            {
                LDA #(typeBit % 256)
                STA ZP.IDYL
                LDA #(typeBit / 256)
                STA ZP.IDYH
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Tools.PrintString();
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            case GlobalTypes.VarString:
            {
                LDA #(typeString % 256)
                STA ZP.IDYL
                LDA #(typeString / 256)
                STA ZP.IDYH
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Tools.PrintString();
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            default:
            {
                LDA #(typeUnknown % 256)
                STA ZP.IDYL
                LDA #(typeUnknown / 256)
                STA ZP.IDYH
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Tools.PrintString();
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
        }
    }
}
