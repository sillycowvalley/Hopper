unit Library
{
    uses "AST"
    
    const byte libSlots = 0xC0;
    const byte formatArg  = libSlots+0;
    const byte formatArgL = libSlots+0;
    const byte formatArgH = libSlots+1;
    
    const string sysprintf = "printf";
    
    // Emit a JSR to the BIOS dispatch function
    // Output: C set on success, clear on failure
    // Note: Assumes X register contains syscall number
    EmitDispatchCall()
    {
        LDA # OpCode.JSR
        EmitByte(); if (NC) { return; }
        
        // Add base to offset to get absolute address (4th byte into our code after the entrypoint JMP)
        CLC
        LDA # (BIOSInterface.EntryPoint % 256)
        ADC # 3
        EmitByte(); if (NC) { return; }
        LDA # (BIOSInterface.EntryPoint / 256)
        EmitByte(); 
    }
    
    // Emit JSR Print.Char
    EmitPrintChar()
    {
        // LDX #SysCall.PrintChar
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.PrintChar
        EmitByte(); if (NC) { return; }
        
        // JSR to dispatcher (will RTS back here)
        EmitDispatchCall();
    }
    
    // Check if a function name corresponds to a system function
    // Input: ZP.STR = function name to check
    // Output: A = syscall number if system function
    //         C set if system function, clear if user function
    IsSystemFunction()
    {
        // Check for "printf"
        LDA #(sysprintf % 256)
        STA ZP.IDYL
        LDA #(sysprintf / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.PrintString
            SEC
            return;
        }
             
        // TODO : add more system functions here...
        
        CLC  // Not a system function
    }
    
    // Generate code for a printf system call
    // Input: IDX = CallExpr node for printf
    // Output: C set on success, clear on failure
    // Note: Currently only supports string literal as first argument
    PrintfCall()
    {
        loop
        {
            // first child is identifier (which we already know is "printf")
            LDY #AST.iChild
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Move to first argument (sibling of identifier)
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            // Should be a StringLit node
            LDY #AST.iNodeType
            LDA [ZP.IDX], Y
            CMP #AST.NodeType.StringLit
            if (NZ) 
            {
                LDA # Token.StringLiteral
                Errors.Expected();
                break;
            }
            
            // Get string's offset (stored during emitStrings)
            LDY #AST.iOffset
            LDA [ZP.IDX], Y
            STA ZP.ACCL
            INY
            LDA [ZP.IDX], Y
            STA ZP.ACCH
            
            CodeGen.AddEntryPoint();
            
            // Generate: LDA #low(string)
            LDA # OpCode.LDA_IMM
            EmitByte(); if (NC) { break;}
            LDA ZP.ACCL
            EmitByte();if (NC) { break;}
            
            // Generate: STA ZP.STRL
            LDA #OpCode.STA_ZP
            EmitByte();if (NC) { break;}
            LDA #ZP.STRL
            EmitByte();if (NC) { break;}
            
            // Generate: LDA #high(string)
            LDA #OpCode.LDA_IMM
            EmitByte();if (NC) { break;}
            LDA ZP.ACCH
            EmitByte();if (NC) { break;}
            
            // Generate: STA ZP.STRH
            LDA # OpCode.STA_ZP
            EmitByte();if (NC) { break;}
            LDA # ZP.STRH
            EmitByte();if (NC) { break;}
            
            // at this point at runtime, the format string is in ZP.STR
            
            // Generate: LDY #0
            LDA # OpCode.LDY_IMM
            EmitByte(); if (NC) { break; }
            LDA # 0
            EmitByte(); if (NC) { break; }
            
//loopStart            :
            // Generate: LDA [ZP.STR],Y
            LDA # OpCode.LDA_IND_Y
            EmitByte(); if (NC) { break; }
            LDA # ZP.STR
            EmitByte(); if (NC) { break; }
            
            // Generate: BEQ done
            LDA # OpCode.BEQ
            EmitByte(); if (NC) { break; }
            LDA # 8     // Skip next 8 bytes: LDX(2) + JSR(3) + INY(1) + BRA(2)
            EmitByte(); if (NC) { break; }
            
            EmitPrintChar();
            
            // Generate: INY
            LDA # OpCode.INY
            EmitByte(); if (NC) { break; }
            
            // Generate: BRA loopStart
            LDA # OpCode.BRA
            EmitByte(); if (NC) { break; }
            LDA # 0xF4  // -12 in two's complement (go back 12 bytes to LDA [ZP.STR],Y)
            EmitByte(); if (NC) { break; }
// done:                      
            
            SEC  // Success
            break;
        } // single exit
    }
    
}
