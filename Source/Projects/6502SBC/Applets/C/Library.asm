unit Library
{
    uses "AST"
    uses "Errors"
    
    const byte libSlots = 0xC0;
    const byte libArg  = libSlots+0;
    const byte libArgL = libSlots+0;
    const byte libArgH = libSlots+1;
    
    const byte libStr  = libSlots+2;
    const byte libStrL = libSlots+2;
    const byte libStrH = libSlots+3;
    
    const string sysprintf  = "printf";
    const string sysmillis  = "millis";
    const string sysseconds = "seconds";
    const string sysputchar = "putchar";
    
    // Memory management
    const string sysmalloc  = "malloc";
    const string sysfree    = "free";
    
    // File I/O
    const string sysfopen   = "fopen";
    const string sysfclose  = "fclose";
    const string sysfgetc   = "fgetc";
    const string sysfputc   = "fputc";
    const string sysfgets   = "fgets";
    const string sysfputs   = "fputs";
    const string sysfeof    = "feof";
    const string sysfread   = "fread";
    const string sysfwrite  = "fwrite";
    
    enum FileFunction
    {
        // starting at 0xF0 so they don't collide with BIOS calls
        FOpen   = 0xF0,
        FClose  = 0xF1,
        FGetC   = 0xF2,
        FPutC   = 0xF3,
        FGetS   = 0xF4,
        FPutS   = 0xF5,
        FEof    = 0xF6,
        FRead   = 0xF7,
        FWrite  = 0xF8,
    }
    
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
         
        // Check for "millis"
        LDA #(sysmillis % 256)
        STA ZP.IDYL
        LDA #(sysmillis / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.TimeMillis
            SEC
            return;
        }    
        
        // Check for "seconds"
        LDA #(sysseconds % 256)
        STA ZP.IDYL
        LDA #(sysseconds / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.TimeSeconds
            SEC
            return;
        }   
        
        // Check for "seconds"
        LDA #(sysputchar % 256)
        STA ZP.IDYL
        LDA #(sysputchar / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.PrintChar
            SEC
            return;
        } 
        
        // Check for "malloc"
        LDA #(sysmalloc % 256)
        STA ZP.IDYL
        LDA #(sysmalloc / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.MemAllocate
            SEC
            return;
        } 
        
         // Check for "free"
        LDA #(sysfree % 256)
        STA ZP.IDYL
        LDA #(sysfree / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA # BIOSInterface.SysCall.MemFree
            SEC
            return;
        }  
        
        
        // TODO : add more system functions here...
        
        CLC  // Not a system function
    }
    
    
    // Check if a function name is a file function
    // Input: ZP.STR = function name to check
    // Output: A = FileFunction enum value if file function
    //         C set if file function, clear if not
    IsFileFunction()
    {
        // Check for "fopen"
        LDA #(sysfopen % 256)
        STA ZP.IDYL
        LDA #(sysfopen / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();  // Compare [STR] with [IDY]
        if (C)
        {
            LDA #FileFunction.FOpen
            SEC
            return;
        }
        
        // Check for "fclose"
        LDA #(sysfclose % 256)
        STA ZP.IDYL
        LDA #(sysfclose / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FClose
            SEC
            return;
        }
        
        // Check for "fgetc"
        LDA #(sysfgetc % 256)
        STA ZP.IDYL
        LDA #(sysfgetc / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FGetC
            SEC
            return;
        }
        
        // Check for "fputc"
        LDA #(sysfputc % 256)
        STA ZP.IDYL
        LDA #(sysfputc / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FPutC
            SEC
            return;
        }
        
        // Check for "fgets"
        LDA #(sysfgets % 256)
        STA ZP.IDYL
        LDA #(sysfgets / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FGetS
            SEC
            return;
        }
        
        // Check for "fputs"
        LDA #(sysfputs % 256)
        STA ZP.IDYL
        LDA #(sysfputs / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FPutS
            SEC
            return;
        }
        
        // Check for "feof"
        LDA #(sysfeof % 256)
        STA ZP.IDYL
        LDA #(sysfeof / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FEof
            SEC
            return;
        }
        
        // Check for "fread"
        LDA #(sysfread % 256)
        STA ZP.IDYL
        LDA #(sysfread / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FRead
            SEC
            return;
        }
        
        // Check for "fwrite"
        LDA #(sysfwrite % 256)
        STA ZP.IDYL
        LDA #(sysfwrite / 256)
        STA ZP.IDYH
        CodeGen.CompareStrings();
        if (C)
        {
            LDA #FileFunction.FWrite
            SEC
            return;
        }
        
        CLC  // Not a file function
    }
    
    
    
    // Emit runtime loop to print literal characters until % or \0
    // Input:  Y = current position in format string
    // Output: Y = position after literal run (at % or \0)
    //         C set on success, clear on failure
    emitLiteralRun()
    {
        // Check if we have any literals to print
        LDA [ZP.STR], Y
        if (Z) { SEC return; }  // End of string
        CMP #'%'
        if (Z)
        {
            PHY
            INY
            LDA [ZP.STR], Y
            CMP #'%'  // Check for %%
            PLY
            if (NZ) { SEC return; }  // Not %%, stop run
        }
        
        // Emit: LDY #offset
        LDA # OpCode.LDY_IMM
        EmitByte(); if (NC) { return; }
        TYA  // Current offset into string
        EmitByte(); if (NC) { return; }
        
// loopStart:
        // Emit: LDA [ZP.STR],Y
        LDA # OpCode.LDA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA # ZP.STR
        EmitByte(); if (NC) { return; }
        
        // find the Y value for the end of this run
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }  // End of string
            
            CMP #'%'
            if (Z)
            {
                INY
                LDA [ZP.STR], Y
                DEY
                CMP #'%'
                if (NZ) { break; }  // Not %%, end run
                INY  // Skip first %
            }
            
            INY
        }
        
        // Emit: CMP #end_offset
        LDA # OpCode.CPY_IMM
        EmitByte(); if (NC) { return; }
        TYA  // End offset
        EmitByte(); if (NC) { return; }
        
        // Emit: BEQ done
        LDA # OpCode.BEQ
        EmitByte(); if (NC) { return; }
        LDA # 8  // Skip next 8 bytes
        EmitByte(); if (NC) { return; }
        
        // Emit: LDX #PrintChar
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.PrintChar
        EmitByte(); if (NC) { return; }
        
        // Emit: JSR dispatch
        EmitDispatchCall(); if (NC) { return; }
        
        // Emit: INY
        LDA # OpCode.INY
        EmitByte(); if (NC) { return; }
        
        // Emit: BRA loop
        LDA # OpCode.BRA
        EmitByte(); if (NC) { return; }
        LDA # 0xF2  // CPY(2) + BEQ(2) + LDX(2) + JSR(3) + INY(1) + LDA(2) + 2 more = 14
        EmitByte(); if (NC) { return; }
        
        // done:
        SEC
    }
    
    PutcharCall()
    {
        // Get first argument node (skip identifier, get its sibling)
        LDY #AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        LDY #AST.iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        
        // Generate argument and pop to NEXT
        CodeGen.generateExpression(); if (NC) { return; }
        
#ifdef DEBUGSTACK        
        LDX #'p'
#endif
        CodeGen.popNEXT(); if (NC) { return; }
        
        // Move character from NEXT0 to A and call PrintChar
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA #BIOSInterface.SysCall.PrintChar
        EmitByte(); if (NC) { return; }
        
        EmitDispatchCall(); if (NC) { return; }
        
        // Push character back as return value (already in NEXT0, rest is zero)
#ifdef DEBUGSTACK
        LDX #'h'
#endif
        CodeGen.pushNEXT();
        
        SEC
    }
    
    MillisCall()
    {
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.TimeMillis
        EmitByte(); if (NC) { return; }
        
        EmitDispatchCall(); if (NC) { return; }
        
#ifdef DEBUGSTACK
        LDX #'n'
#endif
        CodeGen.pushTOP();
        
        SEC
    }
    
    SecondsCall()
    {
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.TimeSeconds
        EmitByte(); if (NC) { return; }
        
        EmitDispatchCall(); if (NC) { return; }
        
#ifdef DEBUGSTACK
        LDX #'o'
#endif
        CodeGen.pushTOP();
        
        SEC
    }
    
    // Generate code to print int/long argument
    // Uses current argument node in ZP.NEXT
    // Output: C set on success, clear on failure
    emitIntFormatter()
    {
        // Check we have an argument
        LDA libArgL
        ORA libArgH
        if (Z)
        {
            LDA # Error.TooFewArguments
            Errors.ShowIDX();
            CLC
            return;
        }
        
        // Generate code to evaluate the expression
        // This will push result onto runtime stack
        LDA libArgL
        STA ZP.IDXL
        LDA libArgH
        STA ZP.IDXH
        CodeGen.generateExpression(); if (NC) { return; }
        
        // Pop from stack into ZP.TOP
#ifdef DEBUGSTACK
        LDX #'a'
#endif
        CodeGen.popTOP(); if (NC) { return; }
        
        // Call Long.Print
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.LongPrint
        EmitByte(); if (NC) { return; }
        
        EmitDispatchCall(); if (NC) { return; }
        
        // Move to next argument
        LDY #AST.iNext
        LDA [libArg], Y
        TAX
        INY
        LDA [libArg], Y
        STA libArgH
        STX libArgL
        
        SEC
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
            
            // Get pointer to actual string data in heap
            LDY #AST.iData
            LDA [ZP.IDX], Y
            STA libStrL
            INY
            LDA [ZP.IDX], Y
            STA libStrH
                        
            // Get first value argument (sibling of format string)
            LDY #AST.iNext
            LDA [ZP.IDX], Y
            STA libArgL
            INY
            LDA [ZP.IDX], Y
            STA libArgH
            
            // Walk format string at compile time
            LDY #0
            loop
            {
                LDA libStrL
                STA ZP.STRL
                LDA libStrH
                STA ZP.STRH
                
                // Start a literal run - emit runtime loop to print chars
                emitLiteralRun(); if (NC) { break; }
                
                // Check what stopped the run
                LDA [ZP.STR], Y
                if (Z) { break; }  // End of string
                
                // Must be a % formatter
                INY  // Skip '%'
                LDA [ZP.STR], Y
                
                CMP #'d'  // %d - int
                if (Z)
                {
                    PHY
                    emitIntFormatter();
                    PLY
                    if (NC) { break; }  // Same as %d
                    INY
                    continue;
                }
                
                CMP #'l'  // %ld - long
                if (Z)
                {
                    INY
                    LDA [ZP.STR], Y
                    CMP #'d'
                    if (Z)
                    {
                        PHY
                        emitIntFormatter(); 
                        PLY
                        if (NC) { break; }  // Same as %d
                        INY
                        continue;
                    }
                    DEY  // Not %ld
                }
                
                // Unknown formatter - treat % as literal
                DEY  // Back to '%'
            }
            
            SEC  // Success
            break;
        } // single exit
    }
    
}
