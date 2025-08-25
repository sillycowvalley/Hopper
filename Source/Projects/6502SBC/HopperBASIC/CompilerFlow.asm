unit CompilerFlow
{

    // Save current opcode buffer position to stack
    // Output: Current position pushed to stack as ZP.TOP
    // Modifies: ZP.TOPL, ZP.TOPH, stack
    saveCurrentPosition()
    {
        LDA ZP.OpCodeBufferContentLengthL
        STA ZP.TOPL
        LDA ZP.OpCodeBufferContentLengthH
        STA ZP.TOPH
        LDA # BASICType.WORD
        Stacks.PushTop(); // type in A
    }
    
    // Calculate forward offset between two buffer positions
    // Input: ZP.IDY = target position (where we want to jump TO)
    //        ZP.IDX = source position (where we're jumping FROM) 
    // Output: ZP.NEXT = forward offset (target - source)
    // Modifies: ZP.NEXTL, ZP.NEXTH
    calculateForwardOffset()
    {
        // Calculate forward offset: target_position - source_position
        SEC
        LDA ZP.IDYL    // Target position LSB
        SBC ZP.IDXL    // Subtract source position LSB
        STA ZP.NEXTL   // Store forward offset LSB
        LDA ZP.IDYH    // Target position MSB
        SBC ZP.IDXH    // Subtract source position MSB
        STA ZP.NEXTH   // Store forward offset MSB
    }
    
    // Patch 2-byte word operand in opcode buffer
    // Input: ZP.IDX = relative position in buffer (instruction start or operand position)
    //        ZP.NEXT = 16-bit value to patch in
    //        A = Y offset to operand bytes (0 if ZP.IDX points to operand, 1 if ZP.IDX points to instruction)
    // Output: Operand patched in buffer
    // Modifies: ZP.IDX (converts to absolute address), Y
    patchWordOperand()
    {
        TAY  // Save Y offset parameter
        
        // Calculate absolute address for patching
        CLC
        LDA ZP.OpCodeBufferL
        ADC ZP.IDXL    // Add relative position
        STA ZP.IDXL    // Absolute patch address LSB
        LDA ZP.OpCodeBufferH
        ADC ZP.IDXH    // Add relative position  
        STA ZP.IDXH    // Absolute patch address MSB
        
        // Patch the operand bytes with value in ZP.NEXT
        LDA ZP.NEXTL   // Forward offset LSB
        STA [ZP.IDX], Y   // Patch LSB
        INY
        LDA ZP.NEXTH   // Forward offset MSB
        STA [ZP.IDX], Y   // Patch MSB
    }
    
    // Calculate backward offset between two buffer positions  
    // Input: ZP.TOP = loop start position (where we want to jump TO)
    //        ZP.IDY = current position (where we're jumping FROM)
    // Output: ZP.TOP = backward offset (loop_start - current_position) 
    // Modifies: ZP.TOPL, ZP.TOPH
    calculateBackwardOffset()
    {
        // Calculate backward offset: loop_start - current_position
        // This will be negative (jumping backward)
        SEC
        LDA ZP.TOPL    // Loop start position LSB
        SBC ZP.IDYL    // Subtract current position LSB
        STA ZP.TOPL    // Store backward offset LSB
        LDA ZP.TOPH    // Loop start position MSB
        SBC ZP.IDYH    // Subtract current position MSB
        STA ZP.TOPH    // Store backward offset MSB
    }
    
    // Add instruction size to position (for calculating position after instruction)
    // Input: ZP.IDY = current position, A = instruction size in bytes
    // Output: ZP.IDY = position after instruction
    // Modifies: ZP.IDYL, ZP.IDYH
    addInstructionSizeToPosition()
    {
        // Add instruction size to current position
        CLC
        ADC ZP.IDYL        // Add instruction size to LSB
        STA ZP.IDYL        // Store updated position LSB
        LDA ZP.IDYH        // Get MSB
        ADC #0             // Add carry
        STA ZP.IDYH        // Store updated position MSB
    }

    // Subtract instruction size from offset (for PC adjustment in relative jumps)
    // Input: ZP.NEXT = raw offset, A = instruction size to subtract
    // Output: ZP.NEXT = PC-adjusted offset  
    // Modifies: ZP.NEXTL, ZP.NEXTH
    subtractInstructionSizeFromOffset()
    {
        // Subtract instruction size from offset: ZP.NEXT = ZP.NEXT - A
        STA ZP.ACCL        // Store instruction size temporarily
        SEC
        LDA ZP.NEXTL       // Get raw offset LSB
        SBC ZP.ACCL        // Subtract instruction size
        STA ZP.NEXTL       // Store adjusted offset LSB
        LDA ZP.NEXTH       // Get raw offset MSB
        SBC #0             // Subtract borrow
        STA ZP.NEXTH       // Store adjusted offset MSB
    }
    
    // Load current opcode buffer position into ZP.IDY
    // Output: ZP.IDY = current buffer position
    // Modifies: ZP.IDYL, ZP.IDYH
    loadCurrentPosition()
    {
        LDA ZP.OpCodeBufferContentLengthL
        STA ZP.IDYL
        LDA ZP.OpCodeBufferContentLengthH  
        STA ZP.IDYH
    }
    
    
    
    // Compile IF...THEN...ELSE...ENDIF statement (both single-line and multi-line forms)
    // Input: ZP.CurrentToken = IF token
    // Output: IF statement compiled to opcodes with correct forward jumps
    // Supports: IF cond THEN stmt ELSE stmt ENDIF (single-line)
    //          IF cond THEN block ELSE block ENDIF (multi-line)
    //          Mixed forms also work naturally
    const string compileIfStatementTrace = "CompIf // IF...THEN...ELSE...ENDIF";
    CompileIfStatement()
    {
    #ifdef TRACE
        LDA #(compileIfStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIfStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif

        // Preserve stack for nesting (WHILE pattern)
        LDA ZP.SP
        PHA
        
        // Preserve IF state for nesting (FOR pattern)
        LDA Compiler.compilerIfClauses
        PHA

        loop // Single exit block
        {
            // Skip IF token and compile condition expression
            Tokenizer.NextToken();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile condition expression (e.g., "X > 10")
            Compiler.CompileFoldedExpressionTree(); // IF <expression>
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            if (BBS0, ZP.CompilerFlags)
            {
                LDA # ZP.TOPT
                CMP # BASICType.BIT
                if (NZ)
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
            }
            
            // Expect THEN token
            LDA ZP.CurrentToken
            CMP #Token.THEN
            if (NZ)
            {
                Error.ExpectedThen(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            Tokenizer.NextToken();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Detect single-line vs multi-line form by peeking ahead
            LDA ZP.CurrentToken
            switch (A)
            {
                case Token.EOL:
                case Token.COMMENT:
                {
                    // Multi-line mode - statements start on next line
                    LDA Compiler.compilerIfClauses
                    AND #0xFE  // Clear BIT0 for multi-line
                    STA Compiler.compilerIfClauses
                    
                    Tokenizer.NextToken();  // Skip the EOL/comment
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                default:
                {
                    // Single-line mode - statement immediately follows
                    LDA Compiler.compilerIfClauses
                    ORA #0x01  // Set BIT0 for single-line
                    STA Compiler.compilerIfClauses
                }
            }
            
            // Save position for JUMPZW operand (jump to ELSE/ENDIF when condition false)
            saveCurrentPosition();
            
            // Emit conditional jump to ELSE/ENDIF (placeholder - will be patched)
            // JUMPZW: Jump if condition is zero/FALSE (skip THEN block)
            LDA #OpCode.JUMPZW
            STA Compiler.compilerOpCode
            STZ Compiler.compilerOperand1  // Placeholder LSB (will be patched)
            STZ Compiler.compilerOperand2  // Placeholder MSB (will be patched)
            Emit.OpCodeWithWord();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // === COMPILE THEN CLAUSE ===
            
            LDA Compiler.compilerIfClauses
            AND #0x01  // Test single-line mode
            if (NZ)    // Single-line mode (BIT0 = 1)?
            {
                // Single-line: compile one statement
                CompileStatement();
                CheckError();
                if (NC) { States.SetFailure(); break; }
            }
            else
            {
                // Multi-line: use statement block (handles ELSE/ENDIF termination)
                CompileStatementBlock();
                CheckError();
                if (NC) { States.SetFailure(); break; }
            }
            
            // === HANDLE ELSE OR ENDIF ===
            
            LDA ZP.CurrentToken
            CMP #Token.ELSE
            if (Z)
            {
                // === ELSE CLAUSE PRESENT ===
                
                // Save position for JUMPW operand (jump from end of THEN to after ENDIF)  
                saveCurrentPosition();
                
                // Emit unconditional jump to ENDIF (placeholder - will be patched)
                LDA #OpCode.JUMPW
                STA Compiler.compilerOpCode
                STZ Compiler.compilerOperand1  // Placeholder LSB (will be patched)
                STZ Compiler.compilerOperand2  // Placeholder MSB (will be patched)
                Emit.OpCodeWithWord();
                CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === PATCH JUMPZW (condition false -> jump to ELSE) ===
                
                // Current position = start of ELSE block (where JUMPZW should jump)
                loadCurrentPosition();
                
                // Pop JUMPZW operand position (it's the second item on stack)
                Stacks.PopTop();  // Pop JUMPW position (save it)
                Stacks.PopIDX();  // Pop JUMPZW position into ZP.IDX
                Stacks.PushTop(); // Push JUMPW position back
                
                // Calculate forward offset: current_position - jumpzw_operand_position
                calculateForwardOffset();
                
                // Adjust for PC being 3 bytes past JUMPZW when it executes
                LDA #3  // JUMPZW instruction size
                subtractInstructionSizeFromOffset();
                
                // Patch the JUMPZW operand
                LDA #1  // Skip opcode byte to get to operand
                patchWordOperand();
                
                // Skip ELSE token
                Tokenizer.NextToken();
                CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === COMPILE ELSE CLAUSE ===
                
                LDA Compiler.compilerIfClauses
                AND #0x01  // Test single-line mode
                if (NZ)    // Single-line mode (BIT0 = 1)?
                {
                    // Single-line: compile one statement
                    CompileStatement();
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                else
                {
                    // Multi-line: use statement block (handles ENDIF termination)
                    CompileStatementBlock();
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                
                // Validate we got ENDIF
                LDA ZP.CurrentToken
                CMP #Token.ENDIF
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected ENDIF
                    States.SetFailure();
                    break;
                }
                
                // Skip ENDIF token
                Tokenizer.NextToken();
                CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === PATCH JUMPW (end of THEN -> after ENDIF) ===
                
                // Current position = after ENDIF (where JUMPW should jump)
                loadCurrentPosition();
                
                // Pop JUMPW operand position
                Stacks.PopIDX();
                
                // Calculate forward offset: current_position - jumpw_operand_position
                calculateForwardOffset();
                
                // Adjust for PC being 3 bytes past JUMPW when it executes
                LDA #3  // JUMPW instruction size
                subtractInstructionSizeFromOffset();
                
                // Patch the JUMPW operand
                LDA #1  // Skip opcode byte to get to operand
                patchWordOperand();
            }
            else
            {
                // === NO ELSE CLAUSE ===
                
                // Validate we got ENDIF
                LDA ZP.CurrentToken
                CMP #Token.ENDIF
                if (NZ)
                {
                    Error.ExpectedEndif(); BIT ZP.EmulatorPCL  // Expected ENDIF
                    States.SetFailure();
                    break;
                }
                
                // Skip ENDIF token
                Tokenizer.NextToken();
                CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === PATCH JUMPZW (condition false -> after ENDIF) ===
                
                // Current position = after ENDIF (where JUMPZW should jump)
                loadCurrentPosition();
                
                // Pop JUMPZW operand position
                Stacks.PopIDX();
                
                // Calculate forward offset: current_position - jumpzw_operand_position
                calculateForwardOffset();
                
                // Adjust for PC being 3 bytes past JUMPZW when it executes
                LDA #3  // JUMPZW instruction size
                subtractInstructionSizeFromOffset();
                
                // Patch the JUMPZW operand
                LDA #1  // Skip opcode byte to get to operand
                patchWordOperand();
            }
            
            States.SetSuccess();
            break;
        } // Single exit block
        
        // Restore IF state for nesting
        PLA
        STA Compiler.compilerIfClauses
        
        // Restore stack for nesting (handles error cleanup automatically)
        PLA
        STA ZP.SP

    #ifdef TRACE
        LDA #(compileIfStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIfStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Compile WHILE...WEND statement
    // Input: ZP.CurrentToken = WHILE token
    // Output: WHILE loop compiled to opcodes with correct relative jumps
    const string compileWhileStatementTrace = "CompWhile // WHILE...WEND";
    CompileWhileStatement()
    {
    #ifdef TRACE
       LDA #(compileWhileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileWhileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
    
       LDA ZP.SP
       PHA

       loop // Single exit block
       {
           // Get and compile condition expression
           Tokenizer.NextToken();  // Skip WHILE token
           CheckError();
           if (NC) 
           {
               States.SetFailure(); 
               break; 
           }
           
           // Mark loop start position for backward jump (start of condition evaluation)
           saveCurrentPosition();
           
           // Compile condition expression (e.g., "I < 10")
           Compiler.CompileFoldedExpressionTree(); // WHILE <expression>
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           if (BBS0, ZP.CompilerFlags)
           {
               LDA # ZP.TOPT
               CMP # BASICType.BIT
               if (NZ)
               {
                   Error.TypeMismatch(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }
           
           // Save forward jump operand position for later patching
           // This is where JUMPZW operand will be stored (after the opcode byte)
           saveCurrentPosition();
           
           // Check for compilation errors after consuming all 4 stack slots
           CheckError();
           if (NC) 
           {
               States.SetFailure();
               break;
           }
           
           // Emit conditional exit jump (placeholder - will be patched after WEND)
           // JUMPZW: Jump if condition is zero/FALSE (exit loop when condition fails)
           LDA # OpCode.JUMPZW
           STA Compiler.compilerOpCode
           STZ Compiler.compilerOperand1  // Placeholder LSB (will be patched)
           STZ Compiler.compilerOperand2  // Placeholder MSB (will be patched)
           Emit.OpCodeWithWord();
           CheckError();
           if (NC)
           {
               States.SetFailure();
               break; 
           }
           
           CompileStatementBlock();
           CheckError();
           if (NC) { States.SetFailure(); break; }
            
           // Validate we got WEND
           LDA ZP.CurrentToken
           CMP #Token.WEND
           if (NZ) 
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break;
           }
           Tokenizer.NextToken();
           CheckError();
           if (NC)
           {
               States.SetFailure(); break; 
           }
           
           // === OFFSET CALCULATION PHASE ===
           // Pop saved positions from stack (in reverse order)
           
           // Pop forward jump operand position (where JUMPZW operand needs patching)
           Stacks.PopIDX();
           
           // Pop loop start position (target for backward jump)
           Stacks.PopTop();
           
           // Current position = end of loop body (where JUMPW will be emitted)
           loadCurrentPosition();
           
           // === FORWARD JUMP OFFSET CALCULATION ===
           // Calculate offset from JUMPZW operand position to loop exit
           // Offset = loop_exit - jumpzw_operand_position
           calculateForwardOffset();
           
           // === BACKWARD JUMP SETUP ===
           // Account for the JUMPW instruction we're about to emit (3 bytes: opcode + 2 operands)
           // This adjusts the current position to be after the JUMPW instruction
           LDA #3  // JUMPW instruction size
           addInstructionSizeToPosition();
           
           // === BACKWARD JUMP OFFSET CALCULATION ===
           // Calculate offset from after JUMPW to loop start (condition evaluation)
           // Offset = loop_start - position_after_jumpw
           calculateBackwardOffset();
           
           // === FORWARD JUMP PATCHING ===
           // Calculate absolute address in opcode buffer for patching
           LDA #1  // Skip opcode byte
           patchWordOperand();
                      
           // === BACKWARD JUMP EMISSION ===
           // Note: When JUMPW executes, PC has already advanced past 
           // the 3-byte instruction, so offset is from position after JUMPW
           //
           // Emit unconditional jump back to condition evaluation
           LDA #OpCode.JUMPW
           STA Compiler.compilerOpCode
           LDA ZP.TOPL    // Backward offset LSB
           STA Compiler.compilerOperand1
           LDA ZP.TOPH    // Backward offset MSB
           STA Compiler.compilerOperand2
           Emit.OpCodeWithWord();
           
           // Final error check
           CheckError();
           if (NC) 
           {
               States.SetFailure(); break;
           }
           
           States.SetSuccess();
           break;
       } // Single exit block

       // leave the Hopper VM stack the way we found it       
       PLA
       STA ZP.SP

    #ifdef TRACE
       LDA #(compileWhileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileWhileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Compile DO...UNTIL statement  
    // Input: ZP.CurrentToken = DO token
    // Output: DO...UNTIL loop compiled to opcodes with backward jump
    const string compileDoUntilStatementTrace = "CompDo // DO...UNTIL";
    CompileDoUntilStatement()
    {
    #ifdef TRACE
        LDA #(compileDoUntilStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileDoUntilStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif

        loop // Single exit block
        {
            // Skip DO token
            Tokenizer.NextToken();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Mark loop start position for backward jump
            saveCurrentPosition();
            
            CompileStatementBlock();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Validate we got WEND
            LDA ZP.CurrentToken
            CMP #Token.UNTIL
            if (NZ) 
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // We should be at UNTIL now
            LDA ZP.CurrentToken
            CMP #Token.UNTIL
            if (NZ)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected UNTIL
                States.SetFailure();
                break;
            }
            
            // Skip UNTIL token and compile condition expression
            Tokenizer.NextToken();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile condition expression (e.g., "I = 10")
            Compiler.CompileFoldedExpressionTree(); // UNTIL <expression>
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            if (BBS0, ZP.CompilerFlags)
            {
                LDA # ZP.TOPT
                CMP # BASICType.BIT
                if (NZ)
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
            }
            
            // === BACKWARD JUMP CALCULATION ===
            // Pop loop start position
            Stacks.PopTop();
            
            // Current position = where JUMPZW will be emitted
            loadCurrentPosition();
            
            // Account for the JUMPZW instruction we're about to emit (3 bytes)
            // PC will be at current_position + 3 after fetching JUMPZW
            LDA #3  // JUMPW instruction size
            addInstructionSizeToPosition();
            
            // Calculate backward offset: loop_start - position_after_jumpzw
            // This will be negative (jumping backward)
            calculateBackwardOffset();
                        
            // Emit JUMPZW with backward offset
            // Jump if condition is FALSE (i.e., UNTIL condition not met yet)
            LDA #OpCode.JUMPZW
            STA Compiler.compilerOpCode
            LDA ZP.TOPL  // Backward offset LSB
            STA Compiler.compilerOperand1
            LDA ZP.TOPH  // Backward offset MSB
            STA Compiler.compilerOperand2
            Emit.OpCodeWithWord();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            States.SetSuccess();
            break;
        } // Single exit block
        
        States.IsSuccess();
        if (NC)
        {
            // Clean up stack on error path (restore stack balance)
            Stacks.PopTop();
        }

    #ifdef TRACE
        LDA #(compileDoUntilStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileDoUntilStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }


    // Prerequisites:
    // 1. In Tokenizer.asm, ensure these tokens exist:
    //    - Token.FOR
    //    - Token.NEXT  
    //    - Token.TO
    //    - Token.STEP
    //
    // 2. In Compiler.asm workspace section, add:
    //    const uint compilerForIteratorOffset = Address.BasicCompilerWorkspace + 11; // 1 byte - signed BP offset of current FOR iterator
    //    const uint compilerOperand3         = Address.BasicCompilerWorkspace + 12; // 1 byte - third operand for 3-operand opcodes
    //
    // 3. In OpCodes.asm, add:
    //    FORCHK = 0xC0,  // FOR initial check [iterator_offset] [forward_offset_lsb] [forward_offset_msb]
    //    FORIT  = 0xC1,  // FOR iterate [iterator_offset] [backward_offset_lsb] [backward_offset_msb]
    //
    // 4. In CompileStatement() switch statement, add:
    //    case Token.FOR:
    //    {
    //        STZ compilerCanDeclareLocals // no more locals after this
    //        compileForStatement();
    //        CheckError();
    //        if (NC) { States.SetFailure(); break; }
    //    }
    //
    // 5. Assumes these methods exist in Locals unit:
    //    - Locals.Find()  // Input: ZP.IDX = function node, ZP.TOP = name pointer, Output: C set if found, A = BP offset
    //    - Locals.Add()   // Input: ZP.IDX = function node, ZP.TOP = name pointer, ZP.SymbolType = type, Output: A = BP offset
    //
    // 6. Requires implementation of executeFORCHK() and executeFORIT() in Executor.asm
    //    (See the design document for implementation details)
    //
    // 7. FOR/NEXT loops must be inside a function (FUNC or BEGIN) - they cannot be at global scope
    //    This is because the iterator becomes a local variable with a BP-relative offset
    //
    // 8. Assumes compilerSavedNodeAddrL/H contains the current function node address
    //    (Set by CompileFunction when entering function compilation)

    // Compile FOR...NEXT statement
    // Input: ZP.CurrentToken = FOR token
    // Output: FOR...NEXT loop compiled to opcodes with FORCHK and FORIT
    const string compileForStatementTrace = "CompFor // FOR...NEXT";
    CompileForStatement()
    {
    #ifdef TRACE
       LDA #(compileForStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileForStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
       
       loop // Single exit block
       {
           // Save parent FOR iterator offset (for nested loops)
           LDA Compiler.compilerForIteratorOffset
           PHA
           LDA Compiler.compilerForIteratorType
           PHA
           LDA Compiler.compilerGlobalIteratorSlot
           PHA
           LDA Compiler.compilerForIteratorBP
           PHA
           LDA ZP.SP // JIT was called from a running process - preserve the stack
           PHA
           LDA ZP.CompilerFlags
           PHA
           
           // Compilation state flags: 
           //     BIT 0 - the current expression being evaluated is numeric (INT|WORD|BYTE) and constant - used by compileExpressionTree()
           //     BIT 1 - we own the implicit variable - used by CompileForStatement
           //     BIT 2 - we used a global for our implicit variable - used by CompileForStatement
           //     BIT 3 - we're creating FORITF (rather than FORCHK & FORIT)
           //     BIT 4 - as "array assignment" flag
           //     BIT 5 - in CompileForStatement, we created an implicit local that needs to be removed at the end of the function
           //     BIT 6 - used in Tokenizer.TokenizeLineWithMode()
           
           STZ  ZP.CompilerFlags
           SMB3 ZP.CompilerFlags // assume we can optimize to use FORITF until proven otherwize
           
           // Skip FOR token
           Tokenizer.NextToken();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Expect iterator identifier
           LDA ZP.CurrentToken
           CMP #Token.IDENTIFIER
           if (NZ)
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected iterator variable
               States.SetFailure();
               break;
           }
           
           // Get iterator name
           Tokenizer.GetTokenString(); // Result in ZP.TOP (name pointer)
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Check if iterator is already a local
           LDA ZP.TOPL
           STA ZP.ACCL
           LDA ZP.TOPH
           STA ZP.ACCH
           
           // ZP.IDX should contain function node address
           LDA Compiler.compilerSavedNodeAddrL
           STA ZP.IDXL
           LDA Compiler.compilerSavedNodeAddrH
           STA ZP.IDXH
           
           // ZP.TOP already contains name pointer from GetTokenString
           Locals.Find(); // Input: ZP.IDX = function node, ZP.TOP = name, Output: C set if found, ZP.ACCL = BP offset
           if (NC)
           {
               // Not a local - check if it's a global
               STZ ZP.SymbolIteratorFilter  // Look for variables
               Variables.Find();  // Check global variables
               if (C)
               {
                    // Iterator exists as a global - create shadow local
                    SMB1 ZP.CompilerFlags  // Set BIT1: we own the shadow local
                    SMB2 ZP.CompilerFlags  // Set BIT2: we used a global
                    
                    // First push a default value to create the stack slot
                    Emit.PushEmptyVar();
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    // Variables.Find() already set ZP.IDY to the index (slot)
                    // Emit PUSHGLOBAL using that slot
                    LDA ZP.IDYL  // The global slot from Find()
                    STA Compiler.compilerGlobalIteratorSlot // SAVE for NEXT name verification!
                    STA Compiler.compilerOperand1
                    LDA #OpCode.PUSHGLOBAL
                    STA Compiler.compilerOpCode
                    Emit.OpCodeWithByte();
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    // Create a shadow local with same name
                    LDA ZP.TOPL
                    STA ZP.SymbolNameL
                    LDA ZP.TOPH
                    STA ZP.SymbolNameH
                    LDA #(SymbolType.LOCAL | BASICType.VAR)
                    STA ZP.SymbolType
                    STA Compiler.compilerForIteratorType
                    
                    // restore ZP.IDX after Variables.Find()
                    LDA Compiler.compilerSavedNodeAddrL
                    STA ZP.IDXL
                    LDA Compiler.compilerSavedNodeAddrH
                    STA ZP.IDXH

                    Locals.Add();          // Creates local
                    CheckError();
                    if (NC) { States.SetFailure(); break; }

                    // Now find it to get the BP offset
                    Locals.Find(); // Input: ZP.IDX = function node, ZP.TOP = name, Output: C set if found, ZP.ACCL = BP offset
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    LDA ZP.ACCL     // Get the offset from Find()
                    STA Compiler.compilerForIteratorBP // keep the actual offset for the NEXT check
                    
                    LDA Compiler.compilerFuncLocals
                    STA Compiler.compilerForIteratorOffset  // BP offset for our shadow puppet!
                    
                    // Emit POPLOCAL to initialize shadow from global
                    LDA Compiler.compilerForIteratorOffset
                    STA Compiler.compilerOperand1
                    LDA #OpCode.POPLOCAL
                    STA Compiler.compilerOpCode
                    Emit.OpCodeWithByte();
                    CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    // ZP.IDX already points to our new local from Locals.Add()
                    // compilerForIteratorOffset has the BP offset
                    // compilerForIteratorType has the type
                    // Ready for subsequent code that expects a local iterator
                    INC Compiler.compilerFuncLocals        // Track new local
                    LDA Compiler.compilerForIteratorOffset // Offset != BP offset for shadow local
               }
               else
               {
                   // restore ZP.IDX after Variables.Find()
                   LDA Compiler.compilerSavedNodeAddrL
                   STA ZP.IDXL
                   LDA Compiler.compilerSavedNodeAddrH
                   STA ZP.IDXH
                    
                   // Iterator not found - check if we can create implicit local
                   /* IMPLICIT SCOPE
                   LDA Compiler.compilerCanDeclareLocals
                   if (Z)  // Can't create new locals
                   {
                       Error.LateDeclaration(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                       break;
                   }
                   */
                    
                   // Iterator not found - create implicit local
                   // First push a default value to create the stack slot
                   Emit.PushEmptyVar();
                   CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
                   // Now create the local node
                   // ZP.TOP still contains name pointer
                   // ZP.IDX still contains function node
                   LDA #(SymbolType.LOCAL|BASICType.VAR)  // LOCAL type (will be updated by FROM expression type)
                   STA ZP.SymbolType      // argument for Locals.Add()
                   SMB1 ZP.CompilerFlags  // we own the iterator
    
                   Locals.Add();
                   CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
                   SMB5 ZP.CompilerFlags // we created an implicit local that needs to be removed at the end of the function
                   
                   // get BP offset
                   Locals.Find(); // Input: ZP.IDX = function node, ZP.TOP = name, Output: C set if found, ZP.ACCL = BP offset
                   CheckError();
                   if (NC) { States.SetFailure(); break; }
                   INC Compiler.compilerFuncLocals  // Track new local
                   LDA ZP.ACCL     // Get the offset from Find()
                   STA Compiler.compilerForIteratorBP      // keep the actual offset for the NEXT check
                   STA Compiler.compilerForIteratorOffset  // Offset = BP offset for implicit local
               }
           }
           else
           {
               // found an exiting local
               LDA ZP.ACCL                            // Get the offset from Find()
               STA Compiler.compilerForIteratorBP     // keep the actual offset for the NEXT check
               STA Compiler.compilerForIteratorOffset // Offset = BP offset for real local
           }
           
           Locals.GetType();
           LDA ZP.ACCT
           STA Compiler.compilerForIteratorType
           
           STZ Compiler.compilerCanDeclareLocals // no more locals after this
           
           // Skip iterator name
           Tokenizer.NextToken();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Expect '=' token
           LDA ZP.CurrentToken
           CMP # Token.EQUALS
           if (NZ)
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected '='
               States.SetFailure();
               break;
           }
           
           // Skip '=' and compile FROM expression
           Tokenizer.NextToken();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           Compiler.CompileFoldedExpressionTree();  // Compile FROM expression
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           if (BBS0, ZP.CompilerFlags)
           {
               if (BBR3, ZP.TOPT) // Bit 3 - LONG
               {
                   Error.TypeMismatch(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }

           // push 2 copies
           Long.PushTopStrict(); // FROM integral value
           Long.PushTopStrict();
           if (BBR0, ZP.CompilerFlags)
           {
               RMB3 ZP.CompilerFlags // optimization to FORITF disqualified
           }
           
           // Store FROM value in iterator (POPLOCAL)
           LDA Compiler.compilerForIteratorOffset
           STA Compiler.compilerOperand1
           LDA #OpCode.POPLOCAL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithByte();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Expect TO token
           LDA ZP.CurrentToken
           CMP #Token.TO
           if (NZ)
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected TO
               States.SetFailure();
               break;
           }
           
           // Skip TO and compile TO expression
           Tokenizer.NextToken();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           Compiler.CompileFoldedExpressionTree();  // Compile TO expression (leaves on stack)
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           if (BBS0, ZP.CompilerFlags)
           {
               if (BBR3, ZP.TOPT) // Bit 3 - LONG
               {
                   Error.TypeMismatch(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }

           LDA ZP.TOPT
           PHA
           LDA ZP.TOP0
           PHA
           LDA ZP.TOP1
           PHA
           LDA ZP.TOP2
           PHA
           LDA ZP.TOP3
           PHA
           
           // placeholder slot
           LDA Compiler.compilerSavedNodeAddrL
           STA ZP.IDXL
           LDA Compiler.compilerSavedNodeAddrH
           STA ZP.IDXH
           LDA #(ForVarName % 256)
           STA ZP.TOPL
           LDA #(ForVarName / 256)
           STA ZP.TOPH
           Locals.Add();
           
           PLA
           STA ZP.TOP3
           PLA
           STA ZP.TOP2
           PLA
           STA ZP.TOP1
           PLA
           STA ZP.TOP0
           PLA
           STA ZP.TOPT
           
           INC Compiler.compilerFuncLocals   // consider a RETURN from within the loop needing to clean the stack
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           LDA ZP.TOPT
           Long.PushTopStrict(); // TO integeral value
           

           if (BBR0, ZP.CompilerFlags) // TO is not constant expression
           {
               RMB3 ZP.CompilerFlags // optimization to FORITF disqualified
           }
           if (BBS3, ZP.TOPT) // LONG not ok
           {
               LDA ZP.TOP2
               ORA ZP.TOP3
               if (NZ)
               {
                   RMB3 ZP.CompilerFlags // optimization to FORITF disqualified
               }
           }
           
           // Check for optional STEP
           LDA ZP.CurrentToken
           CMP #Token.STEP
           if (Z)
           {
               // Skip STEP and compile STEP expression
               Tokenizer.NextToken();
               CheckError();
               if (NC) { States.SetFailure(); break; }
               
               Compiler.CompileFoldedExpressionTree();  // Compile STEP expression (leaves on stack)
               CheckError();
               if (NC) { States.SetFailure(); break; }
               
               if (BBS0, ZP.CompilerFlags)
               {
                   if (BBR3, ZP.TOPT) // Bit 3 - LONG
                   {
                       Error.TypeMismatch(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                       break;
                   }
               }
               
               // placeholder slot
               LDA Compiler.compilerSavedNodeAddrL
               STA ZP.IDXL
               LDA Compiler.compilerSavedNodeAddrH
               STA ZP.IDXH
               LDA #(ForVarName % 256)
               STA ZP.TOP0
               LDA #(ForVarName / 256)
               STA ZP.TOP1
               Locals.Add(); // HERE
               INC Compiler.compilerFuncLocals   // consider a RETURN from within the loop needing to clean the stack
               CheckError();
               if (NC) { States.SetFailure(); break; }

               if (BBR0, ZP.CompilerFlags) // STEP is NOT constant expression
               {
                   RMB3 ZP.CompilerFlags // optimization to FORITF disqualified
               }
               else
               {
                   // STEP == 1?
                   LDA #1
                   CMP ZP.TOP0
                   if (NZ)
                   {
                       RMB3 ZP.CompilerFlags // STEP != 1, optimization to FORITF disqualified
                   }
                   else
                   {
                       LDA ZP.TOP1
                       ORA ZP.TOP2
                       ORA ZP.TOP3
                       if (NZ)
                       {
                           RMB3 ZP.CompilerFlags // STEP != 1, optimization to FORITF disqualified
                       }
                   }
               }
           }
           else
           {
               // No STEP specified - default to 1
               LDA #OpCode.PUSH1
               STA Compiler.compilerOpCode
               Emit.OpCode();
               CheckError();
               if (NC) { States.SetFailure(); break; }
               
               // placeholder slot
               LDA Compiler.compilerSavedNodeAddrL
               STA ZP.IDXL
               LDA Compiler.compilerSavedNodeAddrH
               STA ZP.IDXH
               LDA #(ForVarName % 256)
               STA ZP.TOPL
               LDA #(ForVarName / 256)
               STA ZP.TOPH
               Locals.Add(); // HERE
               INC Compiler.compilerFuncLocals   // consider a RETURN from within the loop needing to clean the stack
           }
           if (BBS3, ZP.CompilerFlags) 
           {
               // still on track to optimize to FORITF ..
               
               // If we created it, then it is (BASICType.VAR|BASICType.INT)
               // At runtime, we'll just switch any VAR to VAR|WORD once.
               LDA Compiler.compilerForIteratorType
               AND # BASICType.VAR
               if (Z)
               {
                   // check user variable types that are not VAR for compile time failure
                   // stack TOP  = TO
                   // stack NEXT = FROM
                   LDA Compiler.compilerForIteratorType
                   AND # BASICType.TYPEMASK 
                   switch (A)
                   {
                       case BASICType.BYTE:
                       {
                           Error.InternalError(); BIT ZP.EmulatorPCL
                           /*
                           // User declared BYTE - can hold 0-255 
                           LDA #0xFF
                           Stacks.GetStackTopSP(); // TO: [SP-1] -> TOP
                           LDA ZP.TOP1
                           if (NZ)
                           {
                               RMB3 ZP.CompilerFlags // TO > 255 (iterator is BYTE), optimization to FORITF disqualified
                               Error.NumericOverflow(); BIT ZP.EmulatorPCL // let the next CheckError() catch this
                           }
                           else
                           {
                               LDA #0xFE
                               Stacks.GetStackTopSP(); // FROM: [SP-2] -> TOP
                               LDA ZP.TOPH
                               if (NZ)
                               {
                                   // FROM > 255 (iterator is BYTE), optimization to FORITF disqualified
                                   Error.NumericOverflow(); BIT ZP.EmulatorPCL // let the next CheckError() catch this
                               }
                           }
                           */
                       }
                       case BASICType.INT:
                       {
                           Error.InternalError(); BIT ZP.EmulatorPCL
                           /*
                           // User declared INT  - can hold 0-32767
                           LDA #0xFF
                           Stacks.GetStackTopSP(); // TO: [SP-1] -> TOP
                           LDA ZP.TOPH
                           if (MI)
                           {
                               RMB3 ZP.CompilerFlags  // TO > 32767 (iterator is INT), optimization to FORITF disqualified
                           }
                           else
                           {
                               LDA #0xFE
                               Stacks.GetStackTopSP(); // FROM: [SP-2] -> TOP
                               LDA ZP.TOPH
                               if (MI)
                               {
                                   RMB3 ZP.CompilerFlags  // FROM > 32767 (iterator is INT), optimization to FORITF disqualified
                               }
                           }
                           */
                       }
                       case BASICType.LONG:
                       {
                           // TODO TYPE DEMOTION
                           LDA #0xFF
                           Long.GetStackTopSP(); // TO: [SP-1] -> TOP
                           LDA ZP.TOP3
                           if (MI)
                           {
                               RMB3 ZP.CompilerFlags  // -ve LONG, optimization to FORITF disqualified
                           }
                           else
                           {
                               ORA ZP.TOP2
                               if (NZ)
                               {
                                   RMB3 ZP.CompilerFlags // > 65535, optimization to FORITF disqualified
                               }
                               else
                               {
                                   LDA #0xFE
                                   Long.GetStackTopSP(); // FROM: [SP-2] -> TOP
                                   LDA ZP.TOP3
                                   if (MI)
                                   {
                                       RMB3 ZP.CompilerFlags  // -ve LONG, , optimization to FORITF disqualified
                                   }
                                   else
                                   {
                                       ORA ZP.TOP2
                                       if (NZ)
                                       {
                                           RMB3 ZP.CompilerFlags // > 65535, optimization to FORITF disqualified
                                       }
                                   }
                               }
                           }
                       }
                       case BASICType.WORD:
                       {
                           Error.InternalError(); BIT ZP.EmulatorPCL
                           // User declared WORD - can hold 0-65535
                           // comparisons below will range check properly (using types)
                       }
                       case BASICType.VOID:
                       {
                           if (BBS1, ZP.CompilerFlags)
                           {
                               // we created it: anything in our +ve range will work - we'll switch to VAR|WORD immediately in FORITF
                           }
                           else
                           {
                               // user VOID VAR ?!
                               RMB3 ZP.CompilerFlags  // Disqualify FORITF
                           }
                       }
                       default:
                       {
                           
                           // Unexpected type for iterator
                           RMB3 ZP.CompilerFlags  // Disqualify FORITF
                       }
                   } // switch
               }

               // FROM < TO?
               ComparisonInstructions.LessThan(); // FROM <= TO   3 - pops 2, + pushes 1 = 2
               Stacks.PopA();                     //              2 - pop 1 = 1
               if (Z)
               {
                   Stacks.PopTop(); // 1 - pop 1 = 0
                   RMB3 ZP.CompilerFlags // FROM == TO, requires FORCHK, optimization to FORITF disqualified
               }
               else
               {
                   STZ ZP.TOP0
                   STZ ZP.TOP1
                   STZ ZP.TOP2
                   STZ ZP.TOP3
                   LDA # BASICType.LONG
                   STA ZP.TOPT
                   Long.PushTopStrict(); // 1 + push 1 = 2
                   
                   // FROM >= 0
                   ComparisonInstructions.GreaterEqual();  // FROM >= 0   2 - pops 2, + pushes 1 = 1
                   Stacks.PopA();                          //             1 - pop 1 = 0
                   if (Z)
                   {
                       RMB3 ZP.CompilerFlags // TO >= FROM, optimization to FORITF disqualified
                   }
               }
           }
           else
           {
               Stacks.PopTop(); // 3 - pops 3 = 0
               Stacks.PopTop();
               Stacks.PopTop();
           }

           if (BBR3, ZP.CompilerFlags)
           {
               // NOT optimizing for FORITF
               // Save FORCHK operand position for later patching
               LDA ZP.OpCodeBufferContentLengthL
               CLC
               ADC #2  // Skip opcode and iterator offset to get operand position
               STA ZP.TOPL
               LDA ZP.OpCodeBufferContentLengthH
               ADC #0
               STA ZP.TOPH
               Stacks.PushTop();  // Push FORCHK operand position
           
           
               // Emit FORCHK with placeholder forward offset
               LDA Compiler.compilerForIteratorOffset
               LDX #0x00  // Placeholder forward offset LSB
               LDY #0x00  // Placeholder forward offset MSB
               Emit.ForCheck();
               CheckError();
               if (NC) 
               { 
                   States.SetFailure(); 
                   break; 
               }
           }
           
           // Save loop body start position for FORIT's backward jump
           saveCurrentPosition();  // Push loop body start position
           
           // Skip any EOL tokens before loop body
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.EOL
               if (NZ) { break; }
               
               Tokenizer.NextToken();
               CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           
           // Compile loop body statements until NEXT
           CompileStatementBlock();
           CheckError();
           if (NC) { break; }
            
           // Validate we got NEXT
           LDA ZP.CurrentToken
           CMP #Token.NEXT
           if (NZ) 
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break;
           }
           
           // Check if we exited due to error
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // We should be at NEXT now
           LDA ZP.CurrentToken
           CMP #Token.NEXT
           if (NZ)
           {
               Error.MissingNext(); BIT ZP.EmulatorPCL  // Expected NEXT
               States.SetFailure();
               break;
           }
           
           // Skip NEXT token
           Tokenizer.NextToken();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Expect iterator identifier
           LDA ZP.CurrentToken
           CMP #Token.IDENTIFIER
           if (NZ)
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected iterator after NEXT
               States.SetFailure();
               break;
           }
           
           // Get iterator name and verify it matches
           Tokenizer.GetTokenString(); // Result in ZP.TOP (name pointer)
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Find the iterator to get its BP offset
           LDA Compiler.compilerSavedNodeAddrL
           STA ZP.IDXL
           LDA Compiler.compilerSavedNodeAddrH
           STA ZP.IDXH
           
           // ZP.TOP already contains name pointer from GetTokenString
           Locals.Find(); // Input: ZP.IDX = function node, ZP.TOP = name, Output: C set if found, ZP.ACCL = BP offset
           if (NC)
           {
               Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL  // Iterator not found
               States.SetFailure();
               break;
           }
         
           // ZP.ACCL = BP offset
           // Verify it matches the current FOR iterator
           LDA ZP.ACCL
           CMP Compiler.compilerForIteratorBP
           if (NZ)
           {
               Error.NextMismatch(); BIT ZP.EmulatorPCL  // NEXT variable doesn't match FOR
               States.SetFailure();
               break;
           }
           
           
           
           // Skip iterator name
           Tokenizer.NextToken();
           CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // === EMIT FORIT WITH BACKWARD JUMP ===
           
           // Pop loop body start position
           Stacks.PopTop();  // Get loop body start in ZP.TOP
           
           // Calculate backward jump offset for FORIT
           loadCurrentPosition();
           LDA #4  // FORIT instruction size
           addInstructionSizeToPosition();
           
           // Backward offset = loop_start - position_after_FORIT
           calculateBackwardOffset();
           LDX ZP.TOPL  // Backward offset LSB
           LDY ZP.TOPH  // Backward offset MSB
                     
           // Emit appropriate iterator based on optimization constraints
           if (BBS3, ZP.CompilerFlags) // Optimized for FORITF
           {
               LDA Compiler.compilerForIteratorOffset
               Emit.ForIterateFast();  // Use FORITF
           }
           else
           {
               // Emit FORIT
               LDA Compiler.compilerForIteratorOffset
               Emit.ForIterate();
               CheckError();
               if (NC) { States.SetFailure(); break; }
               
               // === PATCH FORCHK WITH FORWARD JUMP ===
               
               // Pop FORCHK operand position
               Stacks.PopIDX();  // Get FORCHK operand position in ZP.IDX
           
           
               // Current position = after FORIT (exit point)
               loadCurrentPosition();
               
               // Calculate forward offset: exit_point - FORCHK_operand_position
               calculateForwardOffset();
               
               // Adjust for PC being 4 bytes past FORCHK when it executes
               LDA #2  // JUMPZW instruction size
               subtractInstructionSizeFromOffset();
               
               // Calculate absolute address for patching
               LDA #0  // ZP.IDX already points to operand position
               patchWordOperand();
           }
           
           
           // Emit stack cleanup (remove TO and STEP values)
           LDA #0x02  // Decrement by 2 positions
           Emit.DecSp();
           CheckError();
           if (NC) { States.SetFailure(); break; }
            
           DEC Compiler.compilerFuncLocals
           DEC Compiler.compilerFuncLocals  // Decrement twice since we're removing 2 locals
           
           LDA Compiler.compilerSavedNodeAddrL
           STA ZP.IDXL
           LDA Compiler.compilerSavedNodeAddrH
           STA ZP.IDXH
           Locals.RemoveLast(); 
           Locals.RemoveLast();
           
           
           // BIT2 set : If we used a global and made our own "shadow local", update global
           if (BBS2, ZP.CompilerFlags)
           {
               // Shadow local is now at top of stack
               // Find the global to get its slot
               Tokenizer.GetTokenString(); // Get iterator name in ZP.TOP
               
               STZ ZP.SymbolIteratorFilter  // Accept both variables and constants
               Variables.Find();  // Returns index in ZP.IDY
               
               // POPGLOBAL will both update the global AND remove the shadow!
               LDA ZP.IDYL  // The global slot from Find()
               STA Compiler.compilerOperand1
               LDA #OpCode.POPGLOBAL
               STA Compiler.compilerOpCode
               Emit.OpCodeWithByte();
               CheckError();
               if (NC) { States.SetFailure(); break; }
               
               LDA Compiler.compilerSavedNodeAddrL
               STA ZP.IDXL
               LDA Compiler.compilerSavedNodeAddrH
               STA ZP.IDXH
               Locals.RemoveLast();
               DEC Compiler.compilerFuncLocals
          }
          
          // BIT5 set : We created an implicit local that needs to be removed at the end of the function
          if (BBS5, ZP.CompilerFlags) // IMPLICIT SCOPE : we own it
          {
               LDA #0x01  // Decrement by 1 position
               Emit.DecSp();
               CheckError();
               if (NC) { States.SetFailure(); break; }
               
               LDA Compiler.compilerSavedNodeAddrL
               STA ZP.IDXL
               LDA Compiler.compilerSavedNodeAddrH
               STA ZP.IDXH
               Locals.RemoveLast();
               DEC Compiler.compilerFuncLocals
           }
           States.SetSuccess();
           break;
       } // Single exit block
       
     
       
       // Restore parent FOR iterator offset
       PLA
       if (BBS5, ZP.CompilerFlags)
       {
           STA ZP.CompilerFlags
           SMB5 ZP.CompilerFlags // persist the reminder to function to remove implicit local at the end
       }
       else
       {
           STA ZP.CompilerFlags
       }
       PLA
       STA ZP.SP
       PLA
       STA Compiler.compilerForIteratorBP
       PLA
       STA Compiler.compilerGlobalIteratorSlot
       PLA
       STA Compiler.compilerForIteratorType
       PLA
       STA Compiler.compilerForIteratorOffset
           
       
    #ifdef TRACE
       LDA #(compileForStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileForStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
}
