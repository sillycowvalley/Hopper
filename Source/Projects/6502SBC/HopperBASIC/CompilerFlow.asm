unit CompilerFlow
{

    // Compile WHILE...WEND statement
    // Input: ZP.CurrentToken = WHILE token
    // Output: WHILE loop compiled to opcodes with correct relative jumps
    const string compileWhileStatementTrace = "CompWhile // WHILE...WEND";
    CompileWhileStatement()
    {
    #ifdef TRACE
       LDA #(compileWhileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileWhileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif

       loop // Single exit block
       {
           // Get and compile condition expression
           Tokenizer.NextToken();  // Skip WHILE token
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Mark loop start position for backward jump (start of condition evaluation)
           LDA ZP.OpCodeBufferContentSizeL
           STA ZP.TOPL
           LDA ZP.OpCodeBufferContentSizeH
           STA ZP.TOPH
           Stacks.PushTop();
           
           // Compile condition expression (e.g., "I < 10")
           Compiler.compileExpressionTree();       
           
           // Save forward jump operand position for later patching
           // This is where JUMPZW operand will be stored (after the opcode byte)
           LDA ZP.OpCodeBufferContentSizeL
           STA ZP.TOPL
           LDA ZP.OpCodeBufferContentSizeH
           STA ZP.TOPH
           Stacks.PushTop();
           
           // Check for compilation errors after consuming all 4 stack slots
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Emit conditional exit jump (placeholder - will be patched after WEND)
           // JUMPZW: Jump if condition is zero/FALSE (exit loop when condition fails)
           LDA #OpCode.JUMPZW
           STA Compiler.compilerOpCode
           STZ Compiler.compilerOperand1  // Placeholder LSB (will be patched)
           STZ Compiler.compilerOperand2  // Placeholder MSB (will be patched)
           Emit.OpCodeWithWord();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Compile loop body statements until WEND
           loop // Statement compilation loop
           {
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
               
               LDA ZP.CurrentToken
               
               CMP #Token.WEND
               if (Z) 
               { 
                   // Found WEND - consume it and exit loop
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   break;  // Exit statement compilation loop
               }
               
               CMP #Token.EOF
               if (Z) 
               { 
                   Error.SyntaxError(); BIT ZP.EmulatorPCL  // Missing WEND
                   States.SetFailure();
                   break; 
               }
               
               CMP #Token.EOL
               if (Z)
               {
                   // Skip empty lines in loop body
                   continue;
               }
               
               // Compile statement in loop body (PRINT, assignments, nested loops, etc.)
               CompileStatement();  // RECURSIVE CALL - handles nested constructs
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           
           // Check if we exited due to error
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // === OFFSET CALCULATION PHASE ===
           // Pop saved positions from stack (in reverse order)
           
           // Pop forward jump operand position (where JUMPZW operand needs patching)
           Stacks.PopIDX();
           
           // Pop loop start position (target for backward jump)
           Stacks.PopTop();
           
           // Current position = end of loop body (where JUMPW will be emitted)
           LDA ZP.OpCodeBufferContentSizeH
           STA ZP.IDYH  // Current position MSB
           LDA ZP.OpCodeBufferContentSizeL
           STA ZP.IDYL  // Current position LSB
           
           // === FORWARD JUMP OFFSET CALCULATION ===
           // Calculate offset from JUMPZW operand position to loop exit
           // Offset = loop_exit - jumpzw_operand_position
           SEC
           LDA ZP.IDYL    // Current position (loop exit) LSB
           SBC ZP.IDXL    // Subtract JUMPZW operand position LSB
           STA ZP.NEXTL   // Store forward offset LSB
           LDA ZP.IDYH    // Current position (loop exit) MSB
           SBC ZP.IDXH    // Subtract JUMPZW operand position MSB
           STA ZP.NEXTH   // Store forward offset MSB
           
           // === BACKWARD JUMP SETUP ===
           // Account for the JUMPW instruction we're about to emit (3 bytes: opcode + 2 operands)
           // This adjusts the current position to be after the JUMPW instruction
           CLC
           LDA ZP.IDYL
           ADC #3         // Add 3 bytes for JUMPW instruction
           STA ZP.IDYL    // Updated current position LSB
           LDA ZP.IDYH
           ADC #0
           STA ZP.IDYH    // Updated current position MSB
           
           // === BACKWARD JUMP OFFSET CALCULATION ===
           // Calculate offset from after JUMPW to loop start (condition evaluation)
           // Offset = loop_start - position_after_jumpw
           SEC
           LDA ZP.TOPL    // Loop start position LSB
           SBC ZP.IDYL    // Subtract position after JUMPW LSB
           STA ZP.TOPL    // Store backward offset LSB
           LDA ZP.TOPH    // Loop start position MSB
           SBC ZP.IDYH    // Subtract position after JUMPW MSB
           STA ZP.TOPH    // Store backward offset MSB
           
           // === FORWARD JUMP PATCHING ===
           // Calculate absolute address in opcode buffer for patching
           CLC
           LDA ZP.OpCodeBufferL
           ADC ZP.IDXL    // Add JUMPZW operand position
           STA ZP.IDXL    // Absolute patch address LSB
           LDA ZP.OpCodeBufferH
           ADC ZP.IDXH    // Add JUMPZW operand position
           STA ZP.IDXH    // Absolute patch address MSB

           // Patch the JUMPZW operand bytes with calculated forward offset
           LDY #1         // Skip opcode byte, point to first operand byte
           LDA ZP.NEXTL   // Forward offset LSB
           STA [ZP.IDX], Y   // Patch LSB
           INY
           LDA ZP.NEXTH   // Forward offset MSB
           STA [ZP.IDX], Y   // Patch MSB
           
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
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Final error check
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           States.SetSuccess();
           break;
       } // Single exit block
       
       States.IsSuccess();
       if (NC)
       {
           // Clean up stack on error path (restore stack balance)
           PLA  // Discard forward jump operand position MSB
           PLA  // Discard forward jump operand position LSB  
           PLA  // Discard loop start position MSB
           PLA  // Discard loop start position LSB
       }

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
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Mark loop start position for backward jump
            LDA ZP.OpCodeBufferContentSizeL
            STA ZP.TOPL
            LDA ZP.OpCodeBufferContentSizeH
            STA ZP.TOPH
            Stacks.PushTop();
            
            
            // Compile loop body statements until UNTIL
            loop // Statement compilation loop
            {
                LDA ZP.CurrentToken
                
                CMP #Token.UNTIL
                if (Z) 
                { 
                    // Found UNTIL - exit body compilation (don't consume yet)
                    break;
                }
                
                CMP #Token.EOF
                if (Z) 
                { 
                    Error.SyntaxError(); BIT ZP.EmulatorPCL  // Missing UNTIL
                    States.SetFailure();
                    break; 
                }
                
                CMP #Token.EOL
                if (Z)
                {
                    // Skip empty lines in loop body
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    continue;
                }
                
                // Compile statement in loop body
                CompileStatement();  // RECURSIVE CALL - handles nested constructs
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
            }
            
            // Check if we exited due to error
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
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
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile condition expression (e.g., "I = 10")
            Compiler.compileExpressionTree();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // === BACKWARD JUMP CALCULATION ===
            // Pop loop start position
            Stacks.PopTop();
            
            // Current position = where JUMPZW will be emitted
            LDA ZP.OpCodeBufferContentSizeL
            STA ZP.IDYL  // Current position LSB
            LDA ZP.OpCodeBufferContentSizeH
            STA ZP.IDYH  // Current position MSB
            
            // Account for the JUMPZW instruction we're about to emit (3 bytes)
            // PC will be at current_position + 3 after fetching JUMPZW
            CLC
            LDA ZP.IDYL
            ADC #3
            STA ZP.IDYL  // Position after JUMPZW
            LDA ZP.IDYH
            ADC #0
            STA ZP.IDYH
            
            // Calculate backward offset: loop_start - position_after_jumpzw
            // This will be negative (jumping backward)
            SEC
            LDA ZP.TOPL  // Loop start LSB
            SBC ZP.IDYL  // Subtract position after JUMPZW
            STA ZP.TOPL  // Backward offset LSB
            LDA ZP.TOPH  // Loop start MSB
            SBC ZP.IDYH  // Subtract position after JUMPZW
            STA ZP.TOPH  // Backward offset MSB
            
            // Emit JUMPZW with backward offset
            // Jump if condition is FALSE (i.e., UNTIL condition not met yet)
            LDA #OpCode.JUMPZW
            STA Compiler.compilerOpCode
            LDA ZP.TOPL  // Backward offset LSB
            STA Compiler.compilerOperand1
            LDA ZP.TOPH  // Backward offset MSB
            STA Compiler.compilerOperand2
            Emit.OpCodeWithWord();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            States.SetSuccess();
            break;
        } // Single exit block
        
        States.IsSuccess();
        if (NC)
        {
            // Clean up stack on error path (restore stack balance)
            PLA  // Discard loop start position MSB
            PLA  // Discard loop start position LSB
        }

    #ifdef TRACE
        LDA #(compileDoUntilStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileDoUntilStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }


    // Compile IF...THEN...ELSE...ENDIF statement
    // Input: ZP.CurrentToken = IF token
    // Output: IF statement compiled to opcodes with correct forward jumps
    const string compileIfStatementTrace = "CompIf // IF...THEN...ELSE...ENDIF";
    CompileIfStatement()
    {
    #ifdef TRACE
        LDA #(compileIfStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIfStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif

        // Save CompilerTemp for nesting
        LDA ZP.CompilerTemp
        PHA  // Preserve parent's counter
        
        // Initialize counter for this IF
        STZ ZP.CompilerTemp  // Track how many patch positions we push

        loop // Single exit block
        {
            // Get and compile condition expression
            Tokenizer.NextToken();  // Skip IF token
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile condition expression (e.g., "X > 10")
            Compiler.compileExpressionTree();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Expect THEN token
            LDA ZP.CurrentToken
            CMP #Token.THEN
            if (NZ)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL  // Missing THEN
                States.SetFailure();
                break;
            }
            
            // Save position where JUMPZW operand will be (for patching)
            LDA ZP.OpCodeBufferContentSizeL
            STA ZP.TOPL
            LDA ZP.OpCodeBufferContentSizeH
            STA ZP.TOPH
            Stacks.PushTop();
            
            
            INC ZP.CompilerTemp  // Track that we pushed a patch position 
                        
            // Emit conditional jump to ELSE/ENDIF (placeholder - will be patched)
            // JUMPZW: Jump if condition is zero/FALSE (skip THEN block)
            LDA #OpCode.JUMPZW
            STA Compiler.compilerOpCode
            STZ Compiler.compilerOperand1  // Placeholder LSB (will be patched)
            STZ Compiler.compilerOperand2  // Placeholder MSB (will be patched)
            Emit.OpCodeWithWord();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile THEN block statements
            loop // THEN statement compilation loop
            {
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                LDA ZP.CurrentToken
                
                CMP #Token.ELSE
                if (Z) { break; }  // Found ELSE - exit THEN compilation
                
                CMP #Token.ENDIF
                if (Z) { break; }  // Found ENDIF - exit THEN compilation
                
                CMP #Token.EOF
                if (Z)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL  // Missing ENDIF
                    States.SetFailure();
                    break;
                }
                
                CMP #Token.EOL
                if (Z)
                {
                    // Skip empty lines in THEN block
                    continue;
                }
                
                // Compile statement in THEN block
                CompileStatement();  // RECURSIVE CALL - handles nested constructs
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
            }
            
            // Check if we exited due to error
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Check if we have ELSE clause
            LDA ZP.CurrentToken
            CMP #Token.ELSE
            if (Z)
            {
                // === ELSE CLAUSE PRESENT ===
                
                // Save position where JUMPW operand will be (for patching)
                // This jump skips the ELSE block after THEN executes
                LDA ZP.OpCodeBufferContentSizeL
                STA ZP.TOPL
                LDA ZP.OpCodeBufferContentSizeH
                STA ZP.TOPH
                Stacks.PushTop();
                
                INC ZP.CompilerTemp  // Track that we pushed another patch position (now 2 positions total)
                
                // Emit unconditional jump to ENDIF (placeholder - will be patched)
                LDA #OpCode.JUMPW
                STA Compiler.compilerOpCode
                STZ Compiler.compilerOperand1  // Placeholder LSB (will be patched)
                STZ Compiler.compilerOperand2  // Placeholder MSB (will be patched)
                Emit.OpCodeWithWord();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === PATCH FIRST JUMP (JUMPZW) ===
                // It should jump here (start of ELSE block)
                
                // Current position = start of ELSE block
                LDA ZP.OpCodeBufferContentSizeL
                STA ZP.IDYL  // Current position LSB
                LDA ZP.OpCodeBufferContentSizeH
                STA ZP.IDYH  // Current position MSB
                
                // Get saved JUMPZW operand position (but don't pop yet - we have JUMPW position on top)
                Stacks.PopIDX();
                Stacks.PopIDX();
                INC ZP.SP // but don't pop yet
                INC ZP.SP
                
                // Calculate forward offset: current_position - jumpzw_operand_position
                SEC
                LDA ZP.IDYL    // Current position LSB
                SBC ZP.IDXL    // JUMPZW operand position LSB
                STA ZP.NEXTL   // Forward offset LSB
                LDA ZP.IDYH    // Current position MSB
                SBC ZP.IDXH    // JUMPZW operand position MSB
                STA ZP.NEXTH   // Forward offset MSB
                
                // Adjust for PC being 3 bytes past the JUMPZW instruction start
                SEC
                LDA ZP.NEXTL
                SBC #3
                STA ZP.NEXTL
                LDA ZP.NEXTH
                SBC #0
                STA ZP.NEXTH
                
                // Patch the JUMPZW operand
                CLC
                LDA ZP.OpCodeBufferL
                ADC ZP.IDXL    // Add JUMPZW operand position
                STA ZP.IDXL    // Absolute patch address LSB
                LDA ZP.OpCodeBufferH
                ADC ZP.IDXH
                STA ZP.IDXH    // Absolute patch address MSB
                
                LDY #1         // Skip opcode byte, point to first operand byte
                LDA ZP.NEXTL   // Forward offset LSB
                STA [ZP.IDX], Y   // Patch LSB
                INY
                LDA ZP.NEXTH   // Forward offset MSB
                STA [ZP.IDX], Y   // Patch MSB
                
                // Compile ELSE block statements
                loop // ELSE statement compilation loop
                {
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    LDA ZP.CurrentToken
                    
                    CMP #Token.ENDIF
                    if (Z)
                    {
                        // Found ENDIF - consume it and exit
                        Tokenizer.NextToken();
                        Error.CheckError();
                        if (NC) { States.SetFailure(); break; }
                        break;  // Exit ELSE compilation
                    }
                    
                    CMP #Token.EOF
                    if (Z)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL  // Missing ENDIF
                        States.SetFailure();
                        break;
                    }
                    
                    CMP #Token.EOL
                    if (Z)
                    {
                        // Skip empty lines in ELSE block
                        continue;
                    }
                    
                    // Compile statement in ELSE block
                    CompileStatement();  // RECURSIVE CALL
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                
                // Check if we exited due to error
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === PATCH SECOND JUMP (JUMPW) ===
                // It should jump here (after ENDIF)
                
                // Pop JUMPW operand position
                Stacks.PopIDX();
                DEC ZP.CompilerTemp  // We popped one position
                
                // Current position = after ENDIF
                LDA ZP.OpCodeBufferContentSizeL
                STA ZP.IDYL  // Current position LSB
                LDA ZP.OpCodeBufferContentSizeH
                STA ZP.IDYH  // Current position MSB
                
                // Calculate forward offset: current_position - jumpw_operand_position
                SEC
                LDA ZP.IDYL    // Current position LSB
                SBC ZP.IDXL    // JUMPW operand position LSB
                STA ZP.NEXTL   // Forward offset LSB
                LDA ZP.IDYH    // Current position MSB
                SBC ZP.IDXH    // JUMPW operand position MSB
                STA ZP.NEXTH   // Forward offset MSB
                
                // Adjust for PC being 3 bytes past the JUMPW instruction start
                SEC
                LDA ZP.NEXTL
                SBC #3
                STA ZP.NEXTL
                LDA ZP.NEXTH
                SBC #0
                STA ZP.NEXTH
                
                // Patch the JUMPW operand
                CLC
                LDA ZP.OpCodeBufferL
                ADC ZP.IDXL    // Add JUMPW operand position
                STA ZP.IDXL    // Absolute patch address LSB
                LDA ZP.OpCodeBufferH
                ADC ZP.IDXH
                STA ZP.IDXH    // Absolute patch address MSB
                
                LDY #1         // Skip opcode byte, point to first operand byte
                LDA ZP.NEXTL   // Forward offset LSB
                STA [ZP.IDX], Y   // Patch LSB
                INY
                LDA ZP.NEXTH   // Forward offset MSB
                STA [ZP.IDX], Y   // Patch MSB
                
                // Pop and discard the already-patched JUMPZW position
                DEC ZP.SP
                DEC ZP.CompilerTemp  // We popped the second position
            }
            else
            {
                // === NO ELSE CLAUSE ===
                // CurrentToken should be ENDIF
                
                LDA ZP.CurrentToken
                CMP #Token.ENDIF
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL  // Expected ENDIF
                    States.SetFailure();
                    break;
                }
                
                // Consume ENDIF token
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // === PATCH ONLY JUMP (JUMPZW) ===
                // It should jump here (after ENDIF)
                
                // Pop JUMPZW operand position
                Stacks.PopIDX();
                DEC ZP.CompilerTemp  // We popped the position
                
                // Current position = after ENDIF
                LDA ZP.OpCodeBufferContentSizeL
                STA ZP.IDYL  // Current position LSB
                LDA ZP.OpCodeBufferContentSizeH
                STA ZP.IDYH  // Current position MSB
                
                // Calculate forward offset: current_position - jumpzw_operand_position
                SEC
                LDA ZP.IDYL    // Current position LSB
                SBC ZP.IDXL    // JUMPZW operand position LSB
                STA ZP.NEXTL   // Forward offset LSB
                LDA ZP.IDYH    // Current position MSB
                SBC ZP.IDXH    // JUMPZW operand position MSB
                STA ZP.NEXTH   // Forward offset MSB
                
                //Adjust for PC being 3 bytes past the JUMPZW instruction
                SEC
                LDA ZP.NEXTL
                SBC #3         
                STA ZP.NEXTL
                LDA ZP.NEXTH
                SBC #0
                STA ZP.NEXTH
                
                // Patch the JUMPZW operand
                CLC
                LDA ZP.OpCodeBufferL
                ADC ZP.IDXL    // Add JUMPZW operand position
                STA ZP.IDXL    // Absolute patch address LSB
                LDA ZP.OpCodeBufferH
                ADC ZP.IDXH
                STA ZP.IDXH    // Absolute patch address MSB
                
                LDY #1         // Skip opcode byte, point to first operand byte
                LDA ZP.NEXTL   // Forward offset LSB
                STA [ZP.IDX], Y   // Patch LSB
                INY
                LDA ZP.NEXTH   // Forward offset MSB
                STA [ZP.IDX], Y   // Patch MSB
            }
            
            States.SetSuccess();
            break;
        } // Single exit block
        
        States.IsSuccess();
        if (NC)
        {
            // Clean up stack on error path using counter
            LDA ZP.CompilerTemp
            if (NZ)  // If counter > 0, we have positions to pop
            {
                loop
                {
                    DEC ZP.SP
                    DEC ZP.CompilerTemp
                    if (Z) { break; }  // Done when counter reaches 0
                }
            }
        }
        
        // At the very end (after error cleanup):
        PLA
        STA ZP.CompilerTemp  // Restore parent's counter

    #ifdef TRACE
        LDA #(compileIfStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIfStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
    //        Error.CheckError();
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
           
           // Skip FOR token
           Tokenizer.NextToken();
           Error.CheckError();
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
           Error.CheckError();
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
               // Iterator not found - create implicit local
               // First push a default value to create the stack slot
               LDA #OpCode.PUSHVOID
               STA Compiler.compilerOpCode
               Emit.OpCode();
               
               LDA #BASICType.INT
               ORA #BASICType.VAR
               STA ZP.TOPT
               Emit.PushWord();
               
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
               
               // Now create the local node
               // ZP.TOP still contains name pointer
               // ZP.IDX still contains function node
               LDA #(SymbolType.LOCAL|BASICType.VAR)  // LOCAL type (will be updated by FROM expression type)
               STA ZP.SymbolType      // Locals.Add expects this
               
               Locals.Add();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
               
               // get BP offset
               Locals.Find(); // Input: ZP.IDX = function node, ZP.TOP = name, Output: C set if found, ZP.ACCL = BP offset
               
               INC Compiler.compilerFuncLocals  // Track new local
           }
           
           // Save iterator BP offset (A contains it from either Find or Add)
           LDA ZP.ACCL
           STA Compiler.compilerForIteratorOffset
           
           // Skip iterator name
           Tokenizer.NextToken();
           Error.CheckError();
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
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           Compiler.compileExpressionTree();  // Compile FROM expression
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Store FROM value in iterator (POPLOCAL)
           LDA Compiler.compilerForIteratorOffset
           STA Compiler.compilerOperand1
           LDA #OpCode.POPLOCAL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithByte();
           Error.CheckError();
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
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           Compiler.compileExpressionTree();  // Compile TO expression (leaves on stack)
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Check for optional STEP
           LDA ZP.CurrentToken
           CMP #Token.STEP
           if (Z)
           {
               // Skip STEP and compile STEP expression
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
               
               Compiler.compileExpressionTree();  // Compile STEP expression (leaves on stack)
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           else
           {
               // No STEP specified - default to 1
               LDA #OpCode.PUSH1
               STA Compiler.compilerOpCode
               Emit.OpCode();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           
           // Save FORCHK operand position for later patching
           LDA ZP.OpCodeBufferContentSizeL
           CLC
           ADC #2  // Skip opcode and iterator offset to get operand position
           STA ZP.TOPL
           LDA ZP.OpCodeBufferContentSizeH
           ADC #0
           STA ZP.TOPH
           Stacks.PushTop();  // Push FORCHK operand position
           
           // Emit FORCHK with placeholder forward offset
           LDA Compiler.compilerForIteratorOffset
           LDX #0x00  // Placeholder forward offset LSB
           LDY #0x00  // Placeholder forward offset MSB
           Emit.ForCheck();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Save loop body start position for FORIT's backward jump
           LDA ZP.OpCodeBufferContentSizeL
           STA ZP.TOPL
           LDA ZP.OpCodeBufferContentSizeH
           STA ZP.TOPH
           Stacks.PushTop();  // Push loop body start position
           
           // Skip any EOL tokens before loop body
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.EOL
               if (NZ) { break; }
               
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           
           // Compile loop body statements until NEXT
           loop // Statement compilation loop
           {
               LDA ZP.CurrentToken
               
               CMP #Token.NEXT
               if (Z) 
               { 
                   // Found NEXT - verify iterator and complete loop
                   break;
               }
               
               CMP #Token.EOF
               if (Z) 
               { 
                   Error.MissingNext(); BIT ZP.EmulatorPCL  // Missing NEXT
                   States.SetFailure();
                   break; 
               }
               
               CMP #Token.EOL
               if (Z)
               {
                   // Skip empty lines in loop body
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   continue;
               }
               
               // Compile statement in loop body
               CompileStatement();  // RECURSIVE CALL - handles nested constructs
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           
           // Check if we exited due to error
           Error.CheckError();
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
           Error.CheckError();
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
           Error.CheckError();
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

           // Verify it matches the current FOR iterator
           LDA ZP.ACCL
           CMP Compiler.compilerForIteratorOffset
           if (NZ)
           {
               Error.NextMismatch(); BIT ZP.EmulatorPCL  // NEXT variable doesn't match FOR
               States.SetFailure();
               break;
           }
           
           // Skip iterator name
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // === EMIT FORIT WITH BACKWARD JUMP ===
           
           // Pop loop body start position
           Stacks.PopTop();  // Get loop body start in ZP.TOP
           
           // Calculate backward jump offset for FORIT
           // Current position + 4 (FORIT size) = position after FORIT
           CLC
           LDA ZP.OpCodeBufferContentSizeL
           ADC #4
           STA ZP.IDYL
           LDA ZP.OpCodeBufferContentSizeH
           ADC #0
           STA ZP.IDYH
           
           // Backward offset = loop_start - position_after_FORIT
           SEC
           LDA ZP.TOPL
           SBC ZP.IDYL
           TAX  // Backward offset LSB
           LDA ZP.TOPH
           SBC ZP.IDYH
           TAY  // Backward offset MSB
           
           // Emit FORIT
           LDA Compiler.compilerForIteratorOffset
           Emit.ForIterate();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // === PATCH FORCHK WITH FORWARD JUMP ===
           
           // Pop FORCHK operand position
           Stacks.PopIDX();  // Get FORCHK operand position in ZP.IDX
           
           // Current position = after FORIT (exit point)
           LDA ZP.OpCodeBufferContentSizeL
           STA ZP.IDYL
           LDA ZP.OpCodeBufferContentSizeH
           STA ZP.IDYH
           
           // Calculate forward offset: exit_point - FORCHK_operand_position
           SEC
           LDA ZP.IDYL
           SBC ZP.IDXL
           STA ZP.NEXTL
           LDA ZP.IDYH
           SBC ZP.IDXH
           STA ZP.NEXTH
           
           // Adjust for PC being 4 bytes past FORCHK when it executes
           SEC
           LDA ZP.NEXTL
           SBC #2
           STA ZP.NEXTL
           LDA ZP.NEXTH
           SBC #0
           STA ZP.NEXTH
           
           // Calculate absolute address for patching
           CLC
           LDA ZP.OpCodeBufferL
           ADC ZP.IDXL
           STA ZP.IDXL
           LDA ZP.OpCodeBufferH
           ADC ZP.IDXH
           STA ZP.IDXH
           
           // Patch FORCHK operands
           LDY #0  // Point to first operand byte (forward offset LSB)
           LDA ZP.NEXTL
           STA [ZP.IDX], Y
           INY
           LDA ZP.NEXTH
           STA [ZP.IDX], Y
           
           // Emit stack cleanup (remove TO and STEP values)
           Emit.DecSp();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           Emit.DecSp();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           States.SetSuccess();
           break;
       } // Single exit block
       
       // Restore parent FOR iterator offset
       PLA
       STA Compiler.compilerForIteratorOffset
       
    #ifdef TRACE
       LDA #(compileForStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileForStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
}
