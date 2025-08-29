unit Compiler // Compiler.asm
{
   uses "./Definitions/OpCodes"
   uses "Tokenizer"
   uses "Emit"
   uses "CompilerFlow"
   uses "Optimizer"
   
   friend Emit, Functions, Locals, Compiler, CompilerFlow, Statement, Optimizer;
   
   // API Status: Clean
   // All public methods preserve caller state except for documented outputs
   // Buffer management and opcode emission with proper bounds checking
   
   // Private Compiler layer storage - BasicCompilerWorkspace (32 bytes)
   const uint compilerSavedTokenPosL     = Address.BasicCompilerWorkspace;      // 1 byte - saved tokenizer pos low
   const uint compilerSavedTokenPosH     = Address.BasicCompilerWorkspace + 1;  // 1 byte - saved tokenizer pos high
   const uint compilerOpCode             = Address.BasicCompilerWorkspace + 2;  // 1 byte - opcode to emit
   const uint compilerOperand1           = Address.BasicCompilerWorkspace + 3;  // 1 byte - first operand
   const uint compilerOperand2           = Address.BasicCompilerWorkspace + 4;  // 1 byte - second operand
   const uint compilerOperand3           = Address.BasicCompilerWorkspace + 5;  // 1 byte - third operand
   const uint compilerLastOpCode         = Address.BasicCompilerWorkspace + 6;  // 1 byte - last opcode emitted
   const uint compilerFuncArgs           = Address.BasicCompilerWorkspace + 7;  // 1 byte - number of arguments for current FUNC being compiled
   const uint compilerFuncLocals         = Address.BasicCompilerWorkspace + 8;  // 1 byte - number of locals for current FUNC being compiled
   const uint compilerSavedNodeAddrL     = Address.BasicCompilerWorkspace + 9;  // 1 byte - saved node addr low
   const uint compilerSavedNodeAddrH     = Address.BasicCompilerWorkspace + 10; // 1 byte - saved node addr high
   const uint compilerCanDeclareLocals   = Address.BasicCompilerWorkspace + 11; // 1 byte - flag for statement seen to prevent further local declarations
   const uint compilerForIteratorOffset  = Address.BasicCompilerWorkspace + 12; // 1 byte - signed one byte offset, actual location of for iterator relative to BP
   const uint compilerForIteratorType    = Address.BasicCompilerWorkspace + 13; // 1 byte - type of user or intrinsic for iterator variable
   const uint compilerGlobalIteratorSlot = Address.BasicCompilerWorkspace + 14; // 1 byte - slot of global being shadowed
   const uint compilerForIteratorBP      = Address.BasicCompilerWorkspace + 15; // 1 byte - signed one byte offset, location of for iterator relative to BP (according to Locals.Find)
   const uint compilerIfClauses          = Address.BasicCompilerWorkspace + 16; // 1 byte - compileIfStatement - single or multiline mode
   const uint compilerCurrentArgCount    = Address.BasicCompilerWorkspace + 17; // 1 byte - number of arguments in the current function call
#ifdef PEEPHOLE   
   const uint compilerSetItemObjInstr    = Address.BasicCompilerWorkspace + 18; // 1 byte - PUSHGLOBAL or PUSHLOCAL for SetItem object
   const uint compilerSetItemObjOffset   = Address.BasicCompilerWorkspace + 19; // 1 byte - offset or address for previous
   const uint compilerSetItemIndexInstr  = Address.BasicCompilerWorkspace + 20; // 1 byte - PUSHGLOBAL or PUSHLOCAL for SetItem index
   const uint compilerSetItemIndexOffset = Address.BasicCompilerWorkspace + 21; // 1 byte - offset or address for previous
   
#endif
   
   // Initialize the opcode buffer for compilation
   // Output: OpCode buffer ready for emission
   // Modifies: ZP.OpCodeBufferContentLengthL/H (set to 0), ZP.CompilerTokenPosL/H (set to current), ZP.CompilerFlags (cleared), ZP.XPC (set to buffer start)
   const string initOpCodeBufferTrace = "InitOpBuf";
   InitOpCodeBuffer()
   {
#ifdef TRACEPARSE
       LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Clear opcode buffer length
       STZ ZP.OpCodeBufferContentLengthL
       STZ ZP.OpCodeBufferContentLengthH
       
       // Initialize PC to start of opcode buffer
       LDA ZP.OpCodeBufferL
       STA ZP.XPCL
       LDA ZP.OpCodeBufferH
       STA ZP.XPCH
       
       // Save current tokenizer position for literal references
       LDA ZP.TokenizerPosL
       STA ZP.CompilerTokenPosL
       LDA ZP.TokenizerPosH
       STA ZP.CompilerTokenPosH
       
       // Clear compiler flags
       STZ ZP.CompilerFlags
       //     BIT 0 - the current expression being evaluated is numeric (INT|WORD|BYTE) and constant - used by compileExpressionTree()
       //     BIT 1 - we own the implicit variable - used by CompileForStatement
       //     BIT 2 - we used a global for our implicit variable - used by CompileForStatement
       //     BIT 3 - we're creating FORITF (rather than FORCHK & FORIT)
       //     BIT 4 - as "array assignment" flag
       //     BIT 5 - in CompileForStatement, we created an implicit local that needs to be removed at the end of the function
       //     BIT 6 - used in Tokenizer.TokenizeLineWithMode()
       
       // Reset to global scope
       STZ Compiler.compilerSavedNodeAddrL
       STZ Compiler.compilerSavedNodeAddrH
       
#ifdef PEEPHOLE
       LDA # OpCode.INVALID
       STA compilerLastOpCode
       Optimizer.ClearPeeps();
#endif

       SEC // Success
       
#ifdef TRACEPARSE
       LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   // ZP.TokenBuffer -> ZP.XID
   const string setLiteralBaseTrace = "SetLitBase";
   SetLiteralBase()
   {
#ifdef TRACEPARSE
       LDA #(setLiteralBaseTrace % 256) STA ZP.TraceMessageL LDA #(setLiteralBaseTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
    LDA ZP.TokenBufferL
    STA ZP.IDYL
    STA ZP.XIDL
    LDA ZP.TokenBufferH
    STA ZP.IDYH
    STA ZP.XIDH
       
#ifdef TRACEPARSE
       LDA #(setLiteralBaseTrace % 256) STA ZP.TraceMessageL LDA #(setLiteralBaseTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Check if the last emitted opcode is RETURN or RETURNVAL
   // Input: None (uses compilerLastOpCode tracking)
   // Output: C set if last opcode is RETURN/RETURNVAL, NC if not
   // Modifies: Processor flags only
   const string lastOpCodeIsReturnTrace = "ChkReturn";
   checkLastOpCodeIsReturn()
   {
#ifdef TRACEPARSE
       LDA #(lastOpCodeIsReturnTrace % 256) STA ZP.TraceMessageL LDA #(lastOpCodeIsReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       loop
       {
           LDA compilerLastOpCode
           
           // Check if it's RETURN or RETURNVAL
           CMP #OpCode.RETURN
           if (Z)
           {
               SEC // Found RETURN
               break;
           }
           
           CMP #OpCode.RETURNVAL
           if (Z)
           {
               SEC // Found RETURNVAL
               break;
           }
           
           CLC // Not a RETURN opcode
           break;
       }
       
#ifdef TRACEPARSE
       LDA #(lastOpCodeIsReturnTrace % 256) STA ZP.TraceMessageL LDA #(lastOpCodeIsReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Check if there's space for N bytes in opcode buffer
   // Input: A = number of bytes needed
   // Output: C if space available, NC if buffer would overflow
   // Strategy: Increment buffer length first, then check bounds
   const string checkBufferSpaceTrace = "ChkBufSpc";
   CheckBufferSpace()
   {
#ifdef TRACEPARSE
       PHA LDA #(checkBufferSpaceTrace % 256) STA ZP.TraceMessageL LDA #(checkBufferSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       loop
       {
           // Add required bytes to current buffer length
           CLC
           ADC ZP.OpCodeBufferContentLengthL
           STA ZP.OpCodeBufferContentLengthL
           LDA ZP.OpCodeBufferContentLengthH
           ADC #0
           STA ZP.OpCodeBufferContentLengthH
           
           // Compare against buffer size (512 bytes = 0x0200)
           LDA ZP.OpCodeBufferContentLengthH
           CMP #0x02
           if (C) // >= 0x0200, overflow
           {
               Error.BufferOverflow(); BIT ZP.EmulatorPCL
               break;
           }
           
           SEC // Success - space available
           break;
       }
       
#ifdef TRACEPARSE
       PHA LDA #(checkBufferSpaceTrace % 256) STA ZP.TraceMessageL LDA #(checkBufferSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif
   }
   
   
   
   // Calculate offset from compilation start to current tokenizer position
   // Output: ZP.ACCL/ZP.ACCH = offset from ZP.CompilerTokenPos to current ZP.TokenizerPos
   const string calculateTokenOffsetTrace = "CalcTokOff";
   CalculateTokenOffset()
   {
#ifdef TRACEPARSE
       LDA #(calculateTokenOffsetTrace % 256) STA ZP.TraceMessageL LDA #(calculateTokenOffsetTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Calculate current tokenizer position - compilation start position
       SEC
       LDA ZP.TokenizerPosL
       SBC ZP.CompilerTokenPosL
       STA ZP.ACCL
       LDA ZP.TokenizerPosH
       SBC ZP.CompilerTokenPosH
       STA ZP.ACCH
       
       SEC // Success
       
#ifdef TRACEPARSE
       LDA #(calculateTokenOffsetTrace % 256) STA ZP.TraceMessageL LDA #(calculateTokenOffsetTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   // Compile logical OR operations (lowest precedence)
   // Input: ZP.CurrentToken = current token
   // Output: Logical opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileConstExpressionTreelTrace = "CompFldExprTree // OR";
   CompileFoldedExpressionTree()
   {
#ifdef TRACEPARSE
        LDA #(compileConstExpressionTreelTrace % 256) STA ZP.TraceMessageL LDA #(compileConstExpressionTreelTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        SMB0 ZP.CompilerFlags // constant expression = TRUE
        
        LDA ZP.SP
        PHA
        
        loop
        {
            // Save initial buffer length for potential rollback
            LDA ZP.OpCodeBufferContentLengthL
            PHA
            LDA ZP.OpCodeBufferContentLengthH  
            PHA
            LDA ZP.XPCL
            PHA
            LDA ZP.XPCH
            PHA

            compileExpressionTree();
            CheckError();
            if (C)
            {
                if (BBS0, ZP.CompilerFlags) // constant expression:  was constant expression, the folded value is on VM stack
                {
                    Long.PopTop(); // Get the constant value into ZP.TOP/TOPT
                                
                    // Rollback the opcode buffer to initial state
                    PLA 
                    STA ZP.XPCH
                    PLA 
                    STA ZP.XPCL
                    PLA 
                    STA ZP.OpCodeBufferContentLengthH
                    PLA 
                    STA ZP.OpCodeBufferContentLengthL
                    
                    // Emit single constant opcode based on value and type
                    Emit.OptimizedConstant();
                    
                    break;
                }
            }
            
            // error or not constant - clean up saved state
            PLA PLA PLA PLA
            break;
        } // single exit
        PLA
        STA ZP.SP

#ifdef TRACEPARSE
        LDA #(compileConstExpressionTreelTrace % 256) STA ZP.TraceMessageL LDA #(compileConstExpressionTreelTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   // Compile logical OR operations (lowest precedence)
   // Input: ZP.CurrentToken = current token
   // Output: Logical opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileExpressionTreelTrace = "CompExprTree // OR";
   compileExpressionTree()
   {
#ifdef TRACEPARSE
       LDA #(compileExpressionTreelTrace % 256) STA ZP.TraceMessageL LDA #(compileExpressionTreelTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Compile left operand (higher precedence)
           compileLogicalAnd();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.OR
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
               
               // Get next token for right operand
               Tokenizer.NextTokenCheck();
               if (NC) { break; }
               
               // Compile right operand
               compileLogicalAnd();
               CheckError();
               if (NC) { break; }
               
               // Emit logical OR opcode
               LDA # OpCode.LOGICAL_OR
               Emit.OpCode();
               CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop

#ifdef TRACEPARSE
       LDA #(compileExpressionTreelTrace % 256) STA ZP.TraceMessageL LDA #(compileExpressionTreelTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // NEW: Compile logical NOT operations (level 9 precedence)
    // Input: ZP.CurrentToken = current token
    // Output: Logical NOT opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileLogicalNotTrace = "CompLogNot // NOT";
    compileLogicalNot()
    {
    #ifdef TRACEPARSE
        LDA #(compileLogicalNotTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalNotTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Token.NOT
            if (Z)
            {
                RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
                
                // Get next token for operand
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
                
                // Compile the operand (recursive call to handle multiple NOTs)
                compileLogicalNot();
                CheckError();
                if (NC) { break; }
                
                // Emit logical NOT opcode
                LDA # OpCode.LOGICAL_NOT
                Emit.OpCode();
                CheckError();
                if (NC) { break; }
            }
            else
            {
                // Not logical NOT, compile next precedence level
                compileComparison();
                CheckError();
                if (NC) { break; }
            }
            break;
        } // loop
    
    #ifdef TRACEPARSE
        LDA #(compileLogicalNotTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalNotTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
   
   // Compile logical AND operations
   // Input: ZP.CurrentToken = current token
   // Output: Logical AND opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileLogicalAndTrace = "CompAnd // AND";
   compileLogicalAnd()
   {
#ifdef TRACEPARSE
       LDA #(compileLogicalAndTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalAndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileLogicalNot();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.AND
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
               
               // Get next token for right operand
               Tokenizer.NextTokenCheck();
               if (NC) { break; }
               
               // Compile right operand
               compileLogicalNot();
               CheckError();
               if (NC) { break; }
               
               // Emit logical AND opcode
               LDA # OpCode.LOGICAL_AND
               Emit.OpCode();
               CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       

#ifdef TRACEPARSE
       LDA #(compileLogicalAndTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalAndTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile comparison operations (=, <>, <, >, <=, >=)
   // Input: ZP.CurrentToken = current token
   // Output: Comparison opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileComparisonTrace = "CompCmp // = | <> | < > <= >=";
   compileComparison()
   {
#ifdef TRACEPARSE
       LDA #(compileComparisonTrace % 256) STA ZP.TraceMessageL LDA #(compileComparisonTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileAdditive();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               switch (A)
               {
                   case Token.EQUALS:
                   case Token.NOTEQUAL:
                   case Token.LT:
                   case Token.GT:
                   case Token.LE:
                   case Token.GE:
                   {
                       RMB0 ZP.CompilerFlags // constant expression: BIT (comparison): not an integral constant expression
                       
                       PHA // Save operator on stack
                       
                       // Get next token for right operand
                       Tokenizer.NextTokenCheck();
                       if (NC) 
                       { 
                           PLA // Clean up stack
                           break; 
                       }
                       
                       // Compile right operand
                       compileAdditive();
                       PLA // Retrieve operator
                       CheckError();
                       if (NC) 
                       { 
                           break; 
                       }
                       
                       // Emit comparison opcode
                       switch (A)
                       {
                           case Token.EQUALS:   { LDA # OpCode.EQ }
                           case Token.NOTEQUAL: { LDA # OpCode.NE }
                           case Token.LT:       { LDA # OpCode.LT }
                           case Token.GT:       { LDA # OpCode.GT }
                           case Token.LE:       { LDA # OpCode.LE }
                           case Token.GE:       { LDA # OpCode.GE }
                       }
                       Emit.OpCode();
                       CheckError();
                       
                       if (NC) { break; }
                       
                       continue; // Check for more comparisons
                   }
                   default:
                   {
                       break; // Not a comparison operator
                   }
               }
               break;
           } // loop
           break;
       } // loop
       

#ifdef TRACEPARSE
       LDA #(compileComparisonTrace % 256) STA ZP.TraceMessageL LDA #(compileComparisonTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile bitwise AND operations (&)
   // Input: ZP.CurrentToken = current token
   // Output: Bitwise AND opcodes emitted, ZP.CurrentToken = token after expression  
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileBitwiseAndTrace = "CompBitAnd // &";
   compileBitwiseAnd()
   {
#ifdef TRACEPARSE
       LDA #(compileBitwiseAndTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseAndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Compile left operand (higher precedence)
           compileMultiplicative();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.BITWISE_AND
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: TODO: expand constant folding
               
               // Get next token for right operand
               Tokenizer.NextTokenCheck();
               CheckError();
               if (NC) { break; }
               
               // Compile right operand
               compileMultiplicative();
               CheckError();
               if (NC) { break; }
               
               // Emit bitwise AND opcode
               LDA # OpCode.BITWISE_AND
               Emit.OpCode();
               CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       

#ifdef TRACEPARSE
       LDA #(compileBitwiseAndTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseAndTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile bitwise OR operations
   // Input: ZP.CurrentToken = current token
   // Output: Bitwise OR opcodes emitted, ZP.CurrentToken = token after expression  
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileBitwiseOrTrace = "CompBitOr // |";
   compileBitwiseOr()
   {
#ifdef TRACEPARSE
       LDA #(compileBitwiseOrTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseOrTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileBitwiseAnd();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.BITWISE_OR
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: TODO: expand constant folding
               
               // Get next token for right operand
               Tokenizer.NextTokenCheck();
               if (NC) { break; }
               
               // Compile right operand
               compileBitwiseAnd();
               CheckError();
               if (NC) { break; }
               
               // Emit bitwise OR opcode
               LDA # OpCode.BITWISE_OR
               Emit.OpCode();
               CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       

#ifdef TRACEPARSE
       LDA #(compileBitwiseOrTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseOrTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile additive operations (+, -)
   // Input: ZP.CurrentToken = current token
   // Output: Additive opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileAdditiveTrace = "CompAdd // +";
   compileAdditive()
   {
#ifdef TRACEPARSE
       LDA #(compileAdditiveTrace % 256) STA ZP.TraceMessageL LDA #(compileAdditiveTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Compile left operand (higher precedence)
           compileBitwiseOr();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.PLUS
               if (Z)
               {
                   // Get next token for right operand
                   Tokenizer.NextTokenCheck();
                   CheckError();
                   if (NC) { break; }
                   
                   // Compile right operand
                   compileBitwiseOr();
                   CheckError();
                   if (NC) { break; }


                   // Emit addition opcode
                   LDA # OpCode.ADD
                   Emit.OpCode();
                   CheckError();
                   if (NC) { break; }
                   if (BBS0, ZP.CompilerFlags) // constant expression:  ADD: both sides are still constant
                   {
                       Instructions.Addition(); // Pop Pop + Push
                       CheckError();
                       if (NC) { break; }
                   }
                   
                   continue;
               }
               
               CMP #Token.MINUS
               if (Z)
               {
                   // Get next token for right operand
                   Tokenizer.NextTokenCheck();
                   if (NC) { break; }
                   
                   // Compile right operand
                   compileMultiplicative();
                   CheckError();
                   
                   if (NC) { break; }
                   
                   // Emit subtraction opcode
                   LDA # OpCode.SUB
                   Emit.OpCode();
                   CheckError();
                   if (NC) { break; }
                   if (BBS0, ZP.CompilerFlags) // constant expression:  SUB: both sides are still constant
                   {
                       Instructions.Subtraction(); // Pop Pop + Push
                       CheckError();
                       if (NC) { break; }
                   }
                   
                   continue;
               }
               
               break; // Not an additive operator
           } // loop
           break;
       } // loop
       
#ifdef TRACEPARSE
       LDA #(compileAdditiveTrace % 256) STA ZP.TraceMessageL LDA #(compileAdditiveTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   compileExponential()
   {
       compileUnary();
   }
   
   // Compile multiplicative operations (*, /, MOD)
   // Input: ZP.CurrentToken = current token
   // Output: Multiplicative opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileMultiplicativeTrace = "CompMult // *";
   compileMultiplicative()
   {
#ifdef TRACEPARSE
       LDA #(compileMultiplicativeTrace % 256) STA ZP.TraceMessageL LDA #(compileMultiplicativeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileExponential();
           CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               switch (A)
               {
                   case Token.MULTIPLY:
                   case Token.DIVIDE:
                   case Token.MOD:
                   {
                       PHA // Save operator on stack
                       
                       // Get next token for right operand
                       Tokenizer.NextTokenCheck();
                       if (NC) 
                       { 
                           PLA // Clean up stack
                           break; 
                       }

                       // Compile right operand
                       compileExponential();
                       CheckError();
                       PLA // Retrieve operator
                       if (NC) 
                       { 
                           break; 
                       }
                       
                       // Emit arithmetic opcode
                       switch (A)
                       {
                           case Token.MULTIPLY: { LDA # OpCode.MUL }
                           case Token.DIVIDE:   { LDA # OpCode.DIV }
                           case Token.MOD:      { LDA # OpCode.MOD }
                       }
                       Emit.OpCode();
                       CheckError();
                       
                       if (NC) { break; }
                       if (BBS0, ZP.CompilerFlags) // constant expression:  ADD: both sides are still constant
                       {
                           switch (A)
                           {
                               case OpCode.MUL:
                               {
                                   Instructions.Multiply(); // Pop Pop + Push
                               }
                               case OpCode.DIV:
                               {
                                   Instructions.Divide(); // Pop Pop + Push
                               }
                               case OpCode.MOD:
                               {
                                   Instructions.Modulo(); // Pop Pop + Push
                               }
                           }
                           CheckError();
                           if (NC) { break; }
                       }

                       
                       continue; // Check for more multiplicative operations
                   }
                   default:
                   {
                       break; // Not a multiplicative operator
                   }
               }
               break;
           } // loop
           break;
       } // loop

#ifdef TRACEPARSE
       LDA #(compileMultiplicativeTrace % 256) STA ZP.TraceMessageL LDA #(compileMultiplicativeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile unary operations (-, NOT)
   // Input: ZP.CurrentToken = current token
   // Output: Unary opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileUnaryTrace = "CompUnary // -";
   compileUnary()
   {
#ifdef TRACEPARSE
       LDA #(compileUnaryTrace % 256) STA ZP.TraceMessageL LDA #(compileUnaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           LDA ZP.CurrentToken
           switch (A)
           {
               case Token.MINUS:
               {
                   RMB0 ZP.CompilerFlags // constant expression: TODO: expand constant folding
                   
                   // Get next token for operand
                   Tokenizer.NextTokenCheck();
                   if (NC) { break; }
                   
                   // Compile the operand
                   compilePrimary();
                   if (NC) { break; }
                   
                   // Emit unary minus opcode
                   LDA # OpCode.NEG
                   Emit.OpCode();
                   CheckError();
                   if (NC) { break; }
               }
               case Token.BITWISE_NOT:
               {
                   RMB0 ZP.CompilerFlags // constant expression: TODO: expand constant folding
                   
                   // Get next token for operand
                   Tokenizer.NextTokenCheck();
                   if (NC) { break; }
                   
                   // Compile the operand
                   compilePrimary();
                   if (NC) { break; }
                   
                   // Emit logical NOT opcode
                   LDA # OpCode.BITWISE_NOT
                   Emit.OpCode();
                   CheckError();
                   if (NC) { break; }
               }
               default:
               {
                    // Not unary, compile primary
                    compilePrimary();
                    if (NC) { break; }
                    
                    // Check for postfix operations (indexing)
                    loop
                    {
                        LDA ZP.CurrentToken
                        CMP #Token.LBRACKET
                        if (NZ) { break; }  // No indexing
                        
                        // Handle indexing
                        RMB0 ZP.CompilerFlags // constant expression: LBRACKET not a constant expression
                        
                        // Get next token after '['
                        Tokenizer.NextTokenCheck();
                        if (NC) { break; }
                        
                        // Compile index expression
                        compileExpressionTree();  // Full expression for the index
                        CheckError();
                        if (NC) { break; }
                        
                        // Expect closing bracket
                        LDA ZP.CurrentToken
                        CMP #Token.RBRACKET
                        if (NZ)
                        {
                            Error.ExpectedRightBracket(); BIT ZP.EmulatorPCL
                            break;
                        }
                        
                        // Emit GETITEM opcode
                        Emit.GetItem();
                        CheckError();
                        if (NC) { break; }
                        
                        // Get next token
                        Tokenizer.NextTokenCheck();
                        break;
                    }
                }
           }
           break;
       } // loop

#ifdef TRACEPARSE
       LDA #(compileUnaryTrace % 256) STA ZP.TraceMessageL LDA #(compileUnaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Parse and compile function argument list
   // Input: ZP.CurrentToken = LPAREN (opening parenthesis)
   // Output: Arguments compiled and pushed to stack in correct order, ZP.CurrentToken = RPAREN
   // Modifies: ZP.CurrentToken, buffer state, compilation state
   const string compileArgumentListTrace = "CompArgs // (...)";
   compileArgumentList()
   {
       PHA
       PHX
       PHY
       
#ifdef TRACEPARSE
       LDA #(compileArgumentListTrace % 256) STA ZP.TraceMessageL LDA #(compileArgumentListTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop // Single exit
       {
           // Get token after opening parenthesis
           Tokenizer.NextTokenCheck();
           if (NC) { break; }
           
           // Check for empty argument list
           LDA ZP.CurrentToken
           CMP #Token.RPAREN
           if (Z)
           {
               SEC // Success - empty argument list
               break;
           }
           
           // Compile arguments separated by commas
           loop
           {
               // Compile argument expression
               compileExpressionTree(); // Use full expression compilation
               CheckError();
               if (NC) { break; }
               
               // Check what comes next
               LDA ZP.CurrentToken
               CMP #Token.RPAREN
               if (Z)
               {
                   SEC // Success - end of argument list
                   break;
               }
               
               // Expect comma for more arguments
               CMP #Token.COMMA
               if (NZ)
               {
                   Error.SyntaxError(); BIT ZP.EmulatorPCL
                   break;
               }
               
               // Get token after comma
               Tokenizer.NextTokenCheck();
               if (NC) { break; }
               
               // Continue with next argument
           }
           
           break; // Exit outer loop
       }
#ifdef TRACEPARSE
       LDA #(compileArgumentListTrace % 256) STA ZP.TraceMessageL LDA #(compileArgumentListTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       PLY
       PLX
       PLA
   }
    
   
    // Compile variable reference or function argument
    // Input: ZP.CurrentToken = IDENTIFIER token
    // Output: PUSHGLOBAL or PUSHLOCAL opcode emitted
    // Modifies: ZP.CurrentToken, buffer state, ZP.IDX, ZP.TOP
    const string compileVariableOrArgumentTrace = "CompVarArg // <identifier>";
    compileVariableOrArgument()
    {
        PHA
        PHX
        PHY
        
    #ifdef TRACEPARSE
        LDA #(compileVariableOrArgumentTrace % 256) STA ZP.TraceMessageL LDA #(compileVariableOrArgumentTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit
        {
            // Get the identifier name first!
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            CheckError();
            if (NC) { break; }
            
            // Check if we're compiling a function and this identifier is an argument
            InFunction();     // Are we in a function?
            if (C)
            {
                // Try to find this identifier as an argument
                // Get the function node being compiled
                LDA (compilerSavedNodeAddrL + 0)
                STA ZP.IDXL
                LDA (compilerSavedNodeAddrH + 0)
                STA ZP.IDXH
                
                // Look for argument/local by name
                Locals.Find();  // Input: ZP.IDX = function, ZP.TOP = name
                if (C)  // Found as argument or local
                {
                    // ZP.ACCL now contains the BP offset directly (no calculation needed!)
                    LDA ZP.ACCL
                    
                    // Emit PUSHLOCAL with the offset
                    Emit.PushLocal();  // A contains signed BP offset
                    CheckError();
                    if (NC) { break; }
                    
                    RMB0 ZP.CompilerFlags // constant expression: PUSHLOCAL not an integral constant expression

                    SEC  // Success
                    break;  // Exit - we handled it
                }
            }
            
            // Not an argument - emit push global variable opcode
            SEC
            
            LDA (Compiler.compilerSavedNodeAddrL + 0)
            STA ZP.IDXL
            ORA (Compiler.compilerSavedNodeAddrH + 0)
            STA ZP.IDXH
            ORA ZP.IDXL
            if (Z)
            {
                // no function node?
                // declaring the constant, like BYTE arr[<const>]
                STZ ZP.SymbolIteratorFilter // variables and constants
                Variables.Find(); // ZP.TOP = name, -> ZP.IDX = symbol node address, ZP.IDY is node index
                
                Emit.PushGlobal();
                CheckError();
                if (NC) { break; }
                RMB0 ZP.CompilerFlags // constant expression: PUSHGLOBAL not an integral constant expression

                SEC // Success
            }
            else
            {
                // Try to find this identifier as a global
                // Not found in locals, try globals (input is name in TOP)
                STZ ZP.SymbolIteratorFilter // variables and constants
                Variables.Find(); // ZP.TOP = name, -> ZP.IDX = symbol node address, ZP.IDY is node index
                if (C)
                {
                    // Before emitting PUSHGLOBAL, check for constant optimization
                    Optimizer.IsSimpleIntegralConstant();
                    if (C)
                    {
                        // Emit literal instead of variable reference
                        LDA ZP.TOPT
                        AND #BASICType.MASK
                        switch (A)
                        {
                            case BASICType.LONG:
                            {
                                // DEAD ?
                                LDA ZP.TOP0
                                STA Compiler.compilerOperand1
                                LDA ZP.TOP1
                                STA Compiler.compilerOperand2
                                LDA # BASICType.WORD
                                STA ZP.TOPT
                                Emit.PushWord();
                                
                                LDA ZP.TOP2
                                STA Compiler.compilerOperand1
                                LDA ZP.TOP3
                                STA Compiler.compilerOperand2
                                LDA # BASICType.LONG
                                STA ZP.TOPT
                                Emit.PushWord();
                            }
                            case BASICType.BIT:
                            {
                                LDA ZP.TOP0
                                Emit.PushBit();
                            }
                            case BASICType.BYTE:
                            {
                                Long.ZeroTop3(); // needed?
                                LDA ZP.TOP0
                                Emit.PushByte();
                            }
                            case BASICType.CHAR:
                            {
                                // DEAD ?
                                LDA ZP.TOPL
                                Emit.PushChar();
                            }
                            case BASICType.INT:
                            {
                                // DEAD ?
                                LDA ZP.TOP0
                                STA Compiler.compilerOperand1
                                LDA ZP.TOP1
                                STA Compiler.compilerOperand2
                                // TODO TYPE SIGN EXTENSION
                                if (MI)
                                {
                                    LDA #0xFF
                                    STA ZP.TOP2
                                    STA ZP.TOP3
                                }
                                else
                                {
                                    STZ ZP.TOP2
                                    STZ ZP.TOP3 // x2
                                }
                                Emit.PushWord();
                            }
                            default: // WORD
                            {
                                LDA ZP.TOP0
                                STA Compiler.compilerOperand1
                                LDA ZP.TOP1
                                STA Compiler.compilerOperand2
                                STZ ZP.TOP2
                                STZ ZP.TOP3 // x2
                                Emit.PushWord();
                            }
                        } // switch
                        if (BBS0, ZP.CompilerFlags) // constant expression:  PUSH constant value
                        {
                            LDA #BASICType.LONG
                            STA ZP.TOPT
                            Long.PushTop();
                        }
                    } // simple integral constant
                    else
                    {
                        // Normal variable reference
                        Emit.PushGlobal();
                        RMB0 ZP.CompilerFlags // constant expression: PUSHGLOBAL: not an integral constant expression
                    }
                    CheckError();
                    if (NC) { break; }
                    SEC // Success
                } // found
                else
                {
                    // DEAD ?
                    Functions.Find();  // Input: ZP.TOP = name pointer, Output: ZP.IDX = function node
                    if (C)
                    {
                        Error.SyntaxError(); // function with no ()
                    }
                    else
                    {
                        // not found?!
                        Error.UndefinedIdentifier();
                    }
                    BIT ZP.EmulatorPCL
                    States.SetFailure();
                }
            }
            break;
        } // single exit
        
    #ifdef TRACEPARSE
        LDA #(compileVariableOrArgumentTrace % 256) STA ZP.TraceMessageL LDA #(compileVariableOrArgumentTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
        
        PLY
        PLX
        PLA
    }


    const string compileFunctionCallOrVariableTrace = "CompFuncVar // <identifier>";
    compileFunctionCallOrVariable()
    {
        PHA
        PHX
        PHY
        
    #ifdef TRACEPARSE
        LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        LDA compilerCurrentArgCount
        PHA
        
        loop // Single exit
        {
            // Current token is IDENTIFIER
            // Save the literal position for potential CALL emission (use VM stack slot)
            LDA ZP.TokenLiteralPosL
            STA ZP.TOPL
            LDA ZP.TokenLiteralPosH
            STA ZP.TOPH
            LDA ZP.TOPT
            Stacks.PushTop();
            Tokenizer.PeekToken(); // peek next -> A
            CMP #Token.LPAREN
            if (Z)
            {     
                RMB0 ZP.CompilerFlags // constant expression: LPAREN: not an integral constant expression
                
                STZ compilerCurrentArgCount
                   
                Tokenizer.NextTokenCheck(); // consume LPAREN
                CheckError();
                if (NC) 
                { 
                    Stacks.PopA(); // clean up VM stack slot
                    break; 
                }
                // Create return slot (VOID 0) first
                if (BBS5, ZP.FLAGS)
                {
                    RMB5 ZP.FLAGS // subsequent function calls need RETURN slots
                }
                else
                {
                    Emit.PushVoid();  
                    CheckError();
                    if (NC) 
                    { 
                        Stacks.PopA(); // clean up VM stack slot
                        break; 
                    }
                }
                // Get next token after LPAREN to start argument parsing
                Tokenizer.NextTokenCheck();
                if (NC) 
                { 
                    Stacks.PopA(); // clean up VM stack slot
                    break; 
                }
                
                // Check for empty argument list
                LDA ZP.CurrentToken
                CMP #Token.RPAREN
                if (NZ)
                {
                    // Parse arguments separated by commas
                    loop
                    {
                        INC compilerCurrentArgCount
                        
                        compileExpressionTree();
                        CheckError();
                        if (NC) 
                        { 
                            Stacks.PopA(); // clean up VM stack slot
                            break; 
                        }
                        
                        // Check what comes next
                        LDA ZP.CurrentToken
                        CMP #Token.RPAREN
                        if (Z)
                        {
                            // End of argument list
                            break;
                        }
                        
                        // Expect comma for more arguments
                        CMP #Token.COMMA
                        if (NZ)
                        {
                            Error.SyntaxError(); BIT ZP.EmulatorPCL
                            Stacks.PopA(); // clean up VM stack slot
                            break;
                        }
                        
                        // Get token after comma
                        Tokenizer.NextTokenCheck();
                        if (NC) 
                        { 
                            Stacks.PopA(); // clean up VM stack slot
                            break; 
                        }
                        
                        // Continue with next argument
                    } // loop
                    
                    CheckError();
                    if (NC) 
                    { 
                        Stacks.PopA(); // clean up VM stack slot
                        break; 
                    }
                }
                
                // At this point, CurrentToken should be RPAREN
                LDA ZP.CurrentToken
                CMP #Token.RPAREN
                if (NZ)
                {
                    Error.ExpectedRightParen(); BIT ZP.EmulatorPCL
                    Stacks.PopA(); // clean up VM stack slot
                    break;
                }
                
                // Restore the saved literal position for CALL emission (from VM stack slot)
                Stacks.PopTop();
                LDA ZP.TOPH
                STA ZP.TokenLiteralPosH
                LDA ZP.TOPL
                STA ZP.TokenLiteralPosL
                
                Tokenizer.GetTokenString(); // name -> TOP
                Functions.Find();  // Input: ZP.TOP = name pointer, Output: ZP.IDX = function node
                if (C) // Function found
                {
                    // Compare with actual argument count
                    Locals.GetArgumentsCount(); // Input: ZP.IDX = function node, Output: A = argument count
                    LDA ZP.ACCL
                    CMP compilerCurrentArgCount
                    if (NZ)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                
                // Emit CALL opcode (uses ZP.TokenLiteralPos for function name)
                Emit.Call();
                CheckError();
                if (NC) { break; }
                
                // Get next token after closing parenthesis
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
            }
            else
            {
                Stacks.PopTop();
                LDA ZP.TOPH
                STA ZP.TokenLiteralPosH
                LDA ZP.TOPL
                STA ZP.TokenLiteralPosL
                
                Tokenizer.GetTokenString(); // name -> TOP
                Functions.Find();  // Input: ZP.TOP = name pointer, Output: ZP.IDX = function node
                if (C) 
                {
                    // function without ()
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get the identifier token
                Tokenizer.NextTokenCheck();
                if (NC) { break; }

                // Compile as variable or argument
                compileVariableOrArgument();

                CheckError();
                if (NC) { break; }
            }
            
            SEC // Success
            break;
        }
        
    #ifdef TRACEPARSE
        LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    
        PLA
        STA compilerCurrentArgCount
        
        
        
        PLY
        PLX
        PLA
    }
      
   
   // Compile primary expressions (numbers, identifiers, parentheses)
   // Input: ZP.CurrentToken = current token
   // Output: Primary opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), ZP.TOP/TOPT (via GetTokenNumber), ZP.ACC, buffer state, A/X/Y registers
   const string compilePrimaryTrace = "CompPrim // <literal>";
   compilePrimary()
   {
#ifdef TRACEPARSE
       LDA #(compilePrimaryTrace % 256) STA ZP.TraceMessageL LDA #(compilePrimaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       LDA ZP.CurrentToken
       switch (A)
       {
           case Token.TRUE:
           {
               // Emit PUSHBIT with value 1
               LDA #1
               STA ZP.TOP0
               Emit.PushBit();
               CheckErrorAndSetFailure();
               if (NC) { return; }
               
               if (BBS0, ZP.CompilerFlags) // constant expression: FALSE
               {
                   Long.ZeroTop3();
                   LDA #BASICType.BIT
                   STA ZP.TOPT
                   Long.PushTop();
               }
               
               // Get next token
               Tokenizer.NextTokenCheckSetFailure();
               return;
           }
           case Token.FALSE:
           {
               // Emit PUSHBIT with value 0
               LDA #0
               STA ZP.TOP0
               Emit.PushBit();
               CheckErrorAndSetFailure();
               if (NC) { return; }
               
               if (BBS0, ZP.CompilerFlags) // constant expression: FALSE
               {
                   Long.ZeroTop3();
                   LDA #BASICType.BIT
                   STA ZP.TOPT
                   Long.PushTop();
               }
               // Get next token
               Tokenizer.NextTokenCheckSetFailure();
               return;
           }
           case Token.NUMBER:
           {

               // Get number value and type
               Tokenizer.GetTokenNumber(); // Result in ZP.TOP, type in ZP.TOPT
               CheckErrorAndSetFailure();
               if (NC) { return; }
               
               // Emit appropriate push opcode based on type and value
               LDA ZP.TOPT
               CMP #BASICType.BIT
               if (Z)
               {
                   LDA ZP.TOPL // BIT values are single byte
                   Emit.PushBit();
                   CheckErrorAndSetFailure();
                   if (NC) { return; }
               }
               else
               {
                   if (BBS3, ZP.TOPT) // Bit 3 - LONG
                   {
                       LDA ZP.TOP0
                       STA compilerOperand1  // LSB
                       LDA ZP.TOP1
                       STA compilerOperand2  // MSB
                       
                       LDA # BASICType.WORD
                       STA ZP.TOPT
                       Emit.PushWord();
                       CheckErrorAndSetFailure();
                       if (NC) { return; }
                       
                       LDA ZP.TOP2
                       STA compilerOperand1  // LSB
                       LDA ZP.TOP3
                       STA compilerOperand2  // MSB
                       
                       LDA # BASICType.LONG
                       STA ZP.TOPT
                       Emit.PushWord();
                       CheckErrorAndSetFailure();
                       if (NC) { return; }
                   }
                   else
                   {
                       CMP #BASICType.BYTE
                       if (Z)
                       {
                           Long.ZeroTop3(); // needed?
                           LDA ZP.TOP0
                           Emit.PushByte();
                           CheckErrorAndSetFailure();
                           if (NC) { return; }
                       }
                       else
                       {
                           CMP #BASICType.INT
                           if (Z) // INT
                           {
                               LDA ZP.TOP0
                               STA compilerOperand1  // LSB
                               LDA ZP.TOP1
                               STA compilerOperand2  // MSB
                               // TODO TYPE SIGN EXTENSION
                               if (MI)
                               {
                                   LDA #0xFF
                                   STA ZP.TOP2
                                   STA ZP.TOP3
                               }
                               else
                               {
                                   STZ ZP.TOP2
                                   STZ ZP.TOP3 // x2
                               }
                           }
                           else // WORD
                           {
                               LDA ZP.TOP0
                               STA compilerOperand1  // LSB
                               LDA ZP.TOP1
                               STA compilerOperand2  // MSB
                               STZ ZP.TOP2
                               STZ ZP.TOP3 // x2
                           }
                           Emit.PushWord();
                           CheckErrorAndSetFailure();
                           if (NC) { return; }
                       }
                   }
               }
               if (BBS0, ZP.CompilerFlags) // constant expression: NUMBER: PUSH numeric literal
               {
                   LDA #BASICType.LONG
                   STA ZP.TOPT
                   Long.PushTop();
               }

               // Get next token
               Tokenizer.NextTokenCheckSetFailure();
               return;
           }
           case Token.STRINGLIT:
           {
               RMB0 ZP.CompilerFlags // constant expression: STRINGLIT: not an integral constant expression
               
               // OFFSET : compiling STRINGLIT
               // Emit PUSHCSTRING with pointer to string content from token stream
               LDA ZP.TokenLiteralPosL
               STA Compiler.compilerOperand1  // LSB
               LDA ZP.TokenLiteralPosH
               STA Compiler.compilerOperand2  // MSB
                
               Emit.PushCString();
               CheckErrorAndSetFailure();
               if (NC) { return; }
               
               // Get next token
               Tokenizer.NextTokenCheckSetFailure();
               return;
           }
           case Token.CHARLIT:
            {
                
                // The character byte is stored inline after the CHARLIT token
                
                // Calculate address of character value in token buffer
                LDA ZP.TokenBufferL
                CLC
                ADC ZP.TokenLiteralPosL
                STA ZP.IDXL
                LDA ZP.TokenBufferH
                ADC ZP.TokenLiteralPosH
                STA ZP.IDXH
                
                // Get the character value
                LDY #0
                LDA [ZP.IDX], Y
                STA ZP.TOP0
                
                Emit.PushChar();
                CheckErrorAndSetFailure();
                if (NC) { return; }
                
                if (BBS0, ZP.CompilerFlags) // constant expression: FALSE
                {
                    Long.ZeroTop3();
                    LDA #BASICType.CHAR
                    STA ZP.TOPT
                    Long.PushTop();
                }
                
                // Get next token
                Tokenizer.NextTokenCheckSetFailure();
                return;
            }
            case Token.INPUT:
            {
                RMB0 ZP.CompilerFlags // constant expression: INPUT: not an integral constant expression
                LDA #SysCallType.Input
                compileSysCall();
                return;
            }
           
           case Token.LPAREN:
           {
               // Get next token (start of sub-expression)
               Tokenizer.NextTokenCheck();
               if (NC) { return; }
               
               // Parse the sub-expression
               compileExpressionTree();
               CheckErrorAndSetFailure();
               if (NC) { return; }
               
               // Expect closing parenthesis
               LDA ZP.CurrentToken
               CMP #Token.RPAREN
               if (NZ)
               {
                   Error.SyntaxError(); BIT ZP.EmulatorPCL
                   CheckErrorAndSetFailure();
                   return;
               }
               
               // Get next token
               Tokenizer.NextTokenCheckSetFailure();
               return;
           }
           
            case Token.I2CFIND:
            {
                RMB0 ZP.CompilerFlags // constant expression: I2CFIND: not an integral constant expression
                LDA #SysCallType.I2CFind
                compileSysCall();
                return;
            }
            case Token.I2CEND:
            {
                RMB0 ZP.CompilerFlags // constant expression: I2CEND: not an integral constant expression
                LDA #SysCallType.I2CEnd
                compileSysCall();
                return;
            }
            case Token.I2CGET:
            {
                RMB0 ZP.CompilerFlags // constant expression: I2CGET: not an integral constant expression
                LDA #SysCallType.I2CGet
                compileSysCall();
                return;
            }
            case Token.I2CNEXT:
            {
                RMB0 ZP.CompilerFlags // constant expression: I2CNEXT: not an integral constant expression
                LDA #SysCallType.I2CNext
                compileSysCall();
                return;
            }
            
            case Token.MILLIS:
            {
                RMB0 ZP.CompilerFlags // constant expression: MILLIS: not an integral constant expression
                LDA #SysCallType.Millis
                compileSysCall();
                return;
            }
            case Token.SECONDS:
            {
                RMB0 ZP.CompilerFlags // constant expression: SECONDS: not an integral constant expression
                LDA #SysCallType.Seconds
                compileSysCall();
                return;
            }
            case Token.ABS:
            {
                RMB0 ZP.CompilerFlags // constant expression: ABS: not an integral constant expression
                LDA #SysCallType.Abs
                compileSysCall();
                return;
            }
            case Token.RND:
            {
                RMB0 ZP.CompilerFlags // constant expression: RND: not an integral constant expression
                LDA #SysCallType.Rnd
                compileSysCall();
                return;
            }
            case Token.PEEK:
            {
                RMB0 ZP.CompilerFlags // constant expression: PEEK: not an integral constant expression
                LDA #SysCallType.Peek
                compileSysCall();
                return;
            }
            case Token.READ:
            {
                RMB0 ZP.CompilerFlags // constant expression: READ: not an integral constant expression
                LDA #SysCallType.Read
                compileSysCall();
                return;
            }
            case Token.CHR:
            {
                LDA #SysCallType.Chr
                compileSysCall();
                if (BBS0, ZP.CompilerFlags) // constant expression: CHAR?
                {
                    Long.PopTop();
                    LDA # BASICType.CHAR
                    STA ZP.ACCT
                    // LONG -> CHAR
                    BASICTypes.Coerce();
                    CheckErrorAndSetFailure();
                    Long.PushTop();
                }  
                return;
            }
            case Token.ASC:
            {
                LDA #SysCallType.Asc
                compileSysCall();
                if (BBS0, ZP.CompilerFlags) // constant expression: CHAR?
                {
                    Long.PopTop();
                    LDA ZP.TOPT
                    CMP # BASICType.CHAR
                    if (Z)
                    {
                        // CHAR -> LONG
                        ZeroTop3();
                        Long.PushTopStrictLONG();
                    }
                    else
                    {
                        Long.PushTop();
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        CheckErrorAndSetFailure();
                    }
                }               
                return;
            }
            case Token.LEN:
            {
                RMB0 ZP.CompilerFlags // constant expression: LEN: not an integral constant expression
                LDA #SysCallType.Len
                compileSysCall();
                return;
            }

           // the only two cases that don't "return":                
           case Token.IDENTIFIER:
           {
               compileFunctionCallOrVariable();
           }
           default:
           {
               // Unexpected token
               Error.SyntaxError(); BIT ZP.EmulatorPCL
           }
       } // switch
       CheckErrorAndSetFailure();       

#ifdef TRACEPARSE
       LDA #(compilePrimaryTrace % 256) STA ZP.TraceMessageL LDA #(compilePrimaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
    // Single universal syscall compiler that uses metadata from SysCallType
    // Input: A = SysCallType value (encodes arg count, return type, and function ID)
    // Output: Complete syscall compiled including arguments, C set if successful
    // Modifies: ZP.CurrentToken, compilation state, opcode buffer
    // Stack Management: Properly cleans up the pushed SysCallType on all exit paths
    const string compileSysCallTrace = "CompSysCall";
    compileSysCall()
    {
#ifdef TRACEPARSE
        PHA LDA #(compileSysCallTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileSysCallTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry(); PLA
#endif
        
        PHA  // Save SysCallType value
        
        loop // Single exit
        {
            // Parse opening parenthesis
            Tokenizer.NextTokenCheck();
            if (NC) 
            { 
                PLA  // Clean stack before exit
                break; 
            }
            
            LDA ZP.CurrentToken
            CMP #Token.LPAREN
            if (NZ) 
            { 
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                CheckErrorAndSetFailure();
                PLA  // Clean stack before exit
                break; 
            }
            
            // Extract argument count from bits 1-0 of SysCallType
            PLA
            PHA  // Keep it on stack for later
            AND #0b00000011  // Extract arg count bits
            TAX  // X = argument count
            
            // Handle arguments based on count
            CPX #0
            if (Z)
            {
                // No arguments - expect immediate closing parenthesis
                Tokenizer.NextTokenCheckSetFailure();
                if (NC) 
                { 
                    PLA  // Clean stack before exit
                    break; 
                }
            }
            else
            {
                // Has arguments - compile them
                PHX  // Save argument count
                loop
                {
                    // Get next token (start of argument expression)
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }  // Exit argument loop on error
                    
                    // Compile the argument expression
                    PHX
                    compileExpressionTree();
                    PLX
                    CheckErrorAndSetFailure();
                    if (NC) { break; }  // Exit argument loop on error
                    
                    DEX  // One less argument to process
                    if (Z) { break; }  // Done with arguments
                    
                    // More arguments - expect comma
                    LDA ZP.CurrentToken
                    CMP #Token.COMMA
                    if (NZ)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        CheckErrorAndSetFailure();
                        break;
                    }
                    // Loop for next argument
                }
                PLX  // Restore argument count (for debugging if needed)
                
                // Check if argument loop exited with error
                if (NC) 
                { 
                    PLA  // Clean SysCallType from stack
                    break;  // Exit main loop on error
                }
            }
            
            // Expect closing parenthesis
            LDA ZP.CurrentToken
            CMP #Token.RPAREN
            if (NZ) 
            { 
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                CheckErrorAndSetFailure();
                PLA  // Clean stack before exit
                break; 
            }
            
            // Move past closing parenthesis
            Tokenizer.NextTokenCheckSetFailure();
            if (NC) 
            { 
                PLA  // Clean stack before exit
                break; 
            }
            
            // Emit the SYSCALL with original value
            PLA  // Get SysCallType value
            Emit.SysCall();  // A = SysCallType
            
            SEC // Success
            break;
        }
        
#ifdef TRACEPARSE
        LDA #(compileSysCallTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileSysCallTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
#endif
    }
   
   
   
   
   // Main entry point: Compile current expression to opcodes
   // Input: ZP.CurrentToken = first token of expression
   // Output: Expression compiled to opcode buffer, ZP.CurrentToken = token after expression
   // Modifies: OpCode buffer, ZP.CurrentToken, compilation state, ZP.TokenizerPos (via Tokenizer calls)
   const string strCompileExpression = "CompExpr";
   CompileExpression()
   {
#ifdef TRACEPARSE
       LDA #(strCompileExpression % 256) STA ZP.TraceMessageL LDA #(strCompileExpression / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

       // Initialize opcode buffer if this is the start of compilation
       BufferManager.UseREPLOpCodeBuffer();
       InitOpCodeBuffer();
       
       CheckErrorAndSetFailure();
       if (NC) { return; }
       
       // Compile the expression using same precedence as Expression.asm
       CompileFoldedExpressionTree(); // REPL expression
       CheckErrorAndSetFailure();
       if (NC) { return; }
       
       States.SetSuccess();

#ifdef TRACEPARSE
       LDA #(strCompileExpression % 256) STA ZP.TraceMessageL LDA #(strCompileExpression / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
    // Compile a block of statements separated by colons until any termination token
    // Input: None
    // Output: Statement block compiled, ZP.CurrentToken = terminating token (not consumed)
    //         C set if successful, NC if error
    // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
    // Termination tokens: EOF, WEND, NEXT, UNTIL, ELSE, ENDIF
    const string compileStatementBlockTrace = "CompStmtBlock";
    CompileStatementBlock()
    {
    #ifdef TRACEPARSE
        LDA #(compileStatementBlockTrace % 256) STA ZP.TraceMessageL LDA #(compileStatementBlockTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
    
        loop // Statement compilation loop
        {
            // Check for termination tokens
            LDA ZP.CurrentToken
            switch (A)
            {
                case Token.EOF:
                case Token.WEND:
                case Token.NEXT:
                case Token.UNTIL:
                case Token.ELSE:
                case Token.ENDIF:
                {
                    States.SetSuccess();
                    CheckErrorAndSetFailure();
                    break; // Exit main loop
                }
                
                case Token.EOL:
                {
                    // Skip empty lines
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }
                    continue; // Continue main loop
                }
                
                default:
                {
                    // Compile the statement
                    CompileStatement();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                    
                    // Handle colon separator
                    LDA ZP.CurrentToken
                    CMP #Token.COLON
                    if (Z)
                    {
                        Tokenizer.NextTokenCheckSetFailure();
                        if (NC) { break; }
                    }
                    continue; // Continue main loop
                }
            } // switch
            
            break; // Exit main loop (reached from termination cases or error)
        } // Statement compilation loop
    
    #ifdef TRACEPARSE
        LDA #(compileStatementBlockTrace % 256) STA ZP.TraceMessageL LDA #(compileStatementBlockTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
   
   
   // Compile function body from tokens to opcodes  
   // Input: Function tokens already copied to TokenBuffer, ZP.TokenBufferContentSize set, ZP.ACCL = number of arguments for FUNC
   // Output: Function compiled to opcode buffer, SystemState set
   // Modifies: OpCode buffer, ZP.CurrentToken, ZP.TokenizerPos, compilation state
   // Error: Sets ZP.LastError if compilation fails
   const string compileFunctionTrace = "CompFunc // FUNC";
   CompileFunction()
   {
       PHA
       PHX
       PHY
       
#ifdef TRACEPARSE
       LDA #(compileFunctionTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
        LDA ZP.ACCL
        STA compilerFuncArgs    
        
        LDA #1
        STA compilerCanDeclareLocals // locals are allowed
        
        RMB5 ZP.CompilerFlags // track if an implict local was created in CompileForStatement
        
       loop // Single exit block
       {
           // Initialize opcode buffer
           InitOpCodeBuffer();
           CheckErrorAndSetFailure();
           if (NC) { break; }
           
           //Save the function node address for argument lookups
           LDA ZP.IDXL
           STA compilerSavedNodeAddrL
           LDA ZP.IDXH
           STA compilerSavedNodeAddrH
           
           // Reset tokenizer to start of function body
           STZ ZP.TokenizerPosL
           STZ ZP.TokenizerPosH
           
           // Get first token of function body
           Tokenizer.NextTokenCheckSetFailure();
           if (NC) { break; }
           
           
           LDA #OpCode.ENTER
           Emit.OpCode();
           CheckErrorAndSetFailure();
           if (NC) { break; }
           
           STZ compilerFuncLocals // no locals yet
           
           // Replace the statement loop with:
           CompileStatementBlock();
           if (NC) { break; }
            
           // Validate we got EOF
           LDA ZP.CurrentToken
           CMP #Token.EOF
           if (NZ) 
           {
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               States.SetFailure();
               break;
           }
           
           // Check if last opcode was RETURN or RETURNVAL
           checkLastOpCodeIsReturn();
           if (NC) // Last opcode was not RETURN
           {
                // Emit RETURN with locals cleanup count
                LDA compilerFuncArgs
                // SP = BP cleans locals, RETURN cleans arguments
                //CLC
                //ADC compilerFuncLocals  // Total slots to clean
                Emit.Return();  // Pass total count
                CheckErrorAndSetFailure();
                if (NC) { break; }
           }
           Emit.Halt();  // sentinel for ending opcode iteration
           CheckErrorAndSetFailure();
           if (NC) { break; }
           
           States.SetSuccess(); // Success
           break;
       } // single exit
       
       if (BBS5, ZP.CompilerFlags) // in CompileForStatement, we created an implicit local that needs to be removed at the end of the function
       {
           LDA Compiler.compilerSavedNodeAddrL
           STA ZP.IDXL
           LDA Compiler.compilerSavedNodeAddrH
           STA ZP.IDXH
           Locals.RemoveLast();
       }
       
       // Reset to global scope
       STZ Compiler.compilerSavedNodeAddrL
       STZ Compiler.compilerSavedNodeAddrH
       

#ifdef TRACEPARSE
       LDA #(compileFunctionTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       PLY
       PLX
       PLA
   }
   
   

   // Compile a single statement within a function
   // Input: ZP.CurrentToken = first token of statement
   // Output: Statement compiled to opcodes, ZP.CurrentToken = token after statement  
   // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
   // Error: Sets ZP.LastError if statement compilation fails
   const string compileStatementTrace = "CompStmt // <statement>";
   CompileStatement()
   {
#ifdef TRACEPARSE
       PHA LDA #(compileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       

       loop // Single exit block
       {
           LDA ZP.CurrentToken
           switch (A)
           {
               case Token.WHILE:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   CompilerFlow.CompileWhileStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
               case Token.DO:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   CompilerFlow.CompileDoUntilStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
               case Token.PRINT:
               {
                   compilePrintStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
               case Token.CLS:
               {
                   compileCLSStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
               case Token.RETURN:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   compileReturnStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
               case Token.IF:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   CompilerFlow.CompileIfStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
               case Token.FOR:
               {
                   CompilerFlow.CompileForStatement();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
               }
                case Token.IDENTIFIER:
                {
                    // Could be assignment or function call
                    compileIdentifierStatement();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
                case Token.REM:
                case Token.COMMENT:
                {
                    // Skip comments - advance to next token
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }
                }
               
                case Token.I2CBEGIN:
                {
                    LDA #SysCallType.I2CBegin
                    compileSysCall();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
                case Token.I2CPUT:
                {
                    LDA #SysCallType.I2CPut
                    compileSysCall();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
                // include these so you can ignore/discard the return values (treat them like statements):
                case Token.I2CEND:
                {
                    LDA #SysCallType.I2CEnd
                    compileSysCall();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
                case Token.I2CGET:
                {
                    LDA #SysCallType.I2CGet
                    compileSysCall();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
                case Token.I2CNEXT:
                {
                    LDA #SysCallType.I2CNext
                    compileSysCall();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
               
                // VOID syscalls (procedures that don't return values)
                case Token.DELAY:
                {
                    LDA #SysCallType.Delay
                    compileSysCall();
                    if (NC) { break; }
                }
                case Token.POKE:
                {
                    LDA #SysCallType.Poke
                    compileSysCall();
                    if (NC) { break; }
                }
                case Token.WRITE:
                {
                    LDA #SysCallType.Write
                    compileSysCall();
                    if (NC) { break; }
                }
                case Token.PINMODE:
                {
                    LDA #SysCallType.PinMode
                    compileSysCall();
                    if (NC) { break; }
                }
               
               case Token.INT:
               case Token.WORD:
               case Token.BYTE:
               case Token.CHAR:
               case Token.BIT:
               case Token.STRING:
               case Token.LONG:
               {
                   Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;   
               }
               case Token.VAR:
               {
                   // Check if we've already seen flow altering statements
                   LDA compilerCanDeclareLocals
                   if (Z)
                   {
                       Error.LateDeclaration(); BIT ZP.EmulatorPCL  // "Declarations must come before statements"
                       States.SetFailure();
                       break;
                   }
                   loop
                   {
                       compileLocalDeclaration();
                       CheckError();
                       if (NC) { break; }
                       
                       // Check for comma
                        LDA ZP.CurrentToken
                        CMP #Token.COMMA
                        if (NZ) { break; } // Done
                        
                        // Set up for next iteration: pretend the COMMA is a VAR ..
                        LDA # Token.VAR
                        STA ZP.CurrentToken
                   }
                   if (NC) { break; }
                   break;
               }
               default:
               {
#ifdef DEBUG
                   PHA Tokens.PrintKeyword(); PLA
#endif      
                   CMP # Token.afterConsoleCommands
                   if (C)
                   {
                       Error.SyntaxError(); BIT ZP.EmulatorPCL
                   }
                   else
                   {
                       Error.IllegalInFunction(); BIT ZP.EmulatorPCL // console command    
                   }
                   States.SetFailure();
                   break;
               }
           }
           
           States.SetSuccess();
           break;
       } // loop
       
#ifdef TRACEPARSE
       PHA LDA #(compileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif
   }

    // Complete multi-argument PRINT statement compilation in Compiler.asm  
    // Supports all classic BASIC PRINT syntax:
    // PRINT expr[,expr...]      - Comma-separated with spaces
    // PRINT expr[;expr...]      - Semicolon-separated, no spaces  
    // PRINT expr,               - Trailing comma, no newline, add space
    // PRINT expr;               - Trailing semicolon, no newline, no space
    // PRINT                     - Empty line (newline only)
    //
    // Input: ZP.CurrentToken = PRINT token
    // Output: PRINT statement compiled to opcodes
    // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
    const string compilePrintStatementTrace = "CompPrint // PRINT";
    compilePrintStatement()
    {
    #ifdef TRACEPARSE
        LDA #(compilePrintStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePrintStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Get next token (should be start of expression, separator, or EOL)
            Tokenizer.NextTokenCheckSetFailure();
            if (NC) { break; }
            
            // Check for PRINT with no arguments (just newline)
            LDA ZP.CurrentToken
            CMP #Token.EOL
            if (Z)
            {
                // PRINT (newline only)
                Emit.PrintNewLine();
                CheckErrorAndSetFailure();
                if (NC) { break; }
                States.SetSuccess();
                break;
            }
            
            // Check for PRINT with trailing separator only
            LDA ZP.CurrentToken
            CMP #Token.COMMA
            if (Z)
            {
                // PRINT, - space and no newline
                Emit.PrintSpace();
                CheckErrorAndSetFailure();
                if (NC) { break; }
                
                Tokenizer.NextTokenCheckSetFailure();
                if (NC) { break; }
                States.SetSuccess();
                break;
            }
            
            CMP #Token.SEMICOLON
            if (Z)
            {
                // PRINT; - no space, no newline
                Tokenizer.NextTokenCheckSetFailure();
                if (NC) { break; }
                States.SetSuccess();
                break;
            }
            
            // Must have expression(s) - compile argument list
            loop // Argument processing loop
            {

                // Compile current expression
                CompileFoldedExpressionTree(); // PRINT arguments, use full expression compilation
                CheckErrorAndSetFailure();
                if (NC) { break; }

                // Emit system call to print the value on stack
                Emit.PrintValue();
                CheckErrorAndSetFailure();
                if (NC) { break; }
                
                // Check what follows this expression
                LDA ZP.CurrentToken
                CMP #Token.COMMA
                if (Z)
                {
                    // Comma separator - add space and continue with next expression
                    Emit.PrintSpace();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                    
                    // Get next token for next expression
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }
                    
                    // Check if this is a trailing comma (followed by EOL)
                    LDA ZP.CurrentToken
                    Tokens.IsEndOfPrintStatement();
                    if (C)
                    {
                        // Trailing comma - no newline, we're done
                        States.SetSuccess();
                        break; // Exit argument loop
                    }
                    
                    // Continue with next expression
                    continue;
                }
                
                CMP #Token.SEMICOLON
                if (Z)
                {
                    // Semicolon separator - no space, continue with next expression
                    // Get next token for next expression
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }
                    
                    // Check if this is a trailing semicolon (followed by EOL)
                    LDA ZP.CurrentToken
                    Tokens.IsEndOfPrintStatement();
                    if (C)
                    {
                        // Trailing semicolon - no newline, we're done
                        States.SetSuccess();
                        break; // Exit argument loop
                    }
                    
                    // Continue with next expression
                    continue;
                }
                
                // No separator - end of expression list
                // Default behavior: add newline
                Emit.PrintNewLine();
                CheckErrorAndSetFailure();
                if (NC) { break; }
                
                States.SetSuccess();
                break; // Exit argument loop
            } // End of argument processing loop
            
            break; // Exit main loop
        } // loop

    #ifdef TRACEPARSE
        LDA #(compilePrintStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePrintStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

   // Compile RETURN statement
   // Input: ZP.CurrentToken = RETURN token
   // Output: RETURN statement compiled to opcodes
   // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
   const string compileReturnStatementTrace = "CompReturn // RETURN";
   compileReturnStatement()
   {
#ifdef TRACEPARSE
       LDA #(compileReturnStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileReturnStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Get next token
           Tokenizer.NextTokenCheckSetFailure();
           if (NC) { break; }
           
           // Check if there's a return expression
           LDA ZP.CurrentToken
           CMP #Token.EOL
           if (Z)
           {
                // No return value - emit RETURN
                LDA compilerFuncArgs
                // SP = BP cleans locals, RETURN cleans arguments
                //CLC
                //ADC compilerFuncLocals  // Total slots to clean
                Emit.Return();  // Pass total count
                CheckErrorAndSetFailure();
                if (NC) { break; }
                States.SetSuccess();
                break;
           }
           
           // Compile return expression
           CompileFoldedExpressionTree(); // RETURN <expression>
           CheckErrorAndSetFailure();
           if (NC) { break; }
           
           // Emit RETURN with locals cleanup count
           LDA compilerFuncArgs
           // SP = BP cleans locals, RETURN cleans arguments
           //CLC
           //ADC compilerFuncLocals  // Total slots to clean
           Emit.ReturnVal();
           CheckErrorAndSetFailure();
           if (NC) { break; }
           
           States.SetSuccess();
           break;
       } // loop

#ifdef TRACEPARSE
       LDA #(compileReturnStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileReturnStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }

    
   
   
    
    // Compile local variable declaration inside a function
    // Input: ZP.CurrentToken = type token (only VAR)
    // Output: Local variable created and added to function's locals list
    //         ZP.CurrentToken = token after declaration
    // Modifies: compilerFuncLocals, ZP.CurrentToken, heap allocation
    // Error: Sets ZP.LastError if syntax error or memory allocation fails
    const string compileLocalDeclTrace = "CompLocDecl";
    compileLocalDeclaration()
    {
        PHA
        PHX
        PHY
        
    #ifdef TRACEPARSE
        LDA #(compileLocalDeclTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileLocalDeclTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit
        {
            // Move to identifier
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            // Verify we have an identifier
            LDA ZP.CurrentToken
            CMP #Token.IDENTIFIER
            if (NZ)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Get the identifier name
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            CheckError();
            if (NC) { break; }
            
            // Check for duplicate among existing locals/arguments
            LDA compilerSavedNodeAddrL
            STA ZP.IDXL
            LDA compilerSavedNodeAddrH
            STA ZP.IDXH
            Locals.Find();  // Uses existing Find with compareNames
            if (C)  // Found - duplicate
            {
                Error.VariableExists(); BIT ZP.EmulatorPCL
                break;
            }
            SEC
            
            // Increment local count
            INC compilerFuncLocals

            STZ compilerOperand1
            STZ compilerOperand2
                    
            // Create the slot by pushing appropriate default value
            Emit.PushEmptyVar(); // value zero, type with be (BASICType.LONG|BASICType.VAR) by default
            CheckError();
            if (NC) { break; }

            // Create local using Locals.Add (it handles allocation and linking)
            // Need to set up the type in the node
            LDA #BASICType.VAR
            ORA # SymbolType.LOCAL  // Combine with LOCAL
            STA ZP.SymbolType  // argument for Locals.Add()
            
            // ZP.IDX still has function node, ZP.TOP has name
            Locals.Add();  // Reuse existing Add method
            CheckError();
            if (NC) { break; }
            
            // Move past identifier
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            // Check for initialization
            LDA ZP.CurrentToken
            CMP #Token.EQUALS
            if (Z)
            {
                // Move past '='
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
                
                // Compile initialization expression
                CompileFoldedExpressionTree(); // <type> LOCAL = <expression>
                CheckError();
                if (NC) { break; }
                
                // Emit POPLOCAL with positive offset
                LDA compilerFuncLocals  // Positive BP offset (0-based)
                DEC A 
                Emit.PopLocal();
                CheckError();
                if (NC) { break; }
            }
            
            SEC  // Success
            break;
        }
        
    #ifdef TRACEPARSE
        LDA #(compileLocalDeclTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileLocalDeclTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
        
        PLY
        PLX
        PLA
    }
    
    // Compile assignment statement for both global and local variables
    // Input: ZP.ACCT = IdentifierType (Global or Local)
    //        ZP.IDX = variable/local node address  
    //        ZP.ACCL = BP offset (for locals only, signed byte)
    //        ZP.CurrentToken = current token (should be identifier)
    // Output: Assignment compiled to opcodes
    //         ZP.CurrentToken = token after complete assignment
    //         C set if successful, NC if error
    // Modifies: ZP.CurrentToken, opcode buffer, ZP.IDX (restored), 
    //           compilerOperand1/2, compilerOpCode, stack for expression evaluation
    // Error: Sets ZP.LastError if '=' expected, expression compilation fails,
    //        or opcode emission fails
    const string compileAssignmentTrace = "CompAssign";
    compileAssignment()
    {
        PHA
        PHX
        PHY
        
    #ifdef TRACEPARSE
        LDA #(compileAssignmentTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileAssignmentTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit
        {
            // Save identifier type on stack
            LDA ZP.ACCT // (IdentifierType)
            PHA  
            
            // Save node address (will be munted by expression compilation)
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Save BP offset for locals (will be munted by expression compilation)
            LDA ZP.ACCL
            PHA
            
            // Check if this is an array access before moving to '='
            LDA ZP.CurrentToken
            CMP #Token.IDENTIFIER
            if (Z)
            {
                
                // Look ahead to see what follows identifier
                Tokenizer.PeekToken(); // -> A
                CMP #Token.LBRACKET
                if (Z)
                {
                    // This is array indexing: array[index] = value
                    // Mark that we're doing array assignment
                    SMB4 ZP.CompilerFlags  // Set bit 4 as "array assignment" flag
                    
                    // Push array pointer onto stack
                    compileVariableOrArgument(); 
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
#ifdef PEEPHOLE
                    STZ Compiler.compilerSetItemObjInstr
                    LDA Compiler.compilerLastOpCode
                    CMP # OpCode.PUSHLOCAL
                    if (Z)
                    {
                        STA Compiler.compilerSetItemObjInstr
                    }
                    CMP # OpCode.PUSHGLOBAL
                    if (Z)
                    {
                        STA Compiler.compilerSetItemObjInstr
                    }
#endif     
                    // Move past '[' and compile index expression
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }
                    
                    Tokenizer.NextTokenCheckSetFailure();
                    if (NC) { break; }
                    
                    // Index value on stack
                    CompileFoldedExpressionTree(); // array[<expression>] = 
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                    
#ifdef PEEPHOLE                    
                    STZ Compiler.compilerSetItemIndexInstr
                    LDA Compiler.compilerLastOpCode
                    CMP # OpCode.PUSHLOCAL
                    if (Z)
                    {
                        STA Compiler.compilerSetItemIndexInstr
                        LDA Compiler.compilerSetItemObjInstr
                        if (NZ)
                        {
                            Optimizer.SetItemPrep();
                        }
                    }
                    CMP # OpCode.PUSHGLOBAL
                    if (Z)
                    {
                        STA Compiler.compilerSetItemIndexInstr
                        LDA Compiler.compilerSetItemObjInstr
                        if (NZ)
                        {
                            Optimizer.SetItemPrep();
                        }
                    }
#endif
                    
                    
                    // Expect ']'
                    LDA ZP.CurrentToken
                    CMP #Token.RBRACKET
                    if (NZ) 
                    { 
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break; 
                    }
                }
            }
            
            loop
            {
                // Move to next token - should be '='
                Tokenizer.NextTokenCheckSetFailure();
                if (NC) { break; }
                
                
                // Verify we have '='
                LDA ZP.CurrentToken
                CMP #Token.EQUALS
                if (NZ)
                {
                    Error.ExpectedEqual(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
                
                // Move past '=' to the expression
                Tokenizer.NextTokenCheckSetFailure();
                if (NC) { break; }
                
                // Compile the RHS expression (this WILL munt ZP.IDX and ZP.ACCL)
                CompileFoldedExpressionTree(); // local = <expression>
                CheckErrorAndSetFailure();
                if (NC) { break; }
                break;
            }
            
            // Restore saved values
            PLA  // Restore BP offset
            STA ZP.ACCL
            PLA  // Restore IDXH
            STA ZP.IDXH
            PLA  // Restore IDXL
            STA ZP.IDXL
            PLA  // Restore IdentifierType
            STA ZP.ACCT 
            
            CanContinue();
            if (NC)
            {
                break;
            }
            
            // === NEW: Check if this was array assignment ===
            if (BBS4, ZP.CompilerFlags)  // Check array assignment flag
            {
                RMB4 ZP.CompilerFlags  // Clear flag

                loop
                {
#ifdef PEEPHOLE                    
                    LDA Compiler.compilerSetItemObjInstr
                    CMP # OpCode.PUSHGLOBAL
                    if (Z)
                    {
                        LDA Compiler.compilerSetItemIndexInstr
                        CMP # OpCode.PUSHGLOBAL
                        if (Z)
                        {
                            // Stack now has: [value]
                            LDA Compiler.compilerSetItemObjOffset
                            STA Compiler.compilerOperand1
                            LDA Compiler.compilerSetItemIndexOffset
                            STA Compiler.compilerOperand2
                            LDA #OpCode.SETITEMGG
                            Emit.OpCodeWithWord();
                            break;
                        }
                        CMP # OpCode.PUSHLOCAL
                        if (Z)
                        {
                            // Stack now has: [value]
                            LDA Compiler.compilerSetItemObjOffset
                            STA Compiler.compilerOperand1
                            LDA Compiler.compilerSetItemIndexOffset
                            STA Compiler.compilerOperand2
                            LDA #OpCode.SETITEMGL
                            Emit.OpCodeWithWord();
                            break;
                        }
                    }
                    CMP # OpCode.PUSHLOCAL
                    if (Z)
                    {
                        LDA Compiler.compilerSetItemIndexInstr
                        CMP # OpCode.PUSHGLOBAL
                        if (Z)
                        {
                            // Stack now has: [value]
                            LDA Compiler.compilerSetItemObjOffset
                            STA Compiler.compilerOperand1
                            LDA Compiler.compilerSetItemIndexOffset
                            STA Compiler.compilerOperand2
                            LDA #OpCode.SETITEMLG
                            Emit.OpCodeWithWord();
                            break;
                        }
                        CMP # OpCode.PUSHLOCAL
                        if (Z)
                        {
                            // Stack now has: [value]
                            LDA Compiler.compilerSetItemObjOffset
                            STA Compiler.compilerOperand1
                            LDA Compiler.compilerSetItemIndexOffset
                            STA Compiler.compilerOperand2
                            LDA #OpCode.SETITEMLL
                            Emit.OpCodeWithWord();
                            break;
                        }
                    }
#endif                
                    // Stack now has: [array_ptr][index][value]
                    // Emit SETITEM opcode
                    LDA #OpCode.SETITEM
                    Emit.OpCode();
                    break;
                } 
                CheckErrorAndSetFailure();
                if (NC) { break; }
            }
            else
            {
                // Emit appropriate POP instruction based on identifier type
                LDA ZP.ACCT
                CMP #IdentifierType.Local
                if (Z)
                {
                    // Local variable - use POPLOCAL with BP offset
                    LDA ZP.ACCL  // BP offset (signed)
                    Emit.PopLocal();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
                else  // Must be Global
                {
                    // Need to get variable name again for PopGlobal
                    // The variable node is in ZP.IDX
                    Variables.GetName();  // Returns name in ZP.STR
                    
                    // Copy to ZP.TOP for PopGlobal
                    LDA ZP.STRL
                    STA ZP.TOPL
                    LDA ZP.STRH
                    STA ZP.TOPH
                    
                    // Call the new PopGlobal that finds by name and uses index
                    Emit.PopGlobal();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                }
            }
            States.SetSuccess();
            break;
        }
        
    #ifdef TRACEPARSE
        LDA #(compileAssignmentTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileAssignmentTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
        
        PLY
        PLX
        PLA
    }

   // Compile identifier statement (assignment or function call)
   // Input: ZP.CurrentToken = IDENTIFIER token
   // Output: Statement compiled to opcodes
   const string compileIdentifierStatementTrace = "compIdentStmt";
   compileIdentifierStatement()
   {
#ifdef TRACEPARSE
       PHA LDA #(compileIdentifierStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIdentifierStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
   
       loop
       {
           // Typically called when ZP.CurrentToken is Token.IDENTIFIER, or a keyword
           // Output: symbol or function in IDX, ZP.ACCT = IdentifierType
           Statement.ResolveIdentifier(); // Uses same logic as REPL
           CheckErrorAndSetFailure();
           if (NC) { break; }
           
           LDA ZP.ACCT
           switch (A)
           {
               case IdentifierType.Function:
               {
                   // Compile function call using existing logic
                   compileFunctionCallOrVariable();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
                   
                   // For function calls as statements, discard the return value
                   LDA #0x01  // Decrement by 1 position
                   Emit.DecSp();
                   CheckErrorAndSetFailure();
                   if (NC) { break; }
                   
                   States.SetSuccess();
                   break;
               }
                case IdentifierType.Global:
                case IdentifierType.Local:  // Add this case
                {
                    compileAssignment();
                    CheckErrorAndSetFailure();
                    if (NC) { break; }
                    States.SetSuccess();
                    break;
                }
               
               case IdentifierType.Constant:
               {
                   if (BBS5, ZP.FLAGS)
                   {
                       // populating the stack with global constants
                       compileAssignment();
                       CheckErrorAndSetFailure();
                       if (NC) { break; }
                       States.SetSuccess();
                   }
                   else
                   {
                       // Constants cannot be assigned to
                       Error.IllegalAssignment(); BIT ZP.EmulatorPCL
                       States.SetFailure();
                   }
                   break;
               }
               case IdentifierType.Keyword:
               {
                   // Keywords should not appear as statements
                   Error.SyntaxError(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
               default:
               {
                   // Unknown identifier type
                   Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }
           break;
       } // single exit
#ifdef TRACEPARSE
       PHA LDA #(compileIdentifierStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIdentifierStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif

       
   }
   
    // Check if we're currently compiling within a function context
    // Input: None
    // Output: C set if in function context (BEGIN or FUNC), NC if not
    // Modifies: Processor flags only
    // Note: Works for BEGIN, FUNC with args, and FUNC without args
    InFunction()
    {
        PHA
        
        // Check if we have a saved function node
        LDA compilerSavedNodeAddrL
        ORA compilerSavedNodeAddrH
        if (NZ)
        {
            SEC  // We're in a function context
        }
        else
        {
            CLC  // Not in a function context
        }
        
        PLA
    }
    
    // Compile CLS statement - clear screen
    // Input: ZP.CurrentToken = CLS token
    // Output: CLS opcode emitted to buffer
    // Modifies: OpCode buffer, ZP.CurrentToken
    const string compileCLSStatementTrace = "CompCLS // CLS";
    compileCLSStatement()
    {
    #ifdef TRACEPARSE
        LDA #(compileCLSStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileCLSStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Get next token - should be EOL or COLON (CLS takes no arguments)
            Tokenizer.NextTokenCheckSetFailure();
            if (NC) { break; }
            
            // Emit the CLS opcode
            Emit.ClearScreen();
            CheckErrorAndSetFailure();
            if (NC) { break; }
            
            States.SetSuccess();
            break;
        } // loop
        
    #ifdef TRACEPARSE
        LDA #(compileCLSStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileCLSStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
}
