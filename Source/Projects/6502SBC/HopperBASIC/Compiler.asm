unit Compiler // Compiler.asm
{
   uses "./Definitions/OpCodes"
   uses "Tokenizer"
   uses "Emit"
   uses "CompilerFlow"
   uses "Optimizer"
   
   friend Emit, Functions, Locals, CompilerFlow, Statement, Optimizer;
   
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
   
   // Initialize the opcode buffer for compilation
   // Output: OpCode buffer ready for emission
   // Modifies: ZP.OpCodeBufferContentLengthL/H (set to 0), ZP.CompilerTokenPosL/H (set to current), ZP.CompilerFlags (cleared), ZP.XPC (set to buffer start)
   const string initOpCodeBufferTrace = "InitOpBuf";
   InitOpCodeBuffer()
   {
#ifdef TRACE
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
       
       // Reset to global scope
       STZ Compiler.compilerSavedNodeAddrL
       STZ Compiler.compilerSavedNodeAddrH
       
       LDA # OpCode.INVALID
       STA compilerLastOpCode
       
       Optimizer.ClearPeeps();
       
       SEC // Success
       
#ifdef TRACE
       LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   // ZP.TokenBuffer -> ZP.XID
   const string setLiteralBaseTrace = "SetLitBase";
   SetLiteralBase()
   {
#ifdef TRACE
       LDA #(setLiteralBaseTrace % 256) STA ZP.TraceMessageL LDA #(setLiteralBaseTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
    LDA ZP.TokenBufferL
    STA ZP.IDYL
    STA ZP.XIDL
    LDA ZP.TokenBufferH
    STA ZP.IDYH
    STA ZP.XIDH
       
#ifdef TRACE
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
#ifdef TRACE
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
       
#ifdef TRACE
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
#ifdef TRACE
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
       
#ifdef TRACE
       PHA LDA #(checkBufferSpaceTrace % 256) STA ZP.TraceMessageL LDA #(checkBufferSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif
   }
   
   
   
   // Calculate offset from compilation start to current tokenizer position
   // Output: ZP.ACCL/ZP.ACCH = offset from ZP.CompilerTokenPos to current ZP.TokenizerPos
   const string calculateTokenOffsetTrace = "CalcTokOff";
   CalculateTokenOffset()
   {
#ifdef TRACE
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
       
#ifdef TRACE
       LDA #(calculateTokenOffsetTrace % 256) STA ZP.TraceMessageL LDA #(calculateTokenOffsetTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   
   
   
   // Compile logical OR operations (lowest precedence)
   // Input: ZP.CurrentToken = current token
   // Output: Logical opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileExpressionTreelTrace = "CompExprTree // OR";
   compileExpressionTree()
   {
#ifdef TRACE
       LDA #(compileExpressionTreelTrace % 256) STA ZP.TraceMessageL LDA #(compileExpressionTreelTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       SMB0 ZP.CompilerFlags // constant expression = TRUE
       LDA ZP.SP
       PHA
       
        // Save initial buffer length for potential rollback
        LDA ZP.OpCodeBufferContentLengthL
        PHA
        LDA ZP.OpCodeBufferContentLengthH  
        PHA
        LDA ZP.XPCL
        PHA
        LDA ZP.XPCH
        PHA
       
       loop
       {
           // Compile left operand (higher precedence)
           compileLogicalAnd();
           Error.CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.OR
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #26 HOut();
               
               // Get next token for right operand
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Compile right operand
               compileLogicalAnd();
               Error.CheckError();
               if (NC) { break; }
               
               // Emit logical OR opcode
               LDA #Token.OR
               Emit.LogicalOp();
               Error.CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       
       if (BBS0, ZP.CompilerFlags) // constant expression:  was constant expression, the folded value is on VM stack
       {
            Stacks.PopTop(); // Get the constant value into ZP.TOP/TOPT
Debug.NL(); LDA #'<' COut(); LDA #'-' COut(); TOut();
                        
            // Rollback the opcode buffer to initial state
            PLA 
            STA ZP.XPCH
            PLA 
            STA ZP.XPCL
            PLA 
            STA ZP.OpCodeBufferContentLengthH
            PLA 
            STA ZP.OpCodeBufferContentLengthL
            
Debug.NL(); TOut(); Space(); LDA ZP.XPCH HOut();LDA ZP.XPCL HOut();
            
            // Emit single constant opcode based on value and type
            Emit.OptimizedConstant();
       }
       else
       {
            // Not constant - clean up saved state
            PLA PLA PLA PLA
       }
       
       PLA
       STA ZP.SP

#ifdef TRACE
       LDA #(compileExpressionTreelTrace % 256) STA ZP.TraceMessageL LDA #(compileExpressionTreelTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile logical AND operations
   // Input: ZP.CurrentToken = current token
   // Output: Logical AND opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileLogicalAndTrace = "CompAnd // AND";
   compileLogicalAnd()
   {
#ifdef TRACE
       LDA #(compileLogicalAndTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalAndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileComparison();
           Error.CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.AND
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #25 HOut();
               
               // Get next token for right operand
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Compile right operand
               compileComparison();
               Error.CheckError();
               if (NC) { break; }
               
               // Emit logical AND opcode
               LDA #Token.AND
               Emit.LogicalOp();
               Error.CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       

#ifdef TRACE
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
#ifdef TRACE
       LDA #(compileComparisonTrace % 256) STA ZP.TraceMessageL LDA #(compileComparisonTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileBitwiseOr();
           Error.CheckError();
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
                       RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
                       
                       PHA // Save operator on stack
                       
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #24 HOut();
                       
                       // Get next token for right operand
                       Tokenizer.NextToken();
                       Error.CheckError();
                       if (NC) 
                       { 
                           PLA // Clean up stack
                           break; 
                       }
                       
                       // Compile right operand
                       compileBitwiseOr();
                       Error.CheckError();
                       if (NC) 
                       { 
                           PLA // Clean up stack
                           break; 
                       }
                       
                       // Emit comparison opcode
                       PLA // Retrieve operator
                       Emit.ComparisonOp();
                       Error.CheckError();
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
       

#ifdef TRACE
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
#ifdef TRACE
       LDA #(compileBitwiseAndTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseAndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Compile left operand (higher precedence)
           compileAdditive();
           Error.CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.BITWISE_AND
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: TODO: expand constant folding
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #23 HOut();
               
               // Get next token for right operand
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Compile right operand
               compileAdditive();
               Error.CheckError();
               if (NC) { break; }
               
               // Emit bitwise AND opcode
               LDA #Token.BITWISE_AND
               Emit.BitwiseOp();
               Error.CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       

#ifdef TRACE
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
#ifdef TRACE
       LDA #(compileBitwiseOrTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseOrTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileBitwiseAnd();
           Error.CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.BITWISE_OR
               if (NZ) { break; }
               
               RMB0 ZP.CompilerFlags // constant expression: TODO: expand constant folding
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #22 HOut();
               
               // Get next token for right operand
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Compile right operand
               compileBitwiseAnd();
               Error.CheckError();
               if (NC) { break; }
               
               // Emit bitwise OR opcode
               LDA #Token.BITWISE_OR
               Emit.BitwiseOp();
               Error.CheckError();
               if (NC) { break; }
           } // loop
           break;
       } // loop
       

#ifdef TRACE
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
#ifdef TRACE
       LDA #(compileAdditiveTrace % 256) STA ZP.TraceMessageL LDA #(compileAdditiveTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Compile left operand (higher precedence)
           compileMultiplicative();
           Error.CheckError();
           if (NC) { break; }
           
           loop
           {
               LDA ZP.CurrentToken
               CMP #Token.PLUS
               if (Z)
               {
                   // Get next token for right operand
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Compile right operand
                   compileMultiplicative();
                   Error.CheckError();
                   if (NC) { break; }


                   // Emit addition opcode
                   LDA #Token.PLUS
                   Emit.ArithmeticOp();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   if (BBS0, ZP.CompilerFlags) // constant expression:  ADD: both sides are still constant
                   {
                       Instructions.Addition(); // Pop Pop + Push
                       Error.CheckError();
                       if (NC) { break; }
Debug.NL(); LDA #'+' COut(); TOut();
                   }
                   
                   continue;
               }
               
               CMP #Token.MINUS
               if (Z)
               {
                   // Get next token for right operand
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Compile right operand
                   compileMultiplicative();
                   Error.CheckError();
                   
                   if (NC) { break; }
                   
                   // Emit subtraction opcode
                   LDA #Token.MINUS
                   Emit.ArithmeticOp();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   if (BBS0, ZP.CompilerFlags) // constant expression:  SUB: both sides are still constant
                   {
                       Instructions.Subtraction(); // Pop Pop + Push
                       Error.CheckError();
                       if (NC) { break; }
Debug.NL(); LDA #'-' COut(); TOut();
                   }
                   
                   continue;
               }
               
               break; // Not an additive operator
           } // loop
           break;
       } // loop
#ifdef TRACE
       LDA #(compileAdditiveTrace % 256) STA ZP.TraceMessageL LDA #(compileAdditiveTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Compile multiplicative operations (*, /, MOD)
   // Input: ZP.CurrentToken = current token
   // Output: Multiplicative opcodes emitted, ZP.CurrentToken = token after expression
   // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
   const string compileMultiplicativeTrace = "CompMult // *";
   compileMultiplicative()
   {
#ifdef TRACE
       LDA #(compileMultiplicativeTrace % 256) STA ZP.TraceMessageL LDA #(compileMultiplicativeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       
       loop
       {
           // Compile left operand (higher precedence)
           compileUnary();
           Error.CheckError();
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
                       Tokenizer.NextToken();
                       Error.CheckError();
                       if (NC) 
                       { 
                           PLA // Clean up stack
                           break; 
                       }
                       
                       // Compile right operand
                       compileUnary();
                       Error.CheckError();
                       if (NC) 
                       { 
                           PLA // Clean up stack
                           break; 
                       }
                       
                       // Emit arithmetic opcode
                       PLA // Retrieve operator
                       Emit.ArithmeticOp();
                       Error.CheckError();
                       if (NC) { break; }
                       
                       if (BBS0, ZP.CompilerFlags) // constant expression:  ADD: both sides are still constant
                       {
                           switch (A)
                           {
                               case Token.MULTIPLY:
                               {
                                   Instructions.Multiply(); // Pop Pop + Push
Debug.NL(); LDA #'*' COut(); TOut();
                               }
                               case Token.DIVIDE:
                               {
                                   Instructions.Divide(); // Pop Pop + Push
Debug.NL(); LDA #'/' COut(); TOut();
                               }
                               case Token.MOD:
                               {
                                   Instructions.Modulo(); // Pop Pop + Push
Debug.NL(); LDA #'%' COut(); TOut();
                               }
                           }
                           Error.CheckError();
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
       

#ifdef TRACE
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
#ifdef TRACE
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
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #7 HOut();

                   
                   // Get next token for operand
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Compile the operand
                   compilePrimary();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Emit unary minus opcode
                   Emit.UnaryMinus();
                   Error.CheckError();
                   if (NC) { break; }
               }
               case Token.NOT:
               {
                   RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #20 HOut();
                   
                   // Get next token for operand
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Compile the operand
                   compilePrimary();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Emit logical NOT opcode
                   LDA #Token.NOT
                   Emit.LogicalOp();
                   Error.CheckError();
                   if (NC) { break; }
               }
               default:
                {
                    // Not unary, compile primary
                    compilePrimary();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Check for postfix operations (indexing)
                    loop
                    {
                        LDA ZP.CurrentToken
                        CMP #Token.LBRACKET
                        if (NZ) { break; }  // No indexing
                        
                        // Handle indexing
                        RMB0 ZP.CompilerFlags // constant expression: LBRACKET not a constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #7 HOut();

                        
                        // Get next token after '['
                        Tokenizer.NextToken();
                        Error.CheckError();
                        if (NC) { break; }
                        
                        // Compile index expression
                        compileExpressionTree();  // Full expression for the index
                        Error.CheckError();
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
                        Error.CheckError();
                        if (NC) { break; }
                        
                        // Get next token
                        Tokenizer.NextToken();
                        Error.CheckError();
                        
                        break;
                    }
                }
           }
           break;
       } // loop
       

#ifdef TRACE
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
       
#ifdef TRACE
       LDA #(compileArgumentListTrace % 256) STA ZP.TraceMessageL LDA #(compileArgumentListTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop // Single exit
       {
           // Get token after opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
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
               compileComparison(); // Use full expression compilation
               Error.CheckError();
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
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Continue with next argument
           }
           
           break; // Exit outer loop
       }
#ifdef TRACE
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
        
    #ifdef TRACE
        LDA #(compileVariableOrArgumentTrace % 256) STA ZP.TraceMessageL LDA #(compileVariableOrArgumentTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit
        {
            
            // Get the identifier name first!
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            Error.CheckError();
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
                    Error.CheckError();
                    if (NC) { break; }
                    
                    RMB0 ZP.CompilerFlags // constant expression: PUSHLOCAL not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #6 HOut();

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
                // declaring the constant
                Emit.PushGlobal();
                Error.CheckError();
                if (NC) { break; }
                RMB0 ZP.CompilerFlags // constant expression: PUSHGLOBAL not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #5 HOut();

                SEC // Success
            }
            else
            {
                // Try to find this identifier as a global
                // Not found in locals, try globals (input is name in TOP)
                Variables.Find(); // ZP.IDX = symbol node address
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
                            case BASICType.BIT:
                            {
                                LDA ZP.TOPL
                                Emit.PushBit();
                            }
                            case BASICType.BYTE:
                            {
                                LDA ZP.TOPL  
                                Emit.PushByte();
                            }
                            case BASICType.CHAR:
                            {
                                LDA ZP.TOPL
                                Emit.PushChar();
                            }
                            default: // INT, WORD
                            {
                                LDA ZP.TOPL
                                STA Compiler.compilerOperand1
                                LDA ZP.TOPH
                                STA Compiler.compilerOperand2
                                Emit.PushWord();
                            }
                        }
                        if (BBS0, ZP.CompilerFlags) // constant expression:  PUSH constant value
                        {
Debug.NL(); LDA #'C' COut(); TOut();
                            LDA ZP.TOPT
                            Stacks.PushTop();
                        }
                    }
                    else
                    {
                        // Normal variable reference
                        Emit.PushGlobal();
                        RMB0 ZP.CompilerFlags // constant expression: PUSHGLOBAL: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #4 HOut();

                    }
                    Error.CheckError();
                    if (NC) { break; }
Debug.NL(); LDA #'y' COut();
                    SEC // Success
                }
            }
            break;
        } // single exit
        
    #ifdef TRACE
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
        
    #ifdef TRACE
        LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
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
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #3 HOut();

                   
                Tokenizer.NextToken(); // consume LPAREN
                Error.CheckError();
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
                    Error.CheckError();
                    if (NC) 
                    { 
                        Stacks.PopA(); // clean up VM stack slot
                        break; 
                    }
                }
                // Get next token after LPAREN to start argument parsing
                Tokenizer.NextToken();
                Error.CheckError();
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
                        // Compile argument expression
                        compileComparison(); // Use full expression compilation
                        Error.CheckError();
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
                        Tokenizer.NextToken();
                        Error.CheckError();
                        if (NC) 
                        { 
                            Stacks.PopA(); // clean up VM stack slot
                            break; 
                        }
                        
                        // Continue with next argument
                    } // loop
                    
                    Error.CheckError();
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
                
                // Emit CALL opcode (uses ZP.TokenLiteralPos for function name)
                Emit.Call();
                Error.CheckError();
                if (NC) { break; }
                
                // Get next token after closing parenthesis
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
            }
            else
            {
                // Not a function call - clean up VM stack slot
                Stacks.PopA(); 
                
                // Get the identifier token
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }

                // Compile as variable or argument
                compileVariableOrArgument();
                Error.CheckError();
                if (NC) { break; }
            }
            
            SEC // Success
            break;
        }
        
    #ifdef TRACE
        LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
        
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
#ifdef TRACE
       LDA #(compilePrimaryTrace % 256) STA ZP.TraceMessageL LDA #(compilePrimaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop // Single exit pattern
       {
           LDA ZP.CurrentToken
           switch (A)
           {
               case Token.TRUE:
               {
                   RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #2 HOut();

                   // Emit PUSHBIT with value 1
                   LDA #1
                   Emit.PushBit();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Get next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   break;
               }
               case Token.FALSE:
               {
                   RMB0 ZP.CompilerFlags // constant expression: BIT: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #1 HOut();

                   
                   // Emit PUSHBIT with value 0
                   LDA #0
                   Emit.PushBit();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Get next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   break;
               }
               case Token.NUMBER:
               {
                   // Get number value and type
                   Tokenizer.GetTokenNumber(); // Result in ZP.TOP, type in ZP.TOPT
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Emit appropriate push opcode based on type and value
                   LDA ZP.TOPT
                   CMP #BASICType.BIT
                   if (Z)
                   {
                       LDA ZP.TOPL // BIT values are single byte
                       Emit.PushBit();
                       Error.CheckError();
                       if (NC) { break; }
                   }
                   else
                   {
                       CMP #BASICType.BYTE
                       if (Z)
                       {
                           LDA ZP.TOPL
                           Emit.PushByte();
                           Error.CheckError();
                           if (NC) { break; }
                       }
                       else // 16-bit value (INT or WORD)
                       {
                           // Set up operands for word emission
                           LDA ZP.TOPL
                           STA compilerOperand1  // LSB
                           LDA ZP.TOPH
                           STA compilerOperand2  // MSB
                           
                           Emit.PushWord();
                           Error.CheckError();
                           if (NC) { break; }
                       }
                   }
                   if (BBS0, ZP.CompilerFlags) // constant expression: NUMBER: PUSH numeric literal
                   {
Debug.NL(); LDA #'N' COut(); TOut();
                       LDA ZP.TOPT
                       Stacks.PushTop();
                   }
                   
                   // Get next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   break;
               }
               case Token.STRINGLIT:
               {
                   RMB0 ZP.CompilerFlags // constant expression: STRING: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #19 HOut();
                   
                   // OFFSET : compiling STRINGLIT
                   // Emit PUSHCSTRING with pointer to string content from token stream
                   LDA ZP.TokenLiteralPosL
                   STA Compiler.compilerOperand1  // LSB
                   LDA ZP.TokenLiteralPosH
                   STA Compiler.compilerOperand2  // MSB
                    
                   Emit.PushCString();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Get next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   break;
               }
               case Token.CHARLIT:
                {
                    RMB0 ZP.CompilerFlags // constant expression: CHAR: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #18 HOut();
                    
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
                    
                    Emit.PushChar();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    break;
                }
               
               case Token.IDENTIFIER:
               {
                   compileFunctionCallOrVariable();
                   Error.CheckError();
                   if (BBS0, ZP.CompilerFlags) // constant expression: IDENTIFIER
                   {
if (C)
{
    Debug.NL(); LDA #'y' COut();
}
else
{
    Debug.NL(); LDA #'n' COut();
}
                   }
                   else
                   {
Debug.NL(); LDA #'n' COut();
                   }
                   
                   break;
               }
               case Token.LPAREN:
               {
                   // Get next token (start of sub-expression)
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Parse the sub-expression
                   compileExpressionTree();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Expect closing parenthesis
                   LDA ZP.CurrentToken
                   CMP #Token.RPAREN
                   if (NZ)
                   {
                       Error.SyntaxError(); BIT ZP.EmulatorPCL
                       break;
                   }
                   
                   // Get next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   break;
               }
               
               
               
               // All built-in functions now use compileSysCall()
                case Token.MILLIS:
                {
                    RMB0 ZP.CompilerFlags // constant expression: MILLIS: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #17 HOut();
                    LDA #SysCallType.Millis
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.SECONDS:
                {
                    RMB0 ZP.CompilerFlags // constant expression: SECONDS: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #16 HOut();
                    LDA #SysCallType.Seconds
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.ABS:
                {
                    RMB0 ZP.CompilerFlags // constant expression: ABS: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #15 HOut();
                    LDA #SysCallType.Abs
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.RND:
                {
                    RMB0 ZP.CompilerFlags // constant expression: RND: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #14 HOut();
                    LDA #SysCallType.Rnd
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.PEEK:
                {
                    RMB0 ZP.CompilerFlags // constant expression: PEEK: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #13 HOut();
                    LDA #SysCallType.Peek
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.READ:
                {
                    RMB0 ZP.CompilerFlags // constant expression: READ: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #12 HOut();
                    LDA #SysCallType.Read
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.CHR:
                {
                    RMB0 ZP.CompilerFlags // constant expression: CHR: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #11 HOut();
                    LDA #SysCallType.Chr
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.ASC:
                {
                    RMB0 ZP.CompilerFlags // constant expression: ASC: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #10 HOut();

                    LDA #SysCallType.Asc
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
                case Token.LEN:
                {
                    RMB0 ZP.CompilerFlags // constant expression: LEN: not an integral constant expression
Debug.NL(); LDA #'D' COut(); LDA #'Q' COut(); LDA #9 HOut();
                    
                    LDA #SysCallType.Len
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { break; }
                    break;
                }
               
               
               default:
               {
                   // Unexpected token
                   Error.SyntaxError(); BIT ZP.EmulatorPCL
                   break;
               }
           }
           
           break; // Normal exit point
       } // Single exit point

#ifdef TRACE
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
#ifdef TRACE
        PHA LDA #(compileSysCallTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileSysCallTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry(); PLA
#endif
        
        PHA  // Save SysCallType value
        
        loop // Single exit
        {
            // Parse opening parenthesis
            Tokenizer.NextToken();
            Error.CheckError();
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
                Tokenizer.NextToken();
                Error.CheckError();
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
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }  // Exit argument loop on error
                    
                    // Compile the argument expression
                    PHX
                    compileExpressionTree();
                    PLX
                    Error.CheckError();
                    if (NC) { break; }  // Exit argument loop on error
                    
                    DEX  // One less argument to process
                    if (Z) { break; }  // Done with arguments
                    
                    // More arguments - expect comma
                    LDA ZP.CurrentToken
                    CMP #Token.COMMA
                    if (NZ)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        CLC  // Indicate error
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
                PLA  // Clean stack before exit
                break; 
            }
            
            // Move past closing parenthesis
            Tokenizer.NextToken();
            Error.CheckError();
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
        
#ifdef TRACE
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
#ifdef TRACE
       LDA #(strCompileExpression % 256) STA ZP.TraceMessageL LDA #(strCompileExpression / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

       // Initialize opcode buffer if this is the start of compilation
       BufferManager.UseREPLOpCodeBuffer();
       InitOpCodeBuffer();
       
       Error.CheckError();
       if (NC) { States.SetFailure(); return; }
       
       // Compile the expression using same precedence as Expression.asm
       compileExpressionTree();
       Error.CheckError();
       if (NC) { States.SetFailure(); return; }
       
       States.SetSuccess();

#ifdef TRACE
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
    #ifdef TRACE
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
                    break; // Exit main loop
                }
                
                case Token.EOL:
                {
                    // Skip empty lines
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    continue; // Continue main loop
                }
                
                default:
                {
                    // Compile the statement
                    CompileStatement();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    // Handle colon separator
                    LDA ZP.CurrentToken
                    CMP #Token.COLON
                    if (Z)
                    {
                        Tokenizer.NextToken();
                        Error.CheckError();
                        if (NC) { States.SetFailure(); break; }
                    }
                    continue; // Continue main loop
                }
            } // switch
            
            break; // Exit main loop (reached from termination cases or error)
        } // Statement compilation loop
    
    #ifdef TRACE
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
       
#ifdef TRACE
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
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           //Save the function node address for argument lookups
            LDA ZP.IDXL
            STA compilerSavedNodeAddrL
            LDA ZP.IDXH
            STA compilerSavedNodeAddrH
           
           // Reset tokenizer to start of function body
           STZ ZP.TokenizerPosL
           STZ ZP.TokenizerPosH
           
           // Get first token of function body
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           
           Emit.Enter();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           STZ compilerFuncLocals // no locals yet
           
           // Replace the statement loop with:
           CompileStatementBlock();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
            
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
                CLC
                ADC compilerFuncLocals  // Total slots to clean
                Emit.Return();  // Pass total count
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
           }
           Emit.Halt();  // sentinel for ending opcode iteration
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
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
       

#ifdef TRACE
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
#ifdef TRACE
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
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.DO:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   CompilerFlow.CompileDoUntilStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.PRINT:
               {
                   compilePrintStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.CLS:
               {
                   compileCLSStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.RETURN:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   compileReturnStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.IF:
               {
                   STZ compilerCanDeclareLocals // no more locals after this
                   CompilerFlow.CompileIfStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.FOR:
               {
                   CompilerFlow.CompileForStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.IDENTIFIER:
               {
                   // Could be assignment or function call
                   compileIdentifierStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.REM:
               case Token.COMMENT:
               {
                   // Skip comments - advance to next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               
               
                // VOID syscalls (procedures that don't return values)
                case Token.DELAY:
                {
                    LDA #SysCallType.Delay
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                case Token.POKE:
                {
                    LDA #SysCallType.Poke
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                case Token.WRITE:
                {
                    LDA #SysCallType.Write
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                case Token.PINMODE:
                {
                    LDA #SysCallType.PinMode
                    compileSysCall();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                }
                
                
               
               case Token.INT:
               case Token.WORD:
               case Token.BYTE:
               case Token.CHAR:
               case Token.BIT:
               case Token.STRING:
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
                   compileLocalDeclaration();
                   Error.CheckError();
                   if (NC) { break; }
                   break;
               }
               default:
               {
#ifdef DEBUG
                   Tokens.PrintKeyword();
#endif      
                   // TODO: Add more statement types as needed
                   Error.SyntaxError(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }
           
           States.SetSuccess();
           break;
       } // loop
       
#ifdef TRACE
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
    #ifdef TRACE
        LDA #(compilePrintStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePrintStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Get next token (should be start of expression, separator, or EOL)
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Check for PRINT with no arguments (just newline)
            LDA ZP.CurrentToken
            CMP #Token.EOL
            if (Z)
            {
                // PRINT (newline only)
                Emit.PrintNewLine();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
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
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                States.SetSuccess();
                break;
            }
            
            CMP #Token.SEMICOLON
            if (Z)
            {
                // PRINT; - no space, no newline
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                States.SetSuccess();
                break;
            }
            
            // Must have expression(s) - compile argument list
            loop // Argument processing loop
            {
                // Compile current expression
                compileExpressionTree(); // Use full expression compilation
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // Emit system call to print the value on stack
                Emit.PrintValue();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                // Check what follows this expression
                LDA ZP.CurrentToken
                CMP #Token.COMMA
                if (Z)
                {
                    // Comma separator - add space and continue with next expression
                    Emit.PrintSpace();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
                    // Get next token for next expression
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
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
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    
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
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                
                States.SetSuccess();
                break; // Exit argument loop
            } // End of argument processing loop
            
            break; // Exit main loop
        } // loop

    #ifdef TRACE
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
#ifdef TRACE
       LDA #(compileReturnStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileReturnStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Get next token
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Check if there's a return expression
           LDA ZP.CurrentToken
           CMP #Token.EOL
           if (Z)
           {
                // No return value - emit RETURN
                LDA compilerFuncArgs
                CLC
                ADC compilerFuncLocals  // Total slots to clean
                Emit.Return();  // Pass total count
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
                States.SetSuccess();
                break;
           }
           
           // Compile return expression
           compileExpressionTree();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Emit RETURN with locals cleanup count
           LDA compilerFuncArgs
           CLC
           ADC compilerFuncLocals  // Total slots to clean
           Emit.ReturnVal();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           States.SetSuccess();
           break;
       } // loop

#ifdef TRACE
       LDA #(compileReturnStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileReturnStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }

    
   
   
    
    // Compile local variable declaration inside a function
    // Input: ZP.CurrentToken = type token (INT, WORD, BYTE, CHAR, BIT, VAR)
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
        
    #ifdef TRACE
        LDA #(compileLocalDeclTrace % 256) STA ZP.TraceMessageL 
        LDA #(compileLocalDeclTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit
        {
            // Get the type from current token
            LDX ZP.CurrentToken
            BASICTypes.FromToken();  // Output: C set if valid type, A = BASICType
            if (NC)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            STA ZP.NEXTT // save type
            
            // Move to identifier
            Tokenizer.NextToken();
            Error.CheckError();
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
            Error.CheckError();
            if (NC) { break; }
            
            // Check for duplicate among existing locals/arguments
            LDA compilerSavedNodeAddrL
            STA ZP.IDXL
            LDA compilerSavedNodeAddrH
            STA ZP.IDXH
            Locals.Find();  // Uses existing Find with compareNames
            if (C)  // Found - duplicate
            {
                Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                break;
            }
            SEC
            
            // Increment local count
            INC compilerFuncLocals

            STZ compilerOperand1
            STZ compilerOperand2
                    
            // Create the slot by pushing appropriate default value
            LDA ZP.NEXTT          // Get the type we saved
            
            AND #BASICType.MASK   // Mask off any VAR bit
            PHA
            switch (A)
            {
                case BASICType.STRING:
                {
                    // Find a null byte to use as empty string:
                    // The identifier we just parsed has a null terminator after it

                    LDA ZP.TokenizerPosH
                    STA ZP.ACCH
                    LDA ZP.TokenizerPosL
                    STA ZP.ACCL
                    if (Z)
                    {
                        DEC ZP.ACCH
                    }
                    DEC ZP.ACCL
#ifdef DEBUG             
                    // Verify in case something changes in future:
                    PHY
                    LDY ZP.ACCL
                    LDA [TokenBuffer], Y
                    if (NZ)
                    {
                        PLY
                        Error.InternalError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    PLY
                    LDA ZP.ACCL
                    
#endif        
                    STA compilerOperand1  // LSB of offset to null
                    LDA ZP.ACCH
                    STA compilerOperand2  // MSB of offset to null
                    
                    // Emit PUSHCSTRING with this offset
                    LDA #OpCode.PUSHCSTRING
                    STA compilerOpCode
                    Emit.OpCodeWithWord();
                }
                case BASICType.BIT:
                {
                    LDA #0
                    Emit.PushBit();
                }
                case BASICType.BYTE:
                {
                    // Emit PUSHBYTE 0
                    LDA #0
                    Emit.PushByte();
                }
                case BASICType.CHAR:
                {
                    // Emit PUSHCHAR 0
                    LDA #0
                    Emit.PushChar();
                }
                case BASICType.VAR:
                {
                    Emit.PushEmptyVar(); // value zero, type with be (BASICType.INT|BASICType.VAR) by default
                }
                default:
                {
                    // WORD, INT
                    STA ZP.TOPT
                    Emit.PushWord();
                }
            } // switch
            PLA
            
            Error.CheckError();
            if (NC) { break; }

            // Create local using Locals.Add (it handles allocation and linking)
            // Need to set up the type in the node
            ORA #SymbolType.LOCAL  // Combine with LOCAL
            STA ZP.SymbolType  // argument for Locals.Add()
            
            // ZP.IDX still has function node, ZP.TOP has name
            Locals.Add();  // Reuse existing Add method
            Error.CheckError();
            if (NC) { break; }
            
            // Move past identifier
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            // Check for initialization
            LDA ZP.CurrentToken
            CMP #Token.EQUALS
            if (Z)
            {
                // Move past '='
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Compile initialization expression
                compileExpressionTree();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit POPLOCAL with positive offset
                LDA compilerFuncLocals  // Positive BP offset (0-based)
                DEC A 
                Emit.PopLocal();
                Error.CheckError();
                if (NC) { break; }
            }
            
            SEC  // Success
            break;
        }
        
    #ifdef TRACE
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
        
    #ifdef TRACE
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
                    Error.CheckError();
                    if (NC)
                    {
                        States.SetFailure(); break; 
                    }
                    
                    // Move past '[' and compile index expression
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC)
                    {
                        States.SetFailure(); break; 
                    }
                    
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC)
                    {
                        States.SetFailure(); break; 
                    }
                    
                    compileExpressionTree(); // Index value on stack
                    Error.CheckError();
                    if (NC)
                    {

                        States.SetFailure(); break; 
                    }
                    
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
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) 
                { 
                    States.SetFailure(); 
                    break; 
                }
                
                
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
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) 
                { 
                    States.SetFailure(); 
                    break; 
                }
                
                // Compile the RHS expression (this WILL munt ZP.IDX and ZP.ACCL)
                compileExpressionTree();
                Error.CheckError();
                if (NC) 
                { 
                    States.SetFailure(); 
                    break; 
                }
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
                
                // Stack now has: [array_ptr][index][value]
                // Emit SETITEM opcode
                LDA #OpCode.SETITEM
                STA Compiler.compilerOpCode
                Emit.OpCode();
                Error.CheckError();
                if (NC) { States.SetFailure(); break; }
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
                    Error.CheckError();
                    if (NC) 
                    { 
                        States.SetFailure(); 
                        break; 
                    }
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
                    Error.CheckError();
                    if (NC) 
                    { 
                        States.SetFailure(); 
                        break; 
                    }
                }
            }
            States.SetSuccess();
            break;
        }
        
    #ifdef TRACE
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
#ifdef TRACE
       PHA LDA #(compileIdentifierStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIdentifierStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
   
       loop
       {
           // Typically called when ZP.CurrentToken is Token.IDENTIFIER, or a keyword
           // Output: symbol or function in IDX, A = IdentifierType
           Statement.ResolveIdentifier(); // Uses same logic as REPL
           Error.CheckError();
           if (NC) 
           { 
               States.SetFailure(); break; 
           }
           LDA ZP.ACCT
           switch (A)
           {
               case IdentifierType.Function:
               {
                   // Compile function call using existing logic
                   compileFunctionCallOrVariable();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
                   // For function calls as statements, discard the return value
                   Emit.DecSp();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
                   States.SetSuccess();
                   break;
               }
                case IdentifierType.Global:
                case IdentifierType.Local:  // Add this case
                {
                    compileAssignment();
                    Error.CheckError();
                    if (NC) { States.SetFailure(); break; }
                    States.SetSuccess();
                    break;
                }
               
               case IdentifierType.Constant:
               {
                   if (BBS5, ZP.FLAGS)
                   {
                       // populating the stack with global constants
                       compileAssignment();
                       Error.CheckError();
                       if (NC) { States.SetFailure(); break; }
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
                   Error.SyntaxError(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }
           break;
       } // single exit
#ifdef TRACE
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
    #ifdef TRACE
        LDA #(compileCLSStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileCLSStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Get next token - should be EOL or COLON (CLS takes no arguments)
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Emit the CLS opcode
            Emit.ClearScreen();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            States.SetSuccess();
            break;
        } // loop
        
    #ifdef TRACE
        LDA #(compileCLSStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileCLSStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
}
