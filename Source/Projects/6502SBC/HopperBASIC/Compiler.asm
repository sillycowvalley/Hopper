unit Compiler // Compiler.asm
{
   uses "./Definitions/OpCodes"
   uses "Tokenizer"
   uses "Emit"
   
   friend Emit, Functions;
   
   // API Status: Clean
   // All public methods preserve caller state except for documented outputs
   // Buffer management and opcode emission with proper bounds checking
   
   // Private Compiler layer storage - BasicCompilerWorkspace (32 bytes)
   const uint compilerSavedTokenPosL = Address.BasicCompilerWorkspace;      // 1 byte - saved tokenizer pos low
   const uint compilerSavedTokenPosH = Address.BasicCompilerWorkspace + 1;  // 1 byte - saved tokenizer pos high
   const uint compilerLiteralOffsetL = Address.BasicCompilerWorkspace + 2;  // 1 byte - literal offset low
   const uint compilerLiteralOffsetH = Address.BasicCompilerWorkspace + 3;  // 1 byte - literal offset high
   const uint compilerOpCode         = Address.BasicCompilerWorkspace + 4;  // 1 byte - opcode to emit
   const uint compilerOperand1       = Address.BasicCompilerWorkspace + 5;  // 1 byte - first operand
   const uint compilerOperand2       = Address.BasicCompilerWorkspace + 6;  // 1 byte - second operand
   const uint compilerLastOpCode     = Address.BasicCompilerWorkspace + 7;  // 1 byte - last opcode emitted
   const uint compilerFuncArgs       = Address.BasicCompilerWorkspace + 8;  // 1 byte - number of arguments for current FUNC being compiled
   const uint compilerFuncLocals     = Address.BasicCompilerWorkspace + 9;  // 1 byte - number of locals for current FUNC being compiled
   const uint compilerLiteralBaseL   = Address.BasicCompilerWorkspace + 10; // 1 byte - literal base address low
   const uint compilerLiteralBaseH   = Address.BasicCompilerWorkspace + 11; // 1 byte - literal base address high
   const uint compilerSavedNodeAddrL = Address.BasicCompilerWorkspace + 12; // 1 byte - saved node addr low
   const uint compilerSavedNodeAddrH = Address.BasicCompilerWorkspace + 13; // 1 byte - saved node addr high
   
   // Initialize the opcode buffer for compilation
   // Output: OpCode buffer ready for emission
   // Modifies: ZP.OpCodeBufferContentSizeL/H (set to 0), ZP.CompilerTokenPosL/H (set to current), ZP.CompilerFlags (cleared), ZP.XPC (set to buffer start)
   const string initOpCodeBufferTrace = "InitOpBuf";
   InitOpCodeBuffer()
   {
#ifdef TRACE
       LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Clear opcode buffer length
       STZ ZP.OpCodeBufferContentSizeL
       STZ ZP.OpCodeBufferContentSizeH
       
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
       
       LDA # OpCode.INVALID
       STA compilerLastOpCode
       
       SEC // Success
       
#ifdef TRACE
       LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // IDY -> compilerSavedTokenPos
   const string setLiteralBaseTrace = "SetLitBase";
   SetLiteralBase()
   {
#ifdef TRACE
       LDA #(setLiteralBaseTrace % 256) STA ZP.TraceMessageL LDA #(setLiteralBaseTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       LDA ZP.IDYL
       STA compilerLiteralBaseL
       LDA ZP.IDYH
       STA compilerLiteralBaseH
       
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
           ADC ZP.OpCodeBufferContentSizeL
           STA ZP.OpCodeBufferContentSizeL
           LDA ZP.OpCodeBufferContentSizeH
           ADC #0
           STA ZP.OpCodeBufferContentSizeH
           
           // Compare against buffer size (512 bytes = 0x0200)
           LDA ZP.OpCodeBufferContentSizeH
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
   const string compileLogicalTrace = "CompLog // OR";
   compileLogical()
   {
#ifdef TRACE
       LDA #(compileLogicalTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
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
           

#ifdef TRACE
       LDA #(compileLogicalTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
       
       
#ifdef DEBUG
       //LDA #'[' Debug.COut();
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
#ifdef DEBUG
       //LDA #']' Debug.COut(); // Exit from argument list  
#endif

#ifdef TRACE
       LDA #(compileArgumentListTrace % 256) STA ZP.TraceMessageL LDA #(compileArgumentListTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
       LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop // Single exit
       {
           // Save current token position for potential function name resolution
           LDA ZP.TokenizerPosL
           STA (compilerSavedTokenPosL + 0)
           LDA ZP.TokenizerPosH
           STA (compilerSavedTokenPosH + 0)
           
           // Look ahead to see if this is a function call (identifier followed by '(')
           Tokenizer.NextToken(); // Get token after identifier
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (Z)
           {
#ifdef DEBUG
       //LDA #'(' Debug.COut();
#endif
               // This is a function call - restore tokenizer to identifier and emit call
               LDA (compilerSavedTokenPosL + 0)
               STA ZP.TokenizerPosL
               LDA (compilerSavedTokenPosH + 0)
               STA ZP.TokenizerPosH
               
               // Get the identifier token back
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Create return slot (VOID 0)
               Emit.PushVoid();  
               Error.CheckError();
               if (NC) { break; }
               
               // Emit function call opcode
               Emit.Call();
               Error.CheckError();
               if (NC) { break; }
               
               // Expect opening parenthesis
               LDA ZP.CurrentToken
               CMP #Token.LPAREN
               if (NZ)
               {
                   Error.ExpectedLeftParen(); BIT ZP.EmulatorPCL
                   break;
               }
               
               // Parse function arguments
               compileArgumentList();
               Error.CheckError();
               if (NC) { break; }
               
               // Expect closing parenthesis (should be current token after argument parsing)
               LDA ZP.CurrentToken
               CMP #Token.RPAREN
               if (NZ)
               {
                   Error.ExpectedRightParen(); BIT ZP.EmulatorPCL
                   break;
               }
               
               // Get next token after closing parenthesis
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
#ifdef DEBUG
       //LDA #')' Debug.COut();
#endif

           }
           else
           {
#ifdef DEBUG
       //LDA #'V' Debug.COut();
#endif
               // Not a function call - restore position and emit variable push
               LDA (compilerSavedTokenPosL + 0)
               STA ZP.TokenizerPosL
               LDA (compilerSavedTokenPosH + 0)
               STA ZP.TokenizerPosH
               
               // Get the identifier token back
               Tokenizer.NextToken();
               Error.CheckError();
               if (NC) { break; }
               
               // Emit push global variable opcode (existing functionality)
               Emit.PushGlobal();
               Error.CheckError();
               if (NC) { break; }
           }
           
           SEC // Success
           break;
       }

#ifdef TRACE
       LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       PLY
       PLX
       PLA
   }
   
   
   // Compile ABS(expression) function call
   const string compileAbsFunctionTrace = "CompABS";
   compileAbsFunction()
   {
   #ifdef TRACE
       LDA #(compileAbsFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileAbsFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
   #endif
       
       loop // Single exit
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Get argument expression
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Compile argument
           compileLogical();
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
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit ABS SYSCALL
           Emit.Abs();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compileAbsFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileAbsFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodExit();
   #endif
   }
   
   // Compile DELAY(expression) function call
   const string compileDelayFunctionTrace = "CompDELAY";
   compileDelayFunction()
   {
   #ifdef TRACE
       LDA #(compileDelayFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileDelayFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
   #endif
       
       loop // Single exit
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Get argument expression
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Compile argument
           compileLogical();
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
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit DELAY SYSCALL
           Emit.Delay();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compileDelayFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileDelayFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodExit();
   #endif
   }
   
   

   // Compile MILLIS() function call
   const string compileMillisFunctionTrace = "CompMILLIS";
   compileMillisFunction()
   {
   #ifdef TRACE
       LDA #(compileMillisFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileMillisFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
   #endif
       
       loop // Single exit
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Expect immediate closing parenthesis (no arguments)
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.RPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit MILLIS SYSCALL
           Emit.Millis();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compileMillisFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileMillisFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodExit();
   #endif
   }
   
   // Compile SECONDS() function call
   const string compileSecondsFunctionTrace = "CompSECONDS";
   compileSecondsFunction()
   {
   #ifdef TRACE
       LDA #(compileSecondsFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileSecondsFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
   #endif
       
       loop // Single exit
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Expect immediate closing parenthesis (no arguments)
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.RPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit SECONDS SYSCALL
           Emit.Seconds();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compileSecondsFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileSecondsFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodExit();
   #endif
   }

   // Compile RND(expression) function call
   const string compileRndFunctionTrace = "CompRND";
   compileRndFunction()
   {
   #ifdef TRACE
       LDA #(compileRndFunctionTrace % 256) STA ZP.TraceMessageL 
       LDA #(compileRndFunctionTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
   #endif
       
       loop // Single exit
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Get max value argument
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Compile argument
           compileLogical();
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
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit RND SYSCALL
           Emit.Rnd();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compileRndFunctionTrace % 256) STA ZP.TraceMessageL  LDA #(compileRndFunctionTrace / 256) STA ZP.TraceMessageH   Trace.MethodExit();
   #endif
   }

   // Compile PEEK(address) function call
   const string compilePeekFunctionTrace = "CompPEEK";
   compilePeekFunction()
   {
   #ifdef TRACE
       LDA #(compilePeekFunctionTrace % 256) STA ZP.TraceMessageL LDA #(compilePeekFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       loop // Single exit
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Get address argument
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Compile address expression
           compileLogical();
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
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit PEEK SYSCALL
           Emit.Peek();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compilePeekFunctionTrace % 256) STA ZP.TraceMessageL LDA #(compilePeekFunctionTrace / 256) STA ZP.TraceMessageH  Trace.MethodExit();
   #endif
   }
   
   // Compile POKE statement
   // Input: ZP.CurrentToken = POKE token
   // Output: POKE statement compiled to opcodes
   // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
   const string compilePokeStatementTrace = "CompPOKE // POKE";
   compilePokeStatement()
   {
   #ifdef TRACE
       LDA #(compilePokeStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePokeStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       loop // Single exit block
       {
           // Expect opening parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           LDA ZP.CurrentToken
           CMP #Token.LPAREN
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Get address argument (first parameter)
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Compile address expression
           compileLogical();
           Error.CheckError();
           if (NC) { break; }
           
           // Expect comma separator
           LDA ZP.CurrentToken
           CMP #Token.COMMA
           if (NZ) 
           { 
               Error.SyntaxError(); BIT ZP.EmulatorPCL
               break; 
           }
           
           // Get value argument (second parameter)
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Compile value expression
           compileLogical();
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
           
           // Move past closing parenthesis
           Tokenizer.NextToken();
           Error.CheckError();
           if (NC) { break; }
           
           // Emit POKE SYSCALL
           Emit.Poke();
           
           SEC // Success
           break;
       }
       
   #ifdef TRACE
       LDA #(compilePokeStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePokeStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
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
                   
                   // Get next token
                   Tokenizer.NextToken();
                   Error.CheckError();
                   break;
               }
               case Token.STRINGLIT:
               {
                   Emit.PushCString();
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
                   break;
               }
               case Token.LPAREN:
               {
                   // Get next token (start of sub-expression)
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { break; }
                   
                   // Parse the sub-expression
                   compileLogical();
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
               
               case Token.ABS:
               {
                   compileAbsFunction();
                   Error.CheckError();
                   if (NC) { break; }
                   break;
               }
               case Token.MILLIS:
               {
                   compileMillisFunction();
                   Error.CheckError();
                   if (NC) { break; }
                   break;
               }
               case Token.SECONDS:
               {
                   compileSecondsFunction();
                   Error.CheckError();
                   if (NC) { break; }
                   break;
               }
               case Token.RND:
               {
                   compileRndFunction();
                   Error.CheckError();
                   if (NC) { break; }
                   break;
               }
               case Token.PEEK:
               {
                   compilePeekFunction();
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
       InitOpCodeBuffer();
       Error.CheckError();
       if (NC) { States.SetFailure(); return; }
       
       // Compile the expression using same precedence as Expression.asm
       compileLogical();
       Error.CheckError();
       if (NC) { States.SetFailure(); return; }
       
       States.SetSuccess();

#ifdef TRACE
       LDA #(strCompileExpression % 256) STA ZP.TraceMessageL LDA #(strCompileExpression / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
       
       loop // Single exit block
       {
           // Initialize opcode buffer
           InitOpCodeBuffer();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
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
           
           // Compile statements until end of function
           loop // Statement compilation loop
           {
               // Check for end of function
               LDA ZP.CurrentToken
               
               CMP #Token.EOF
               if (Z) { break; } // End of token stream
               
               CMP #Token.EOL
               if (Z)
               {
                   // Skip empty lines
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   continue;
               }
               
               // Compile the statement
               CompileStatement();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           } // Statement compilation loop
           
           Emit.Halt(); // DASM needs this
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Check if last opcode was RETURN or RETURNVAL
           checkLastOpCodeIsReturn();
           if (NC) // Last opcode was not RETURN
           {
               // Emit RETURN with locals cleanup count
               LDA compilerFuncLocals
               Emit.Return();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
           }
           
           States.SetSuccess(); // Success
           break;
       }

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
                   compileWhileStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.DO:
               {
                   compileDoUntilStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.PRINT:
               {
                   compilePrintStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.RETURN:
               {
                   compileReturnStatement();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
               }
               case Token.IF:
               {
                   compileIfStatement();
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
               case Token.DELAY:
               {
                   compileDelayFunction();
                   Error.CheckError();
                   if (NC) { break; }
                   break;
               }
               case Token.POKE:
               {
                   compilePokeStatement();
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
                compileLogical(); // Use full expression compilation
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
                    Tokens.IsEndOfStatement();
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
                    Tokens.IsEndOfStatement();
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
               LDA #0  // No locals to clean up for now
               Emit.Return();
               Error.CheckError();
               if (NC) { States.SetFailure(); break; }
               States.SetSuccess();
               break;
           }
           
           // Compile return expression
           compileLogical();
           Error.CheckError();
           if (NC) { States.SetFailure(); break; }
           
           // Emit RETURNVAL (expects value on stack)
           LDA #0  // No locals to clean up for now
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

   // Compile DO...UNTIL statement  
    // Input: ZP.CurrentToken = DO token
    // Output: DO...UNTIL loop compiled to opcodes with backward jump
    const string compileDoUntilStatementTrace = "CompDo // DO...UNTIL";
    compileDoUntilStatement()
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
            PHA  // Save loop start LSB
            LDA ZP.OpCodeBufferContentSizeH
            PHA  // Save loop start MSB
            
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
            compileLogical();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // === BACKWARD JUMP CALCULATION ===
            // Pop loop start position
            PLA
            STA ZP.TOPH  // Loop start position MSB
            PLA
            STA ZP.TOPL  // Loop start position LSB
            
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
   
   // Compile WHILE...WEND statement
   // Input: ZP.CurrentToken = WHILE token
   // Output: WHILE loop compiled to opcodes with correct relative jumps
   const string compileWhileStatementTrace = "CompWhile // WHILE...WEND";
   compileWhileStatement()
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
           PHA  // Save loop start LSB
           LDA ZP.OpCodeBufferContentSizeH
           PHA  // Save loop start MSB
           
           // Compile condition expression (e.g., "I < 10")
           compileLogical();       
           
           // Save forward jump operand position for later patching
           // This is where JUMPZW operand will be stored (after the opcode byte)
           LDA ZP.OpCodeBufferContentSizeL
           PHA     // Push operand position LSB
           LDA ZP.OpCodeBufferContentSizeH
           PHA     // Push operand position MSB
           
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
           PLA
           STA ZP.IDXH  // Forward jump operand position MSB
           PLA  
           STA ZP.IDXL  // Forward jump operand position LSB
           
           // Pop loop start position (target for backward jump)
           PLA
           STA ZP.TOPH  // Loop start position MSB
           PLA
           STA ZP.TOPL  // Loop start position LSB
           
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

   
   

   // Compile IF...THEN...ELSE...ENDIF statement
    // Input: ZP.CurrentToken = IF token
    // Output: IF statement compiled to opcodes with correct forward jumps
    const string compileIfStatementTrace = "CompIf // IF...THEN...ELSE...ENDIF";
    compileIfStatement()
    {
    #ifdef TRACE
        LDA #(compileIfStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileIfStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif

        // Save CompilerTemp for nesting
        LDA ZP.CompilerTemp
        PHA  // Preserve parent's counter
        
        // Initialize counter for this IF
        STZ ZP.CompilerTemp  // Track how many 2-byte positions we push

        loop // Single exit block
        {
            // Get and compile condition expression
            Tokenizer.NextToken();  // Skip IF token
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile condition expression (e.g., "X > 10")
            compileLogical();
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
            PHA     // Push JUMPZW operand position LSB
            LDA ZP.OpCodeBufferContentSizeH
            PHA     // Push JUMPZW operand position MSB
            INC ZP.CompilerTemp  // Track that we pushed 2 bytes
            
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
                PHA     // Push JUMPW operand position LSB
                LDA ZP.OpCodeBufferContentSizeH
                PHA     // Push JUMPW operand position MSB
                INC ZP.CompilerTemp  // Track that we pushed 2 more bytes (now 2 positions total)
                
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
                TSX
                LDA 0x0104,X  // JUMPZW operand position LSB (skip JUMPW positions)
                STA ZP.IDXL
                LDA 0x0103,X  // JUMPZW operand position MSB
                STA ZP.IDXH
                
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
                PLA
                STA ZP.IDXH  // JUMPW operand position MSB
                PLA
                STA ZP.IDXL  // JUMPW operand position LSB
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
                PLA  // Discard JUMPZW operand position MSB
                PLA  // Discard JUMPZW operand position LSB
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
                PLA
                STA ZP.IDXH  // JUMPZW operand position MSB
                PLA
                STA ZP.IDXL  // JUMPZW operand position LSB
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
                    PLA  // Discard MSB
                    PLA  // Discard LSB
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
               {
                   // Variable assignment: identifier = expression
                   // Save variable node address for POPGLOBAL emission
                   LDA ZP.IDXL
                   STA (compilerSavedNodeAddrL + 0)
                   LDA ZP.IDXH
                   STA (compilerSavedNodeAddrH + 0)
                   
                   // Move to next token - should be '='
                   Tokenizer.NextToken();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
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
                   if (NC) { States.SetFailure(); break; }
                   
                   // Compile the RHS expression
                   compileLogical();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
                   // Emit POPGLOBAL with the saved node address
                   LDA (compilerSavedNodeAddrL + 0)
                   STA compilerOperand1  // LSB
                   LDA (compilerSavedNodeAddrH + 0)
                   STA compilerOperand2  // MSB
                   
                   LDA #OpCode.POPGLOBAL
                   STA compilerOpCode
                   Emit.OpCodeWithWord();
                   Error.CheckError();
                   if (NC) { States.SetFailure(); break; }
                   
                   States.SetSuccess();
                   break;
               }
               case IdentifierType.Constant:
               {
                   // Constants cannot be assigned to
                   Error.IllegalAssignment(); BIT ZP.EmulatorPCL
                   States.SetFailure();
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

}
