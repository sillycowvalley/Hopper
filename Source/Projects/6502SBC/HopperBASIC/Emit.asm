unit Emit
{

   // Emit a single-byte opcode (no operands)
   // Input: compilerOpCode = opcode value
   // Output: OpCode written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented), ZP.XPC (incremented)
   const string emitOpCodeTrace = "EmitOp";
   OpCode()
   {
#ifdef TRACE
       LDA #(emitOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
#ifdef TRACEJIT
           Tools.NL(); LDA #'>' Debug.COut();
           LDA ZP.XPCH Debug.HOut(); LDA ZP.XPCL Debug.HOut();
           LDA #' ' Debug.COut(); LDA Compiler.compilerOpCode Debug.HOut(); LDA #' ' Debug.COut();
#endif        
           // Check space for 1 byte
           LDA #1
           CheckBufferSpace();
           if (NC) 
           { 
               break; 
           } // Buffer overflow
       
           // Write opcode to buffer
           LDA Compiler.compilerOpCode
           STA [ZP.XPC]
           STA Compiler.compilerLastOpCode
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           SEC // Success
           break;
       }
#ifdef TRACE
       LDA #(emitOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit opcode with one byte operand
   // Input: compilerOpCode = opcode value, compilerOperand1 = operand byte
   // Output: OpCode and operand written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented by 2), ZP.XPC (incremented by 2)
   const string emitOpCodeWithByteTrace = "EmitOpByte";
   OpCodeWithByte()
   {
#ifdef TRACE
       LDA #(emitOpCodeWithByteTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithByteTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       loop
       {
#ifdef TRACEJIT       
           Tools.NL(); LDA #'>' Debug.COut();
           LDA ZP.XPCH Debug.HOut(); LDA ZP.XPCL Debug.HOut();
           LDA #' ' Debug.COut(); LDA Compiler.compilerOpCode Debug.HOut(); LDA #' ' Debug.COut(); 
                                  LDA Compiler.compilerOperand1 Debug.HOut(); LDA #' ' Debug.COut();
#endif
           // Check space for 2 bytes
           LDA #2
           CheckBufferSpace();
           if (NC) 
           { 
               break; 
           } // Buffer overflow
       
           // Write opcode
           LDA Compiler.compilerOpCode
           STA [ZP.XPC]
           STA Compiler.compilerLastOpCode
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           // Write operand
           LDA Compiler.compilerOperand1
           STA [ZP.XPC]
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           SEC // Success
           break;
       }
       
#ifdef TRACE
       LDA #(emitOpCodeWithByteTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit opcode with two byte operands (word value)
   // Input: compilerOpCode = opcode value, compilerOperand1 = LSB, compilerOperand2 = MSB
   // Output: OpCode and operands written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented by 3), ZP.XPC (incremented by 3)
   const string emitOpCodeWithWordTrace = "EmitOpWord";
   OpCodeWithWord()
   {
#ifdef TRACE
       LDA #(emitOpCodeWithWordTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithWordTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       loop
       {
#ifdef TRACEJIT       
           Tools.NL(); LDA #'>' Debug.COut();
           LDA ZP.XPCH Debug.HOut(); LDA ZP.XPCL Debug.HOut();
           LDA #' ' Debug.COut(); LDA Compiler.compilerOpCode Debug.HOut(); LDA #' ' Debug.COut(); 
                                  LDA Compiler.compilerOperand1 Debug.HOut(); LDA #' ' Debug.COut();
                                  LDA Compiler.compilerOperand2 Debug.HOut(); LDA #' ' Debug.COut();
#endif        
           // Check space for 3 bytes
           LDA #3
           CheckBufferSpace();
           if (NC) 
           { 
               break; 
           } // Buffer overflow
           
           // Write opcode
           LDA Compiler.compilerOpCode
           STA [ZP.XPC]
           STA Compiler.compilerLastOpCode
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           // Write LSB
           LDA Compiler.compilerOperand1
           STA [ZP.XPC]
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           // Write MSB  
           LDA Compiler.compilerOperand2
           STA [ZP.XPC]
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           SEC // Success
           break;
       }
#ifdef TRACE
       LDA #(emitOpCodeWithWordTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit PUSHCSTRING opcode with word operand
   // Input: compilerOperand1 = string pointer LSB, compilerOperand2 = string pointer MSB  
   // Output: PUSHCSTRING opcode emitted with operands, C set if successful
   // Modifies: A, ZP.OpCodeBufferContentSizeL/H, buffer state
   const string emitPushCStringTrace = "EmitPUSHCSTRING";
   PushCString()
   {
       PHA
       
#ifdef TRACE
       LDA #(emitPushCStringTrace % 256) STA ZP.TraceMessageL LDA #(emitPushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Set up opcode
           LDA #OpCode.PUSHCSTRING
           STA Compiler.compilerOpCode
           
           // Emit opcode with word operand (uses compilerOperand1/2)
           Emit.OpCodeWithWord();
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitPushCStringTrace % 256) STA ZP.TraceMessageL LDA #(emitPushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       PLA
   }
   
   // Emit PUSHGLOBAL opcode for identifier
   // Input: Current token is IDENTIFIER
   // Output: PUSHGLOBAL opcode with node address emitted, C set if successful
   // Modifies: A, X, Y, ZP.TOP, ZP.IDX, compilerOperand1/2
   const string emitPushGlobalTrace = "Emit PUSHGLOBAL";
   PushGlobal()
   {
       PHA
       PHX
       PHY
       
#ifdef TRACE
       LDA #(emitPushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop // Single exit
       {
           // Get the identifier name from the tokenizer
           Tokenizer.GetTokenString(); // Result in ZP.TOP (name pointer)
           Error.CheckError();
           if (NC) { break; }
           
           // Find the variable/constant by name
           STZ ZP.SymbolIteratorFilter  // Accept both variables and constants
           Variables.Find();  // Input: ZP.TOP = name, Output: ZP.IDX = node address
           if (NC)
           {
               // Variable not found
               Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
               break;
           }
           
           // Store node address as operands
           LDA ZP.IDXL
           STA Compiler.compilerOperand1  // LSB
           LDA ZP.IDXH
           STA Compiler.compilerOperand2  // MSB
           
           // Emit PUSHGLOBAL with word operand
           LDA # OpCode.PUSHGLOBAL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithWord();
           break;
       }
       
#ifdef TRACE
       LDA #(emitPushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       PLY
       PLX
       PLA
   }
   
   
   // Emit PUSHBIT opcode with immediate value
   // Input: A = bit value (0 or 1)
   // Output: PUSHBIT opcode emitted with value
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitPushBitTrace = "Emit PUSHBIT";
   PushBit()
   {
#ifdef TRACE
       PHA LDA #(emitPushBitTrace % 256) STA ZP.TraceMessageL LDA #(emitPushBitTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       loop
       {
           // Validate bit value (must be 0 or 1)
           CMP #2
           if (C) // >= 2, invalid
           {
               Error.InvalidBitValue(); BIT ZP.EmulatorPCL
               break;
           }
       
           // Set up parameters for emission
           STA Compiler.compilerOperand1          // Store value as operand
           LDA #OpCode.PUSHBIT
           STA Compiler.compilerOpCode
           
           Emit.OpCodeWithByte();
           break;
       }
#ifdef TRACE
       LDA #(emitPushBitTrace % 256) STA ZP.TraceMessageL LDA #(emitPushBitTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit PUSHBYTE opcode with immediate value
   // Input: A = byte value
   // Output: PUSHBYTE opcode emitted with value
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitPushByteTrace = "Emit PUSHBYTE";
   PushByte()
   {
#ifdef TRACE
       PHA LDA #(emitPushByteTrace % 256) STA ZP.TraceMessageL LDA #(emitPushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       // Set up parameters for emission
       STA Compiler.compilerOperand1          // Store value as operand
       LDA #OpCode.PUSHBYTE
       STA Compiler.compilerOpCode
       
       Emit.OpCodeWithByte();
       
#ifdef TRACE
       LDA #(emitPushByteTrace % 256) STA ZP.TraceMessageL LDA #(emitPushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit PUSHVOID opcode with zero value and VOID type
   // Input: None (VOID always pushes 0 with VOID type)
   // Output: PUSHVOID opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode()
   const string emitPushVoidTrace = "Emit PUSHVOID";
   PushVoid()
   {
#ifdef TRACE
       LDA #(emitPushVoidTrace % 256) STA ZP.TraceMessageL LDA #(emitPushVoidTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       LDA #OpCode.PUSHVOID
       STA Compiler.compilerOpCode
       Emit.OpCode();
       
#ifdef TRACE
       LDA #(emitPushVoidTrace % 256) STA ZP.TraceMessageL LDA #(emitPushVoidTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit PUSHINT or PUSHWORD opcode with word value
   // Input: ZP.TOPT = type (determines opcode), compilerOperand1 = LSB, compilerOperand2 = MSB
   // Output: Appropriate opcode emitted with value
   // Modifies: compilerOpCode, buffer state via Emit.OpCodeWithWord()
   const string emitPushWordTrace = "Emit PUSHWORD";
   PushWord()
   {
#ifdef TRACE
       LDA #(emitPushWordTrace % 256) STA ZP.TraceMessageL LDA #(emitPushWordTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Select opcode based on type
           LDA ZP.TOPT
           CMP #BASICType.INT
           if (Z)
           {
               LDA #OpCode.PUSHINT
               STA Compiler.compilerOpCode
               Emit.OpCodeWithWord();
               break;
           }
           
           CMP #BASICType.WORD
           if (Z)
           {
               LDA #OpCode.PUSHWORD
               STA Compiler.compilerOpCode
               Emit.OpCodeWithWord();
               break;
           }
           
           // Invalid type for word push
           Error.TypeMismatch(); BIT ZP.EmulatorPCL
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitPushWordTrace % 256) STA ZP.TraceMessageL LDA #(emitPushWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit POPGLOBAL opcode to store to variable  
   // Input: No parameters (uses current token position for variable name offset)
   // Output: POPGLOBAL opcode emitted with token offset
   // Modifies: compilerOpCode, compilerOperand1, ZP.ACC (via CalculateTokenOffset), buffer state
   const string emitPopGlobalTrace = "Emit POPGLOBAL";
   PopGlobal()
   {
#ifdef TRACE
       LDA #(emitPopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Calculate offset to current token (variable name)
           CalculateTokenOffset();
           if (NC) { break; }
           
           // Check if offset fits in single byte
           LDA ZP.ACCH
           if (Z) // High byte is 0, use single-byte operand
           {
               LDA ZP.ACCL
               STA Compiler.compilerOperand1
               LDA #OpCode.POPGLOBAL
               STA Compiler.compilerOpCode
               Emit.OpCodeWithByte();
               break;
           }
           
           // Offset too large
           Error.BufferOverflow(); BIT ZP.EmulatorPCL
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitPopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   // Emit arithmetic operation opcode
   // Input: A = operation token (Token.PLUS, Token.MINUS, etc.)
   // Output: Corresponding arithmetic opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode(), A/X/Y registers
   const string emitArithmeticOpTrace = "Emit ADD";
   ArithmeticOp()
   {
#ifdef TRACE
       PHA LDA #(emitArithmeticOpTrace % 256) STA ZP.TraceMessageL LDA #(emitArithmeticOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       loop
       {
           switch (A)
           {
               case Token.PLUS:
               {
                   LDA #OpCode.ADD
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.MINUS:
               {
                   LDA #OpCode.SUB
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.MULTIPLY:
               {
                   LDA #OpCode.MUL
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.DIVIDE:
               {
                   LDA #OpCode.DIV
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.MOD:
               {
                   LDA #OpCode.MOD
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               default:
               {
                   Error.InvalidOperator(); BIT ZP.EmulatorPCL
                   break;
               }
           }
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitArithmeticOpTrace % 256) STA ZP.TraceMessageL LDA #(emitArithmeticOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit comparison operation opcode
   // Input: A = comparison token (Token.EQUALS, Token.LESSTHAN, etc.)
   // Output: Corresponding comparison opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode(), A/X/Y registers
   const string emitComparisonOpTrace = "Emit EQ";
   ComparisonOp()
   {
#ifdef TRACE
       PHA LDA #(emitComparisonOpTrace % 256) STA ZP.TraceMessageL LDA #(emitComparisonOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       loop
       {
           switch (A)
           {
               case Token.EQUALS:
               {
                   LDA #OpCode.EQ
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.NOTEQUAL:
               {
                   LDA #OpCode.NE
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.LT:
               {
                   LDA #OpCode.LT
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.GT:
               {
                   LDA #OpCode.GT
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.LE:
               {
                   LDA #OpCode.LE
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.GE:
               {
                   LDA #OpCode.GE
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               default:
               {
                   Error.InvalidOperator(); BIT ZP.EmulatorPCL
                   break;
               }
           }
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitComparisonOpTrace % 256) STA ZP.TraceMessageL LDA #(emitComparisonOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit logical operation opcode
   // Input: A = logical token (Token.AND, Token.OR, Token.NOT)
   // Output: Corresponding logical opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode(), A/X/Y registers
   const string emitLogicalOpTrace = "Emit LOGICAL_AND";
   LogicalOp()
   {
#ifdef TRACE
       PHA LDA #(emitLogicalOpTrace % 256) STA ZP.TraceMessageL LDA #(emitLogicalOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       loop
       {
           switch (A)
           {
               case Token.AND:
               {
                   LDA #OpCode.LOGICAL_AND
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.OR:
               {
                   LDA #OpCode.LOGICAL_OR
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.NOT:
               {
                   LDA #OpCode.LOGICAL_NOT
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               default:
               {
                   Error.InvalidOperator(); BIT ZP.EmulatorPCL
                   break;
               }
           }
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitLogicalOpTrace % 256) STA ZP.TraceMessageL LDA #(emitLogicalOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit bitwise operation opcode
   // Input: A = bitwise token 
   // Output: Corresponding bitwise opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode(), A/X/Y registers
   const string emitBitwiseOpTrace = "Emit BITWISE_AND";
   BitwiseOp()
   {
#ifdef TRACE
       PHA LDA #(emitBitwiseOpTrace % 256) STA ZP.TraceMessageL LDA #(emitBitwiseOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       loop
       {
           switch (A)
           {
               case Token.BITWISE_AND:
               {
                   LDA #OpCode.BITWISE_AND
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               case Token.BITWISE_OR:
               {
                   LDA #OpCode.BITWISE_OR
                   STA Compiler.compilerOpCode
                   Emit.OpCode();
                   break;
               }
               default:
               {
                   Error.InvalidOperator(); BIT ZP.EmulatorPCL
                   break;
               }
           }
           break;
       } // loop
       
#ifdef TRACE
       LDA #(emitBitwiseOpTrace % 256) STA ZP.TraceMessageL LDA #(emitBitwiseOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit unary minus (negation) opcode
   // Output: NEG opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode()
   const string emitUnaryMinusTrace = "Emit NEG";
   UnaryMinus()
   {
#ifdef TRACE
       LDA #(emitUnaryMinusTrace % 256) STA ZP.TraceMessageL LDA #(emitUnaryMinusTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       LDA #OpCode.NEG
       STA Compiler.compilerOpCode
       Emit.OpCode();
       
#ifdef TRACE
       LDA #(emitUnaryMinusTrace % 256) STA ZP.TraceMessageL LDA #(emitUnaryMinusTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit system call opcode
   // Input: A = system call ID
   // Output: SYSCALL opcode emitted with ID
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitSysCallTrace = "Emit SYSCALL";
   SysCall()
   {
#ifdef TRACE
       PHA LDA #(emitSysCallTrace % 256) STA ZP.TraceMessageL LDA #(emitSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       STA Compiler.compilerOperand1      // Store ID as operand
       LDA #OpCode.SYSCALL
       STA Compiler.compilerOpCode
       Emit.OpCodeWithByte();
       
#ifdef TRACE
       LDA #(emitSysCallTrace % 256) STA ZP.TraceMessageL LDA #(emitSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit ENTER opcode for function entry (stack frame setup)
   // Output: ENTER opcode with argument count emitted
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitEnterTrace = "Emit ENTER";
   Enter()
   {
#ifdef TRACE
       LDA #(emitEnterTrace % 256) STA ZP.TraceMessageL LDA #(emitEnterTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       LDA #OpCode.ENTER
       STA Compiler.compilerOpCode
       Emit.OpCode();
       
#ifdef TRACE
       LDA #(emitEnterTrace % 256) STA ZP.TraceMessageL LDA #(emitEnterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   // Emit RETURN opcode for function exit (no return value)
   // Input: A = total stack slots to clean up (arguments + locals)
   // Output: RETURN opcode with cleanup count emitted
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitReturnTrace = "Emit RETURN";
   Return()
   {
#ifdef TRACE
       PHA LDA #(emitReturnTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       STA Compiler.compilerOperand1          // Store cleanup count as operand
       LDA #OpCode.RETURN
       STA Compiler.compilerOpCode
       Emit.OpCodeWithByte();
       
#ifdef TRACE
       LDA #(emitReturnTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit RETURNVAL opcode for function exit with return value
   // Input: A = total stack slots to clean up (arguments + locals)
   // Output: RETURNVAL opcode with cleanup count emitted (expects return value on stack)
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitReturnValTrace = "Emit RETURNVAL";
   ReturnVal()
   {
#ifdef TRACE
       PHA LDA #(emitReturnValTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnValTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       STA Compiler.compilerOperand1          // Store cleanup count as operand
       LDA #OpCode.RETURNVAL
       STA Compiler.compilerOpCode
       Emit.OpCodeWithByte();
       
#ifdef TRACE
       LDA #(emitReturnValTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnValTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }  
   
   // Emit CALL opcode for unresolved function call
   // Input: Current token is IDENTIFIER (function name), tokenizer positioned at function name
   // Output: CALL opcode with absolute name address emitted, C set if successful
   // Modifies: compilerOpCode, compilerOperand1/2, buffer state
   const string emitCallTrace = "Emit CALL";
   Call()
   {
       PHA
       PHX
       PHY
       
#ifdef TRACE
       LDA #(emitCallTrace % 256) STA ZP.TraceMessageL LDA #(emitCallTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop // Single exit
       {
#ifdef TRACEJIT
           Tools.NL();
           LDA ZP.TokenLiteralPosH Debug.HOut();
           LDA ZP.TokenLiteralPosL Debug.HOut();
#endif            
           // Calculate absolute address of function name in token buffer
           // The tokenizer's TokenLiteralPos points to the start of the identifier string
           CLC
           LDA ZP.TokenBufferL
           ADC ZP.TokenLiteralPosL    // TokenLiteralPos points to identifier string
           STA Compiler.compilerOperand1       // Absolute address LSB
           LDA ZP.TokenBufferH
           ADC ZP.TokenLiteralPosH
           STA Compiler.compilerOperand2       // Absolute address MSB

#ifdef TRACEJIT
           LDA #'-' Debug.COut(); LDA #'>' Debug.COut();
           LDA Compiler.compilerOperand2 Debug.HOut();
           LDA Compiler.compilerOperand1 Debug.HOut();
#endif                        
                                  
           // Emit CALL with absolute address (not offset!)
           LDA # OpCode.CALL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithWord();
           break;
       }
       
#ifdef TRACE
       LDA #(emitCallTrace % 256) STA ZP.TraceMessageL LDA #(emitCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       PLY
       PLX
       PLA
   }
   
   // Emit DECSP opcode to discard top stack value
   // Input: None
   // Output: DECSP opcode emitted
   // Modifies: compilerOpCode, buffer state via EmitOpCode()
   const string emitDecSpTrace = "Emit DECSP";
   DecSp()
   {
   #ifdef TRACE
       LDA #(emitDecSpTrace % 256) STA ZP.TraceMessageL LDA #(emitDecSpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #OpCode.DECSP
       STA Compiler.compilerOpCode
       Emit.OpCode();
       
   #ifdef TRACE
       LDA #(emitDecSpTrace % 256) STA ZP.TraceMessageL LDA #(emitDecSpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }

   // Emit MILLIS SYSCALL - no operands needed
   const string emitMillisTrace = "Emit MILLIS";
   Millis()
   {
   #ifdef TRACE
       LDA #(emitMillisTrace % 256) STA ZP.TraceMessageL LDA #(emitMillisTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Millis
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitMillisTrace % 256) STA ZP.TraceMessageL LDA #(emitMillisTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   // Emit SECONDS SYSCALL - no operands needed
   const string emitSecondsTrace = "Emit SECONDS";
   Seconds()
   {
   #ifdef TRACE
       LDA #(emitSecondsTrace % 256) STA ZP.TraceMessageL LDA #(emitSecondsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Seconds
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitSecondsTrace % 256) STA ZP.TraceMessageL LDA #(emitSecondsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }

   // Emit ABS SYSCALL - operates on top of stack  
   const string emitAbsTrace = "Emit ABS";
   Abs()
   {
   #ifdef TRACE
       LDA #(emitAbsTrace % 256) STA ZP.TraceMessageL LDA #(emitAbsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Abs
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitAbsTrace % 256) STA ZP.TraceMessageL LDA #(emitAbsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   // Emit DELAY SYSCALL - operates on top of stack  
   const string emitDelayTrace = "Emit DELAY";
   Delay()
   {
   #ifdef TRACE
       LDA #(emitDelayTrace % 256) STA ZP.TraceMessageL LDA #(emitDelayTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Delay
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitDelayTrace % 256) STA ZP.TraceMessageL LDA #(emitDelayTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }

   // Emit RND SYSCALL - operates on top of stack  
   const string emitRndTrace = "Emit RND";
   Rnd()
   {
   #ifdef TRACE
       LDA #(emitRndTrace % 256) STA ZP.TraceMessageL LDA #(emitRndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Rnd
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitRndTrace % 256) STA ZP.TraceMessageL LDA #(emitRndTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }

   // Emit PEEK SYSCALL - operates on top of stack  
   const string emitPeekTrace = "Emit PEEK";
   Peek()
   {
   #ifdef TRACE
       LDA #(emitPeekTrace % 256) STA ZP.TraceMessageL LDA #(emitPeekTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Peek
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitPeekTrace % 256) STA ZP.TraceMessageL LDA #(emitPeekTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }

   // Emit POKE SYSCALL - operates on top of stack  
   const string emitPokeTrace = "Emit POKE";
   Poke()
   {
   #ifdef TRACE
       LDA #(emitPokeTrace % 256) STA ZP.TraceMessageL LDA #(emitPokeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       LDA #SysCallType.Poke
       Emit.SysCall();
       
   #ifdef TRACE
       LDA #(emitPokeTrace % 256) STA ZP.TraceMessageL LDA #(emitPokeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
    // Emit PRINTCHAR SYSCALL with specific character
    // Input: A = character to print
    // Output: PrintChar syscall emitted with character as argument
    const string emitPrintCharTrace = "Emit PRINTCHAR";
    PrintChar()
    {
    #ifdef TRACE
        PHA LDA #(emitPrintCharTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintCharTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
    #endif
        
        PHA  // Save character
        
        loop // Single exit block
        {
            // Emit PUSHBYTE with the character (A already contains character)
            Emit.PushByte();  // Uses A register
            Error.CheckError();
            if (NC) { break; }
            
            // Emit the SYSCALL
            LDA #SysCallType.PrintChar
            Emit.SysCall();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
        
        PLA  // Restore character (for caller)
        
    #ifdef TRACE
        LDA #(emitPrintCharTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintCharTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Emit PrintChar for newline character
    const string emitPrintNewLineTrace = "Emit PRINTNEWLINE";
    PrintNewLine()
    {
    #ifdef TRACE
        LDA #(emitPrintNewLineTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintNewLineTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            LDA #'\n'
            Emit.PrintChar();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
        
    #ifdef TRACE
        LDA #(emitPrintNewLineTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintNewLineTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Emit PrintChar for space character  
    const string emitPrintSpaceTrace = "Emit PRINTSPACE";
    PrintSpace()
    {
    #ifdef TRACE
        LDA #(emitPrintSpaceTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            LDA #' '
            Emit.PrintChar();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
        
    #ifdef TRACE
        LDA #(emitPrintSpaceTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Emit PRINTVALUE SYSCALL - prints value from stack
    const string emitPrintValueTrace = "Emit PRINTVALUE";
    PrintValue()
    {
    #ifdef TRACE
        LDA #(emitPrintValueTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintValueTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            LDA #SysCallType.PrintValue
            SysCall();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
        
    #ifdef TRACE
        LDA #(emitPrintValueTrace % 256) STA ZP.TraceMessageL LDA #(emitPrintValueTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Emit HALT opcode to terminate execution
    // Output: HALT opcode emitted
    // Modifies: compilerOpCode, buffer state via Emit.OpCode()
    const string emitHaltTrace = "Emit HALT";
    Halt()
    {
    #ifdef TRACE
        LDA #(emitHaltTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitHaltTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        LDA #OpCode.HALT
        STA Compiler.compilerOpCode
        Emit.OpCode();
        
    #ifdef TRACE
        LDA #(emitHaltTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitHaltTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

}
