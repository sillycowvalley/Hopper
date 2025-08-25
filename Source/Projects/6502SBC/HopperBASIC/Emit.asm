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
           Print.NewLine(); LDA #'>' Debug.COut();
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
           
#ifdef PEEPHOLE
           Optimizer.Peep(); // current opcode is in Compiler.compilerOpCode
#endif           
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
           Print.NewLine(); LDA #'>' Debug.COut();
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
           
#ifdef PEEPHOLE           
           LDA Compiler.compilerOpCode
           switch (A)
           {
               case OpCode.JUMPZB:
               case OpCode.JUMPB:
               case OpCode.JUMPNZB:
               case OpCode.RETURN:
               case OpCode.RETURNVAL:
               {
                   Optimizer.ClearPeeps();
               }
               default:
               {
                   Optimizer.Peep(); // current opcode is in Compiler.compilerOpCode
               }
           }
#endif
           
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
           Print.NewLine(); LDA #'>' Debug.COut();
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
           
#ifdef PEEPHOLE
           LDA Compiler.compilerOpCode
           switch (A)
           {
               case OpCode.JUMPZW:
               case OpCode.JUMPW:
               case OpCode.JUMPNZW:
               case OpCode.CALL:
               case OpCode.CALLF: 
               {
                   Optimizer.ClearPeeps();
               }
               default:
               {
                   Optimizer.Peep(); // current opcode is in Compiler.compilerOpCode
               }
           }
#endif
           
           SEC // Success
           break;
       }
#ifdef TRACE
       LDA #(emitOpCodeWithWordTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   // Emit opcode with three byte operands (byte + word)
   // Input: compilerOpCode = opcode value, 
   //        compilerOperand1 = first byte operand,
   //        compilerOperand2 = word LSB, 
   //        compilerOperand3 = word MSB
   // Output: OpCode and operands written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented by 4), ZP.XPC (incremented by 4)
   const string emitOpCodeWithThreeBytesTrace = "EmitOp3B";
   OpCodeWithThreeBytes()
   {
#ifdef TRACE
       LDA #(emitOpCodeWithThreeBytesTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithThreeBytesTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       loop
       {
#ifdef TRACEJIT       
           Print.NewLine(); LDA #'>' Debug.COut();
           LDA ZP.XPCH Debug.HOut(); LDA ZP.XPCL Debug.HOut();
           LDA #' ' Debug.COut(); LDA Compiler.compilerOpCode Debug.HOut(); LDA #' ' Debug.COut(); 
                                  LDA Compiler.compilerOperand1 Debug.HOut(); LDA #' ' Debug.COut();
                                  LDA Compiler.compilerOperand2 Debug.HOut(); LDA #' ' Debug.COut();
                                  LDA Compiler.compilerOperand3 Debug.HOut(); LDA #' ' Debug.COut();
#endif        
           // Check space for 4 bytes
           LDA #4
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
           
           // Write first operand (iterator offset)
           LDA Compiler.compilerOperand1
           STA [ZP.XPC]
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           // Write second operand (jump offset LSB)
           LDA Compiler.compilerOperand2
           STA [ZP.XPC]
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
           // Write third operand (jump offset MSB)  
           LDA Compiler.compilerOperand3
           STA [ZP.XPC]
           
           // Increment PC
           INC ZP.XPCL
           if (Z)
           {
               INC ZP.XPCH
           }
           
#ifdef PEEPHOLE
           LDA Compiler.compilerOpCode
           switch (A)
           {
               case OpCode.FORIT:
               case OpCode.FORITF:
               case OpCode.FORCHK:
               {
                   Optimizer.ClearPeeps();
               }
               default:
               {
                   Optimizer.Peep(); // current opcode is in Compiler.compilerOpCode
               }
           }
#endif
           SEC // Success
           break;
       }
#ifdef TRACE
       LDA #(emitOpCodeWithThreeBytesTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithThreeBytesTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
    // Emit the most efficient opcode for a constant value
    // Input: ZP.TOP/TOPT = constant value and type
    // Output: Appropriate constant push opcode emitted
    // Modifies: Compiler state, buffer
    const string emitOptimizedConstantTrace = "OptConst";
    OptimizedConstant()
    {
    #ifdef TRACE
        LDA #(emitOptimizedConstantTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitOptimizedConstantTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        loop
        {
            // Check for special common values first
            LDA ZP.TOPT
            AND #BASICType.TYPEMASK
            switch (A)
            {
                case BASICType.LONG:
                {
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
                    
                    break;
                }
                
                case BASICType.INT:
                case BASICType.WORD:
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    
                    // Full word value needed
                    //LDA ZP.TOPL
                    //STA Compiler.compilerOperand1
                    //LDA ZP.TOPH  
                    //STA Compiler.compilerOperand2
                    //Emit.PushWord(); // Handles INT vs WORD based on ZP.TOPT
                    break;
                }
                
                case BASICType.BYTE:
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    //LDA ZP.TOPL
                    //Emit.PushByte();
                    break;
                }
                
                case BASICType.CHAR:
                {
                    LDA ZP.TOP0
                    Emit.PushChar();
                    break;
                }
                
                case BASICType.BIT:
                {
                    LDA ZP.TOP0
                    Emit.PushBit();
                    break;
                }
                
                default:
                {
                    // For other types, don't use this method
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    SEC
                    break;
                }
            }
            break;
        } // single exit
        
    #ifdef TRACE
        LDA #(emitOptimizedConstantTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitOptimizedConstantTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
   
    // Emit CLS (clear screen) opcode
    // Input: None
    // Output: CLEARSCREEN opcode added to buffer
    // Modifies: compilerOpCode, buffer state via Emit.OpCode()
    const string emitClearScreenTrace = "Emit CLS";
    ClearScreen()
    {
    #ifdef TRACE
        LDA #(emitClearScreenTrace % 256) STA ZP.TraceMessageL LDA #(emitClearScreenTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        LDA # OpCode.CLEARSCREEN
        STA Compiler.compilerOpCode
        Emit.OpCode();
        
    #ifdef TRACE
        LDA #(emitClearScreenTrace % 256) STA ZP.TraceMessageL LDA #(emitClearScreenTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
   
  
   // In emit.asm

   // Emit PUSHGLOBAL opcode for identifier
   // Input: Current token is IDENTIFIER
   // Output: PUSHGLOBAL opcode with index emitted, C set if successful
   // Modifies: A, X, Y, ZP.TOP, ZP.IDX, compilerOperand1
   const string emitPushGlobalTrace = "Emit PUSHGLOBAL";
   PushGlobal()
   {
       PHA
       PHX
       PHY
       
#ifdef TRACE
       LDA #(emitPushGlobalTrace % 256) STA ZP.TraceMessageL 
       LDA #(emitPushGlobalTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
#endif
       
       loop // Single exit
       {
           // Get the identifier name from the tokenizer
           Tokenizer.GetTokenString(); // Result in ZP.TOP (name pointer)
           CheckError();
           if (NC) { break; }
           
           // Find the variable/constant by name
           STZ ZP.SymbolIteratorFilter  // Accept both variables and constants
           Variables.Find();  // Input: ZP.TOP = name
                              // Output: ZP.IDX = node address, ZP.IDYL = index
           if (NC)
           {
               // Variable not found
               Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
               break;
           }
           
           // Check if index is valid (< 256 variables)
           // Since IDYL is a byte, it's always valid
           
           // Store index as operand
           LDA ZP.IDYL
           STA Compiler.compilerOperand1  // Index
           
           // Emit PUSHGLOBAL with byte operand
           LDA #OpCode.PUSHGLOBAL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithByte();
           break;
       }
       
#ifdef TRACE
       LDA #(emitPushGlobalTrace % 256) STA ZP.TraceMessageL 
       LDA #(emitPushGlobalTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodExit();
#endif
       
       PLY
       PLX
       PLA
   }
   
   
   // Emit POPGLOBAL opcode to store to variable  
   // Input: Variable name in ZP.TOP (from assignment compilation)
   // Output: POPGLOBAL opcode emitted with index
   // Modifies: compilerOpCode, compilerOperand1, ZP.IDX, ZP.IDYL
   const string emitPopGlobalTrace = "Emit POPGLOBAL";
   PopGlobal()
   {
#ifdef TRACE
       LDA #(emitPopGlobalTrace % 256) STA ZP.TraceMessageL 
       LDA #(emitPopGlobalTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodEntry();
#endif
       
       loop
       {
           // TOP already contains variable name from assignment compilation
           
           // Find the variable by name
           LDA #SymbolType.VARIABLE
           STA ZP.SymbolIteratorFilter  // Only variables (not constants)
           Variables.Find();  // Input: ZP.TOP = name
                             // Output: ZP.IDX = node address, ZP.IDYL = index
           if (NC)
           {
               // Variable not found or is a constant
               Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
               break;
           }
           
           // Store index as operand
           LDA ZP.IDYL
           STA Compiler.compilerOperand1  // Index
           
           // Emit POPGLOBAL with byte operand
           LDA # OpCode.POPGLOBAL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithByte();
           break;
       }
       
#ifdef TRACE
       LDA #(emitPopGlobalTrace % 256) STA ZP.TraceMessageL 
       LDA #(emitPopGlobalTrace / 256) STA ZP.TraceMessageH 
       Trace.MethodExit();
#endif
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
       switch (A)
       {
           case 0:
           {
               LDA #OpCode.PUSH0
               STA Compiler.compilerOpCode
               Emit.OpCode();
           }
           case 1:
           {
               LDA #OpCode.PUSH1
               STA Compiler.compilerOpCode
               Emit.OpCode();
           }
           default:
           {
               LDA #OpCode.PUSHBYTE
               STA Compiler.compilerOpCode
               Emit.OpCodeWithByte();
           }
       }
       
#ifdef TRACE
       LDA #(emitPushByteTrace % 256) STA ZP.TraceMessageL LDA #(emitPushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit PUSHCHAR opcode with immediate value
   // Input: A = char value
   // Output: PUSHCHAR opcode emitted with value
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   const string emitPushCharTrace = "Emit PUSHCHAR";
   PushChar()
   {
#ifdef TRACE
       PHA LDA #(emitPushCharTrace % 256) STA ZP.TraceMessageL LDA #(emitPushCharTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       // Set up parameters for emission
       STA Compiler.compilerOperand1          // Store value as operand
       LDA #OpCode.PUSHCHAR
       STA Compiler.compilerOpCode
       
       Emit.OpCodeWithByte();
       
#ifdef TRACE
       LDA #(emitPushCharTrace % 256) STA ZP.TraceMessageL LDA #(emitPushCharTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
   
   // Emit PUSHEMPTYVAR opcode with zero value and VAR|INT type
   // Input: None
   // Output: PUSHEMPTYVAR opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode()
   const string emitPushEmptyVarTrace = "Emit PUSHVAR";
   PushEmptyVar()
   {
#ifdef TRACE
       LDA #(emitPushEmptyVarTrace % 256) STA ZP.TraceMessageL LDA #(emitPushEmptyVarTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       LDA #OpCode.PUSHEMPTYVAR
       STA Compiler.compilerOpCode
       Emit.OpCode();
       
#ifdef TRACE
       LDA #(emitPushEmptyVarTrace % 256) STA ZP.TraceMessageL LDA #(emitPushEmptyVarTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit PUSHINT, PUSHWORD or PUSHLONG opcode with word value
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
           CMP # BASICType.INT
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
           CMP #BASICType.LONG
           if (Z)
           {
               LDA #OpCode.PUSHLONG
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
       PHA
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
       PLA
#ifdef TRACE
       PHA LDA #(emitArithmeticOpTrace % 256) STA ZP.TraceMessageL LDA #(emitArithmeticOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
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
   
   
   
    // Emit DECSP opcode to discard N stack values
    // Input: A = number of stack positions to discard
    // Output: DECSP opcode emitted with count
    // Modifies: compilerOpCode, compilerOperand1, buffer state via OpCodeWithByte()
    const string emitDecSpTrace = "Emit DECSP";
    DecSp()
    {
    #ifdef TRACE
        PHA LDA #(emitDecSpTrace % 256) STA ZP.TraceMessageL LDA #(emitDecSpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
    #endif
        
        STA Compiler.compilerOperand1   // Store count as operand
        LDA #OpCode.DECSP
        STA Compiler.compilerOpCode
        Emit.OpCodeWithByte();          // Changed from Emit.OpCode()
        
    #ifdef TRACE
        LDA #(emitDecSpTrace % 256) STA ZP.TraceMessageL LDA #(emitDecSpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Emit system call opcode - THIS IS THE ONLY SYSCALL METHOD WE NEED
    // Input: A = system call ID (from SysCallType enum with embedded metadata)
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
            // Emit PUSHCHAR with the character (A already contains character)
            Emit.PushChar();  // Uses A register
            CheckError();
            if (NC) { break; }
            
            // Emit the SYSCALL
            LDA #SysCallType.PrintChar
            Emit.SysCall();
            CheckError();
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
            CheckError();
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
            CheckError();
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
            CheckError();
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
           LDA ZP.TokenLiteralPosL
           STA Compiler.compilerOperand1  // LSB
           LDA ZP.TokenLiteralPosH
           STA Compiler.compilerOperand2  // MSB
                                  
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
            // Get string content pointer
            Tokenizer.GetTokenString(); // Result in ZP.TOP
            CheckError();
            if (NC) { break; }

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
   
   // Emit PUSHLOCAL opcode to load argument/local
    // Input: A = signed BP offset (negative for args, positive for locals)
    // Output: PUSHLOCAL opcode emitted with offset
    // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
    const string emitPushLocalTrace = "Emit PUSHLOCAL";
    PushLocal()
    {
    #ifdef TRACE
        PHA LDA #(emitPushLocalTrace % 256) STA ZP.TraceMessageL LDA #(emitPushLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
    #endif
        
        STA Compiler.compilerOperand1      // Store offset as operand
        LDA #OpCode.PUSHLOCAL
        STA Compiler.compilerOpCode
        Emit.OpCodeWithByte();
        
    #ifdef TRACE
        LDA #(emitPushLocalTrace % 256) STA ZP.TraceMessageL LDA #(emitPushLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Emit POPLOCAL opcode to store to argument/local
    // Input: A = signed BP offset (negative for args, positive for locals)
    // Output: POPLOCAL opcode emitted with offset
    // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
    const string emitPopLocalTrace = "Emit POPLOCAL";
    PopLocal()
    {
    #ifdef TRACE
        PHA LDA #(emitPopLocalTrace % 256) STA ZP.TraceMessageL LDA #(emitPopLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
    #endif
        
        STA Compiler.compilerOperand1      // Store offset as operand
        LDA #OpCode.POPLOCAL
        STA Compiler.compilerOpCode
        Emit.OpCodeWithByte();
        
    #ifdef TRACE
        LDA #(emitPopLocalTrace % 256) STA ZP.TraceMessageL LDA #(emitPopLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    
   // Emit FORCHK opcode for initial FOR loop check
   // Input: A = iterator BP offset (signed)
   //        X = forward jump offset LSB
   //        Y = forward jump offset MSB
   // Output: FORCHK opcode emitted with iterator offset and jump offset
   // Modifies: compilerOpCode, compilerOperand1-3, buffer state via OpCodeWithThreeBytes()
   const string emitForCheckTrace = "Emit FORCHK";
   ForCheck()
   {
#ifdef TRACE
       PHA LDA #(emitForCheckTrace % 256) STA ZP.TraceMessageL LDA #(emitForCheckTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       // Set up parameters for emission
       STA Compiler.compilerOperand1      // Store iterator offset as first operand
       STX Compiler.compilerOperand2      // Store jump offset LSB
       STY Compiler.compilerOperand3      // Store jump offset MSB
       LDA #OpCode.FORCHK
       STA Compiler.compilerOpCode
       
       Emit.OpCodeWithThreeBytes();
       
#ifdef TRACE
       LDA #(emitForCheckTrace % 256) STA ZP.TraceMessageL LDA #(emitForCheckTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Emit FORIT opcode for FOR loop iteration (increment and check)
   // Input: A = iterator BP offset (signed)
   //        X = backward jump offset LSB
   //        Y = backward jump offset MSB
   // Output: FORIT opcode emitted with iterator offset and jump offset
   // Modifies: compilerOpCode, compilerOperand1-3, buffer state via OpCodeWithThreeBytes()
   const string emitForIterateTrace = "Emit FORIT";
   ForIterate()
   {
#ifdef TRACE
       PHA LDA #(emitForIterateTrace % 256) STA ZP.TraceMessageL LDA #(emitForIterateTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       // Set up parameters for emission
       STA Compiler.compilerOperand1      // Store iterator offset as first operand
       STX Compiler.compilerOperand2      // Store jump offset LSB
       STY Compiler.compilerOperand3      // Store jump offset MSB
       LDA #OpCode.FORIT
       STA Compiler.compilerOpCode
       
       Emit.OpCodeWithThreeBytes();
       
#ifdef TRACE
       LDA #(emitForIterateTrace % 256) STA ZP.TraceMessageL LDA #(emitForIterateTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
    // Emit FORITF opcode for fast FOR loop iteration (increment by 1, unsigned compare)
    // Input: A = iterator BP offset (signed)
    //        X = backward jump offset LSB
    //        Y = backward jump offset MSB
    // Output: FORITF opcode emitted with iterator offset and jump offset
    // Modifies: compilerOpCode, compilerOperand1-3, buffer state via OpCodeWithThreeBytes()
    const string emitForIterateFastTrace = "Emit FORITF";
    ForIterateFast()
    {
    #ifdef TRACE
        PHA LDA #(emitForIterateFastTrace % 256) STA ZP.TraceMessageL LDA #(emitForIterateFastTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
    #endif
        
        // Set up parameters for emission
        STA Compiler.compilerOperand1      // Store iterator offset as first operand
        STX Compiler.compilerOperand2      // Store jump offset LSB
        STY Compiler.compilerOperand3      // Store jump offset MSB
        LDA #OpCode.FORITF
        STA Compiler.compilerOpCode
        
        Emit.OpCodeWithThreeBytes();
        
    #ifdef TRACE
        LDA #(emitForIterateFastTrace % 256) STA ZP.TraceMessageL LDA #(emitForIterateFastTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Emit GETITEM opcode for string/array indexing
    // Input: None (operates on stack values)
    // Output: GETITEM opcode emitted
    // Modifies: compilerOpCode, buffer state via Emit.OpCode()
    const string emitIndexTrace = "Emit GETITEM";
    GetItem()
    {
    #ifdef TRACE
        LDA #(emitIndexTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitIndexTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        LDA #OpCode.GETITEM
        STA Compiler.compilerOpCode
        Emit.OpCode();
        
    #ifdef TRACE
        LDA #(emitIndexTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitIndexTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    // Emit SETITEM opcode for array element assignment
    // Input: None (operates on stack values)
    // Output: SETITEM opcode emitted
    // Modifies: compilerOpCode, buffer state via Emit.OpCode()
    const string emitSetItemTrace = "Emit SETITEM";
    SetItem()
    {
    #ifdef TRACE
        LDA #(emitSetItemTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitSetItemTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        LDA #OpCode.SETITEM
        STA Compiler.compilerOpCode
        Emit.OpCode();
        
    #ifdef TRACE
        LDA #(emitSetItemTrace % 256) STA ZP.TraceMessageL 
        LDA #(emitSetItemTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
}
