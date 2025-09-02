unit Emit
{
   // looks at the current compilation PC (ZP.XPC) and comparse to the runtime error PC (ZP.PC)
   RuntimeErrorCheck()
   {
       loop
       {
           if (BBS7, ZP.FLAGS) // compiling to find error location?
           {
//Debug.NL(); LDA ZP.XPCH HOut(); LDA ZP.XPCL HOut(); Space(); LDA ZP.PCH HOut(); LDA ZP.PCL HOut(); Space(); LDA ZP.TokenizerPosH HOut(); LDA ZP.TokenizerPosL HOut(); 
//Space(); LDA Compiler.compilerOpCode HOut(); TAX OpCodes.ToString(); Space(); Print.String();

                // have we reached the runtime error location?
                LDA ZP.XPCH
                CMP ZP.PCH
                if (Z)
                {
                    LDA ZP.XPCL
                    CMP ZP.PCL
                }
                if (C) //C set if XPC >= PC
                {
                    // set error so Functions.Compile will emit it
                    LDA ZP.RuntimeError
                    STA ZP.LastError
                    
                    CheckError();
                    break;
                }
            }
            SEC
            break;
       } // single exit
   }
   
   // Emit a single-byte opcode (no operands)
   // Input:  A = opcode value
   // Output: OpCode written to buffer, C or NC depending on success or failure
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented), ZP.XPC (incremented)
   OpCode()
   {
       PHA
       loop
       {
           STA Compiler.compilerOpCode
            
           // Check space for 1 byte
           LDA #1
           CheckBufferSpace();
           if (NC) 
           { 
               CheckError();
               break; 
           } // Buffer overflow
           
           RuntimeErrorCheck(); if (NC) { break; }
                 
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
       PLA
   }
   
   // Emit opcode with one byte operand
   // Input: A = opcode value, compilerOperand1 = operand byte
   // Output: OpCode and operand written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented by 2), ZP.XPC (incremented by 2)
   OpCodeWithByte()
   {
       loop
       {
           STA Compiler.compilerOpCode
            
           // Check space for 2 bytes
           LDA #2
           CheckBufferSpace();
           if (NC) 
           { 
               break; 
           } // Buffer overflow
           
           RuntimeErrorCheck(); if (NC) { break; }
           
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
               //case OpCode.JUMPZB:
               //case OpCode.JUMPB:
               //case OpCode.JUMPNZB:
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
   }
   
   // Emit opcode with two byte operands (word value)
   // Input: A = opcode value, compilerOperand1 = LSB, compilerOperand2 = MSB
   // Output: OpCode and operands written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented by 3), ZP.XPC (incremented by 3)
   OpCodeWithWord()
   {
       loop
       {
           STA Compiler.compilerOpCode
           
           // Check space for 3 bytes
           LDA #3
           CheckBufferSpace();
           if (NC) 
           { 
               break; 
           } // Buffer overflow

           RuntimeErrorCheck(); if (NC) { break; }

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
               //case OpCode.JUMPNZW:
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
   }
   
   
   // Emit opcode with three byte operands (byte + word)
   // Input: A = opcode value, 
   //        compilerOperand1 = first byte operand,
   //        compilerOperand2 = word LSB, 
   //        compilerOperand3 = word MSB
   // Output: OpCode and operands written to buffer
   // Modifies: ZP.OpCodeBufferContentSizeL/H (incremented by 4), ZP.XPC (incremented by 4)
   OpCodeWithThreeBytes()
   {
       loop
       {
           STA Compiler.compilerOpCode
           // Check space for 4 bytes
           LDA #4
           CheckBufferSpace();
           if (NC) 
           { 
               break; 
           } // Buffer overflow

           RuntimeErrorCheck(); if (NC) { break; }           

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
   }
   
    // Emit the most efficient opcode for a constant value
    // Input: ZP.TOP/TOPT = constant value and type
    // Output: Appropriate constant push opcode emitted
    // Modifies: Compiler state, buffer
    OptimizedConstant()
    {
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
    }
   
   // In emit.asm

   // Emit PUSHGLOBAL opcode for identifier
   // Input: IIDY is current variable index
   // Output: PUSHGLOBAL opcode with index emitted, C set if successful
   // Modifies: A, X, Y, ZP.TOP, ZP.IDX, compilerOperand1
   PushGlobal()
   {
       PHX
       PHY
       
       loop // Single exit
       {
           // Store index as operand
           LDA ZP.IDYL
           STA Compiler.compilerOperand1  // Index
           
           // Emit PUSHGLOBAL with byte operand
           LDA #OpCode.PUSHGLOBAL
           STA Compiler.compilerOpCode
           Emit.OpCodeWithByte();
           break;
       }
       
       PLY
       PLX
   }
   
   
   // Emit POPGLOBAL opcode to store to variable  
   // Input: Variable name in ZP.TOP (from assignment compilation)
   // Output: POPGLOBAL opcode emitted with index
   // Modifies: compilerOpCode, compilerOperand1, ZP.IDX, ZP.IDYL
   PopGlobal()
   {       
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
               Error.UndefinedIdentifierTOP(); BIT ZP.EmulatorPCL
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
   }
     
   
     
   
   // Emit PUSHBIT opcode with immediate value
   // Input: A = bit value (0 or 1)
   // Output: PUSHBIT opcode emitted with value
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   PushBit()
   {
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
           Emit.OpCodeWithByte();
           break;
       }
   }
   
      
   // Emit PUSHBYTE opcode with immediate value
   // Input: A = byte value
   // Output: PUSHBYTE opcode emitted with value
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   PushByte()
   {       
       // Set up parameters for emission
       STA Compiler.compilerOperand1          // Store value as operand
       switch (A)
       {
           case 0:
           {
               LDA #OpCode.PUSH0
               Emit.OpCode();
           }
           case 1:
           {
               LDA #OpCode.PUSH1
               Emit.OpCode();
           }
           default:
           {
               LDA #OpCode.PUSHBYTE
               Emit.OpCodeWithByte();
           }
       }
   }
   
   // Emit PUSHCHAR opcode with immediate value
   // Input: A = char value
   // Output: PUSHCHAR opcode emitted with value
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   PushChar()
   {       
       // Set up parameters for emission
       STA Compiler.compilerOperand1          // Store value as operand
       LDA #OpCode.PUSHCHAR
       Emit.OpCodeWithByte();
   }
   
   // Emit PUSHVOID opcode with zero value and VOID type
   // Input: None (VOID always pushes 0 with VOID type)
   // Output: PUSHVOID opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode()
   PushVoid()
   {       
       LDA #OpCode.PUSHVOID
       Emit.OpCode();
   }
   
   // Emit PUSHEMPTYVAR opcode with zero value and VAR|INT type
   // Input: None
   // Output: PUSHEMPTYVAR opcode emitted
   // Modifies: compilerOpCode, buffer state via Emit.OpCode()
   PushEmptyVar()
   {       
       LDA #OpCode.PUSHEMPTYVAR
       Emit.OpCode();
   }
   
   // Emit PUSHINT, PUSHWORD or PUSHLONG opcode with word value
   // Input: ZP.TOPT = type (determines opcode), compilerOperand1 = LSB, compilerOperand2 = MSB
   // Output: Appropriate opcode emitted with value
   // Modifies: compilerOpCode, buffer state via Emit.OpCodeWithWord()
   PushWord()
   {       
       loop
       {
           // Select opcode based on type
           LDA ZP.TOPT
           CMP # BASICType.INT
           if (Z)
           {
               LDA #OpCode.PUSHINT
               Emit.OpCodeWithWord();
               break;
           }
           
           CMP #BASICType.WORD
           if (Z)
           {
               LDA #OpCode.PUSHWORD
               Emit.OpCodeWithWord();
               break;
           }
           CMP #BASICType.LONG
           if (Z)
           {
               LDA Compiler.compilerOperand1
               ORA Compiler.compilerOperand2
               if (Z)
               {
                   LDA #OpCode.PUSHLONG0
                   Emit.OpCode();
               }
               else
               {
                   LDA #OpCode.PUSHLONG
                   Emit.OpCodeWithWord();
               }
               break;
           }
           
           // Invalid type for word push
           Error.TypeMismatch(); BIT ZP.EmulatorPCL
           break;
       } // loop
   }
   
   
   // Emit RETURN opcode for function exit (no return value)
   // Input: A = total stack slots to clean up (arguments + locals)
   // Output: RETURN opcode with cleanup count emitted
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   Return()
   {       
       STA Compiler.compilerOperand1          // Store cleanup count as operand
       LDA #OpCode.RETURN
       Emit.OpCodeWithByte();
   }
   
   // Emit RETURNVAL opcode for function exit with return value
   // Input: A = total stack slots to clean up (arguments + locals)
   // Output: RETURNVAL opcode with cleanup count emitted (expects return value on stack)
   // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
   ReturnVal()
   {       
       STA Compiler.compilerOperand1          // Store cleanup count as operand
       LDA #OpCode.RETURNVAL
       Emit.OpCodeWithByte();
   }  
   
   
   
    // Emit DECSP opcode to discard N stack values
    // Input: A = number of stack positions to discard
    // Output: DECSP opcode emitted with count
    // Modifies: compilerOpCode, compilerOperand1, buffer state via OpCodeWithByte()
    DecSp()
    {        
        STA Compiler.compilerOperand1   // Store count as operand
        LDA #OpCode.DECSP
        Emit.OpCodeWithByte();          // Changed from Emit.OpCode()
    }

    // Emit system call opcode - THIS IS THE ONLY SYSCALL METHOD WE NEED
    // Input: A = system call ID (from SysCallType enum with embedded metadata)
    // Output: SYSCALL opcode emitted with ID
    // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
    SysCall()
    {        
        STA Compiler.compilerOperand1      // Store ID as operand
        LDA #OpCode.SYSCALL
        Emit.OpCodeWithByte();
    }
    
    
    VT100Escape() // single code in A
    {
        loop
        {
            TAX
            LDA #0x1B        // ESC
            Emit.PrintChar();
            if (NC) { break; }
            
            LDA #'['
            Emit.PrintChar();
            if (NC) { break; }
            
            TXA
            Emit.PrintChar();
            break;
        }
    }
    
    // Emit PRINTCHAR SYSCALL with specific character
    // Input: A = character to print
    // Output: PrintChar syscall emitted with character as argument
    PrintChar()
    {        
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
    }
    
    // Emit PrintChar for newline character
    PrintNewLine()
    {        
        LDA #'\n'
        Emit.PrintChar();
    }
    
    // Emit PrintChar for space character  
    PrintSpace()
    {        
        LDA #' '
        Emit.PrintChar();
    }
    
    // Emit PRINTVALUE SYSCALL - prints value from stack
    PrintValue()
    {        
        loop // Single exit block
        {
            LDA #SysCallType.PrintValue
            SysCall();
            CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
    }
    
    // Emit HALT opcode to terminate execution
    // Output: HALT opcode emitted
    // Modifies: compilerOpCode, buffer state via Emit.OpCode()
    Halt()
    {        
        LDA #OpCode.HALT
        Emit.OpCode();
    }
    
    
    // Emit CALL opcode for unresolved function call
   // Input: Current token is IDENTIFIER (function name), tokenizer positioned at function name
   // Output: CALL opcode with absolute name address emitted, C set if successful
   // Modifies: compilerOpCode, compilerOperand1/2, buffer state
   Call()
   {
       PHX
       PHY
       
       loop // Single exit
       {
           LDA ZP.TokenLiteralPosL
           STA Compiler.compilerOperand1  // LSB
           LDA ZP.TokenLiteralPosH
           STA Compiler.compilerOperand2  // MSB
                                  
           // Emit CALL with absolute address (not offset!)
           LDA # OpCode.CALL
           Emit.OpCodeWithWord();
           break;
       }
       
       PLY
       PLX
   }
   
   
   // Emit PUSHCSTRING opcode with word operand
   // Input: compilerOperand1 = string pointer LSB, compilerOperand2 = string pointer MSB  
   // Output: PUSHCSTRING opcode emitted with operands, C set if successful
   // Modifies: A, ZP.OpCodeBufferContentSizeL/H, buffer state
   PushCString()
   {
       loop
       {
            // Get string content pointer
            Tokenizer.GetTokenString(); // Result in ZP.TOP
            CheckError();
            if (NC) { break; }

            // Set up opcode
            LDA #OpCode.PUSHCSTRING
            Emit.OpCodeWithWord();
            break;
       } // loop
   }
   
   // Emit PUSHLOCAL opcode to load argument/local
    // Input: A = signed BP offset (negative for args, positive for locals)
    // Output: PUSHLOCAL opcode emitted with offset
    // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
    PushLocal()
    {        
        STA Compiler.compilerOperand1      // Store offset as operand
        LDA #OpCode.PUSHLOCAL
        Emit.OpCodeWithByte();
    }

    // Emit POPLOCAL opcode to store to argument/local
    // Input: A = signed BP offset (negative for args, positive for locals)
    // Output: POPLOCAL opcode emitted with offset
    // Modifies: compilerOpCode, compilerOperand1, buffer state via Emit.OpCodeWithByte()
    PopLocal()
    {        
        STA Compiler.compilerOperand1      // Store offset as operand
        LDA #OpCode.POPLOCAL
        Emit.OpCodeWithByte();
    }
    
    
   // Emit FORCHK opcode for initial FOR loop check
   // Input: A = iterator BP offset (signed)
   //        X = forward jump offset LSB
   //        Y = forward jump offset MSB
   // Output: FORCHK opcode emitted with iterator offset and jump offset
   // Modifies: compilerOpCode, compilerOperand1-3, buffer state via OpCodeWithThreeBytes()
   ForCheck()
   {       
       // Set up parameters for emission
       STA Compiler.compilerOperand1      // Store iterator offset as first operand
       STX Compiler.compilerOperand2      // Store jump offset LSB
       STY Compiler.compilerOperand3      // Store jump offset MSB
       LDA #OpCode.FORCHK
       Emit.OpCodeWithThreeBytes();
   }
   
   // Emit FORIT opcode for FOR loop iteration (increment and check)
   // Input: A = iterator BP offset (signed)
   //        X = backward jump offset LSB
   //        Y = backward jump offset MSB
   // Output: FORIT opcode emitted with iterator offset and jump offset
   // Modifies: compilerOpCode, compilerOperand1-3, buffer state via OpCodeWithThreeBytes()
   ForIterate()
   {       
       // Set up parameters for emission
       STA Compiler.compilerOperand1      // Store iterator offset as first operand
       STX Compiler.compilerOperand2      // Store jump offset LSB
       STY Compiler.compilerOperand3      // Store jump offset MSB
       LDA #OpCode.FORIT
       Emit.OpCodeWithThreeBytes();
   }
   
    // Emit FORITF opcode for fast FOR loop iteration (increment by 1, unsigned compare)
    // Input: A = iterator BP offset (signed)
    //        X = backward jump offset LSB
    //        Y = backward jump offset MSB
    // Output: FORITF opcode emitted with iterator offset and jump offset
    // Modifies: compilerOpCode, compilerOperand1-3, buffer state via OpCodeWithThreeBytes()
    ForIterateFast()
    {        
        // Set up parameters for emission
        STA Compiler.compilerOperand1      // Store iterator offset as first operand
        STX Compiler.compilerOperand2      // Store jump offset LSB
        STY Compiler.compilerOperand3      // Store jump offset MSB
        LDA #OpCode.FORITF
        Emit.OpCodeWithThreeBytes();
    }
    
    
}
