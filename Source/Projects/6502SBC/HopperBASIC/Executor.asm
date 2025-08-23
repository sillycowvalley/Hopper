unit Executor // Executor.asm
{
   uses "Instructions"
   uses "ComparisonInstructions"
   uses "BASICSysCalls"
   
   friend Functions;
   
   // Memory layout for executor state - BasicExecutorWorkspace (32 bytes)
   const uint executorOperandL      = Address.BasicExecutorWorkspace +  9;  // current operand low
   const uint executorOperandH      = Address.BasicExecutorWorkspace + 10;  // current operand high
   const uint executorTokenAddrL    = Address.BasicExecutorWorkspace + 11;  // token fetch addr low
   const uint executorTokenAddrH    = Address.BasicExecutorWorkspace + 12;  // token fetch addr high
   const uint executorOperandBP     = Address.BasicExecutorWorkspace + 13;  // BP offset operand (FORCHK and FORIT)
   
   // Reset Hopper VM to clean state before REPL execution
    Reset()
    {
       // Reset stacks (already proven to work in ExecuteOpCodes)
       STZ ZP.SP    // Reset value/type stack pointer to 0
       STZ ZP.BP    // Reset base pointer to 0
       STZ ZP.CSP   // Reset call stack pointer to 0
       
       // Clear any error conditions
       Error.ClearError();
       States.SetSuccess();
    }
   
   
    // Load all global variables and constants from symbol table to VM stack
    // Called after Reset() to initialize stack with globals
    // Output: All globals copied to stack at their index positions
    // Modifies: A, X, Y, ZP.IDX, ZP.TOP, ZP.TOPT, ZP.ACCT
    const string strLoadGlobals = "LoadGlobals // Variables -> Stack";
    LoadGlobals()
    {
#ifdef TRACE
       LDA #(strLoadGlobals % 256) STA ZP.TraceMessageL LDA #(strLoadGlobals / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif        
        // Iterate through all symbols (variables and constants)
        Variables.IterateAll(); // Output: ZP.IDX = first symbol, C set if found
        if (C)
        {
            loop
            {
                if (NC) { SEC break; }  // No more symbols
                
                // Get symbol's value and type
                Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = dataType (VAR|ARRAY masked away)
                // Get symbol's full type (might have VAR|ARRAY bit)
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType (packed)
                // Store type in type stack (preserving VAR bit for variables)
                LDA ZP.ACCT
                AND # BASICType.MASK // keep VAR when creating the global slots
                Stacks.PushTop();  // LoadGlobals: type is in A
                
                // Move to next symbol
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
        }
#ifdef TRACE
       LDA #(strLoadGlobals % 256) STA ZP.TraceMessageL LDA #(strLoadGlobals / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif        
    }
    
    // Save all global variables from VM stack back to symbol table
    // Called before exit to persist changes to globals (skips constants)
    // Output: All variable values updated from stack
    // Modifies: A, X, Y, ZP.IDX, ZP.TOP, ZP.TOPT, ZP.ACCT
    const string strSaveGlobals = "SaveGlobals // Stack -> Variables";
    const string strSaveGlobalsGV = "SaveGlobals // Stack -> Variables (ignore GVI)";
    SaveGlobals()
    {
#ifdef TRACE
       IsTracing();
       if (C)
       {
           LDA ZP.GVIL
           ORA ZP.GVIH
           if (Z)
           {
               LDA #(strSaveGlobals % 256) STA ZP.TraceMessageL LDA #(strSaveGlobals / 256) STA ZP.TraceMessageH Trace.MethodEntry();
           }
           else
           {
               LDA #(strSaveGlobalsGV % 256) STA ZP.TraceMessageL LDA #(strSaveGlobalsGV / 256) STA ZP.TraceMessageH Trace.MethodEntry();
           }
       }
#endif        
        // Iterate through all symbols (variables and constants)
        Variables.IterateAll(); // Output: ZP.IDX = first symbol, C set if found
        if (C)
        {
            LDY #0  // Index counter - tracks position on stack
            loop
            {
                if (NC) { SEC break; }  // No more symbols

                // Check if this is a constant (skip if so)
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = type
                
                LDA ZP.ACCT
                AND # SymbolType.CONSTANT
                if (NZ)  // It's a constant - skip it
                {
                    INY  // Still increment index to stay in sync with stack position
                    Variables.IterateNext();
                    continue;  // Skip to next iteration
                }
                if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
                {
                    // It's an ARRAY - skip it
                    INY  // Still increment index to stay in sync with stack position
                    Variables.IterateNext();
                    continue;  // Skip to next iteration
                }
                
                // Get new type from type stack at this index
                LDA Address.TypeStackLSB, Y
                STA ZP.TOPT
                
                // Get value from value stacks at this index
                LDA Address.ValueStackLSB, Y
                STA ZP.TOPL
                LDA Address.ValueStackMSB, Y
                STA ZP.TOPH
                
                if (BBS4, ZP.FLAGS) // Bit 4 - initialization mode: skip current variable GVI?
                {
                    LDA ZP.IDXL
                    CMP ZP.GVIL
                    if (Z)
                    {
                        LDA ZP.IDXH
                        CMP ZP.GVIH
                        if (Z)
                        {
                            // Skip SetValue() and move to next symbol
                            INY  // Increment index for next stack position
                            Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next    
                            continue;
                        }
                    }
                }
                
                // Never assign back to an ARRAY (read-only on the stack)
                LDA Address.TypeStackLSB, Y  // Get type from stack
                AND # BASICType.ARRAY
                if (NZ) 
                {
                    // Skip SetValue() and move to next symbol
                    INY  // Increment index for next stack position
                    Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next 
                    continue;
                }
                
                // reload  ZP.ACCT = symbolType|dataType (packed)
                Variables.GetType();

                // Set the new value in symbol table:
                // If the old value was a string, not the same as this one, it will Free it
                // If the new value is a string, it will create a copy for itself.
                // If the new value is the same string as the old value, it will do nothing.
                Variables.SetValue(); // preserves Y, Input: ZP.IDX = node, ZP.TOP = value, ZP.TOPT = type
                
                // Move to next symbol
                INY  // Increment index for next stack position
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            } // loop
        }
#ifdef TRACE
       LDA #(strSaveGlobals % 256) STA ZP.TraceMessageL LDA #(strSaveGlobals / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif        
        SEC
    }
    
   
   // Main entry point - Execute compiled opcodes
   // Input: ZP.OpCodeBufferContentSizeL/H contains opcode buffer length
   // Output: SystemState set (Success, Failure, or Exiting)
   // Uses: BasicOpCodeBuffer at Address.BasicOpCodeBuffer (0x0C00)
   const string strExecuteOpCodes = "ExecOpCodes // Execute compiled opcodes";
   ExecuteOpCodes()
   {
       PHA
       PHX
       PHY

#ifdef TRACE
       LDA #(strExecuteOpCodes % 256) STA ZP.TraceMessageL LDA #(strExecuteOpCodes / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
       // CRITICAL: Reset stacks before execution to ensure clean state
       // Previous expression errors or interruptions could leave stacks inconsistent
       Executor.Reset();
       
       
       loop // Single exit block
       {
           Executor.LoadGlobals();
           CheckError();
           if (NC) 
           { 
              States.SetFailure();
              break;
           }
       
           // Initialize executor state
           InitExecutor();
           if (NC) { break; } // empty opcode stream -> State.Failure set
           
           // Main execution loop (assume there is at least one opcode)
           loop
           {
                // Fetch and execute next opcode
                LDA [ZP.PC]
                // Advance PC
                INC ZP.PCL
                if (Z)
                {
                    INC ZP.PCH
                }
                
                DispatchOpCode(); // expect State.Success to continue
                
#ifdef TRACEEXE        
                PHP IsTracing(); // only affects C flag
                if (C)
                {
                    Debug.CompactStack();
                }
                PLP
#endif
                if (BBS0, ZP.SerialBreakFlag) 
                {
                    RMB0 ZP.SerialBreakFlag   // Clear the BREAK flag
                    Error.Break();            // "BREAK" error message
                    States.SetFailure();
                    CLC
                    break;
                }
                
                // Shortcut to Mushrooms:
                LDA ZP.LastError
                if (Z)
                {
                    LDA ZP.SystemState
                    CMP # State.Success        
                    if (Z)
                    {
                        continue;
                    }
                }
                // Regular status and error processing if the above was not true:
                
                // Check if any instruction set an error
                CheckError();
                if (NC) 
                { 
                   States.SetFailure();
                }
                States.CanContinue();  // Returns C for Success|Exiting|Return, NC for Failure
                if (NC)
                {
                    // State.Failure - runtime error
                    CLC
                    break;  // Exit with NC
                }
                States.IsSuccess();
                if (NC)
                {
                    // State.Exiting or State.Return - successful completion
                    SEC
                    break;  // Exit with C (from CanContinue)
                }
                // State.Success - get another opcode ..
           } // loop
           break;
       } // Single exit block
       
       Executor.SaveGlobals();
       
       
#ifdef TRACE
       LDA #(strExecuteOpCodes % 256) STA ZP.TraceMessageL LDA #(strExecuteOpCodes / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif

       PLY
       PLX
       PLA
   }
   
   // Initialize executor state from opcode buffer
   // Input: ZP.OpCodeBufferContentSizeL/H contains opcode buffer length
   // Output: SystemState set (Success or Failure)
   const string initExecutorTrace = "InitExec // Initialize executor state";
   InitExecutor()
   {
#ifdef TRACE
       LDA #(initExecutorTrace % 256) STA ZP.TraceMessageL LDA #(initExecutorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
        // Set PC to start of opcode buffer
        LDA ZP.OpCodeBufferL
        STA ZP.PCL
        LDA ZP.OpCodeBufferH
        STA ZP.PCH
        
        // Validate buffer length is not zero
        LDA ZP.OpCodeBufferContentLengthL
        ORA ZP.OpCodeBufferContentLengthH
        if (Z)
        {
            Error.InternalError(); BIT ZP.EmulatorPCL
            States.SetFailure();
            CLC
        }
        else
        {
            States.SetSuccess();
            SEC
        }
       
#ifdef TRACE
       LDA #(initExecutorTrace % 256) STA ZP.TraceMessageL LDA #(initExecutorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   
   
   // Fetch single byte operand from buffer
   // Input: ZP.PC points to operand position
   // Output: A contains operand byte, ZP.PC advanced, SystemState set
   FetchOperandByte()
   {
       loop
       {
           // clear state
           LDA #State.Success
           STA ZP.SystemState
        
           // Fetch operand
           LDA [ZP.PC]
           
           // Advance PC
           INC ZP.PCL
           if (Z)
           {
               INC ZP.PCH
           }
#ifdef TRACEEXE
           PHP IsTracing(); // only affects C flag
           if (C)
           {
                PHA PHX
                Space();  HOut();
                PLX PLA
           }
           PLP
#endif           
           break;
       } // loop exit
   }
   
   // Fetch word operand from buffer (little-endian)
   // Input: ZP.PC points to operand position
   // Output: executorOperandL/H contains word, ZP.PC advanced by 2, SystemState set
   FetchOperandWord()
   {
       loop
       {
           // clear state
           LDA #State.Success
           STA ZP.SystemState
           
           LDA [ZP.PC]
           STA executorOperandL // Save operand
           
           // Advance PC
           INC ZP.PCL
           if (Z)
           {
               INC ZP.PCH
           }
           
           LDA [ZP.PC]
           STA executorOperandH // Save operand
           
           // Advance PC
           INC ZP.PCL
           if (Z)
           {
               INC ZP.PCH
           }
#ifdef TRACEEXE
           PHP IsTracing(); // only affects C flag
           if (C)
           {
                PHA PHX
                Space(); LDA executorOperandH HOut(); LDA executorOperandL HOut();
                PLX PLA
           }
           PLP
#endif           
           break;
       }
   }
   
   // Dispatch opcode to appropriate handler
   // Input: Y contains opcode value
   // Output: SystemState set based on execution result
   DispatchOpCode()
   {
       TAY
#ifdef TRACEEXE
       PHP IsTracing(); // only affects C flag
       if (C)
       {
            PHA PHX
            NL(); DecPC(); LDA ZP.PCH HOut(); LDA ZP.PCL HOut(); IncPC(); Space(); TYA TAX HOut(); Space(); OpCodes.ToString(); Print.String();Space(); 
            PLX PLA
       }
       PLP
#endif
       // Use switch statement for opcode dispatch
       // Register Y contains the opcode value
       switch (Y) // bug in jump table
       {
           // === NO OPERAND OPCODES (0x00-0x3F) ===
           
           // Arithmetic operations
           case OpCode.ADD:
           {
               Instructions.Addition();
           }
           case OpCode.SUB:
           {
               Instructions.Subtraction();
           }
           case OpCode.MUL:
           {
               Instructions.Multiply();
           }
           case OpCode.DIV:
           {
               Instructions.Divide();
           }
           case OpCode.MOD:
           {
               Instructions.Modulo();
           }
           case OpCode.NEG:
           {
               Instructions.UnaryMinus();
           }
           
           // Bitwise operations
           case OpCode.BITWISE_AND:
           {
               Instructions.BitwiseAnd();
           }
           case OpCode.BITWISE_OR:
           {
               Instructions.BitwiseOr();
           }
           
           // Logical operations (BIT operands only)
           case OpCode.LOGICAL_AND:
           {
               Instructions.LogicalAnd();
           }
           case OpCode.LOGICAL_OR:
           {
               Instructions.LogicalOr();
           }
           case OpCode.LOGICAL_NOT:
           {
               Instructions.LogicalNot();
           }
           
           // Comparison operations (all return BIT)
           case OpCode.EQ:
           {
               ComparisonInstructions.Equal();
           }
           case OpCode.NE:
           {
               ComparisonInstructions.NotEqual();
           }
           case OpCode.LT:
           {
               ComparisonInstructions.LessThan();
           }
           case OpCode.GT:
           {
               ComparisonInstructions.GreaterThan();
           }
           case OpCode.LE:
           {
               ComparisonInstructions.LessEqual();
           }
           case OpCode.GE:
           {
               ComparisonInstructions.GreaterEqual();
           }
           
            case OpCode.TOLONG:
            {
                executeToLong();
            }
            case OpCode.PUSHLONG:
            {
                executePushLong();
            }
           
           case OpCode.GETITEM:
           {
               executeGetItem();
           }
           case OpCode.GETITEMGG:
           {
#ifdef PEEPHOLE
               executeGetItemGG();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.GETITEMGL:
           {
#ifdef PEEPHOLE
               executeGetItemGL();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.GETITEMLG:
           {
#ifdef PEEPHOLE
               executeGetItemLG();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.GETITEMLL:
           {
#ifdef PEEPHOLE
               executeGetItemLL();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.SETITEM:
           {
               
               executeSetItem();
           }
           case OpCode.SETITEMGG:
           {
#ifdef PEEPHOLE
               executeSetItemGG();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.SETITEMGL:
           {
#ifdef PEEPHOLE
               executeSetItemGL();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.SETITEMLG:
           {
#ifdef PEEPHOLE
               executeSetItemLG();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.SETITEMLL:
           {
#ifdef PEEPHOLE
               executeSetItemLL();
#else
               executeNotImplemented();
#endif
           }
           
           case OpCode.HALT:
           {
               executeHalt();
           }
           case OpCode.CLEARSCREEN:
           {
               executeClearScreen();
           }
           case OpCode.PUSHEMPTYVAR:
           {
               executePushEmptyVar();
           }
           
           // Function operations
           case OpCode.ENTER:
           {
               executeEnter();
           }
           case OpCode.RETURN:
           {
               executeReturn();
           }
           case OpCode.RETURNVAL:
           {
               executeReturnVal();
           }
           
           // Stack manipulation
           case OpCode.DECSP:
           {
               executeDecSP();
           }
           case OpCode.DUP:
           {
               executeDup();
           }
           case OpCode.NOP:
           {
               executeNop();
           }
           
           case OpCode.PUSH0:
           {
               executePush0();
           }
           case OpCode.PUSH1:
           {
               executePush1();
           }
           case OpCode.PUSHVOID:
           {
               executePushVoid();
           }
           
           // === ONE BYTE OPERAND OPCODES (0x40-0x7F) ===
           
           // Literal pushes (8-bit)
           case OpCode.PUSHBIT:
           {
               executePushBit();
           }
           case OpCode.PUSHBYTE:
           {
               executePushByte();
           }
           case OpCode.PUSHCHAR:
           {
               executePushChar();
           }
           case OpCode.PUSHCSTRING:
           {
               executePushCString();
           }
           
           // Variable operations
           case OpCode.PUSHGLOBAL:
           {
               executePushGlobal();
           }
           case OpCode.PUSHLOCAL:
           {
               executePushLocal();
           }
           case OpCode.POPGLOBAL:
           {
               executePopGlobal();
           }
           case OpCode.POPLOCAL:
           {
               executePopLocal();
           }
           case OpCode.INCLOCAL:
           {
#ifdef PEEPHOLE
               executeIncLocal();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.INCGLOBAL:
           {
#ifdef PEEPHOLE
               executeIncGlobal();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.ADDLOCALS:
           {
#ifdef PEEPHOLE
               executeAddLocals();
#else
               executeNotImplemented();
#endif
           }
           case OpCode.ADDGLOBALS:
           {
#ifdef PEEPHOLE
               executeAddGlobals();
#else
               executeNotImplemented();
#endif
           }
           // Control flow (short jumps)
           case OpCode.JUMPB:
           {
               executeJumpB();
           }
           case OpCode.JUMPZB:
           {
               executeJumpZB();
           }
           case OpCode.JUMPNZB:
           {
               executeJumpNZB();
           }
           
           // Function and system calls
           case OpCode.CALL:
           {
               executeCall();
           }
           case OpCode.CALLF:
           {
               executeCallF();
           }
           case OpCode.SYSCALL:
           {
               ExecuteSysCall();
           }
           
           // === TWO BYTE OPERAND OPCODES (0x80-0xBF) ===
           
           // Literal pushes (16-bit)
           case OpCode.PUSHINT:
           {
               executePushInt();
           }
           case OpCode.PUSHWORD:
           {
               executePushWord();
           }
           
           // Control flow (long jumps)
           case OpCode.JUMPW:
           {
               executeJumpW();
           }
           case OpCode.JUMPZW:
           {
               executeJumpZW();
           }
           case OpCode.JUMPNZW:
           {
               executeJumpNZW();
           }
           
           // === THREE BYTE OPERAND OPCODES (0xC0-0xFF) ===
           case OpCode.FORCHK:
           {
               executeFORCHK();
           }
           case OpCode.FORIT:
           {
               executeFORIT();
           }
           case OpCode.FORITF:
           {
               executeFORITF();
           }
           
           default:
           {
               executeNotImplemented();
           }
       }
   }
   
   // Execute unknown opcode
   const string executeNotImplementedTrace = "NotImpl // Unknown opcode";
   executeNotImplemented()
   {
#ifdef TRACE
       LDA #(executeNotImplementedTrace % 256) STA ZP.TraceMessageL LDA #(executeNotImplementedTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
#ifdef DEBUG
       LDA #'?' Debug.COut();
       TYA Debug.HOut();
       Debug.DumpBuffers();
#endif                
       // Unknown opcode
       TODO(); BIT ZP.EmulatorPCL
       States.SetFailure();
       
#ifdef TRACE
       LDA #(executeNotImplementedTrace % 256) STA ZP.TraceMessageL LDA #(executeNotImplementedTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === CONTROL FLOW AND STACK MANIPULATION HANDLERS ===
   
   // Common return processing for both RETURN and RETURNVAL opcodes
   // Input: ZP.TOP = return value (16-bit), ZP.TOPT = return type
   //        Operand byte = argument count (number of arguments and locals to clean up)
   // Output: Return value stored in return slot, stack frame restored, execution returned to caller
   // Stack: Removes all arguments and locals, preserves return slot with return value
   // State: Sets States.SetReturn() if returned to entry call, States.SetSuccess() otherwise
   // Modifies: ZP.SP (stack cleanup), ZP.BP (restored), ZP.PC (restored), ZP.CSP (decremented)
   //           Return slot contents (value and type), executorOperand1 (operand fetch)
   // Preserves: All other zero page variables
   // Error: Sets error state via CheckError() if operand fetch fails
   commonReturn()
   {
       loop
       {
           // Fetch cleanup count operand (number of locals)

           FetchOperandByte();
           STA executorOperandL
                                 
           // Calculate return slot position: BP - (arg_count + 1)
           SEC
           LDA ZP.BP
           SBC executorOperandL         // BP - arg_count
           STA ZP.SP
           TAX                     
           DEX     
           // X = return slot index // SP-1
           PHX
           
           Stacks.PopBP();
           Stacks.PopXID();
           Stacks.PopPC();
           
           LDA ZP.CSP
           if (Z) // CallStack pointer == 0?
           {
               PLX // discard
               States.SetReturn(); // popped back down to entry call
           }
           else
           {
               // Store return value in return slot
               PLX
               LDA ZP.TOPL
               STA Address.ValueStackLSB, X
               LDA ZP.TOPH
               STA Address.ValueStackMSB, X
               LDA ZP.TOPT
               STA Address.TypeStackLSB, X
               States.SetSuccess();
           }
           break;
       } // exit loop
   }
   
   // Execute HALT opcode - return from REPL
   const string executeHaltTrace = "HALT // Return from REPL";
   executeHalt()
   {
#ifdef TRACE
       LDA #(executeHaltTrace % 256) STA ZP.TraceMessageL LDA #(executeHaltTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       States.SetExiting();
#ifdef TRACE
       LDA #(executeHaltTrace % 256) STA ZP.TraceMessageL LDA #(executeHaltTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute RETURN opcode - return from function
   const string executeReturnTrace = "RETURN // Return from function";
   executeReturn()
   {
#ifdef TRACE
       LDA #(executeReturnTrace % 256) STA ZP.TraceMessageL LDA #(executeReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       LDA # BASICType.VOID
       STA ZP.TOPT
       STZ ZP.TOPL
       STZ ZP.TOPH
       commonReturn();
               
#ifdef TRACE
       LDA #(executeReturnTrace % 256) STA ZP.TraceMessageL LDA #(executeReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute RETURNVAL opcode - return from function with value
   // Input: cleanup_count operand, return value on top of stack
   // Output: Return value moved to return slot, stack frame restored
   // Modifies: Stack pointers, return slot contents
   const string executeReturnValTrace = "RETURNVAL // Return with value";
   executeReturnVal()
   {
#ifdef TRACE
       LDA #(executeReturnValTrace % 256) STA ZP.TraceMessageL LDA #(executeReturnValTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       Stacks.PopTop(); // ReturnVal
       commonReturn();        
       
#ifdef TRACE
       LDA #(executeReturnValTrace % 256) STA ZP.TraceMessageL LDA #(executeReturnValTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute ENTER opcode - enter function frame
   const string executeEnterTrace = "ENTER // Enter function frame";
   executeEnter()
   {
#ifdef TRACE
       LDA #(executeEnterTrace % 256) STA ZP.TraceMessageL LDA #(executeEnterTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       Stacks.PushBP();
       LDA ZP.SP
       STA ZP.BP
       States.SetSuccess();
       
//DumpStack();
       
#ifdef TRACE
       LDA #(executeEnterTrace % 256) STA ZP.TraceMessageL LDA #(executeEnterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
    // Execute DECSP opcode - decrement stack pointer by N positions
    const string executeDecSPTrace = "DECSP // Decrement stack pointer by N";
    executeDecSP()
    {
    #ifdef TRACE
        LDA #(executeDecSPTrace % 256) STA ZP.TraceMessageL LDA #(executeDecSPTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
// Fetch operand (number of positions to decrement)
#ifdef TRACEEXE
        FetchOperandByte(); // -> A
#else            
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif
        SEC          // Set carry for proper subtraction (1 byte, 2 cycles)
        EOR #0xFF    // One's complement of operand (2 bytes, 2 cycles) 
        ADC ZP.SP    // Two's complement subtraction: SP + (~operand + 1) = SP - operand (2 bytes, 3 cycles)
        STA ZP.SP    // Store result (2 bytes, 3 cycles)   
        
        LDA #State.Success
        STA ZP.SystemState
        
    #ifdef TRACE
        LDA #(executeDecSPTrace % 256) STA ZP.TraceMessageL LDA #(executeDecSPTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
   
   // Execute DUP opcode - duplicate top stack value
   const string executeDupTrace = "DUP // Duplicate top stack value";
   executeDup()
   {
#ifdef TRACE
       LDA #(executeDupTrace % 256) STA ZP.TraceMessageL LDA #(executeDupTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Duplicate top stack value
       Stacks.PopTop();  // Dup: Get top value in ZP.TOP and ZP.TOPT
       LDA ZP.TOPT
       Stacks.PushTop(); // Dup: push value and type to stack -> always Success
       LDA ZP.TOPT  
       Stacks.PushTop(); // Dup: push value and type to stack -> always Success
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executeDupTrace % 256) STA ZP.TraceMessageL LDA #(executeDupTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute NOP opcode - no operation
   const string executeNopTrace = "NOP // No operation";
   executeNop()
   {
#ifdef TRACE
       LDA #(executeNopTrace % 256) STA ZP.TraceMessageL LDA #(executeNopTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // No operation - do nothing
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executeNopTrace % 256) STA ZP.TraceMessageL LDA #(executeNopTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === LITERAL PUSH HANDLERS (ONE BYTE OPERAND) ===
   
   // Execute PUSH0 opcode - push INT 0
   const string executePush0Trace = "PUSH0 // Push INT 0";
   executePush0()
   {
   #ifdef TRACE
       LDA #(executePush0Trace % 256) STA ZP.TraceMessageL LDA #(executePush0Trace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       // Store INT 0 in ZP.TOP
       STZ ZP.TOPL
       STZ ZP.TOPH
       LDA #BASICType.INT
       STA ZP.TOPT
       Stacks.PushTop(); // Push0: push value and type to stack -> always Success
       
       States.SetSuccess();
       
   #ifdef TRACE
       LDA #(executePush0Trace % 256) STA ZP.TraceMessageL LDA #(executePush0Trace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   // Execute PUSH1 opcode - push INT 1
   const string executePush1Trace = "PUSH1 // Push INT 1";
   executePush1()
   {
   #ifdef TRACE
       LDA #(executePush1Trace % 256) STA ZP.TraceMessageL LDA #(executePush1Trace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       // Store INT 1 in ZP.TOP
       LDA #1
       STA ZP.TOPL
       STZ ZP.TOPH
       LDA #BASICType.INT
       STA ZP.TOPT
       Stacks.PushTop(); // Push1: push value and type to stack -> always Success
       
       States.SetSuccess();
       
   #ifdef TRACE
       LDA #(executePush1Trace % 256) STA ZP.TraceMessageL LDA #(executePush1Trace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   
   
   // Execute PUSHVOID opcode - push VOID 0
   const string executePushEmptyVarTrace = "PUSHEMPTYVAR // Push VAR|INT 0";
   executePushEmptyVar()
   {
   #ifdef TRACE
       LDA #(executePushEmptyVarTrace % 256) STA ZP.TraceMessageL LDA #(executePushEmptyVarTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       // Store VOID 0 in ZP.TOP
       STZ ZP.TOP0
       STZ ZP.TOP1
/*
#ifdef BASICLONG
       STZ ZP.TOP2
       STZ ZP.TOP3
       LDA # (BASICType.VAR|BASICType.LONG)
#else       
*/
       LDA # (BASICType.VAR|BASICType.INT)
//#endif
       STA ZP.TOPT
       Stacks.PushTop(); // PushEmptyVar: push value and type to stack -> always Success
       
       States.SetSuccess();
       
   #ifdef TRACE
       LDA #(executePushEmptyVarTrace % 256) STA ZP.TraceMessageL LDA #(executePushEmptyVarTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   // Execute PUSHVOID opcode - push VOID 0
   const string executePushVoidTrace = "PUSHVOID // Push VOID 0";
   executePushVoid()
   {
   #ifdef TRACE
       LDA #(executePushVoidTrace % 256) STA ZP.TraceMessageL LDA #(executePushVoidTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
   #endif
       
       // Store VOID 0 in ZP.TOP
       STZ ZP.TOPL
       STZ ZP.TOPH
       LDA #BASICType.VOID
       STA ZP.TOPT
       Stacks.PushTop(); // PushVoid: push value and type to stack -> always Success
       
       States.SetSuccess();
       
   #ifdef TRACE
       LDA #(executePushVoidTrace % 256) STA ZP.TraceMessageL LDA #(executePushVoidTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   // Execute PUSHBIT opcode - push BIT immediate
   const string executePushBitTrace = "PUSHBIT // Push BIT immediate";
   executePushBit()
   {
#ifdef TRACE
       LDA #(executePushBitTrace % 256) STA ZP.TraceMessageL LDA #(executePushBitTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
// Fetch operand (number of positions to decrement)
#ifdef TRACEEXE
        FetchOperandByte(); // -> A
#else            
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif
        // Store as BIT value (0 or 1)
        LDX ZP.SP
        INC ZP.SP
        
        STA Address.ValueStackLSB, X
        STZ Address.ValueStackMSB, X
        LDA # BASICType.BIT
        STA Address.TypeStackLSB, X
        
        LDA #State.Success
        STA ZP.SystemState
       
#ifdef TRACE
       LDA #(executePushBitTrace % 256) STA ZP.TraceMessageL LDA #(executePushBitTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute PUSHBYTE opcode - push BYTE immediate
   const string executePushByteTrace = "PUSHBYTE // Push BYTE immediate";
   executePushByte()
   {
#ifdef TRACE
       LDA #(executePushByteTrace % 256) STA ZP.TraceMessageL LDA #(executePushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
// Fetch operand (number of positions to decrement)
#ifdef TRACEEXE
        FetchOperandByte(); // -> A
#else            
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif
           
       // Store as BYTE value
       LDX ZP.SP
       INC ZP.SP
        
       STA Address.ValueStackLSB, X
       STZ Address.ValueStackMSB, X
       LDA # BASICType.BYTE
       STA Address.TypeStackLSB, X
        
       LDA #State.Success
       STA ZP.SystemState
       
#ifdef TRACE
       LDA #(executePushByteTrace % 256) STA ZP.TraceMessageL LDA #(executePushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute PUSHCHAR opcode - push CHAR immediate
   const string executePushCharTrace = "PUSHCHAR // Push CHAR immediate";
   executePushChar()
   {
#ifdef TRACE
       LDA #(executePushCharTrace % 256) STA ZP.TraceMessageL LDA #(executePushCharTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
#ifdef TRACEEXE
        FetchOperandByte(); // -> A
#else            
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif

        // Store as BYTE value
       LDX ZP.SP
       INC ZP.SP
        
       STA Address.ValueStackLSB, X
       STZ Address.ValueStackMSB, X
       LDA # BASICType.CHAR
       STA Address.TypeStackLSB, X
        
       LDA #State.Success
       STA ZP.SystemState
       
#ifdef TRACE
       LDA #(executePushCharTrace % 256) STA ZP.TraceMessageL LDA #(executePushCharTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute PUSHCSTRING opcode - push string pointer to stack
   // Input: PC points to operand bytes (string pointer LSB, MSB)
   // Output: String pointer pushed to stack as STRING type, PC advanced by 2
   // Modifies: A, X, Y, ZP.PC, ZP.TOP, ZP.TOPT, stack
   const string executePushCStringTrace = "PUSHCSTRING // Push CONSTSTRING pointer";
   executePushCString()
   {
#ifdef TRACE
       LDA #(executePushCStringTrace % 256) STA ZP.TraceMessageL LDA #(executePushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Fetch string pointer (little-endian)
           FetchOperandWord(); // Result in executorOperandL/H

           // Store pointer in ZP.TOP as STRING value
           CLC
           LDA executorOperandL
           ADC ZP.XIDL
           STA ZP.TOPL
           LDA executorOperandH
           ADC ZP.XIDH
           STA ZP.TOPH

           LDA # BASICType.STRING
           STA ZP.TOPT
           
           // Push to stack with STRING type
           Stacks.PushTop(); // PushCString: push value and type to stack -> always Success
           CheckError();
           if (NC) 
           { 
               States.SetFailure();
               break; 
           }
           
           States.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executePushCStringTrace % 256) STA ZP.TraceMessageL LDA #(executePushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === FUNCTION AND SYSTEM CALL HANDLERS (ONE BYTE OPERAND) ===
   
   // Execute CALL opcode - call function by name (unresolved)
   const string executeCallTrace = "CALL // Call function by name (unresolved)";
   executeCall()
   {
#ifdef TRACE
       LDA #(executeCallTrace % 256) STA ZP.TraceMessageL LDA #(executeCallTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           FetchOperandWord();
           
           CLC
           LDA executorOperandL
           ADC ZP.XIDL
           STA ZP.TOPL
           LDA executorOperandH
           ADC ZP.XIDH
           STA ZP.TOPH

           // 1. resolve Function <index> to function call <address>
           Functions.Find(); // Input: ZP.TOP = name
           if (NC)
           {
               Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
               States.SetFailure();
               break;
           }
           

           // ZP.IDX = function node address
           Functions.IsCompiled();
           if (NC)
           {
               // JIT
               Functions.Compile();
               States.CanContinue();
               if (NC)
               {
                   // handle error
                   break;
               }
           }
           
           // 2. replace own opcode CALL -> CALLF, <index> by <address>
           // 3, PC -= 3 (so CALLF happens instead)
           //
           // <address> MSB
           // PC--
           LDA ZP.PCL
           if (Z)
           {
               DEC ZP.PCH
           }
           DEC ZP.PCL
           LDA ZP.IDXH 
           STA [ZP.PC]
           
           // <address> LSB
           // PC--
           LDA ZP.PCL
           if (Z)
           {
               DEC ZP.PCH
           }
           DEC ZP.PCL
           LDA ZP.IDXL
           STA [ZP.PC]
           
           // CALLF
           // PC--
           LDA ZP.PCL
           if (Z)
           {
               DEC ZP.PCH
           }
           DEC ZP.PCL
           LDA # OpCode.CALLF
           STA [ZP.PC]
           
           States.SetSuccess();
           break;
       } // loop
       
#ifdef TRACE
       LDA #(executeCallTrace % 256) STA ZP.TraceMessageL LDA #(executeCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute CALLF opcode - call function fast (resolved)
   const string executeCallFTrace = "CALLF // Call function fast (resolved)";
   executeCallF()
   {
#ifdef TRACE
       LDA #(executeCallFTrace % 256) STA ZP.TraceMessageL LDA #(executeCallFTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Function call by <address>
       // PUSH PC
       // PUSH BP
       // PC = <address>
       Functions.JumpToOpCodes();
       // State management handled by Functions.JumpToOpCodes()
       
#ifdef TRACE
       LDA #(executeCallFTrace % 256) STA ZP.TraceMessageL LDA #(executeCallFTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === LITERAL PUSH HANDLERS (TWO BYTE OPERANDS) ===
   
   // Execute PUSHINT opcode - push INT immediate
   const string executePushIntTrace = "PUSHINT // Push INT immediate";
   executePushInt()
   {
#ifdef TRACE
       LDA #(executePushIntTrace % 256) STA ZP.TraceMessageL LDA #(executePushIntTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Fetch 16-bit operand
       FetchOperandWord();
       // Store in ZP.TOP as INT value
       LDA executorOperandL
       STA ZP.TOPL
       LDA executorOperandH
       STA ZP.TOPH
       LDA # BASICType.INT
       Stacks.PushTop(); // PushInt: push value and type to stack -> always Success
       
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executePushIntTrace % 256) STA ZP.TraceMessageL LDA #(executePushIntTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Execute PUSHWORD opcode - push WORD immediate
   const string executePushWordTrace = "PUSHWORD // Push WORD immediate";
   executePushWord()
   {
#ifdef TRACE
       LDA #(executePushWordTrace % 256) STA ZP.TraceMessageL LDA #(executePushWordTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Fetch 16-bit operand
       FetchOperandWord();
       
       // Store in ZP.TOP as WORD value
       LDA executorOperandL
       STA ZP.TOPL
       LDA executorOperandH
       STA ZP.TOPH
       LDA # BASICType.WORD
       Stacks.PushTop(); // PushWord: push value and type to stack -> always Success
       
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executePushWordTrace % 256) STA ZP.TraceMessageL LDA #(executePushWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === HELPER METHODS FOR JUMP OPERATIONS ===
   
   // Sign extend byte offset to word offset
   // Input: A = signed byte offset (-128 to +127)
   // Output: ZP.NEXT = sign-extended word offset
   // Modifies: A, ZP.NEXTL, ZP.NEXTH
   signExtendByteToWord()
   {
       STA ZP.NEXTL
       
       // Sign extension - based on IntMath.IntToLong pattern
       ASL           // sign bit into carry
       LDA #0x00
       ADC #0xFF     // C set:   A = 0xFF + C = 0x00
                     // C clear: A = 0xFF + C = 0xFF  
       EOR #0xFF     // Flip all bits to match carry
       
       STA ZP.NEXTH
   }
   
   // Pop expression result and validate it's a BIT type
   // Input: Stack top must contain a BIT value
   // Output: A = BIT value (0 or 1), NC if not BIT type
   // Modifies: A, X, ZP.SP, flags
   popAndValidateBitType()
   {
        DEC ZP.SP
        LDX ZP.SP
        LDA Address.TypeStackLSB, X
        CMP #BASICType.BIT
        if (Z)
        {
            LDA Address.ValueStackLSB, X
            SEC  // Success - is BIT type
        }
        else
        {
            Error.TypeMismatch(); BIT ZP.EmulatorPCL
            CLC  // Failure - not BIT type
        }
   }
   
   // Apply signed offset to emulator PC
   // Input: ZP.NEXT = signed 16-bit offset
   // Output: ZP.EmulatorPC updated with new address
   // Modifies: ZP.EmulatorPCL, ZP.EmulatorPCH, flags
   applySignedOffsetToPC()
   {
       // Two's complement addition works for both positive and negative offsets
       CLC
       LDA ZP.PCL
       ADC ZP.NEXTL
       STA ZP.PCL
       LDA ZP.PCH
       ADC ZP.NEXTH
       STA ZP.PCH
   }
   
   // === CONTROL FLOW HANDLERS (ONE BYTE OPERANDS) ===
   
   const string executeJumpBTrace = "JUMPB // Unconditional jump with signed byte offset";
   executeJumpB()
   {
#ifdef TRACE
       LDA #(executeJumpBTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpBTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Fetch signed byte operand
       FetchOperandByte();
       // Sign extend byte to word
       signExtendByteToWord();
       
       // Apply offset to PC
       applySignedOffsetToPC();
       
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executeJumpBTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpBTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   const string executeJumpZBTrace = "JUMPZB // Jump if zero with signed byte offset";
   executeJumpZB()
   {
#ifdef TRACE
       LDA #(executeJumpZBTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpZBTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Pop and validate BIT type from stack
           popAndValidateBitType(); // ZP.TOPL -> A
           if (NC) { break; }  // Type error already set
           
           // Check if value is zero (FALSE)
           if (Z)  // Value is zero/FALSE - take the jump
           {
               // Fetch signed byte operand
               FetchOperandByte();
               
               // Sign extend byte to word
               signExtendByteToWord();
               
               // Apply offset to PC
               applySignedOffsetToPC();
           }
           else  // Value is non-zero/TRUE - skip the jump
           {
               // Still need to fetch and skip the operand
               FetchOperandByte();
               // Don't apply offset - just continue
           }
           
           States.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executeJumpZBTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpZBTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   const string executeJumpNZBTrace = "JUMPNZB // Jump if non-zero with signed byte offset";
   executeJumpNZB()
   {
#ifdef TRACE
       LDA #(executeJumpNZBTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpNZBTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Pop and validate BIT type from stack
           popAndValidateBitType(); // ZP.TOPL -> A
           if (NC) { break; }  // Type error already set
           
           // Check if value is non-zero (TRUE)
           if (NZ)  // Value is non-zero/TRUE - take the jump
           {
               // Fetch signed byte operand
               FetchOperandByte();
               
               // Sign extend byte to word
               signExtendByteToWord();
               
               // Apply offset to PC
               applySignedOffsetToPC();
           }
           else  // Value is zero/FALSE - skip the jump
           {
               // Still need to fetch and skip the operand
               FetchOperandByte();
               // Don't apply offset - just continue to next instruction
           }
           
           States.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executeJumpNZBTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpNZBTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === CONTROL FLOW HANDLERS (TWO BYTE OPERANDS) ===
   
   const string executeJumpWTrace = "JUMPW // Unconditional jump with signed word offset";
   executeJumpW()
   {
#ifdef TRACE
       LDA #(executeJumpWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpWTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Fetch 16-bit signed operand
       FetchOperandWord();
       // Operand is already in executorOperandL/H, move to ZP.NEXT
       LDA executorOperandL
       STA ZP.NEXTL
       LDA executorOperandH
       STA ZP.NEXTH
       
       // Apply offset to PC
       applySignedOffsetToPC();
       
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executeJumpWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpWTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   const string executeJumpZWTrace = "JUMPZW // Jump if zero with signed word offset";
   executeJumpZW()
   {
#ifdef TRACE
       LDA #(executeJumpZWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpZWTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
            // clear state
            LDA #State.Success
            STA ZP.SystemState
            
            // Pop and validate BIT type from stack
            DEC ZP.SP
            LDX ZP.SP
            LDA Address.TypeStackLSB, X
            CMP #BASICType.BIT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                CLC  // Failure - not BIT type
                break;    
            }
           
            // Check if value is zero (FALSE)     
            LDA Address.ValueStackLSB, X
            LDA Address.ValueStackLSB, X
            if (Z)  // Value is zero/FALSE - take the jump
            {
                // Fetch 16-bit signed operand
#ifdef TRACEEXE
                FetchOperandWord();
                LDA executorOperandL
                STA ZP.NEXTL
                LDA executorOperandH
                STA ZP.NEXTH

                // Apply offset to PC
                applySignedOffsetToPC();
#else
                LDA [ZP.PC]
                STA ZP.NEXTL // Save operand
                
                // Advance PC
                INC ZP.PCL
                if (Z)
                {
                   INC ZP.PCH
                }
                
                LDA [ZP.PC]
                STA ZP.NEXTH // Save operand
                
                // Advance PC
                INC ZP.PCL
                if (Z)
                {
                   INC ZP.PCH
                }
                
                // Two's complement addition works for both positive and negative offsets
                CLC
                LDA ZP.PCL
                ADC ZP.NEXTL
                STA ZP.PCL
                LDA ZP.PCH
                ADC ZP.NEXTH
                STA ZP.PCH
#endif                
            }
            else  // Value is non-zero/TRUE - skip the jump
            {
                // Still need to skip the operand
#ifdef TRACEEXE
                FetchOperandWord();
#else
                // PC += 2
                INC ZP.PCL
                if (Z)
                {
                   INC ZP.PCH
                }
                INC ZP.PCL
                if (Z)
                {
                   INC ZP.PCH
                }
#endif                
                // Don't apply offset - just continue to next instruction
            }
            SEC
            break;
       }
       
#ifdef TRACE
       LDA #(executeJumpZWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpZWTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   const string executeJumpNZWTrace = "JUMPNZW // Jump if non-zero with signed word offset";
   executeJumpNZW()
   {
#ifdef TRACE
       LDA #(executeJumpNZWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpNZWTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
       
       loop
       {
           // Pop and validate BIT type from stack
           popAndValidateBitType(); // ZP.TOPL -> A
           if (NC) { break; }  // Type error already set
           
           // Check if value is non-zero (TRUE)
           if (NZ)  // Value is non-zero/TRUE - take the jump
           {
               // Fetch 16-bit signed operand
               FetchOperandWord();
               
               // Operand is already in executorOperandL/H, move to ZP.NEXT
               LDA executorOperandL
               STA ZP.NEXTL
               LDA executorOperandH
               STA ZP.NEXTH
               
               // Apply offset to PC
               applySignedOffsetToPC();
           }
           else  // Value is zero/FALSE - skip the jump
           {
               // Still need to fetch and skip the operand
               FetchOperandWord();
               // Don't apply offset - just continue to next instruction
           }
           
           States.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executeJumpNZWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpNZWTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
    // === VARIABLE OPERATION HANDLERS (ONE BYTE OPERAND) ===
   
    // Execute PUSHGLOBAL opcode - push global variable value
    // Input: PC points to operand bytes (node address LSB, MSB)
    // Output: Variable value pushed to stack, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.TOP, ZP.TOPT, stack
    const string executePushGlobalTrace = "PUSHGLOBAL // Push global by name";
    executePushGlobal()
    {
    #ifdef TRACE
       LDA #(executePushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executePushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
       
       loop
       {
// Fetch operand (number of positions to decrement)
#ifdef TRACEEXE
            FetchOperandByte(); // -> A
#else            
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif           
           TAY  // Y = global index
           
           LDX ZP.SP
           INC ZP.SP
           
           // Get global type from type varibale
           LDA Address.TypeStack, Y
           STA ZP.TOPT
           if (BBS4, ZP.TOPT) // Bit 4 - VAR
           {
               // VAR type - strip VAR bit for runtime use
               AND # (BASICType.TYPEMASK | BASICType.ARRAY)  // Strip VAR bit but not ARRAY
               STA Address.TypeStackLSB, X
           }
           else
           {
               STA Address.TypeStack, X
           }
           LDA Address.ValueStackB0, Y
           STA Address.ValueStackB0, X
           LDA Address.ValueStackB1, Y  
           STA Address.ValueStackB1, X
#ifdef BASICLONG
           if (BBS3, ZP.TOPT) // Bit 3 - LONG
           {
               LDA Address.ValueStackB2, Y
               STA Address.ValueStackB2, X
               LDA Address.ValueStackB3, Y
               STA Address.ValueStackB3, X
           }
#endif
           LDA #State.Success
           STA ZP.SystemState
           break;
       }
       
    #ifdef TRACE
       LDA #(executePushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executePushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    } 
   
    // Execute POPGLOBAL opcode - pop value from stack and store to global variable
    // Input: PC points to operand bytes (node address LSB, MSB)
    // Output: Value popped from stack and stored to variable, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.TOP, ZP.TOPT, stack
    const string executePopGlobalTrace = "POPGLOBAL // Pop to global by name";
    executePopGlobal()
    {
    #ifdef TRACE
       LDA #(executePopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executePopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
       
       loop
       {
// Fetch operand (number of positions to decrement)
#ifdef TRACEEXE
            FetchOperandByte(); // -> A
#else            
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif
            
            TAY         // Y = global index
           
            // Pop value from stack
            DEC ZP.SP
            LDX ZP.SP   // X = stack position
            
            // get the type of the variable slot
            LDA Address.TypeStack, Y
            STA ZP.NEXTT
            
            // get the type of the stack value
            LDA Address.TypeStack, X
            STA ZP.TOPT
            
            // Pop the RHS
            LDA Address.ValueStackB0, X
            STA ZP.TOP0
            LDA Address.ValueStackB1, X
            STA ZP.TOP1
#ifdef BASICLONG
            if (BBS3, ZP.TOPT) // Bit 3 - LONG RHS
            {
                // popping 4 bytes
                LDA Address.ValueStackB2, X
                STA ZP.TOP2
                LDA Address.ValueStackB3, X
                STA ZP.TOP3
            }
            else
            {
                if (BBS3, ZP.NEXTT) // Bit 3 - LONG LHS but short RHS
                {
                    Long.TopToLong(); // RHS -> LONG
                }
            }
#endif            
            
            // Check if variable has VAR bit set
            if (BBS4, ZP.NEXTT) // Bit 4 - VAR
            {
                // VAR type: keep the VAR bit in the slot but change the type of the variable
                LDA ZP.TOPT
                ORA #BASICType.VAR
                STA Address.TypeStack, Y
            }
            else
            {   // Non-VAR variable - use normal type checking
                LDA ZP.NEXTT
                
                // VAR slots should never be popped directly but there is a special case for
                // global shadow variables in FOR iterators (where we are popping and actual
                // variable slot rather than stack data). Regular stack data should NEVER have
                // VAR (so most of the time this should be a NOP)
                LDA Address.TypeStackLSB, X
                AND # (BASICType.TYPEMASK | BASICType.ARRAY)  // Strip VAR bit but not ARRAY
                STA ZP.TOPT 
                
                Instructions.CheckRHSTypeCompatibility(); // Input: ZP.NEXTT = LHS type, ZP.TOPT = RHS type
                if (NC) 
                { 
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break; 
                }
                // keep LHS type when writing back
                LDA ZP.NEXTT
                STA Address.TypeStackLSB, Y
            }
            
            LDA ZP.TOP0
            STA Address.ValueStackB0, Y
            LDA ZP.TOP1
            STA Address.ValueStackB1, Y
#ifdef BASICLONG
            if (BBS3, ZP.NEXTT) // Bit 3 - LONG LHS
            {
                LDA ZP.TOP2
                STA Address.ValueStackB2, Y
                LDA ZP.TOP3
                STA Address.ValueStackB3, Y
            }
#endif             
            LDA #State.Success
            STA ZP.SystemState
        
            break;
       } // single exit loop
       
    #ifdef TRACE
       LDA #(executePopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executePopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute POPLOCAL opcode - pop value from stack and store to local/argument
    // Input: PC points to operand byte (signed BP offset)
    // Output: Value popped from stack and stored to local/argument, PC advanced by 1
    // Modifies: A, X, Y, ZP.PC, ZP.TOP, ZP.TOPT, stack
    const string executePopLocalTrace = "POPLOCAL // Pop to local/arg by BP offset";
    executePopLocal()
    {
    #ifdef TRACE
        LDA #(executePopLocalTrace % 256) STA ZP.TraceMessageL LDA #(executePopLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
            // Fetch signed offset operand
            // FetchOperandByte(); // Result in A -> always success
            
            // clear state
            LDA #State.Success
            STA ZP.SystemState
#ifdef TRACEEXE
            FetchOperandByte();
#else
            // Fetch operand
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif       
            // Add signed offset to BP (handles negative naturally)
            CLC
            ADC ZP.BP
            TAY                     // Y = local position
            
            // Pop value from stack
            DEC ZP.SP
            LDX ZP.SP               // X = stack position
            
            // get the type of the variable slot
            LDA Address.TypeStack, Y
            STA ZP.NEXTT
            
            // get the type of the stack value
            LDA Address.TypeStack, X
            STA ZP.TOPT
            
            // Pop the RHS
            LDA Address.ValueStackB0, X
            STA ZP.TOP0
            LDA Address.ValueStackB1, X
            STA ZP.TOP1
#ifdef BASICLONG
            if (BBS3, ZP.TOPT) // Bit 3 - LONG RHS
            {
                // popping 4 bytes
                LDA Address.ValueStackB2, X
                STA ZP.TOP2
                LDA Address.ValueStackB3, X
                STA ZP.TOP3
            }
            else
            {
                if (BBS3, ZP.NEXTT) // Bit 3 - LONG LHS but short RHS
                {
                    Long.TopToLong(); // RHS -> LONG
                }
            }
#endif
            // Check if variable has VAR bit set
            if (BBS4, ZP.NEXTT) // Bit 4 - VAR
            {
                // VAR type: keep the VAR bit in the slot but change the type of the variable
                LDA ZP.TOPT
                ORA #BASICType.VAR
                STA Address.TypeStack, Y
            }
            else
            {   // Check type compatibility for assignment
                LDA ZP.NEXTT
                AND # BASICType.TYPEMASK  // Extract LHS data type
                STA ZP.NEXTT
                
                Instructions.CheckRHSTypeCompatibility(); // Input: ZP.NEXTT = LHS type, ZP.TOPT = RHS type
                if (NC) 
                { 
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break; 
                }
            }
            LDA ZP.TOP0
            STA Address.ValueStackB0, Y
            LDA ZP.TOP1
            STA Address.ValueStackB1, Y
#ifdef BASICLONG
            if (BBS3, ZP.NEXTT) // Bit 3 - LONG LHS
            {
                LDA ZP.TOP2
                STA Address.ValueStackB2, Y
                LDA ZP.TOP3
                STA Address.ValueStackB3, Y
            }
#endif
            LDA #State.Success
            STA ZP.SystemState
            break;
        } // single exit
        
    #ifdef TRACE
        LDA #(executePopLocalTrace % 256) STA ZP.TraceMessageL LDA #(executePopLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute PUSHLOCAL opcode - push local variable or argument value
    // Input: PC points to operand byte (signed BP offset)
    // Output: Local/argument value pushed to stack, PC advanced by 1
    // Modifies: A, X, Y, ZP.PC, ZP.TOP, ZP.TOPT, stack
    const string executePushLocalTrace = "PUSHLOCAL // Push local/arg by BP offset";
    executePushLocal()
    {
    #ifdef TRACE
        LDA #(executePushLocalTrace % 256) STA ZP.TraceMessageL LDA #(executePushLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        // Fetch signed offset operand
        // FetchOperandByte(); // Result in A -> always Success
        
        // clear state
        LDA #State.Success
        STA ZP.SystemState
#ifdef TRACEEXE
        FetchOperandByte();
#else            
        // Fetch operand
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif
        
        // signed BP offset  in A
        // Stacks.GetStackTopBP() -> Stacks.PushTop()
        CLC
        ADC ZP.BP
        TAY
        
        LDX ZP.SP
        
        LDA Address.ValueStackB0, Y
        STA Address.ValueStackB0, X
        LDA Address.ValueStackB1, Y
        STA Address.ValueStackB1, X
        LDA Address.TypeStack, Y
        AND # (BASICType.TYPEMASK | BASICType.ARRAY)  // Strip VAR bit but not ARRAY
        STA Address.TypeStack, X
        AND #BASICType.LONG
        if (NZ)
        {
            LDA Address.ValueStackB2, Y
            STA Address.ValueStackB2, X
            LDA Address.ValueStackB3, Y
            STA Address.ValueStackB3, X
        }
        INC ZP.SP
        
    #ifdef TRACE
        LDA #(executePushLocalTrace % 256) STA ZP.TraceMessageL LDA #(executePushLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    } 
    
    executeClearScreen()
    {
        // Send ANSI escape sequence to clear screen
        LDA #0x0C  // Form feed
        Serial.WriteChar();
    }
    
    // Execute FORCHK opcode - FOR loop initial check
    // Stack layout: Iterator at BP+offset, TO at BP+offset+1, STEP at BP+offset+2
    // Operands: [0] = iterator BP offset, [1-2] = forward jump offset (exit loop)
    // Output: Jump forward if loop should not execute, otherwise fall through
    // Modifies: A, X, Y, executorOperandL/H, executorOperandBP, ZP.TOP/TOPT, ZP.NEXT/NEXTT
    const string executeFORCHKTrace = "FORCHK // FOR initial check";
    executeFORCHK()
    {
    #ifdef TRACE
        LDA #(executeFORCHKTrace % 256) STA ZP.TraceMessageL LDA #(executeFORCHKTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Fetch iterator BP offset
            FetchOperandByte(); // Result in A
            STA Executor.executorOperandBP  // Save iterator offset
            
            // Fetch forward jump offset (16-bit)
            FetchOperandWord(); // Result in executorOperandL/H
            
            // First, get STEP value to check sign
            LDA #0xFF
            Stacks.GetStackTopSP();  // [SP-1] => STEP in ZP.TOP/TOPT, strips BASICTypes.VAR
            
            // Store direction in X based on STEP sign
            LDX #0  // Assume positive
            LDA ZP.TOPH
            if (MI)
            {
                LDX #1  // Negative STEP
            }
            
            // Load iterator value (at BP+offset)
            LDA Executor.executorOperandBP
            Stacks.GetStackTopBP();  // Iterator in ZP.TOP/TOPT, preserves X, strips BASICTypes.VAR
            // Load TO value
            LDA #0xFE
            Stacks.GetStackNextSP();  //[SP-2] => TO in ZP.NEXT/NEXTT, strips BASICTypes.VAR

            // Now TOP = iterator, NEXT = TO
            // Push them for the comparison instructions
            LDA ZP.TOPT
            Stacks.PushTop();   // FORCHK: Push iterator => NEXT slot
            LDA ZP.NEXTT
            Stacks.PushNext();  // FORCHK: Push TO       => TOP slot
            
                
            // Now TOP = iterator, NEXT = TO
            // Use comparison based on STEP direction
            CPX #0
            if (NZ)  // Negative STEP
            {
                // For negative STEP: check if iterator >= TO
                // Continue if iterator >= TO, exit if iterator < TO
                ComparisonInstructions.GreaterEqual();  // Sets Z if iterator >= TO
                States.CanContinue();
                if (NC) { break; }  // Type mismatch
                Stacks.PopA(); // FORCHK
                if (NZ) // TRUE'
                {
                    // Iterator >= TO, continue loop
                    States.SetSuccess();
                    break;
                }
                // Iterator < TO, exit loop
            }
            else  // Positive STEP
            {
                // For positive STEP: check if iterator <= TO
                // Continue if iterator <= TO, exit if iterator > TO
                ComparisonInstructions.LessEqual();
                States.CanContinue();
                if (NC) { break; }  // Type mismatch
                Stacks.PopA();   // FORCHK
                if (NZ) // 'TRUE'
                {
                    // Iterator <= TO, continue loop
                    States.SetSuccess();
                    break;
                }
                // Iterator > TO, exit loop
            }
            
            // Take the forward jump (exit loop)
            CLC
            LDA ZP.PCL
            ADC Executor.executorOperandL
            STA ZP.PCL
            LDA ZP.PCH
            ADC Executor.executorOperandH
            STA ZP.PCH
            
            States.SetSuccess();
            break;
            
        } // Single exit block
        
    #ifdef TRACE
        LDA #(executeFORCHKTrace % 256) STA ZP.TraceMessageL LDA #(executeFORCHKTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute FORIT opcode - FOR loop iterate (increment and check)
    // Stack layout: Iterator at BP+offset, TO at BP+offset+1, STEP at BP+offset+2
    // Operands: [0] = iterator BP offset, [1-2] = backward jump offset (continue loop)
    // Output: Jump backward if loop should continue, otherwise fall through
    // Modifies: A, X, Y, executorOperandL/H, executorOperandBP, ZP.TOP/TOPT, ZP.NEXT/NEXTT
    const string executeFORITTrace = "FORIT // FOR iterate";
    executeFORIT()
    {
    #ifdef TRACE
        LDA #(executeFORITTrace % 256) STA ZP.TraceMessageL LDA #(executeFORITTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Fetch iterator BP offset
            FetchOperandByte(); // Result in A
            STA Executor.executorOperandBP  // Save iterator offset
            
            // Fetch backward jump offset (16-bit)
            FetchOperandWord(); // Result in executorOperandL/H
            
            // Get STEP value
            LDA #0xFF
            Stacks.GetStackNextSP();  // [SP-1] => STEP in ZP.NEXT/NEXTT
            
            // Store direction in X based on STEP sign
            LDX #0  // Assume positive
            LDA ZP.NEXTH
            if (MI)
            {
                LDX #1  // Negative STEP
            }
            
            
            // Load iterator value (at BP+offset)
            LDA Executor.executorOperandBP
            Stacks.GetStackTopBP();  // Iterator in ZP.TOP/TOPT, preserves X, strips BASICTypes.VAR

            // Add STEP to iterator (TOP = iterator + NEXT)
            LDA ZP.TOPT
            Stacks.PushTop();   // FORIT: Push iterator => NEXT slot
            LDA ZP.NEXTT
            Stacks.PushNext();  // FORIT: Push STEP     => TOP slot
            Instructions.Addition();  // Handles signed/unsigned, type checking, preserves X
            PHX Stacks.PopTop(); PLX // FORIT
            
            States.CanContinue(); // preserves X
            if (NC) { break; }  // Type mismatch or overflow


            // Store updated iterator back (TOP now has new value)
            LDA Executor.executorOperandBP
            Stacks.SetStackTopBP();  // Store ZP.TOP/TOPT to BP+offset, preserves X
            
            // Load TO value for comparison
            LDA #0xFE
            Stacks.GetStackNextSP();  // [SP-2] => TO in ZP.NEXT/NEXTT, preserves X, strips BASICTypes.VAR
            
            // Now TOP = iterator, NEXT = TO
            // Push them for the comparison instructions
            LDA ZP.TOPT
            Stacks.PushTop();   // FORIT: Push iterator => NEXT slot
            LDA ZP.NEXTT
            Stacks.PushNext();  // FORIT: Push       TO => TOP slot
            
            // TOP = updated iterator, NEXT = TO
            // Use comparison based on STEP direction
            CPX #0
            if (NZ)  // Negative STEP
            {
                // For negative STEP: check if iterator >= TO
                // Continue if iterator >= TO, exit if iterator < TO
                ComparisonInstructions.GreaterEqual();
                States.CanContinue();
                if (NC) { break; }  // Type mismatch
                Stacks.PopA();   // FORIT
                if (Z) // 'FALSE'
                {
                    // Iterator < TO, exit loop (fall through)
                    States.SetSuccess();
                    break;
                }
                // Iterator >= TO, continue loop
            }
            else  // Positive STEP
            {
                // For positive STEP: check if iterator <= TO
                // Continue if iterator <= TO, exit if iterator > TO
                ComparisonInstructions.LessEqual();
                States.CanContinue();
                if (NC) { break; }  // Type mismatch
                Stacks.PopA(); // FORIT
                if (Z) // FALSE
                {
                    // Iterator > TO, exit loop (fall through)
                    States.SetSuccess();
                    break;
                }
                // Iterator <= TO, continue loop
            }
            
            // Take the backward jump (continue loop)
            CLC
            LDA ZP.PCL
            ADC Executor.executorOperandL
            STA ZP.PCL
            LDA ZP.PCH
            ADC Executor.executorOperandH
            STA ZP.PCH
            
            States.SetSuccess();
            break;
            
        } // Single exit block
        
    #ifdef TRACE
        LDA #(executeFORITTrace % 256) STA ZP.TraceMessageL LDA #(executeFORITTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    
    // Execute FORITF opcode - Hybrid: safe stack access, inlined computation
    // Stack layout: TO at SP-2, STEP at SP-1 (ignored, always 1)
    // Operands: [0] = iterator BP offset, [1-2] = backward jump offset
    const string executeFORITFTrace = "FORITF // Fast iterate";
    executeFORITF()
    {
    #ifdef TRACE
        LDA #(executeFORITFTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeFORITFTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            LDA [ZP.PC]
            INC ZP.PCL
            if (Z) { INC ZP.PCH }
            
            // Load iterator value
            CLC
            ADC ZP.BP
            TAY
            LDA Address.ValueStackLSB, Y
            STA ZP.TOPL
            LDA Address.ValueStackMSB, Y
            STA ZP.TOPH
            
            // Fetch backward jump offset (16-bit)
            LDA [ZP.PC]
            STA executorOperandL
            INC ZP.PCL
            if (Z) { INC ZP.PCH }
            
            LDA [ZP.PC]
            STA executorOperandH
            INC ZP.PCL
            if (Z) { INC ZP.PCH }

            // Increment iterator++ and store back immediately with WORD type
            INC ZP.TOPL
            if (Z) 
            {
                INC ZP.TOPH 
                LDA ZP.TOPH
                STA Address.ValueStackMSB, Y
            }
            LDA ZP.TOPL
            STA Address.ValueStackLSB, Y
            
            // Store back as WORD type : perhaps only if it was implicit or VAR? (consider a local or global INT -  we're changing its type)
            //LDA #(BASICType.WORD | BASICType.VAR)
            //STA Address.TypeStackLSB, Y
            
            // Get TO value from stack (safe)
            LDA #0xFE  // -2 from SP
            CLC
            ADC ZP.SP
            TAY
            LDA Address.ValueStackLSB, Y
            STA ZP.NEXTL
            LDA Address.ValueStackMSB, Y
            STA ZP.NEXTH
            
            // Fast path: Check if clearly continuing (TO high > iterator high)
            LDA ZP.NEXTH  // TO high
            CMP ZP.TOPH   // iterator high
            if (NC)       // TO high < iterator high - definitely exit
            {
                break;
            }
            if (NZ)       // TO high > iterator high - definitely continue
            {
                CLC
                LDA ZP.PCL
                ADC Executor.executorOperandL
                STA ZP.PCL
                LDA ZP.PCH  
                ADC Executor.executorOperandH
                STA ZP.PCH
                break;
            }
            
            // High bytes equal - check low bytes for final decision
            LDA ZP.NEXTL          // TO low
            CMP ZP.TOPL           // Compare with iterator low
            if (NC)               // TO low < iterator low - exit
            {
                break;
            }
            // TO >= iterator - continue loop
            CLC
            LDA ZP.PCL
            ADC Executor.executorOperandL
            STA ZP.PCL
            LDA ZP.PCH  
            ADC Executor.executorOperandH
            STA ZP.PCH
            break;
        } // Single exit block
        
        LDA #State.Success
        STA ZP.SystemState
        
    #ifdef TRACE
        LDA #(executeFORITFTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeFORITFTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
    // Execute GETITEMGG opcode - get array[index] where both are globals
    // Input: PC points to operand bytes (array global index, index global index)
    // Output: Array element pushed to stack, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.IDY, stack
    const string executeGetItemGGTrace = "GETITEMGG // Get array[index] global/global";
    executeGetItemGG()
    {
    #ifdef TRACE
        LDA #(executeGetItemGGTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemGGTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            TAX  // X = array global index
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            TAY  // Y = index global index
            
            // Load array pointer and type
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X  // Array type
            STA ZP.ACCT
            
            // Load index value  
            LDA Address.ValueStackLSB, Y
            STA ZP.IDYL
            LDA Address.ValueStackMSB, Y
            STA ZP.IDYH
            
            // Call common implementation
            commonGetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeGetItemGGTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemGGTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Execute GETITEMGL opcode - get array[index] where array=global, index=local
    // Input: PC points to operand bytes (array global index, index local offset)
    // Output: Array element pushed to stack, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.IDY, stack
    const string executeGetItemGLTrace = "GETITEMGL // Get array[index] global/local";
    executeGetItemGL()
    {
    #ifdef TRACE
        LDA #(executeGetItemGLTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemGLTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            TAX  // X = array global index
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Calculate local position: BP + offset
            CLC
            ADC ZP.BP
            TAY  // Y = index local position
            
            // Load array pointer and type
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X  // Array type
            STA ZP.ACCT
            
            // Load index value from local  
            LDA Address.ValueStackLSB, Y
            STA ZP.IDYL
            LDA Address.ValueStackMSB, Y
            STA ZP.IDYH
            
            // Call common implementation
            commonGetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeGetItemGLTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemGLTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Execute GETITEMLG opcode - get array[index] where array=local, index=global
    // Input: PC points to operand bytes (array local offset, index global index)
    // Output: Array element pushed to stack, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.IDY, stack
    const string executeGetItemLGTrace = "GETITEMLG // Get array[index] local/global";
    executeGetItemLG()
    {
    #ifdef TRACE
        LDA #(executeGetItemLGTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemLGTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            if (NC) { break; }
            
            // Calculate local position: BP + offset
            CLC
            ADC ZP.BP
            TAX  // X = array local position
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            TAY  // Y = index global index
            
            // Load array pointer and type from local
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X  // Array type
            STA ZP.ACCT
            
            // Load index value from global
            LDA Address.ValueStackLSB, Y
            STA ZP.IDYL
            LDA Address.ValueStackMSB, Y
            STA ZP.IDYH
            
            // Call common implementation
            commonGetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeGetItemLGTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemLGTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Execute GETITEMLL opcode - get array[index] where both are locals
    // Input: PC points to operand bytes (array local offset, index local offset)
    // Output: Array element pushed to stack, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.IDY, stack
    const string executeGetItemLLTrace = "GETITEMLL // Get array[index] local/local";
    executeGetItemLL()
    {
    #ifdef TRACE
        LDA #(executeGetItemLLTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemLLTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Calculate array local position: BP + offset
            CLC
            ADC ZP.BP
            TAX  // X = array local position
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Calculate index local position: BP + offset
            CLC
            ADC ZP.BP
            TAY  // Y = index local position
            
            // Load array pointer and type from local
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X  // Array type
            STA ZP.ACCT
            
            // Load index value from local
            LDA Address.ValueStackLSB, Y
            STA ZP.IDYL
            LDA Address.ValueStackMSB, Y
            STA ZP.IDYH
            
            // Call common implementation
            commonGetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeGetItemLLTrace % 256) STA ZP.TraceMessageL LDA #(executeGetItemLLTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute INDEX opcode - string/array indexing
    // Stack: [..., collection_ref, index] -> [..., element_value]
    // Input: Two values on stack (collection reference, then index on top)
    // Output: Element at index pushed to stack (CHAR for strings, element type for arrays)
    // Modifies: ZP.TOP, ZP.TOPT, ZP.NEXT, ZP.NEXTT, ZP.ACC, stack
    const string executeGetItemTrace = "GETITEM // String/Array indexing";
    executeGetItem()
    {
    #ifdef TRACE
        LDA #(executeGetItemTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeGetItemTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Pop index value from stack
            DEC ZP.SP
            LDX ZP.SP
            LDA Address.ValueStackLSB, X
            STA ZP.IDYL
            LDA Address.ValueStackMSB, X
            STA ZP.IDYH
            LDA Address.TypeStackLSB, X
            STA ZP.TOPT
            
            // Pop collection reference from stack
            DEC ZP.SP
            LDX ZP.SP
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X
            STA ZP.ACCT
            
            // Inputs: ZP.ACCT = array type, ZP.IDX = array pointer, ZP.TOPT = index type, ZP.IDY = index
            // Output: Element pushed to stack, States set appropriately  
            commonGetItem();
            
            break;
        } // loop
        
    #ifdef TRACE
        LDA #(executeGetItemTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeGetItemTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
    // Common GETITEM implementation
    // Inputs: ZP.ACCT = array type, ZP.IDX = array pointer, ZP.TOPT = index type, ZP.IDY = index
    // Output: Element pushed to stack, States set appropriately  
    commonGetItem()
    {
        loop
        {
            // Check index type is numeric (INT, WORD, or BYTE)
            LDA ZP.TOPT
            AND #BASICType.TYPEMASK  // Remove VAR bit if present
            CMP #BASICType.INT
            if (Z) 
            { 
                // INT type - check if negative
                LDA ZP.TOPH
                if (MI)  // Negative index
                {
                    Error.RangeError(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
            }
            else
            {
                CMP #BASICType.WORD
                if (NZ)
                {
                    CMP #BASICType.BYTE
                    if (NZ)
                    {
                        // Invalid index type
                        Error.TypeMismatch(); BIT ZP.EmulatorPCL
                        States.SetFailure();
                        break;
                    }
                }
            }
            
            // Dispatch based on collection type
            LDA ZP.ACCT
            AND # BASICType.TYPEMASK
            CMP # BASICType.STRING
            if (Z)
            {
                // Get string length
                LDY #0
                loop
                {
                    LDA [ZP.IDX], Y
                    if (Z) { break; }
                    INY
        #ifdef DEBUG
                    if (Z) // wrapped around from 0xFF to 0x00
                    {
                        LDA # 0x04  Debug.Crash(); // runaway StringLength() calculation
                    }
        #endif
                }
                STY ZP.TOPL
                                        
                // Check if index is within bounds (index < length)
                // Compare index (IDY) with length (TOP)
                LDA ZP.IDYH
                if (NZ)  // strings are always <= 255
                {
                    Error.RangeError(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
                LDA ZP.IDYL
                CMP ZP.TOPL
                if (C)   // IDYL >= TOPL (index >= length
                {
                    Error.RangeError(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
                
                // Read the character at the calculated address
                LDY ZP.IDYL
                LDA [ZP.IDX], Y
                
                // Store character value in ZP.TOP as CHAR type
                STA ZP.TOPL
                STZ ZP.TOPH  // Clear high byte
                LDA #BASICType.CHAR
            }
            else
            {
                if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
                {
                    // Get the element value
                    BASICArray.GetItem();  // Returns value in ZP.TOP, type in ZP.ACCT
                    if (NC)
                    {
                        States.SetFailure();
                        break;
                    }
                    
                    // Push result with correct type
                    LDA ZP.ACCT
                }
                else
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
            }
            
            LDY ZP.SP
            STA Address.TypeStackLSB, Y
            LDA ZP.TOPL
            STA Address.ValueStackLSB, Y
            LDA ZP.TOPH
            STA Address.ValueStackMSB, Y
            INC ZP.SP
            
            LDA #State.Success
            STA ZP.SystemState
            break;
        } // single exit
    }

    // Execute SETITEM opcode - array element assignment
    // Stack layout on entry: [array_ptr][index][value]
    // Stack layout on exit: [] (all consumed)
    const string executeSetItemTrace = "SETITEM // Array element assignment";
    executeSetItem()
    {
    #ifdef TRACE
        LDA #(executeSetItemTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop
        {
            // Pop value into TOP (includes type)
            LDX ZP.SP
            DEX
            LDA Address.ValueStackLSB, X
            STA ZP.TOPL
            LDA Address.ValueStackMSB, X
            STA ZP.TOPH
            LDA Address.TypeStackLSB, X
            STA ZP.TOPT
            
            // Pop index into IDY
            DEX
            LDA Address.ValueStackLSB, X
            STA ZP.IDYL
            LDA Address.ValueStackMSB, X
            STA ZP.IDYH
            
            // Pop array pointer into IDX
            DEX
            STX ZP.SP
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            
            // Verify this is actually an array
            LDA Address.TypeStackLSB, X
            
            // Inputs:
            //     - ARRAY type in A
            //     - ARRAY ptr in IDX
            //     - <index> in IDY
            //     - <value> in TOP (including TOPT)
            commonSetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeSetItemTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
    
    // Execute SETITEMGL opcode - pop from stack and SETITEM <array global address> <index local offset>
    // Input: Value on stack, PC points to array global address (2 bytes), then index local offset (1 byte)
    // Output: Array element set, value popped from stack, PC advanced by 3
    const string executeSetItemGLTrace = "SETITEMGL // Array[global] index[local] = value";
    executeSetItemGL()
    {
    #ifdef TRACE
        LDA #(executeSetItemGLTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemGLTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop
        {
            // Pop value from stack into TOP
            Stacks.PopTop();
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Load array value from global address
            TAX  // global index LSB
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X
            STA ZP.ACCT  // array type
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Calculate stack position: BP + offset
            CLC
            ADC ZP.BP
            TAX  // X = stack position
            
            // Load index value from local position
            LDA Address.ValueStackLSB, X
            STA ZP.IDYL
            LDA Address.ValueStackMSB, X
            STA ZP.IDYH
            
            // Set up for commonSetItem: array type in A, array ptr in IDX, index in IDY, value in TOP
            LDA ZP.ACCT
            commonSetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeSetItemGLTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemGLTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

    // Execute SETITEMLG opcode - pop from stack and SETITEM <array local offset> <index global address>
    // Input: Value on stack, PC points to array local offset (1 byte), then index global address (2 bytes)
    // Output: Array element set, value popped from stack, PC advanced by 3
    const string executeSetItemLGTrace = "SETITEMLG // Array[local] index[global] = value";
    executeSetItemLG()
    {
    #ifdef TRACE
        LDA #(executeSetItemLGTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemLGTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop
        {
            // Pop value from stack into TOP
            Stacks.PopTop();
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Calculate stack position: BP + offset
            CLC
            ADC ZP.BP
            TAX  // X = stack position
            
            // Load array value from local position
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X
            STA ZP.ACCT  // array type
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Load index value from global address
            TAX
            LDA Address.ValueStackLSB, X
            STA ZP.IDYL
            LDA Address.ValueStackMSB, X
            STA ZP.IDYH
            
            // Set up for commonSetItem: array type in A, array ptr in IDX, index in IDY, value in TOP
            LDA ZP.ACCT
            commonSetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeSetItemLGTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemLGTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

    // Execute SETITEMLL opcode - pop from stack and SETITEM <array local offset> <index local offset>
    // Input: Value on stack, PC points to array local offset (1 byte), then index local offset (1 byte)
    // Output: Array element set, value popped from stack, PC advanced by 2
    const string executeSetItemLLTrace = "SETITEMLL // Array[local] index[local] = value";
    executeSetItemLL()
    {
    #ifdef TRACE
        LDA #(executeSetItemLLTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemLLTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop
        {
            // Pop value from stack into TOP
            Stacks.PopTop();
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   

            // Calculate stack position: BP + offset
            CLC
            ADC ZP.BP
            TAX  // X = stack position
            
            // Load array value from local position
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X
            STA ZP.ACCT  // array type
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Calculate stack position: BP + offset
            CLC
            ADC ZP.BP
            TAX  // X = stack position
            
            // Load index value from local position
            LDA Address.ValueStackLSB, X
            STA ZP.IDYL
            LDA Address.ValueStackMSB, X
            STA ZP.IDYH
            
            // Set up for commonSetItem: array type in A, array ptr in IDX, index in IDY, value in TOP
            LDA ZP.ACCT
            commonSetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeSetItemLLTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemLLTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
    // Execute SETITEMGG opcode - pop from stack and SETITEM <array global address> <index global address>
    // Input: Value on stack, PC points to array global address (2 bytes), then index global address (2 bytes)
    // Output: Array element set, value popped from stack, PC advanced by 4
    const string executeSetItemGGTrace = "SETITEMGG // Array[global] index[global] = value";
    executeSetItemGG()
    {
    #ifdef TRACE
        LDA #(executeSetItemGGTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemGGTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop
        {
            // Pop value from stack into TOP
            Stacks.PopTop();
           
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif            
            
            // Load array value from global address
            TAX
            LDA Address.ValueStackLSB, X
            STA ZP.IDXL
            LDA Address.ValueStackMSB, X
            STA ZP.IDXH
            LDA Address.TypeStackLSB, X
            STA ZP.ACCT  // array type
            
#ifdef TRACEEXE            
            FetchOperandByte();
#else
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif   
            
            // Load index value from global address
            TAX
            LDA Address.ValueStackLSB, X
            STA ZP.IDYL
            LDA Address.ValueStackMSB, X
            STA ZP.IDYH
            
            // Set up for commonSetItem: array type in A, array ptr in IDX, index in IDY, value in TOP
            LDA ZP.ACCT
            commonSetItem();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeSetItemGGTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeSetItemGGTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
    // Inputs:
    //     - ARRAY type in A
    //     - ARRAY ptr in IDX
    //     - <index> in IDY
    //     - <value> in TOP (including TOPT)
    commonSetItem()
    {
        loop
        {
            AND # BASICType.ARRAY
            if (Z)
            {
                // not an ARRAY
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            // Get array element type for type checking
            BASICArray.GetItemType(); // Returns type in ZP.ACCT
            
            // Check type compatibility: array element type vs value type
            LDA ZP.ACCT
            STA ZP.NEXTT  // LHS type (array element)
            // TOPT already has RHS type (value)
            
            Instructions.CheckRHSTypeCompatibility();
            if (NC)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            // BASICArray.SetItem expects: 
            // Input: ZP.IDX = array ptr, ZP.IDY = index, ZP.TOP = value
            // Output: C set on success
            BASICArray.SetItem();
            if (NC)
            {
                // Range error already set by SetItem
                States.SetFailure();
                break;
            }
            
            LDA #State.Success
            STA ZP.SystemState
            break;
        } // single exit
    }
    
    // Execute INCLOCAL opcode - increment local/argument variable by 1
    // Input: PC points to operand byte (signed BP offset)
    // Output: Local/argument value incremented, PC advanced by 1
    // Modifies: A, X, ZP.PC
    const string executeIncLocalTrace = "INCLOCAL // Increment local/arg by 1";
    executeIncLocal()
    {
    #ifdef TRACE
        LDA #(executeIncLocalTrace % 256) STA ZP.TraceMessageL LDA #(executeIncLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        // method never fails
        LDA #State.Success
        STA ZP.SystemState
        
        // Fetch signed offset operand
#ifdef TRACEEXE
        FetchOperandByte();
#else            
        // Fetch operand
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif
        
        // Calculate stack position: BP + offset
        CLC
        ADC ZP.BP
        TAX  // X = stack position
        
        // Increment the value (equivalent to +1)
        INC Address.ValueStackLSB, X
        if (Z)
        {
            INC Address.ValueStackMSB, X
        }
        
    #ifdef TRACE
        LDA #(executeIncLocalTrace % 256) STA ZP.TraceMessageL LDA #(executeIncLocalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute INCGLOBAL opcode - increment global variable by 1
    // Input: PC points to operand byte (global index)
    // Output: Global variable value incremented, PC advanced by 1
    // Modifies: A, X, ZP.PC
    const string executeIncGlobalTrace = "INCGLOBAL // Increment global by 1";
    executeIncGlobal()
    {
    #ifdef TRACE
        LDA #(executeIncGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executeIncGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        // method never fails
        LDA #State.Success
        STA ZP.SystemState
        
        // Fetch unsigned stack address
#ifdef TRACEEXE
        FetchOperandByte();
#else            
        // Fetch operand
        LDA [ZP.PC]
       
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#endif
        TAX  // X = global index
        
        // Increment the value (equivalent to +1)
        INC Address.ValueStackLSB, X
        if (Z)
        {
            INC Address.ValueStackMSB, X
        }
    
    #ifdef TRACE
        LDA #(executeIncGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executeIncGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute ADDLOCALS opcode - add one local variable to another
    // Input: PC points to operand bytes (target BP offset, source BP offset, unused)
    // Output: target local += source local, PC advanced by 3
    // Modifies: A, X, Y, ZP.PC, ZP.TOP, ZP.NEXT
    const string executeAddLocalsTrace = "ADDLOCALS // Add local to local";
    executeAddLocals()
    {
    #ifdef TRACE
        LDA #(executeAddLocalsTrace % 256) STA ZP.TraceMessageL LDA #(executeAddLocalsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
            // Fetch target BP offset (first operand)
#ifdef TRACEEXE
            FetchOperandByte(); // -> A
#else            
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif            
            // Calculate target stack position: BP + target_offset
            CLC
            ADC ZP.BP
            TAX                                 // X = target stack position (LHS)
            
#ifdef TRACEEXE
            FetchOperandByte(); // -> A
#else            
            LDA [ZP.PC]
           
            // Advance PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
#endif
              
            // Calculate source stack position: BP + source_offset
            CLC
            ADC ZP.BP
            TAY                                // Y = source stack position (RHS)
            
            
            LDA Address.TypeStack, Y
            STA ZP.TOPT
            LDA Address.TypeStack, X
            STA ZP.NEXTT
            
            
            CLC
            LDA Address.ValueStackB0, X
            ADC Address.ValueStackB0, Y
            STA Address.ValueStackB0, X
            LDA Address.ValueStackB1, X
            ADC Address.ValueStackB1, Y
            STA Address.ValueStackB1, X
            
            if (BBS3, ZP.NEXTT) // LHS is LONG
            {
                if (BBS3, ZP.TOPT) // RHS is LONG
                {
                    LDA Address.ValueStackB2, X
                    ADC Address.ValueStackB2, Y
                    STA Address.ValueStackB2, X
                    LDA Address.ValueStackB3, X
                    ADC Address.ValueStackB3, Y
                    STA Address.ValueStackB3, X
                } 
                else
                {
                    // TODO: if RHS is -ve INT we need to sign extend
                    LDA Address.ValueStackB2, X
                    ADC #0
                    STA Address.ValueStackB2, X
                    LDA Address.ValueStackB3, X
                    ADC #0
                    STA Address.ValueStackB3, X
                }
            }
            else
            {
                // LHS is not LONG
                if (BBS3, ZP.TOPT) // RHS is LONG
                {
                    if (BBS4, ZP.NEXTT) // LHS is VAR
                    {
                        // make LHS LONG
                        LDA #(BASICType.VAR | BASICType.LONG)
                        STA Address.TypeStack, X
                        
                        // TODO: if LHS is -ve INT we need to sign extend
                        
                        LDA #0
                        ADC Address.ValueStackB2, Y
                        STA Address.ValueStackB2, X
                        LDA #0
                        ADC Address.ValueStackB3, Y
                        STA Address.ValueStackB3, X
                    }
                    else
                    {
                        // TODO: overflow?
                    }
                }
                else
                {
                    // neither LHS nor RHS are long
                }
            }
            
            
            LDA #State.Success
            STA ZP.SystemState
            break;
        }
        
    #ifdef TRACE
        LDA #(executeAddLocalsTrace % 256) STA ZP.TraceMessageL LDA #(executeAddLocalsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute ADDGLOBALS opcode - add one global variable to another
    // Input: PC points to operand bytes (target global index, source global index, unused)
    // Output: target global += source global, PC advanced by 3
    // Modifies: A, X, Y, ZP.PC, ZP.TOP, ZP.NEXT
    const string executeAddGlobalsTrace = "ADDGLOBALS // Add global to global";
    executeAddGlobals()
    {
    #ifdef TRACE
        LDA #(executeAddGlobalsTrace % 256) STA ZP.TraceMessageL LDA #(executeAddGlobalsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop
        {
            // Fetch target global index (first operand)
            FetchOperandByte();
            
            TAX  // X = target global index
            
            // Fetch source global index (second operand)
            FetchOperandByte();
            
            TAY  // Y = source global index
            
            // Add source to target: 
            CLC
            LDA Address.ValueStackLSB, X
            ADC Address.ValueStackLSB, Y
            STA Address.ValueStackLSB, X
            LDA Address.ValueStackMSB, X
            ADC Address.ValueStackMSB, Y
            STA Address.ValueStackMSB, X
            
            States.SetSuccess();
            break;
        }
        
    #ifdef TRACE
        LDA #(executeAddGlobalsTrace % 256) STA ZP.TraceMessageL LDA #(executeAddGlobalsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    const string executePushLongTrace = "PUSHLONG";
    executePushLong()
    {
#ifdef BASICLONG        
        
    #ifdef TRACE
        LDA #(executePushLongTrace % 256) STA ZP.TraceMessageL 
        LDA #(executePushLongTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Pop the LSW from stack
            Stacks.PopTop();
            Error.CheckError();
            if (NC) { break; }
            
            FetchOperandWord();
            LDA executorOperandL
            STA ZP.TOP2
            LDA executorOperandH
            STA ZP.TOP3
   
            // Push the recombined LONG value to stack
            LDA # BASICType.LONG
            STA ZP.TOPT
            Long.PushTop();
            
            SEC  // Set C (successful conversion)
            break;
        } // Single exit block
        
    #ifdef TRACE
        LDA #(executePushLongTrace % 256) STA ZP.TraceMessageL 
        LDA #(executePushLongTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
        
#else     
        Error.InternalError(); BIT ZP.EmulatorPCL   
#endif        
    }
    
    // Execute TOLONG opcode - convert stack top to LONG type
    // Input: Stack top contains 16 bit value to convert
    // Output: Stack top contains LONG value, C set if successful, NC if error
    // Modifies: ZP.TOP*, ZP.LTOP0-3, A, X, Y
    // Preserves: All other registers and zero page locations
    const string executeToLongTrace = "TOLONG";
    executeToLong()
    {
#ifdef BASICLONG        
        
    #ifdef TRACE
        LDA #(executeToLongTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeToLongTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
        
        loop // Single exit block
        {
            // Pop the value to convert from stack
            Stacks.PopTop();
            Error.CheckError();
            if (NC) { break; }

            // Input: ZP.TOPL, ZP.TOPH, ZP.TOPT
            // Output: ZP.LTOP0-3, ZP.TOPT
            Long.TopToLong();
            
            // Push the converted LONG value back to stack
            // type in ZP.TOPT
            Long.PushTop();
            
            SEC  // Set C (successful conversion)
            break;
        } // Single exit block
        
    #ifdef TRACE
        LDA #(executeToLongTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeToLongTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
        
#else     
        Error.InternalError(); BIT ZP.EmulatorPCL   
#endif        
    }
}
