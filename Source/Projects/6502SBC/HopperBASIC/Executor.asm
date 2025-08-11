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
    LoadGlobals()
    {
        // Iterate through all symbols (variables and constants)
        Variables.IterateAll(); // Output: ZP.IDX = first symbol, C set if found
        if (C)
        {
            loop
            {
                if (NC) { SEC break; }  // No more symbols

                // Get symbol's value and type
                Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = dataType (VAR masked away)
                // Get symbol's full type (might have VAR bit)
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType (packed)
                // Store type in type stack (preserving VAR bit for variables)
                LDA ZP.ACCT
                AND # BASICType.MASK // keep VAR when creating the global slots
                Stacks.PushTop();  // type is in A
                // Move to next symbol
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
        }
    }
    
    // Save all global variables from VM stack back to symbol table
    // Called before exit to persist changes to globals (skips constants)
    // Output: All variable values updated from stack
    // Modifies: A, X, Y, ZP.IDX, ZP.TOP, ZP.TOPT, ZP.ACCT
    SaveGlobals()
    {
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
                AND #SymbolType.MASK
                CMP #SymbolType.CONSTANT
                if (Z)  // It's a constant - skip it
                {
                    INY  // Still increment index to stay in sync with stack position
                    Variables.IterateNext();
                    continue;  // Skip to next iteration
                }
                
                // It's a variable - check if type changed (for VAR and STRING handling)
                LDA ZP.ACCT
                AND #BASICType.TYPEMASK
                STA ZP.ACCH  // Save old base type
                
                // Get new type from type stack at this index
                LDA Address.TypeStackLSB, Y
                STA ZP.TOPT
                
                // Check if old type was STRING and type changed
                AND #BASICType.TYPEMASK
                CMP ZP.ACCH
                if (NZ)  // Type changed
                {
                    // If old type was STRING, need to free it
                    LDA ZP.ACCH
                    CMP #BASICType.STRING
                    if (Z)
                    {
                        Variables.GetValue(); // preserves Y, Get old string pointer from symbol table
                        LDA ZP.TOPL
                        ORA ZP.TOPH
                        if (NZ)  // Not null
                        {
                            LDA ZP.TOPL
                            STA ZP.IDXL
                            LDA ZP.TOPH
                            STA ZP.IDXH
                            Memory.Free();  // preserves Y, Free old string
                        }
                    }
                }
                
                // Get value from value stacks at this index
                LDA Address.ValueStackLSB, Y
                STA ZP.TOPL
                LDA Address.ValueStackMSB, Y
                STA ZP.TOPH

                // Set the new value in symbol table
                Variables.SetValue(); // Input: ZP.IDX = node, ZP.TOP = value
                
                // Update type if it's a VAR variable
                LDA Address.TypeStackLSB, Y  // Get type from stack
                AND #BASICType.VAR
                if (NZ)  // VAR variable - update type in symbol table
                {
                    
                    // Need to get the type from the correct stack position
                    LDA Address.TypeStackLSB, Y  // Get from stack using index
                    PHY
                    LDY #Objects.snType
                    STA [ZP.IDX], Y
                    PLY
                }
                
                // Move to next symbol
                INY  // Increment index for next stack position
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
        }
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
           if (BBS4, ZP.FLAGS) // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
           {
               Executor.LoadGlobals();
               Error.CheckError();
               if (NC) 
               { 
                  States.SetFailure();
                  break;
               }
           }
           // Initialize executor state
           InitExecutor();
           if (NC) { break; } // empty opcode stream -> State.Failure set
           
           // Main execution loop (assume there is at least one opcode)
           loop
           {
                // Fetch and execute next opcode
                FetchOpCode(); // -> A
                DispatchOpCode(); // expect State.Success to continue
                
#ifdef TRACEEXE                
                Debug.CompactStack();
#endif
                // Shortcut to Mushrooms:
                LDA ZP.LastErrorL
                ORA ZP.LastErrorH
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
                Error.CheckError();
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
                // State.Success - continue executing
                LDA ZP.SerialBreakFlag
                if (NZ) 
                {
                    Error.Break();  // "BREAK" error message
                    States.SetFailure();
                    CLC
                    break;
                }
                // State.Success - get another opcode ..
           } // loop
           break;
       } // Single exit block
       if (BBS4, ZP.FLAGS) // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
       {
           Executor.SaveGlobals();
       }
       
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
        LDA ZP.OpCodeBufferContentSizeL
        ORA ZP.OpCodeBufferContentSizeH
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
   
   // Fetch next opcode from buffer
   // Input: ZP.PC points to current position
   // Output: A contains opcode, ZP.PC advanced, SystemState set
   const string fetchOpCodeTrace = "FetchOp // Fetch next opcode";
   FetchOpCode()
   {
#ifdef TRACE
       LDA #(fetchOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(fetchOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Fetch opcode using indirect addressing
           LDA [ZP.PC]
           
           // Advance PC
           INC ZP.PCL
           if (Z)
           {
               INC ZP.PCH
           }
           
           break;
       }
       
#ifdef TRACE
       PHA LDA #(fetchOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(fetchOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
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
PHA PHX
Space();  HOut();
PLX PLA
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
PHA PHX
Space(); LDA executorOperandH HOut(); LDA executorOperandL HOut();
PLX PLA
#endif           
           break;
       }
   }
   
   // Dispatch opcode to appropriate handler
   // Input: A contains opcode value
   // Output: SystemState set based on execution result
   DispatchOpCode()
   {
       TAY // for jump table optimization
#ifdef TRACEEXE
PHA PHX
NL(); DecPC(); LDA ZP.PCH HOut(); LDA ZP.PCL HOut(); IncPC(); Space(); TYA TAX HOut(); Space(); OpCodes.ToString(); PrintStringSTR();Space(); 
PLX PLA
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
               executeDecSp();
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
   // Error: Sets error state via Error.CheckError() if operand fetch fails
   commonReturn()
   {
       loop
       {
           // Fetch cleanup count operand (number of locals)
           
           FetchOperandByte();
           States.CanContinue();
           if (NC)
           {
               break;
           }
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
       LDA BASICType.VOID
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
       Stacks.PopTop();
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
   
   // Execute DECSP opcode - decrement stack pointer
   const string executeDecSpTrace = "DECSP // Decrement stack pointer";
   executeDecSp()
   {
#ifdef TRACE
       LDA #(executeDecSpTrace % 256) STA ZP.TraceMessageL LDA #(executeDecSpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Decrement stack pointer (discard top value)
       DEC ZP.SP
       States.SetSuccess();
       
#ifdef TRACE
       LDA #(executeDecSpTrace % 256) STA ZP.TraceMessageL LDA #(executeDecSpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
       Stacks.PopTop(); // Get top value in ZP.TOP and ZP.TOPT
       LDA ZP.TOPT
       Stacks.PushTop(); // Push it back
       LDA ZP.TOPT  
       Stacks.PushTop(); // Push duplicate
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
       Stacks.PushTop();
       
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
       Stacks.PushTop();
       
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
       STZ ZP.TOPL
       STZ ZP.TOPH
       LDA # (BASICType.VAR|BASICType.INT)
       STA ZP.TOPT
       Stacks.PushTop();
       
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
       Stacks.PushTop();
       
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
       
       // Fetch operand byte
       FetchOperandByte();
       States.CanContinue();
       if (C)
       {
           // Store in ZP.TOP as BIT value (0 or 1)
           STA ZP.TOPL
           LDA #0
           STA ZP.TOPH
           LDA # BASICType.BIT
           Stacks.PushTop();
           
           States.SetSuccess();
       }
       
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
    
       // Fetch operand byte
       FetchOperandByte();
       States.CanContinue();
       if (C)
       {
           
           // Store in ZP.TOP as BYTE value
           STA ZP.TOPL
           LDA #0
           STA ZP.TOPH
           LDA # BASICType.BYTE
           Stacks.PushTop();
           
           States.SetSuccess();
       }
       
#ifdef TRACE
       LDA #(executePushByteTrace % 256) STA ZP.TraceMessageL LDA #(executePushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
           Error.CheckError();
           if (NC) 
           { 
               States.SetFailure();
               break; 
           }
#ifdef DEBUG
// XIDHERE xS    
//NL(); LDA #'x' COut(); LDA #'S' COut(); LDA #',' COut();
//Space(); LDA ZP.XIDH HOut(); LDA ZP.XIDL HOut();
//Space(); LDA executorOperandH HOut(); LDA executorOperandL HOut();
#endif
           // Store pointer in ZP.TOP as STRING value
           CLC
           LDA executorOperandL
           ADC ZP.XIDL
           STA ZP.TOPL
           LDA executorOperandH
           ADC ZP.XIDH
           STA ZP.TOPH
#ifdef DEBUG
//Space(); LDA ZP.TOPH HOut(); LDA ZP.TOPL HOut();
#endif    
           LDA #BASICType.STRING
           STA ZP.TOPT
           
           // Push to stack with STRING type
           Stacks.PushTop();
           Error.CheckError();
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
           States.CanContinue();
           if (NC) { break; }
#ifdef DEBUG
// XIDHERE xC        
//NL(); LDA #'x' COut(); LDA #'C' COut(); LDA #',' COut();
//Space(); LDA ZP.XIDH HOut(); LDA ZP.XIDL HOut();
//Space(); LDA executorOperandH HOut(); LDA executorOperandL HOut();
#endif
           CLC
           LDA executorOperandL
           ADC ZP.XIDL
           STA ZP.TOPL
           LDA executorOperandH
           ADC ZP.XIDH
           STA ZP.TOPH
#ifdef DEBUG
//Space(); LDA ZP.TOPH HOut(); LDA ZP.TOPL HOut();
#endif
           // 1. resolve Function <index> to function call <address>
           Functions.Find(); // Input: ZP.TOP = name
           if (NC)
           {

//NL(); LDA executorOperandH HOut(); LDA executorOperandL HOut();
//NL(); LDA ZP.XIDH HOut(); LDA ZP.XIDL HOut();
//NL(); LDA ZP.TOPH HOut(); LDA ZP.TOPL HOut();
//NL(); PrintStringTOP();
//DumpBuffers();           
           
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
       States.CanContinue();
       if (C)
       {
           
           // Store in ZP.TOP as INT value
           LDA executorOperandL
           STA ZP.TOPL
           LDA executorOperandH
           STA ZP.TOPH
           LDA # BASICType.INT
           Stacks.PushTop();
           
           States.SetSuccess();
       }
       
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
       States.CanContinue();
       if (C)
       {
           
           // Store in ZP.TOP as WORD value
           LDA executorOperandL
           STA ZP.TOPL
           LDA executorOperandH
           STA ZP.TOPH
           LDA # BASICType.WORD
           Stacks.PushTop();
           
           States.SetSuccess();
       }
       
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
   // Output: ZP.TOP = BIT value (0 or 1), NC if not BIT type
   // Modifies: ZP.TOP, ZP.TOPT, ZP.SP, flags
   popAndValidateBitType()
   {
       Stacks.PopTop();
       
       LDA ZP.TOPT
       CMP #BASICType.BIT
       if (Z)
       {
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
       States.CanContinue();
       if (C)
       {
           // Sign extend byte to word
           signExtendByteToWord();
           
           // Apply offset to PC
           applySignedOffsetToPC();
           
           States.SetSuccess();
       }
       
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
           popAndValidateBitType();
           if (NC) { break; }  // Type error already set
           
           // Check if value is zero (FALSE)
           LDA ZP.TOPL
           if (Z)  // Value is zero/FALSE - take the jump
           {
               // Fetch signed byte operand
               FetchOperandByte();
               States.CanContinue();
               if (NC) { break; }
               
               // Sign extend byte to word
               signExtendByteToWord();
               
               // Apply offset to PC
               applySignedOffsetToPC();
           }
           else  // Value is non-zero/TRUE - skip the jump
           {
               // Still need to fetch and skip the operand
               FetchOperandByte();
               States.CanContinue();
               if (NC) { break; }
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
           popAndValidateBitType();
           if (NC) { break; }  // Type error already set
           
           // Check if value is non-zero (TRUE)
           LDA ZP.TOPL
           if (NZ)  // Value is non-zero/TRUE - take the jump
           {
               // Fetch signed byte operand
               FetchOperandByte();
               States.CanContinue();
               if (NC) { break; }
               
               // Sign extend byte to word
               signExtendByteToWord();
               
               // Apply offset to PC
               applySignedOffsetToPC();
           }
           else  // Value is zero/FALSE - skip the jump
           {
               // Still need to fetch and skip the operand
               FetchOperandByte();
               States.CanContinue();
               if (NC) { break; }
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
       States.CanContinue();
       if (C)
       {
           // Operand is already in executorOperandL/H, move to ZP.NEXT
           LDA executorOperandL
           STA ZP.NEXTL
           LDA executorOperandH
           STA ZP.NEXTH
           
           // Apply offset to PC
           applySignedOffsetToPC();
           
           States.SetSuccess();
       }
       
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
           // Pop and validate BIT type from stack
           popAndValidateBitType();
           if (NC) { break; }  // Type error already set
           
           // Check if value is zero (FALSE)
           LDA ZP.TOPL           
           if (Z)  // Value is zero/FALSE - take the jump
           {
               // Fetch 16-bit signed operand
               FetchOperandWord();
               States.CanContinue();
               if (NC) { break; }
               
               // Operand is already in executorOperandL/H, move to ZP.NEXT
               LDA executorOperandL
               STA ZP.NEXTL
               LDA executorOperandH
               STA ZP.NEXTH
               
               // Apply offset to PC
               applySignedOffsetToPC();
           }
           else  // Value is non-zero/TRUE - skip the jump
           {
               // Still need to fetch and skip the operand
               FetchOperandWord();
               States.CanContinue();
               if (NC) { break; }
               // Don't apply offset - just continue to next instruction
           }
           
           States.SetSuccess();
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
           popAndValidateBitType();
           if (NC) { break; }  // Type error already set
           
           // Check if value is non-zero (TRUE)
           LDA ZP.TOPL
           if (NZ)  // Value is non-zero/TRUE - take the jump
           {
               // Fetch 16-bit signed operand
               FetchOperandWord();
               States.CanContinue();
               if (NC) { break; }
               
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
               States.CanContinue();
               if (NC) { break; }
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
           // Fetch index operand (single byte)
           FetchOperandByte(); // Result in A -> always Success
           
           TAY  // Y = global index
           
           // Get global type from type stack
           LDA Address.TypeStackLSB, Y
           STA ZP.TOPT
           
           // Check if this is a VAR type
           AND #BASICType.VAR
           if (NZ)  // VAR type - strip VAR bit for runtime use
           {
               LDA ZP.TOPT
               AND #BASICType.TYPEMASK  // Strip VAR bit to get actual runtime type
               STA ZP.TOPT
           }
           
           // Get value from value stacks
           LDA Address.ValueStackLSB, Y
           STA ZP.TOPL
           LDA Address.ValueStackMSB, Y  
           STA ZP.TOPH
                 
           // Push value to stack with type
           LDA ZP.TOPT
           Stacks.PushTop(); // Push value and type to stack -> always Success
           
           States.SetSuccess();
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
            // Fetch index operand (single byte)
            FetchOperandByte(); // Result in A -> always Success
            
            TAY  // X = global index
           
            // Get global type from type stack
            LDA Address.TypeStackLSB, Y
            STA ZP.ACCT
           
            // Pop value from stack (RHS of assignment)
            Stacks.PopTop(); // Uses X, Result in ZP.TOP (value), ZP.TOPT (type)
            
            // Check if variable has VAR bit set
            LDA ZP.ACCT
            AND #BASICType.VAR
            if (Z) // Non-VAR variable - use normal type checking
            {
                // Move LHS type to correct register for CheckRHSTypeCompatibility
                LDA ZP.ACCT
                STA ZP.NEXTT 

                // VAR slots should never be popped directly but there is a special case for
                // global shadow variables in FOR iterators (where we are popping and actual
                // variable slot rather than stack data). Regular stack data should NEVER have
                // VAR (so most of the time this should be a NOP)
                LDA ZP.TOPT
                AND # BASICType.TYPEMASK  // Extract data type
                STA ZP.TOPT 
                
                // Check type compatibility for assignment
                Instructions.CheckRHSTypeCompatibility(); // preserves Y, Input: ZP.NEXTT = LHS type, ZP.TOPT = RHS type
                if (NC) 
                { 
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break; 
                }
                // keep LHS type when writing back
                LDA ZP.NEXTT
                STA ZP.TOPT
            }
            else  // VAR variable
            {
                // For VAR variables, update underlying type but keep VAR bit
                LDA ZP.TOPT
                ORA #BASICType.VAR           // Add VAR bit
                STA Address.TypeStackLSB, Y  // Update stored type
                STA ZP.TOPT                  // This is what we'll use for storage
            }

            // Store value to global slot
            LDA ZP.TOPT
            STA Address.TypeStackLSB, Y
            LDA ZP.TOPL
            STA Address.ValueStackLSB, Y
            LDA ZP.TOPH
            STA Address.ValueStackMSB, Y
            States.SetSuccess();
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
            TAY                     // Y = stack position
            
            // get the type of the variable slot
            LDA Address.TypeStackLSB, Y
            STA ZP.ACCT
            
            // Pop value from stack
            // Stacks.PopTop(); // Result in ZP.TOP, ZP.TOPT
            DEC ZP.SP
            LDX ZP.SP
            LDA Address.ValueStackLSB, X
            STA ZP.TOPL
            LDA Address.ValueStackMSB, X
            STA ZP.TOPH
            LDA Address.TypeStackLSB, X
            STA ZP.TOPT
            
            // Check if variable has VAR bit set
            LDA ZP.ACCT
            AND #BASICType.VAR
            if (Z) // Non-VAR variable - use normal type checking
            {
                // Move LHS type to correct register for CheckRHSTypeCompatibility
                LDA ZP.ACCT
                AND #BASICType.MASK  // Extract data type
                STA ZP.NEXTT 
                
                // Check type compatibility for assignment
                Instructions.CheckRHSTypeCompatibility(); // Input: ZP.NEXTT = LHS type, ZP.TOPT = RHS type
                if (NC) 
                { 
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break; 
                }
            }
            else
            {
                // VAR type: keep the VAR bit in the slot
                LDA ZP.TOPT
                ORA #BASICType.VAR
                STA ZP.TOPT
            }
            
            // Store to calculated stack position
            LDA ZP.TOPL
            STA Address.ValueStackLSB, Y
            LDA ZP.TOPH
            STA Address.ValueStackMSB, Y
            LDA ZP.TOPT
            STA Address.TypeStackLSB, Y
            
            States.SetSuccess();
            
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
        
        LDA Address.ValueStackLSB, Y
        STA Address.ValueStackLSB, X
        LDA Address.ValueStackMSB, Y
        STA Address.ValueStackMSB, X
        LDA Address.TypeStackLSB, Y
        AND #BASICType.TYPEMASK  // Strip VAR bit 
        STA Address.TypeStackLSB, X
        
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
            States.CanContinue();
            if (NC) { break; }
            STA Executor.executorOperandBP  // Save iterator offset
            
            // Fetch forward jump offset (16-bit)
            FetchOperandWord(); // Result in executorOperandL/H
            States.CanContinue();
            if (NC) { break; }
            
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
            Stacks.PushTop();   // Push iterator => NEXT slot
            LDA ZP.NEXTT
            Stacks.PushNext();  // Push TO       => TOP slot
            
                
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
                Stacks.PopA();
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
                Stacks.PopA();   
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
            States.CanContinue();
            if (NC) { break; }
            STA Executor.executorOperandBP  // Save iterator offset
            
            // Fetch backward jump offset (16-bit)
            FetchOperandWord(); // Result in executorOperandL/H
            States.CanContinue();
            if (NC) { break; }
            
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
            Stacks.PushTop();   // Push iterator => NEXT slot
            LDA ZP.NEXTT
            Stacks.PushNext();  // Push STEP     => TOP slot
            Instructions.Addition();  // Handles signed/unsigned, type checking, preserves X
            PHX Stacks.PopTop(); PLX
            
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
            Stacks.PushTop();   // Push iterator => NEXT slot
            LDA ZP.NEXTT
            Stacks.PushNext();  // Push       TO => TOP slot
            
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
                Stacks.PopA();   
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
                Stacks.PopA();
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
            // Fetch iterator BP offset
            LDA [ZP.PC]
            INC ZP.PCL
            if (Z) { INC ZP.PCH }
            // BP offset -> A
            
            // Load iterator value
            CLC
            ADC ZP.BP
            TAY
            LDA Address.ValueStackLSB, Y
            STA ZP.TOPL
            LDA Address.ValueStackMSB, Y
            STA ZP.TOPH

            // Store updated iterator++ back with WORD type
            INC ZP.TOPL
            if (Z) 
            {
                INC ZP.TOPH 
                LDA ZP.TOPH
                STA Address.ValueStackMSB, Y
            }
            LDA ZP.TOPL
            STA Address.ValueStackLSB, Y
            LDA #(BASICType.WORD | BASICType.VAR)
            STA Address.TypeStackLSB, Y

                        
            // Get TO value from stack (safe)
            LDA #0xFE  // -2 from SP
            CLC
            ADC ZP.SP
            TAY
            LDA Address.ValueStackLSB, Y
            STA ZP.NEXTL
            LDA Address.ValueStackMSB, Y
            STA ZP.NEXTH
            
            // Inline comparison: TO >= iterator?
            // Compare high bytes first
            LDA ZP.NEXTH  // TO high
            CMP ZP.TOPH   // iterator high
            if (NC)  // TO high < iterator high
            {
                // Exit loop: PC += 2
                CLC
                LDA ZP.PCL
                ADC #2
                STA ZP.PCL
                LDA ZP.PCH
                ADC #0
                STA ZP.PCH
                break;
            }
            if (NZ)  // TO high > iterator high
            {
                // Fetch backward jump offset (16-bit)
                LDY #1
                LDA [ZP.PC]
                STA executorOperandL // Save operand
                LDA [ZP.PC], Y
                STA executorOperandH // Save operand
                
                // Continue loop - jump back
                CLC
                LDA ZP.PCL
                ADC Executor.executorOperandL
                STA ZP.PCL
                LDA ZP.PCH
                ADC Executor.executorOperandH
                STA ZP.PCH
                break;
            }
            
            // High bytes equal, check low bytes
            LDA ZP.NEXTL  // TO low
            CMP ZP.TOPL   // iterator low
            if (NC)  // TO < iterator - exit
            {
                // Exit loop: PC += 2
                CLC
                LDA ZP.PCL
                ADC #2
                STA ZP.PCL
                LDA ZP.PCH
                ADC #0
                STA ZP.PCH
                break;
            }
            
            // Fetch backward jump offset (16-bit)
            LDY #1
            LDA [ZP.PC]
            STA executorOperandL // Save operand
            LDA [ZP.PC], Y
            STA executorOperandH // Save operand
            
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
}
