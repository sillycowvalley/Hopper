unit Executor 
{
   uses "OpCodes"
   uses "Variables"
   uses "Error"
   uses "State"  // Add State dependency
   uses "BasicTypes"
   uses "Instructions"
   uses "ComparisonInstructions"
   uses "BASICSysCalls"
   uses "Tools"
   
   
   friend Functions;
   
   // Memory layout for executor state - BasicExecutorWorkspace (32 bytes)
   const uint executorStartAddrL    = Address.BasicExecutorWorkspace + 9;   // opcode buffer start low
   const uint executorStartAddrH    = Address.BasicExecutorWorkspace + 10;  // opcode buffer start high
   const uint executorEndAddrL      = Address.BasicExecutorWorkspace + 11;  // opcode buffer end low
   const uint executorEndAddrH      = Address.BasicExecutorWorkspace + 12;  // opcode buffer end high
   const uint executorOperandL      = Address.BasicExecutorWorkspace + 13;  // current operand low
   const uint executorOperandH      = Address.BasicExecutorWorkspace + 14;  // current operand high
   const uint executorTokenAddrL    = Address.BasicExecutorWorkspace + 15;  // token fetch addr low
   const uint executorTokenAddrH    = Address.BasicExecutorWorkspace + 16;  // token fetch addr high
   
   // Reset Hopper VM to clean state before execution
   Reset()
   {
       // Reset stacks (already proven to work in ExecuteOpCodes)
       STZ ZP.SP    // Reset value/type stack pointer to 0
       STZ ZP.BP    // Reset base pointer to 0
       STZ ZP.CSP   // Reset call stack pointer to 0
       
       // Push return slot in case we are calling a function to start the VM
       // If not, no biggie - just one wasted stack slot
       LDA #BasicType.VOID
       STA ZP.TOPT
       STZ ZP.TOPL  
       STZ ZP.TOPH
       Stacks.PushTop();
       
       // Clear any error conditions
       Error.ClearError();
       State.SetSuccess();
   }
   
   // Main entry point - Execute compiled opcodes
   // Input: ZP.OpCodeBufferLengthL/H contains opcode buffer length
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
           // Initialize executor state
           InitExecutor();
           State.IsSuccess();
           if (NC) { break; } // Error already set by InitExecutor
           
           // Main execution loop (assume there is at least one opcode)
           loop
           {
               // Fetch and execute next opcode
               FetchOpCode();
               State.IsSuccess(); // expect SystemState.Success to continue
               if (NC) 
               {
                   break; // Error fetching opcode: SystemState.Exiting because EOF success
               }
               
               // Dispatch opcode (A contains opcode value)
               DispatchOpCode(); // expect SystemState.Success to continue
               State.IsSuccess();
               if (NC)
               {
                   // SystemState.Failure - runtime error: Type Mismatch, Overflow, etc ..
                   // SystemState.Return  - function return popped CallStack pointer (CSP) to zero
                   break; 
               }

               // Check if we've reached end of opcodes
               LDA ZP.PCL
               CMP executorEndAddrL
               if (Z)
               {
                   LDA ZP.PCH
                   CMP executorEndAddrH
                   if (Z) 
                   { 
                       // REPL uses this exit - EOF success
                       State.SetExiting();
                       break; 
                   }
               }
               State.IsReturn();
               if (C)
               {
                   break; // other exit conditions like popping the last return address from the callstack
               }
           } // loop
           break;
       } // Single exit block

#ifdef TRACE
       LDA #(strExecuteOpCodes % 256) STA ZP.TraceMessageL LDA #(strExecuteOpCodes / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif

       PLY
       PLX
       PLA
   }
   
   // Initialize executor state from opcode buffer
   // Input: ZP.OpCodeBufferLengthL/H contains opcode buffer length
   // Output: SystemState set (Success or Failure)
   const string initExecutorTrace = "InitExec // Initialize executor state";
   InitExecutor()
   {
#ifdef TRACE
       LDA #(initExecutorTrace % 256) STA ZP.TraceMessageL LDA #(initExecutorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       // Set start address to BasicOpCodeBuffer
       LDA #(Address.BasicOpCodeBuffer % 256)
       STA executorStartAddrL
       STA ZP.PCL // Start execution at beginning
       LDA #(Address.BasicOpCodeBuffer / 256)
       STA executorStartAddrH
       STA ZP.PCH
       
       // Calculate end address = start + length
       CLC
       LDA executorStartAddrL
       ADC ZP.OpCodeBufferLengthL
       STA executorEndAddrL
       LDA executorStartAddrH
       ADC ZP.OpCodeBufferLengthH
       STA executorEndAddrH
       
       // Validate buffer length is not zero
       LDA ZP.OpCodeBufferLengthL
       ORA ZP.OpCodeBufferLengthH
       if (Z)
       {
           Error.InternalError(); BIT ZP.EmulatorPCL
           State.SetFailure();
       }
       else
       {
           State.SetSuccess();
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
           // Check bounds
           LDA ZP.PCL
           CMP executorEndAddrL
           if (Z)
           {
               LDA ZP.PCH
               CMP executorEndAddrH
               if (Z) 
               { 
                   // At end of buffer
                   State.SetExiting();
                   break; 
               }
           }
#ifdef TRACE            
           Trace.IsTracing();
           if (C)
           {
               Trace.PrintIndent();
               LDA ZP.PCH Debug.HOut(); LDA ZP.PCL Debug.HOut();
               LDA #' ' Debug.COut();
           }
#endif

           // Fetch opcode using indirect addressing
           LDY #0
           LDA [ZP.PC], Y
           PHA // Save opcode
           
#ifdef TRACE            
           Trace.IsTracing();
           if (C)
           {
               PHA Debug.HOut(); LDA #' ' Debug.COut(); PLA
               Tools.NL(); 
           }
#endif            
           // Advance PC
           INC ZP.PCL
           if (Z)
           {
               INC ZP.PCH
           }
           
           PLA // Restore opcode to A
           State.SetSuccess();
           break;
       }
       
#ifdef TRACE
       PHA LDA #(fetchOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(fetchOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif
   }
   
   // Fetch single byte operand from buffer
   // Input: ZP.PC points to operand position
   // Output: A contains operand byte, ZP.PC advanced, SystemState set
   const string fetchOperandByteTrace = "FetchByte // Fetch byte operand";
   FetchOperandByte()
   {
#ifdef TRACEVERBOSE
       LDA #(fetchOperandByteTrace % 256) STA ZP.TraceMessageL LDA #(fetchOperandByteTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       
       loop
       {
           // Check bounds
           LDA ZP.PCL
           CMP executorEndAddrL
           if (Z)
           {
               LDA ZP.PCH
               CMP executorEndAddrH
               if (Z) 
               { 
                   // At end of buffer - should exit successfully
                   Error.InternalError(); BIT ZP.EmulatorPCL
                   State.SetFailure();
                   break; 
               }
           }
           // Fetch operand
           LDY #0
           LDA [ZP.PC], Y
               
           PHA // Save operand
           
           // Advance PC
           INC ZP.PCL
           if (Z)
           {
               INC ZP.PCH
           }
           
           PLA // Restore operand to A
           State.SetSuccess();
           break;
       } // loop exit
       
#ifdef TRACEVERBOSE
       PHA LDA #(fetchOperandByteTrace % 256) STA ZP.TraceMessageL LDA #(fetchOperandByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif
   }
   
   // Fetch word operand from buffer (little-endian)
   // Input: ZP.PC points to operand position
   // Output: executorOperandL/H contains word, ZP.PC advanced by 2, SystemState set
   const string fetchOperandWordTrace = "FetchWord // Fetch word operand";
   FetchOperandWord()
   {
#ifdef TRACEVERBOSE
       PHA LDA #(fetchOperandWordTrace % 256) STA ZP.TraceMessageL LDA #(fetchOperandWordTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
       
       loop
       {
           // Fetch low byte
           FetchOperandByte();
           State.CanContinue();
           if (NC) { break; }
           STA executorOperandL
           
           // Fetch high byte
           FetchOperandByte();
           State.CanContinue();
           if (NC) { break; }
           STA executorOperandH
           
           State.SetSuccess();
           break;
       }
       
#ifdef TRACEVERBOSE
       LDA #(fetchOperandWordTrace % 256) STA ZP.TraceMessageL LDA #(fetchOperandWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // Dispatch opcode to appropriate handler
   // Input: A contains opcode value
   // Output: SystemState set based on execution result
   const string dispatchOpCodeTrace = "Dispatch // Dispatch opcode";
   DispatchOpCode()
   {
       TAY // for jump table optimization
#ifdef TRACEVERBOSE
       LDA #(dispatchOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(dispatchOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       // Use switch statement for opcode dispatch
       // Register Y contains the opcode value
       switch (Y)
       {
           // === NO OPERAND OPCODES (0x00-0x3F) ===
           
           // Arithmetic operations
           case OpCodeType.ADD:
           {
               Instructions.Addition();
           }
           case OpCodeType.SUB:
           {
               Instructions.Subtraction();
           }
           case OpCodeType.MUL:
           {
               Instructions.Multiply();
           }
           case OpCodeType.DIV:
           {
               Instructions.Divide();
           }
           case OpCodeType.MOD:
           {
               Instructions.Modulo();
           }
           case OpCodeType.NEG:
           {
               Instructions.UnaryMinus();
           }
           
           // Bitwise operations
           case OpCodeType.BITWISE_AND:
           {
               Instructions.BitwiseAnd();
           }
           case OpCodeType.BITWISE_OR:
           {
               Instructions.BitwiseOr();
           }
           
           // Logical operations (BIT operands only)
           case OpCodeType.LOGICAL_AND:
           {
               Instructions.LogicalAnd();
           }
           case OpCodeType.LOGICAL_OR:
           {
               Instructions.LogicalOr();
           }
           case OpCodeType.LOGICAL_NOT:
           {
               Instructions.LogicalNot();
           }
           
           // Comparison operations (all return BIT)
           case OpCodeType.EQ:
           {
               ComparisonInstructions.Equal();
           }
           case OpCodeType.NE:
           {
               ComparisonInstructions.NotEqual();
           }
           case OpCodeType.LT:
           {
               ComparisonInstructions.LessThan();
           }
           case OpCodeType.GT:
           {
               ComparisonInstructions.GreaterThan();
           }
           case OpCodeType.LE:
           {
               ComparisonInstructions.LessEqual();
           }
           case OpCodeType.GE:
           {
               ComparisonInstructions.GreaterEqual();
           }
           
           // Function operations
           case OpCodeType.ENTER:
           {
               executeEnter();
           }
           case OpCodeType.RETURN:
           {
               executeReturn();
           }
           case OpCodeType.RETURNVAL:
           {
               executeReturnVal();
           }
           
           // Stack manipulation
           case OpCodeType.DECSP:
           {
               executeDecSp();
           }
           case OpCodeType.DUP:
           {
               executeDup();
           }
           case OpCodeType.NOP:
           {
               executeNop();
           }
           
           case OpCodeType.PUSH0:
           {
               executePush0();
           }
           case OpCodeType.PUSH1:
           {
               executePush1();
           }
           case OpCodeType.PUSHVOID:
           {
               executePushVoid();
           }
           
           // === ONE BYTE OPERAND OPCODES (0x40-0x7F) ===
           
           // Literal pushes (8-bit)
           case OpCodeType.PUSHBIT:
           {
               executePushBit();
           }
           case OpCodeType.PUSHBYTE:
           {
               executePushByte();
           }
           case OpCodeType.PUSHCSTRING:
           {
               executePushCString();
           }
           
           // Variable operations
           case OpCodeType.PUSHGLOBAL:
           {
               executePushGlobal();
           }
           case OpCodeType.PUSHLOCAL:
           {
               executePushLocal();
           }
           case OpCodeType.POPGLOBAL:
           {
               executePopGlobal();
           }
           case OpCodeType.POPLOCAL:
           {
               executePopLocal();
           }
           
           // Control flow (short jumps)
           case OpCodeType.JUMPB:
           {
               executeJumpB();
           }
           case OpCodeType.JUMPZB:
           {
               executeJumpZB();
           }
           case OpCodeType.JUMPNZB:
           {
               executeJumpNZB();
           }
           
           // Function and system calls
           case OpCodeType.CALL:
           {
               executeCall();
           }
           case OpCodeType.CALLF:
           {
               executeCallF();
           }
           case OpCodeType.SYSCALL:
           {
               ExecuteSysCall();
           }
           
           // === TWO BYTE OPERAND OPCODES (0x80-0xBF) ===
           
           // Literal pushes (16-bit)
           case OpCodeType.PUSHINT:
           {
               executePushInt();
           }
           case OpCodeType.PUSHWORD:
           {
               executePushWord();
           }
           
           // Control flow (long jumps)
           case OpCodeType.JUMPW:
           {
               executeJumpW();
           }
           case OpCodeType.JUMPZW:
           {
               executeJumpZW();
           }
           case OpCodeType.JUMPNZW:
           {
               executeJumpNZW();
           }
           
           default:
           {
               executeNotImplemented();
           }
       }
       
       // Check if any instruction set an error state
       Error.CheckError();
       if (NC) 
       { 
           State.SetFailure(); 
       }
       else
       {
           // Only set success if no error occurred
           State.CanContinue();
           if (C) 
           {
               State.SetSuccess();
           }
           else
           {
               // State was already set to Failure, Return or Exiting by instruction
           }
       }
       
#ifdef TRACEVERBOSE
       LDA #(dispatchOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(dispatchOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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
       Debug.DumpBasicBuffers();
#endif                
       // Unknown opcode
       Error.NotImplemented(); BIT ZP.EmulatorPCL
       State.SetFailure();
       
#ifdef TRACE
       LDA #(executeNotImplementedTrace % 256) STA ZP.TraceMessageL LDA #(executeNotImplementedTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
   
   // === CONTROL FLOW AND STACK MANIPULATION HANDLERS ===
   
   // Common return processing for both RETURN and RETURNVAL opcodes
   // Input: ZP.TOP = return value (16-bit), ZP.TOPT = return type
   //        Operand byte = argument count (number of arguments to clean up)
   // Output: Return value stored in return slot, stack frame restored, execution returned to caller
   // Stack: Removes all arguments and locals, preserves return slot with return value
   // State: Sets State.SetReturn() if returned to entry call, State.SetSuccess() otherwise
   // Modifies: ZP.SP (stack cleanup), ZP.BP (restored), ZP.PC (restored), ZP.CSP (decremented)
   //           Return slot contents (value and type), executorOperand1 (operand fetch)
   // Preserves: All other zero page variables
   // Error: Sets error state via Error.CheckError() if operand fetch fails
   commonReturn()
   {
       loop
       {
           // Fetch cleanup count operand (number of locals)
           FetchOperandWord();
           Error.CheckError();
           if (NC) { break; }
           STA executorOperandL
           
           // Calculate return slot position: BP - (arg_count + 1)
           SEC
           LDA ZP.BP
           SBC executorOperandL         // BP - arg_count
           SEC
           SBC #1                       // BP - arg_count - 1 = return slot position
           TAX                          // X = return slot index
           
           // Store return value in return slot
           LDA ZP.TOPL
           STA Address.ValueStackLSB, X
           LDA ZP.TOPH
           STA Address.ValueStackMSB, X
           LDA ZP.TOPT
           STA Address.TypeStackLSB, X
           
            // Cleanup locals and arguments - set SP to return slot + 1
           CLC
           TXA                          // Return slot index
           ADC #1                       // Return slot + 1
           STA ZP.SP
           
           Stacks.PopBP();
           Stacks.PopPC();
           LDA ZP.CSP
           if (Z) // CallStack pointer == 0?
           {
               State.SetReturn(); // popped back down to entry call
           }
           else
           {
               State.SetSuccess();
           }
           break;
       } // exit loop
   }
   
   // Execute RETURN opcode - return from function
   const string executeReturnTrace = "RETURN // Return from function";
   executeReturn()
   {
#ifdef TRACE
       LDA #(executeReturnTrace % 256) STA ZP.TraceMessageL LDA #(executeReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
       LDA BasicType.VOID
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
       State.SetSuccess();
       
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
       State.SetSuccess();
       
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
       State.SetSuccess();
       
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
       State.SetSuccess();
       
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
       LDA #BasicType.INT
       STA ZP.TOPT
       Stacks.PushTop();
       
       State.SetSuccess();
       
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
       LDA #BasicType.INT
       STA ZP.TOPT
       Stacks.PushTop();
       
       State.SetSuccess();
       
   #ifdef TRACE
       LDA #(executePush1Trace % 256) STA ZP.TraceMessageL LDA #(executePush1Trace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
       LDA #BasicType.VOID
       STA ZP.TOPT
       Stacks.PushTop();
       
       State.SetSuccess();
       
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
       State.CanContinue();
       if (C)
       {
           // Store in ZP.TOP as BIT value (0 or 1)
           STA ZP.TOPL
           LDA #0
           STA ZP.TOPH
           LDA # BasicType.BIT
           Stacks.PushTop();
           
           State.SetSuccess();
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
       State.CanContinue();
       if (C)
       {
           
           // Store in ZP.TOP as BYTE value
           STA ZP.TOPL
           LDA #0
           STA ZP.TOPH
           LDA # BasicType.BYTE
           Stacks.PushTop();
           
           State.SetSuccess();
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
               State.SetFailure();
               break; 
           }
           
           // Store pointer in ZP.TOP as STRING value
           LDA executorOperandL
           STA ZP.TOPL
           LDA executorOperandH
           STA ZP.TOPH
           LDA #BasicType.STRING
           STA ZP.TOPT
           
           // Push to stack with STRING type
           Stacks.PushTop();
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           State.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executePushCStringTrace % 256) STA ZP.TraceMessageL LDA #(executePushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
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
           // Fetch node address (little-endian)
           FetchOperandWord(); // Result in executorOperandL/H
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           // Transfer node address to ZP.IDX
           LDA executorOperandL
           STA ZP.IDXL
           LDA executorOperandH
           STA ZP.IDXH
           
           // Get the variable's value and type
           Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = type
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           // Push value to stack with type
           LDA ZP.TOPT
           Stacks.PushTop(); // Push value and type to stack
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           State.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executePushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executePushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }    
   
   executePushLocal()
   {
       // TODO: Fetch local variable by BP offset and push value
       Error.NotImplemented(); BIT ZP.EmulatorPCL
       State.SetFailure();
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
           // Fetch node address (little-endian)
           FetchOperandWord(); // Result in executorOperandL/H
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           // Transfer node address to ZP.IDX
           LDA executorOperandL
           STA ZP.IDXL
           LDA executorOperandH
           STA ZP.IDXH
           
           // Get the variable's current type for compatibility checking
           Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = type
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           // Pop value from stack (RHS of assignment)
           Stacks.PopTop(); // Result in ZP.TOP (value), ZP.TOPT (type)
           
           // Move LHS type to correct register for CheckRHSTypeCompatibility
           LDA ZP.ACCT
           AND #0x0F  // Extract data type (low nibble)
           STA ZP.NEXTT 
           
           // Check type compatibility for assignment
           Instructions.CheckRHSTypeCompatibility(); // Input: ZP.NEXTT = LHS type, ZP.TOPT = RHS type
           if (NC) 
           { 
               Error.TypeMismatch();
               State.SetFailure();
               break; 
           }
           
           // Store the value to the variable
           Variables.SetValue(); // Input: ZP.IDX = node address, ZP.TOP = value
           Error.CheckError();
           if (NC) 
           { 
               State.SetFailure();
               break; 
           }
           
           State.SetSuccess();
           break;
       }
       
   #ifdef TRACE
       LDA #(executePopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(executePopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
   #endif
   }
   
   executePopLocal()
   {
       // TODO: Pop value and store in local variable by BP offset
       Error.NotImplemented(); BIT ZP.EmulatorPCL
       State.SetFailure();
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
           State.CanContinue();
           if (NC) { break; }
   
           LDA executorOperandL
           STA ZP.TOPL
           LDA executorOperandH
           STA ZP.TOPH
           
   #ifdef TRACEJIT
           LDA #'@' Debug.COut(); 
           LDA executorOperandH Debug.HOut(); 
           LDA executorOperandL Debug.HOut();
   #endif            
           
           // 1. resolve Function <index> to function call <address>
           Functions.Find(); // Input: ZP.TOP = name
           if (NC)
           {
   #ifdef TRACEJIT
               LDA #'?' Debug.COut(); LDA #'F' Debug.COut();
   #endif
               Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
               State.SetFailure();
               break;
           }
#ifdef TRACEJIT
           LDA #' ' Debug.COut(); LDA #'\'' Debug.COut(); Tools.PrintStringTOP(); LDA #'\'' Debug.COut(); LDA #' ' Debug.COut();
           LDA #'=' Debug.COut(); LDA #'\'' Debug.COut(); LDA ZP.IDXH Debug.HOut(); LDA ZP.IDXL Debug.HOut(); LDA #' ' Debug.COut(); 
#endif
           // ZP.IDX = function node address
           Functions.IsCompiled();
           if (NC)
           {
#ifdef TRACEJIT
               Tools.NL(); LDA #' ' Tools.NL(); LDA #'J' Debug.COut(); LDA #'I' Debug.COut(); LDA #'T' Debug.COut();
#endif
               // JIT
               Functions.Compile();
               State.CanContinue();
               if (NC)
               {
                   // handle error
                   break;
               }
           }
           
   #ifdef TRACEJIT
           Tools.NL(); 
           LDA #'P' Debug.COut(); LDA #'A' Debug.COut(); LDA #'T' Debug.COut(); LDA #'C' Debug.COut(); LDA #'H' Debug.COut();
           LDA #':' Debug.COut(); LDA #' ' Debug.COut();
           Debug.XOut(); // IDX
   #endif
           
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
           LDA # OpCodeType.CALLF
           STA [ZP.PC]
           
   #ifdef TRACEJIT
           LDA ZP.PCH Debug.HOut();
           LDA ZP.PCL Debug.HOut();
           LDA #' ' Debug.COut();
           LDY # 0
           LDA [ZP.PC], Y
           Debug.HOut(); LDA #' ' Debug.COut();
           INY
           LDA [ZP.PC], Y
           Debug.HOut(); LDA #' ' Debug.COut();
           INY
           LDA [ZP.PC], Y
           Debug.HOut(); LDA #' ' Debug.COut();
           
   #endif
           
           State.SetSuccess();
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
       State.CanContinue();
       if (C)
       {
           
           // Store in ZP.TOP as INT value
           LDA executorOperandL
           STA ZP.TOPL
           LDA executorOperandH
           STA ZP.TOPH
           LDA # BasicType.INT
           Stacks.PushTop();
           
           State.SetSuccess();
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
       State.CanContinue();
       if (C)
       {
           
           // Store in ZP.TOP as WORD value
           LDA executorOperandL
           STA ZP.TOPL
           LDA executorOperandH
           STA ZP.TOPH
           LDA # BasicType.WORD
           Stacks.PushTop();
           
           State.SetSuccess();
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
       CMP #BasicType.BIT
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
       State.CanContinue();
       if (C)
       {
           // Sign extend byte to word
           signExtendByteToWord();
           
           // Apply offset to PC
           applySignedOffsetToPC();
           
           State.SetSuccess();
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
               State.CanContinue();
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
               State.CanContinue();
               if (NC) { break; }
               // Don't apply offset - just continue
           }
           
           State.SetSuccess();
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
               State.CanContinue();
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
               State.CanContinue();
               if (NC) { break; }
               // Don't apply offset - just continue to next instruction
           }
           
           State.SetSuccess();
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
       State.CanContinue();
       if (C)
       {
           // Operand is already in executorOperandL/H, move to ZP.NEXT
           LDA executorOperandL
           STA ZP.NEXTL
           LDA executorOperandH
           STA ZP.NEXTH
           
           // Apply offset to PC
           applySignedOffsetToPC();
           
           State.SetSuccess();
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
               State.CanContinue();
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
               State.CanContinue();
               if (NC) { break; }
               // Don't apply offset - just continue to next instruction
           }
           
           State.SetSuccess();
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
               State.CanContinue();
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
               State.CanContinue();
               if (NC) { break; }
               // Don't apply offset - just continue to next instruction
           }
           
           State.SetSuccess();
           break;
       }
       
#ifdef TRACE
       LDA #(executeJumpNZWTrace % 256) STA ZP.TraceMessageL LDA #(executeJumpNZWTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
}
