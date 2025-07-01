using System;
using System.Collections;
using System.Collections.Generic;
using HopperRuntime.SystemLibrary;

namespace HopperRuntime.Core
{
    /// <summary>
    /// The Hopper Virtual Machine - executes Hopper bytecode programs
    /// Features a stack-based architecture with 32-bit slots for efficiency on modern platforms
    /// </summary>
    public class HopperVM
    {
        #region Private Fields

        private readonly VMStack stack = new VMStack();

        private byte[] program = new byte[0];
        private byte[] constants = new byte[0];
        private Dictionary<uint, uint> methodTable = new Dictionary<uint, uint>();

        private uint pc;  // Program counter
        private uint bp;  // Base pointer for locals
        private readonly Stack<uint> callStack = new Stack<uint>();
        private readonly Stack<uint> basePointers = new Stack<uint>();

        private SystemCallDispatcher? systemCallDispatcher;

        #endregion

        #region Public Interface

        /// <summary>
        /// Load a Hopper program from bytecode for execution
        /// </summary>
        /// <param name="bytecode">Raw bytecode from a Hopper binary file</param>
        public void LoadProgram(byte[] bytecode)
        {
            // Use BinaryLoader to parse the file
            var loadedProgram = BinaryLoader.LoadProgram(bytecode);

            // Extract the components
            program = loadedProgram.Program;
            constants = loadedProgram.Constants;
            methodTable = loadedProgram.MethodTable;

            // Initialize system call dispatcher with VMStack
            var screenUnit = new ScreenUnit();
            var stringUnit = new StringUnit();
            systemCallDispatcher = new SystemCallDispatcher(stack, constants, screenUnit, stringUnit);

            // Start execution at the beginning of the extracted program
            pc = 0;

            if (Program.TraceEnabled)
            {
                Console.WriteLine(BinaryLoader.GetProgramSummary(loadedProgram));
            }
        }

        /// <summary>
        /// Execute the loaded program until completion
        /// </summary>
        public void Run()
        {
            while (pc < program.Length)
            {
                ExecuteInstruction();
            }
        }

        /// <summary>
        /// Get the current state of the VM for debugging
        /// </summary>
        public string GetState()
        {
            return $"PC: 0x{pc:X4}, BP: {bp}, Stack: {stack.Count} items";
        }

        /// <summary>
        /// Get detailed stack trace for debugging
        /// </summary>
        public string GetStackTrace()
        {
            return stack.GetStackTrace();
        }

        #endregion

        #region Instruction Execution

        /// <summary>
        /// Execute a single instruction at the current program counter
        /// </summary>
        private void ExecuteInstruction()
        {
            bool copyNextPop = false;
            if (pc == 0x024B)
            {
                //int wtf = 0;
            }
            
            OpCode opcode = (OpCode)program[pc++];

            if (Program.TraceEnabled)
            {
                // Read operands for formatting without advancing PC
                byte[]? operands = null;
                int operandBytes = OpCodeInfo.GetOperandBytes(opcode);
                if (operandBytes > 0 && pc + operandBytes <= program.Length)
                {
                    operands = new byte[operandBytes];
                    Array.Copy(program, pc, operands, 0, operandBytes);
                }

                Console.WriteLine($"{pc - 1:X4}: {OpCodeInfo.FormatInstruction(opcode, operands)}");
            }


            switch (opcode)
            {
                // Add these 16 cases to your ExecuteInstruction() switch statement:

                case OpCode.NOP:
                    break;

                case OpCode.SYSCALL:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 3); // "3" means 'Pop the overload'
                    }
                    break;
                case OpCode.SYSCALL0:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 0);
                    }
                    break;
                case OpCode.SYSCALL1:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 1);
                    }
                    break;
                case OpCode.SYSCALL2:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 2);
                    }
                    break;

                case OpCode.SYSCALL00:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 0);
                        syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 0);
                    }
                    break;
                case OpCode.SYSCALL01:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 0);
                        syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 1);
                    }
                    break;
                case OpCode.SYSCALL10:
                    {
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 1);
                        syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 0);
                    }
                    break;

                case OpCode.SYSCALLB0:
                    {
                        ushort value = program[pc++];
                        stack.Push(StackValue.FromUInt(value));
                        byte syscallId = program[pc++];
                        HandleSyscall((byte)syscallId, 0);
                    }
                    break;

                case OpCode.CALL:           // 0x34 - Call function (method table lookup)
                    {
                        ushort methodIndex = ReadUInt16();
                        if (methodTable.TryGetValue(methodIndex, out uint address))
                        {
                            callStack.Push(pc);
                            pc = address;
                        }
                        else
                        {
                            throw new InvalidOperationException($"Method index {methodIndex:X4} not found in method table");
                        }
                    }
                    break;

                case OpCode.CALLI:          // 0x6A - Call function indirectly (address from stack)
                    {
                        ushort address = ReadUInt16();
                        callStack.Push(pc);
                        pc = address;
                    }
                    break;

                case OpCode.CALLREL:
                    {
                        var methodIndex = stack.Pop();
                        if (methodTable.TryGetValue(methodIndex.UIntValue, out uint address))
                        {
                            callStack.Push(pc);
                            pc = address;
                        }
                        else
                        {
                            throw new InvalidOperationException($"Method index {methodIndex:X4} not found in method table");
                        }
                    }
                    break;

                case OpCode.ENTER:          // 0x49 - Enter function (set up stack frame)
                    {
                        basePointers.Push(bp);
                        bp = (uint)stack.Count;
                    }
                    break;
                case OpCode.ENTERB:
                    {
                        byte zeroes = program[pc++];
                        basePointers.Push(bp);
                        bp = (uint)stack.Count;
                        for (uint i = 0; i < zeroes; i++)
                        {
                            stack.Push(StackValue.FromUInt(0));
                        }
                    }
                    break;
                    

                case OpCode.JZ:             // 0x31 - Jump if zero (16-bit address)
                    {
                        long address = (long)pc + (short)ReadUInt16() - 3;
                        var condition = stack.Pop();
                        if (condition.UIntValue == 0)
                        {
                            pc = (uint)address;
                        }
                    }
                    break;
                case OpCode.JZB:
                    {
                        long address = (long)pc + (sbyte)program[pc++] - 3;
                        var condition = stack.Pop();
                        if (condition.UIntValue == 0)
                        {
                            pc = (uint)address;
                        }
                    }
                    break;
                    

                case OpCode.JNZ:            // 0x32 - Jump if not zero (16-bit address)
                    {
                        long address = (short)ReadUInt16() + (long)pc - 3;
                        var condition = stack.Pop();
                        if (condition.UIntValue != 0)
                        {
                            pc = (uint)address;
                        }
                    }
                    break;
                case OpCode.JNZB:
                    {
                        long address = (long)pc + (sbyte)program[pc++] - 3;
                        var condition = stack.Pop();
                        if (condition.UIntValue != 0)
                        {
                            pc = (uint)address;
                        }
                    }
                    break;

                case OpCode.J:              // 0x33 - Unconditional jump (16-bit address)
                    {
                        long address = (short)ReadUInt16() + (long)pc - 3;
                        pc = (uint)address;
                    }
                    break;
                case OpCode.JB:
                    {
                        long address = (long)pc + (sbyte)program[pc++] - 3;
                        pc = (uint)address;
                    }
                    break;

                case OpCode.RET:            // 0x35 - Return from function (16-bit cleanup)
                    {
                        ushort cleanup = ReadUInt16();
                        stack.PopMultiple(cleanup);

                        if (callStack.Count > 0)
                        {
                            pc = callStack.Pop();
                            if (basePointers.Count > 0)
                            {
                                bp = basePointers.Pop();
                            }
                        }
                        else
                        {
                            pc = (uint)program.Length;
                        }
                    }
                    break;

                case OpCode.DECSP:
                    {
                        byte cleanup = program[pc++];
                        stack.PopMultiple(cleanup);
                    }
                    break;
                case OpCode.SWAP:
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(top);
                        stack.Push(next);
                    }
                    break;
                case OpCode.CAST:
                    {
                        ValueType toType = (ValueType)program[pc++];
                        stack.Cast(toType);
                    }
                    break;


                case OpCode.RETRES:         // 0x36 - Return with result (16-bit cleanup)
                    {
                        ushort cleanup = ReadUInt16();
                        var result = stack.Pop();
                        stack.PopMultiple(cleanup);
                        stack.Push(result);

                        if (callStack.Count > 0)
                        {
                            pc = callStack.Pop();
                            if (basePointers.Count > 0)
                                bp = basePointers.Pop();
                        }
                        else
                        {
                            pc = (uint)program.Length;
                        }
                    }
                    break;

                case OpCode.PUSHI:
                    {
                        ushort value = ReadUInt16();
                        stack.Push(StackValue.FromUInt(value));
                    }
                    break;
                case OpCode.PUSHIB:
                    {
                        ushort value = program[pc++];
                        stack.Push(StackValue.FromUInt(value));
                    }
                    break;
                case OpCode.PUSHI0:
                    {
                        stack.Push(StackValue.FromUInt(0));
                    }
                    break;
                case OpCode.PUSHI1:
                    {
                        stack.Push(StackValue.FromUInt(1));
                    }
                    break;
                case OpCode.PUSHIM1:
                    {
                        stack.Push(StackValue.FromUInt(0xFFFF));
                    }
                    break;

                case OpCode.POPLOCAL:
                    {
                        short signedOffset = (short)ReadUInt16();
                        if (copyNextPop)
                        {
                            // TODO
                        }
                        var value = stack.Pop();
                        stack.SetLocal(bp, signedOffset, value);
                    }
                    break;
                case OpCode.POPLOCALB00:
                    {
                        short signedOffset = 0;
                        if (copyNextPop)
                        {
                            // TODO
                        }
                        var value = stack.Pop();
                        stack.SetLocal(bp, signedOffset, value);
                    }
                    break;

                case OpCode.POPCOPYLOCALB00:
                    {
                        short signedOffset = 0;
                        
                        // TODO : copyNextPop
                        
                        var value = stack.Pop();
                        stack.SetLocal(bp, signedOffset, value);
                    }
                    break;
                case OpCode.POPLOCALB01:
                    {
                        short signedOffset = 1;
                        if (copyNextPop)
                        {
                            // TODO
                        }
                        var value = stack.Pop();
                        stack.SetLocal(bp, signedOffset, value);
                    }
                    break;

                case OpCode.PUSHLOCAL:
                    {
                        short signedOffset = (short)ReadUInt16();
                        stack.Push(stack.GetLocal(bp, signedOffset));
                    }
                    break;
                case OpCode.PUSHLOCALB:
                    {
                        short signedOffset = (sbyte)program[pc++];
                        stack.Push(stack.GetLocal(bp, signedOffset));
                    }
                    break;
                case OpCode.PUSHLOCALBB:
                    {
                        short signedOffset = (sbyte)program[pc++];
                        stack.Push(stack.GetLocal(bp, signedOffset));
                        signedOffset = (sbyte)program[pc++];
                        stack.Push(stack.GetLocal(bp, signedOffset));
                    }
                    break;
                case OpCode.PUSHLOCALB00:
                    {
                        short signedOffset = 0;
                        stack.Push(stack.GetLocal(bp, signedOffset));
                    }
                    break;
                case OpCode.PUSHLOCALB01:
                    {
                        short signedOffset = 1;
                        stack.Push(stack.GetLocal(bp, signedOffset));
                    }
                    break;

                case OpCode.POPGLOBAL:
                    {
                        ushort globalAddress = ReadUInt16();
                        if (copyNextPop)
                        {
                            // TODO
                        }

                        var value = stack.Pop();
                        stack.SetGlobal(globalAddress, value);
                    }
                    break;
                case OpCode.POPGLOBALB:
                    {
                        ushort globalAddress = program[pc++];
                        if (copyNextPop)
                        {
                            // TODO
                        }

                        var value = stack.Pop();
                        stack.SetGlobal(globalAddress, value);
                    }
                    break;

                case OpCode.PUSHGLOBAL:     // 0x3D - Push global variable (16-bit address)
                    {
                        ushort globalAddress = ReadUInt16();
                        stack.Push(stack.GetGlobal(globalAddress));
                    }
                    break;

                case OpCode.PUSHSTACKADDR:  // 0x3E - Push stack address (16-bit offset)
                    {
                        ushort offset = ReadUInt16();
                        uint stackAddress = bp + offset;
                        stack.Push(StackValue.FromUInt(stackAddress));
                    }
                    break;

                case OpCode.COPYNEXTPOP:    
                    {
                        copyNextPop = true;
                    }
                    break;

                case OpCode.PUSHD:          // 0x60 - Push double/delegate (16-bit value)
                    {
                        uint value = ReadUInt16();
                        stack.Push(StackValue.FromUInt(value));
                    }
                    break;

                case OpCode.LT:             // 0x8C - Less than (unsigned)
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(next.UIntValue < top.UIntValue ? 1 : 0)));
                    }
                    break;
                case OpCode.LE:             // 0x90 - Less than or equal (unsigned)
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(next.UIntValue <= top.UIntValue ? 1 : 0)));
                    }
                    break;
                case OpCode.EQ:             // 0x92 - equal
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(next.UIntValue == top.UIntValue ? 1 : 0)));
                    }
                    break;

                case OpCode.NE:             // 0x94 - Not equal
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(next.UIntValue != top.UIntValue ? 1 : 0)));
                    }
                    break;

                case OpCode.SUB:            // 0x82 - Subtract two stack values
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt(next.UIntValue - top.UIntValue));
                    }
                    break;
                case OpCode.ADD:
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt(next.UIntValue + top.UIntValue));
                    }
                    break;
                case OpCode.MUL:
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt(next.UIntValue * top.UIntValue));
                    }
                    break;
                case OpCode.DIV:
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt(next.UIntValue / top.UIntValue));
                    }
                    break;
                case OpCode.MOD:
                    {
                        var top = stack.Pop();
                        var next = stack.Pop();
                        stack.Push(StackValue.FromUInt(next.UIntValue % top.UIntValue));
                    }
                    break;

                



                default:
                    {
                        byte unknownOpcode = (byte)opcode;
                        uint errorPC = pc - 1;

                        // Try to get the name of the unknown opcode
                        string opcodeName = "UNKNOWN";
                        try
                        {
                            if (Enum.IsDefined(typeof(OpCode), unknownOpcode))
                            {
                                opcodeName = ((OpCode)unknownOpcode).ToString();
                            }
                        }
                        catch
                        {
                            opcodeName = "INVALID";
                        }

                        // Try to read potential operands for debugging
                        var operandBytes = new List<byte>();
                        int maxOperands = Math.Min(4, (int)(program.Length - pc)); // Read up to 4 bytes or until end
                        for (int i = 0; i < maxOperands; i++)
                        {
                            operandBytes.Add(program[pc + i]);
                        }

                        // Build detailed error message
                        var errorMessage = new System.Text.StringBuilder();
                        errorMessage.AppendLine($"EXECUTION FAILED: Unimplemented opcode encountered");
                        errorMessage.AppendLine($"  Opcode: 0x{unknownOpcode:X2} ({opcodeName}) (decimal: {unknownOpcode})");
                        errorMessage.AppendLine($"  Location: PC = 0x{errorPC:X4} (decimal: {errorPC})");
                        errorMessage.AppendLine($"  VM State: BP = {bp}, Stack Count = {stack.Count}");

                        // Try to get opcode info if available
                        try
                        {
                            int expectedOperands = OpCodeInfo.GetOperandBytes((OpCode)unknownOpcode);
                            var formattedInstruction = OpCodeInfo.FormatInstruction((OpCode)unknownOpcode, null);
                            errorMessage.AppendLine($"  Expected format: {formattedInstruction}");
                            errorMessage.AppendLine($"  Expected operands: {expectedOperands} bytes");
                        }
                        catch
                        {
                            errorMessage.AppendLine($"  Opcode info: Not available in OpCodeInfo");
                        }

                        if (operandBytes.Count > 0)
                        {
                            errorMessage.AppendLine($"  Next bytes: {string.Join(" ", operandBytes.Select(b => $"0x{b:X2}"))}");
                        }

                        // Show context around the error
                        errorMessage.AppendLine($"  Context:");
                        int contextStart = Math.Max(0, (int)errorPC - 4);
                        int contextEnd = Math.Min(program.Length - 1, (int)errorPC + 4);

                        for (int i = contextStart; i <= contextEnd; i++)
                        {
                            string marker = (i == errorPC) ? " <-- ERROR" : "";
                            errorMessage.AppendLine($"    0x{i:X4}: 0x{program[i]:X2}{marker}");
                        }

                        // Try to suggest similar opcodes using OpCodeInfo methods
                        var allOpcodes = Enum.GetValues<OpCode>();
                        

                        // Show current stack state
                        if (stack.Count > 0)
                        {
                            errorMessage.AppendLine($"  Current stack (top 5 items):");
                            var stackTrace = stack.GetStackTrace(5);
                            errorMessage.AppendLine($"    {stackTrace.Replace("\n", "\n    ")}");
                        }
                        else
                        {
                            errorMessage.AppendLine($"  Stack is empty");
                        }

                        // Show recent call stack if available
                        if (callStack.Count > 0)
                        {
                            errorMessage.AppendLine($"  Call stack depth: {callStack.Count}");
                            var recentCalls = callStack.Take(3).ToArray();
                            errorMessage.AppendLine($"  Recent return addresses: {string.Join(", ", recentCalls.Select(addr => $"0x{addr:X4}"))}");
                        }

                        Console.WriteLine();
                        Console.WriteLine(errorMessage);

                        throw new NotImplementedException();
                    }
            }
        }

        #endregion

        #region Helper Methods

        /// <summary>
        /// Read a 16-bit unsigned integer from the program at the current PC
        /// </summary>
        private ushort ReadUInt16()
        {
            ushort value = (ushort)(program[pc] | (program[pc + 1] << 8));
            pc += 2;
            return value;
        }

        /// <summary>
        /// Handle a system call by delegating to the system call dispatcher
        /// </summary>
        private void HandleSyscall(byte syscallId, byte param)
        {
            systemCallDispatcher?.HandleSyscall(syscallId, param);
        }

        #endregion
    }
}