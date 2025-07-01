using System;
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

        private readonly Stack<StackValue> stack = new Stack<StackValue>();
        private readonly Dictionary<uint, StackValue> globals = new Dictionary<uint, StackValue>();
        private readonly Dictionary<uint, StackValue> locals = new Dictionary<uint, StackValue>();

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

            // Initialize system call dispatcher
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
            return $"PC: 0x{pc:X4}, BP: {bp}, Stack: {stack.Count} items, Globals: {globals.Count}";
        }

        #endregion

        #region Instruction Execution

        /// <summary>
        /// Execute a single instruction at the current program counter
        /// </summary>
        private void ExecuteInstruction()
        {
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
                case OpCode.PUSHI0:
                    stack.Push(StackValue.FromUInt(0));
                    break;

                case OpCode.PUSHI:
                    {
                        uint value = ReadUInt16();
                        stack.Push(StackValue.FromUInt(value));
                    }
                    break;

                case OpCode.POPLOCALB:  // 0x1B - Pop to local variable (byte offset)
                    {
                        byte offset = program[pc++];
                        sbyte signedOffset = (sbyte)offset;
                        var value = stack.Pop();

                        // Store in local variable at BP + signedOffset
                        int stackPosition = (int)bp + signedOffset;
                        var stackArray = stack.ToArray();
                        int index = stackArray.Length - stackPosition;

                        // This is complex - you may want to use a different approach for locals
                        // Consider maintaining a separate locals array per stack frame
                    }
                    break;
                case OpCode.PUSHLOCALB:
                    {
                        byte offset = program[pc++];
                        sbyte signedOffset = (sbyte)offset; // Convert to signed byte

                        // Calculate actual stack position: BP + signedOffset
                        int stackPosition = (int)bp + signedOffset;

                        // Convert to index from bottom of stack
                        var stackArray = stack.ToArray();
                        int index = stackArray.Length - 1 - stackPosition;

                        if (index >= 0 && index < stackArray.Length)
                        {
                            stack.Push(stackArray[index]);
                        }
                        else
                        {
                            throw new InvalidOperationException($"Invalid local/parameter access: offset {signedOffset} from BP {bp}");
                        }
                    }
                    break;

                case OpCode.PUSHLOCALBB:    // 0x56 - Push two local variables
                    {
                        byte offset1 = program[pc++];
                        byte offset2 = program[pc++];

                        // Convert to signed offsets
                        sbyte signedOffset1 = (sbyte)offset1;
                        sbyte signedOffset2 = (sbyte)offset2;

                        // Get the current stack as an array for indexing
                        var stackArray = stack.ToArray();

                        // Calculate actual stack positions: BP + signedOffset
                        int stackPosition1 = (int)bp + signedOffset1;
                        int stackPosition2 = (int)bp + signedOffset2;

                        // Convert to indices from bottom of stack
                        int index1 = stackArray.Length - 1 - stackPosition1;
                        int index2 = stackArray.Length - 1 - stackPosition2;

                        // Validate bounds for first value
                        if (index1 >= 0 && index1 < stackArray.Length)
                        {
                            stack.Push(stackArray[index1]);
                        }
                        else
                        {
                            throw new InvalidOperationException($"Invalid local/parameter access: offset {signedOffset1} from BP {bp}");
                        }

                        // Validate bounds for second value  
                        if (index2 >= 0 && index2 < stackArray.Length)
                        {
                            stack.Push(stackArray[index2]);
                        }
                        else
                        {
                            throw new InvalidOperationException($"Invalid local/parameter access: offset {signedOffset2} from BP {bp}");
                        }

                        if (Program.TraceEnabled)
                        {
                            Console.WriteLine($"  -> Pushed local[{signedOffset1}] and local[{signedOffset2}] from BP {bp}");
                        }
                    }
                    break;

                case OpCode.POPGLOBALB:
                    {
                        byte globalId = program[pc++];
                        globals[globalId] = stack.Pop();
                    }
                    break;

                case OpCode.PUSHGLOBALB:
                    {
                        byte globalId = program[pc++];
                        if (globals.TryGetValue(globalId, out StackValue value))
                            stack.Push(value);
                        else
                            stack.Push(StackValue.FromUInt(0)); // Default to 0
                    }
                    break;

                case OpCode.ENTERB:
                    {
                        byte localCount = program[pc++];
                        basePointers.Push(bp);
                        bp = (uint)stack.Count;
                        // Initialize locals to zero
                        for (int i = 0; i < localCount; i++)
                        {
                            stack.Push(StackValue.FromUInt(0));
                        }
                    }
                    break;

                case OpCode.ENTER:
                    basePointers.Push(bp);
                    bp = (uint)stack.Count;
                    break;

                case OpCode.SWAP:       // 0x43 - Swap top two values
                    {
                        var a = stack.Pop();
                        var b = stack.Pop();
                        stack.Push(a);
                        stack.Push(b);
                    }
                    break;

                case OpCode.DECSP:      // 0x28 - Decrement stack pointer
                    {
                        byte count = program[pc++];
                        for (int i = 0; i < count; i++)
                        {
                            if (stack.Count > 0) stack.Pop();
                        }
                    }
                    break;

                case OpCode.CAST:       // 0x51 - Type cast
                    {
                        byte targetType = program[pc++];
                        var value = stack.Pop();
                        // For now, just change the type without conversion
                        value.Type = (ValueType)targetType;
                        stack.Push(value);
                    }
                    break;



                case OpCode.SYSCALLB0:
                    {
                        byte syscallId = program[pc++];
                        byte param = program[pc++];
                        HandleSyscall(syscallId, param);
                    }
                    break;

                case OpCode.SYSCALL10:
                    {
                        byte syscallId1 = program[pc++];
                        byte syscallId2 = program[pc++];
                        HandleSyscall(syscallId1, 0);
                        HandleSyscall(syscallId2, 0);
                    }
                    break;

                case OpCode.CALL:
                    {
                        uint methodIndex = ReadUInt16();
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
                case OpCode.CALLI:
                    {
                        uint address = ReadUInt16();
                        callStack.Push(pc);
                        pc = address;
                    }
                    break;

                case OpCode.JNZB:       // 0x2F - Jump if not zero (byte offset)
                    {
                        byte offset = program[pc++];
                        var condition = stack.Pop();
                        if (condition.UIntValue != 0)
                        {
                            pc = (uint)((int)pc + (sbyte)offset);
                        }
                    }
                    break;

                case OpCode.JB:         // 0x30 - Unconditional jump (byte offset)
                    {
                        byte offset = program[pc++];
                        pc = (uint)((int)pc + (sbyte)offset);
                    }
                    break;

                case OpCode.RET0:       // 0x4A - Optimized return
                    {
                        if (callStack.Count > 0)
                        {
                            pc = callStack.Pop();
                            if (basePointers.Count > 0)
                                bp = basePointers.Pop();
                        }
                        else
                        {
                            pc = (uint)program.Length; // End execution
                        }
                    }
                    break;

                case OpCode.RETRESB:    // 0x2B - Return with result and cleanup
                    {
                        byte cleanup = program[pc++];
                        var result = stack.Pop();

                        // Clean up locals
                        for (int i = 0; i < cleanup; i++)
                        {
                            if (stack.Count > 0) stack.Pop();
                        }

                        stack.Push(result); // Put result back

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

                case OpCode.RETB:
                    {
                        byte cleanup = program[pc++];
                        // Clean up locals
                        for (int i = 0; i < cleanup; i++)
                        {
                            if (stack.Count > 0) stack.Pop();
                        }

                        if (callStack.Count > 0)
                        {
                            pc = callStack.Pop();
                            if (basePointers.Count > 0)
                                bp = basePointers.Pop();
                        }
                        else
                        {
                            pc = (uint)program.Length; // End execution
                        }
                    }
                    break;

                case OpCode.ADD:        // 0x80 - Add two stack values
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(StackValue.FromUInt(a.UIntValue + b.UIntValue));
                    }
                    break;

                case OpCode.SUB:        // 0x82 - Subtract (used as 0x6E SUBB in your code)
                case OpCode.SUBB:       // 0x6E - Subtract immediate byte
                    {
                        byte immediate = program[pc++];
                        var value = stack.Pop();
                        stack.Push(StackValue.FromUInt(value.UIntValue - immediate));
                    }
                    break;

                case OpCode.DIV:        // 0x84 - Divide
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        if (b.UIntValue == 0) throw new DivideByZeroException();
                        stack.Push(StackValue.FromUInt(a.UIntValue / b.UIntValue));
                    }
                    break;

                case OpCode.MOD:        // 0x88 - Modulus
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        if (b.UIntValue == 0) throw new DivideByZeroException();
                        stack.Push(StackValue.FromUInt(a.UIntValue % b.UIntValue));
                    }
                    break;

                case OpCode.ADDB:       // 0x6D - Add immediate byte
                    {
                        byte immediate = program[pc++];
                        var value = stack.Pop();
                        stack.Push(StackValue.FromUInt(value.UIntValue + immediate));
                    }
                    break;



                case OpCode.LT:         // 0x8C - Less than (unsigned)
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(a.UIntValue < b.UIntValue ? 1 : 0)));
                    }
                    break;

                case OpCode.EQ:         // 0x92 - Equality
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(a.UIntValue == b.UIntValue ? 1 : 0)));
                    }
                    break;

                case OpCode.PUSHIBLE:   // 0x6B - Push immediate byte and test ≤
                    {
                        byte immediate = program[pc++];
                        var value = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(value.UIntValue <= immediate ? 1 : 0)));
                    }
                    break;

                case OpCode.PUSHIBEQ:   // 0x6C - Push immediate byte and test ==
                    {
                        byte immediate = program[pc++];
                        var value = stack.Pop();
                        stack.Push(StackValue.FromUInt((uint)(value.UIntValue == immediate ? 1 : 0)));
                    }
                    break;



                default:
                    throw new InvalidOperationException($"Unknown opcode: 0x{(byte)opcode:X2} at PC: 0x{pc - 1:X4}");
            }
        }

        #endregion

        #region Helper Methods

        /// <summary>
        /// Read a 16-bit unsigned integer from the program at the current PC
        /// </summary>
        private uint ReadUInt16()
        {
            uint value = (uint)(program[pc] | (program[pc + 1] << 8));
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