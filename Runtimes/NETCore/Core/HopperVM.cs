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

        private readonly List<StackValue> stack = new List<StackValue>();

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
            return $"PC: 0x{pc:X4}, BP: {bp}, Stack: {stack.Count} items";
        }

        #endregion

        #region Stack Operations

        /// <summary>
        /// Push a value onto the stack
        /// </summary>
        private void Push(StackValue value)
        {
            stack.Add(value);
        }

        /// <summary>
        /// Pop a value from the stack
        /// </summary>
        private StackValue Pop()
        {
            if (stack.Count == 0)
                throw new InvalidOperationException($"Stack underflow at PC: 0x{pc:X4}");

            var value = stack[stack.Count - 1];
            stack.RemoveAt(stack.Count - 1);
            return value;
        }

        /// <summary>
        /// Peek at the top stack value without removing it
        /// </summary>
        private StackValue Peek()
        {
            if (stack.Count == 0)
                throw new InvalidOperationException($"Stack underflow at PC: 0x{pc:X4}");

            return stack[stack.Count - 1];
        }

        /// <summary>
        /// Get stack value at absolute index (0 = bottom)
        /// </summary>
        private StackValue GetStackValue(int index)
        {
            if (index < 0 || index >= stack.Count)
                throw new InvalidOperationException($"Invalid stack access at index {index}");

            return stack[index];
        }

        /// <summary>
        /// Set stack value at absolute index (0 = bottom)
        /// </summary>
        private void SetStackValue(int index, StackValue value)
        {
            if (index < 0 || index >= stack.Count)
                throw new InvalidOperationException($"Invalid stack access at index {index}");

            stack[index] = value;
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
                    Push(StackValue.FromUInt(0));
                    break;

                case OpCode.PUSHI:
                    {
                        uint value = ReadUInt16();
                        Push(StackValue.FromUInt(value));
                    }
                    break;

                case OpCode.POPLOCALB:  // 0x1B - Pop to local variable (byte offset)
                    {
                        byte offset = program[pc++];
                        sbyte signedOffset = (sbyte)offset;
                        var value = Pop();

                        // Calculate stack position: BP + signedOffset
                        int stackPosition = (int)bp + signedOffset;

                        if (stackPosition >= 0 && stackPosition < stack.Count)
                        {
                            SetStackValue(stackPosition, value);
                        }
                        else
                        {
                            throw new InvalidOperationException($"Invalid local/parameter access: offset {signedOffset} from BP {bp}");
                        }
                    }
                    break;

                case OpCode.PUSHLOCALB: // 0x1C - Push local variable (byte offset)
                    {
                        byte offset = program[pc++];
                        sbyte signedOffset = (sbyte)offset;

                        // Calculate stack position: BP + signedOffset
                        int stackPosition = (int)bp + signedOffset;

                        if (stackPosition >= 0 && stackPosition < stack.Count)
                        {
                            Push(GetStackValue(stackPosition));
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

                        // Calculate stack positions: BP + signedOffset
                        int stackPosition1 = (int)bp + signedOffset1;
                        int stackPosition2 = (int)bp + signedOffset2;

                        // Push first value
                        if (stackPosition1 >= 0 && stackPosition1 < stack.Count)
                        {
                            Push(GetStackValue(stackPosition1));
                        }
                        else
                        {
                            throw new InvalidOperationException($"Invalid local/parameter access: offset {signedOffset1} from BP {bp}");
                        }

                        // Push second value
                        if (stackPosition2 >= 0 && stackPosition2 < stack.Count)
                        {
                            Push(GetStackValue(stackPosition2));
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

                case OpCode.POPGLOBALB: // 0x1F - Pop to global variable (byte offset)
                    {
                        byte globalOffset = program[pc++];
                        var value = Pop();

                        // Ensure stack is large enough for this global
                        while (stack.Count <= globalOffset)
                        {
                            stack.Add(StackValue.FromUInt(0));
                        }

                        SetStackValue(globalOffset, value);
                    }
                    break;

                case OpCode.PUSHGLOBALB: // 0x20 - Push global variable (byte offset)
                    {
                        byte globalOffset = program[pc++];

                        if (globalOffset < stack.Count)
                        {
                            Push(GetStackValue(globalOffset));
                        }
                        else
                        {
                            Push(StackValue.FromUInt(0)); // Default value for uninitialized global
                        }
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
                            Push(StackValue.FromUInt(0));
                        }
                    }
                    break;

                case OpCode.ENTER:
                    basePointers.Push(bp);
                    bp = (uint)stack.Count;
                    break;

                case OpCode.SWAP:       // 0x43 - Swap top two values
                    {
                        var a = Pop();
                        var b = Pop();
                        Push(a);
                        Push(b);
                    }
                    break;

                case OpCode.DECSP:      // 0x28 - Decrement stack pointer
                    {
                        byte count = program[pc++];
                        for (int i = 0; i < count; i++)
                        {
                            if (stack.Count > 0)
                                stack.RemoveAt(stack.Count - 1);
                        }
                    }
                    break;

                case OpCode.CAST:       // 0x51 - Type cast
                    {
                        byte targetType = program[pc++];
                        var value = Pop();
                        // For now, just change the type without conversion
                        value.Type = (ValueType)targetType;
                        Push(value);
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
                        var condition = Pop();
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
                        var result = Pop();

                        // Clean up locals
                        for (int i = 0; i < cleanup; i++)
                        {
                            if (stack.Count > 0)
                                stack.RemoveAt(stack.Count - 1);
                        }

                        Push(result); // Put result back

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
                            if (stack.Count > 0)
                                stack.RemoveAt(stack.Count - 1);
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
                        var b = Pop();
                        var a = Pop();
                        Push(StackValue.FromUInt(a.UIntValue + b.UIntValue));
                    }
                    break;

                case OpCode.SUB:        // 0x82 - Subtract two stack values
                    {
                        var b = Pop();
                        var a = Pop();
                        Push(StackValue.FromUInt(a.UIntValue - b.UIntValue));
                    }
                    break;

                case OpCode.SUBB:       // 0x6E - Subtract immediate byte
                    {
                        byte immediate = program[pc++];
                        var value = Pop();
                        Push(StackValue.FromUInt(value.UIntValue - immediate));
                    }
                    break;

                case OpCode.DIV:        // 0x84 - Divide
                    {
                        var b = Pop();
                        var a = Pop();
                        if (b.UIntValue == 0) throw new DivideByZeroException();
                        Push(StackValue.FromUInt(a.UIntValue / b.UIntValue));
                    }
                    break;

                case OpCode.MOD:        // 0x88 - Modulus
                    {
                        var b = Pop();
                        var a = Pop();
                        if (b.UIntValue == 0) throw new DivideByZeroException();
                        Push(StackValue.FromUInt(a.UIntValue % b.UIntValue));
                    }
                    break;

                case OpCode.ADDB:       // 0x6D - Add immediate byte
                    {
                        byte immediate = program[pc++];
                        var value = Pop();
                        Push(StackValue.FromUInt(value.UIntValue + immediate));
                    }
                    break;

                case OpCode.LT:         // 0x8C - Less than (unsigned)
                    {
                        var b = Pop();
                        var a = Pop();
                        Push(StackValue.FromUInt((uint)(a.UIntValue < b.UIntValue ? 1 : 0)));
                    }
                    break;

                case OpCode.EQ:         // 0x92 - Equality
                    {
                        var b = Pop();
                        var a = Pop();
                        Push(StackValue.FromUInt((uint)(a.UIntValue == b.UIntValue ? 1 : 0)));
                    }
                    break;

                case OpCode.PUSHIBLE:   // 0x6B - Push immediate byte and test ≤
                    {
                        byte immediate = program[pc++];
                        var value = Pop();
                        Push(StackValue.FromUInt((uint)(value.UIntValue <= immediate ? 1 : 0)));
                    }
                    break;

                case OpCode.PUSHIBEQ:   // 0x6C - Push immediate byte and test ==
                    {
                        byte immediate = program[pc++];
                        var value = Pop();
                        Push(StackValue.FromUInt((uint)(value.UIntValue == immediate ? 1 : 0)));
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