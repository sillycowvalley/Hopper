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

            // Optional: Print program summary for debugging
            Console.WriteLine(BinaryLoader.GetProgramSummary(loadedProgram));
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
            Console.WriteLine($"{pc - 1:X4}: {OpCodeInfo.GetName(opcode)}");

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