using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using HopperRuntime.SystemLibrary;
using HopperRuntime.Core;

namespace HopperRuntime
{
    
    public class HopperVM
    {
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

        private void OptimizeCallInstructions()
        {
            for (int i = 0; i < program.Length - 2; i++)
            {
                if (program[i] == (byte)OpCode.CALL)
                {
                    uint methodIndex = (uint)(program[i + 1] | (program[i + 2] << 8));
                    if (methodTable.TryGetValue(methodIndex, out uint address))
                    {
                        // Replace CALL with CALLI and index with address
                        program[i] = (byte)OpCode.CALLI;
                        program[i + 1] = (byte)(address & 0xFF);
                        program[i + 2] = (byte)((address >> 8) & 0xFF);
                    }
                }
            }
        }

        public void LoadProgram(byte[] bytecode)
        {
            // Parse header
            //uint version = BitConverter.ToUInt16(bytecode, 0);
            uint constantOffset = BitConverter.ToUInt16(bytecode, 2);
            uint entryPoint = BitConverter.ToUInt16(bytecode, 4);

            // Load method table (between header and constants)
            uint methodTableStart = 6; // After header
            uint methodTableEnd = constantOffset;
            for (uint i = methodTableStart; i < methodTableEnd; i += 4)
            {
                uint methodIndex = BitConverter.ToUInt16(bytecode, (int)i);
                uint methodAddress = BitConverter.ToUInt16(bytecode, (int)i + 2);
                methodTable[methodIndex] = methodAddress;
            }

            // Split program and constants
            long programLength = bytecode.Length - entryPoint;
            program = new byte[programLength];
            Array.Copy(bytecode, entryPoint, program, 0, programLength); // Skip header

            long constantLength = entryPoint - constantOffset;
            constants = new byte[constantLength];
            if (constantLength != 0)
            {
                Array.Copy(bytecode, (int)constantOffset, constants, 0, constantLength);
            }

            // Optimize CALL instructions to CALLI at load time
            OptimizeCallInstructions();

            // Initialize system call dispatcher after constants are loaded
            var screenUnit = new ScreenUnit();
            var stringUnit = new StringUnit();
            systemCallDispatcher = new SystemCallDispatcher(stack, constants, screenUnit, stringUnit);

            pc = 0;
        }

        public void Run()
        {
            while (pc < program.Length)
            {
                ExecuteInstruction();
            }
        }

        private void ExecuteInstruction()
        {
            OpCode opcode = (OpCode)program[pc++];

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
                        if (offset == 0xFF) // BP-1 (parameter)
                        {
                            // Push the parameter (should be on stack at BP-1)
                            var stackArray = stack.ToArray();
                            if (stackArray.Length > 0)
                                stack.Push(stackArray[0]); // Top of stack when we entered
                        }
                        else
                        {
                            // Regular local variable
                            stack.Push(StackValue.FromUInt(0)); // Placeholder
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
                        uint address = ReadUInt16();
                        callStack.Push(pc);
                        pc = address;
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

        private uint ReadUInt16()
        {
            uint value = (uint)(program[pc] | (program[pc + 1] << 8));
            pc += 2;
            return value;
        }

        private void HandleSyscall(byte syscallId, byte param)
        {
            systemCallDispatcher?.HandleSyscall(syscallId, param);
        }

        private string ReadConstantString(byte offset)
        {
            var sb = new StringBuilder();
            int pos = offset;
            while (pos < constants.Length && constants[pos] != 0)
            {
                sb.Append((char)constants[pos]);
                pos++;
            }
            return sb.ToString();
        }
    }

    

    // Main program
    public class Program
    {
        public static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Usage: Hopper <program.hexe>");
                return;
            }

            var vm = new HopperVM();

            var bytecode = File.ReadAllBytes(args[0]);

            vm.LoadProgram(bytecode);
            vm.Run();
        }


    }
}