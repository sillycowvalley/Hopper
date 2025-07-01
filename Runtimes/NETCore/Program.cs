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

        public void LoadProgram(byte[] bytecode)
        {
            // Use BinaryLoader to parse the file
            var loadedProgram = BinaryLoader.LoadProgram(bytecode);

            // Extract the components
            program = loadedProgram.Program;
            constants = loadedProgram.Constants;

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