using System;
using System.Collections.Generic;

using HopperRuntime.Core;

namespace HopperRuntime.SystemLibrary
{
    /// <summary>
    /// Manages system call routing and execution for the Hopper runtime.
    /// System calls provide the interface between Hopper programs and the runtime environment.
    /// </summary>
    public class SystemCallDispatcher
    {
        private readonly Dictionary<byte, SystemCall> systemCalls = new Dictionary<byte, SystemCall>();
        private readonly ScreenUnit screenUnit;
        private readonly StringUnit stringUnit;
        private readonly VMStack stack;
        private readonly byte[] constants;

        public SystemCallDispatcher(VMStack vmStack, byte[] constantData, 
                                   ScreenUnit screen, StringUnit strings)
        {
            stack = vmStack;
            constants = constantData;
            screenUnit = screen;
            stringUnit = strings;
            
            RegisterSystemCalls();
        }

        /// <summary>
        /// Execute a system call with the given ID and optional parameter
        /// </summary>
        public void HandleSyscall(byte syscallId, byte overload)
        {
            if (overload == 3)
            {
                overload = (byte)stack.Pop().UIntValue;
            }
            if (systemCalls.TryGetValue(syscallId, out SystemCall? syscall))
            {
                syscall.Execute(overload);
            }
            else
            {
                Console.WriteLine($"Unknown syscall: 0x{syscallId:X2}");
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// Register all system calls with their implementations
        /// </summary>
        private void RegisterSystemCalls()
        {
            // String Unit - 0x00-0x0F range
            systemCalls[0x00] = new SystemCall("String.NewFromConstant", StringNewFromConstant);
            systemCalls[0x02] = new SystemCall("String.New", StringNew);
            systemCalls[0x03] = new SystemCall("String.BuildFront", StringBuildFront);
            systemCalls[0x06] = new SystemCall("String.LengthGet", StringLengthGet);
            systemCalls[0x0A] = new SystemCall("String.GetChar", StringGetChar);
            systemCalls[0x5E] = new SystemCall("String.Append", StringAppend);

            // Screen Unit - 0x20-0x3F range  
            systemCalls[0x29] = new SystemCall("Screen.Print", ScreenPrint);
            systemCalls[0x2A] = new SystemCall("Screen.PrintLn", ScreenPrintLn);

            systemCalls[0x36] = new SystemCall("UInt.ToLong", UIntToLong);

            systemCalls[0x1D] = new SystemCall("Long.New", LongNew);
            systemCalls[0x3A] = new SystemCall("Long.ToFloat", LongToFloat);
            systemCalls[0x40] = new SystemCall("Long.Add", LongAdd);

            systemCalls[0x21] = new SystemCall("Float.New", FloatNew);
            systemCalls[0x22] = new SystemCall("Float.NewFromConstant", FloatNewFromConstant);
            systemCalls[0x4A] = new SystemCall("Float.ToString", FloatToString);
            systemCalls[0x4E] = new SystemCall("Float.Add", FloatAdd);
            systemCalls[0x50] = new SystemCall("Float.Div", FloatDiv);

            systemCalls[0x25] = new SystemCall("Time.MillisGet", TimeMillisGet);

            
        }

        #region String System Calls

        private void StringNew(byte overload)
        {
            string str = System.String.Empty;
            stack.Push(StackValue.FromString(str));
        }

        private void StringNewFromConstant(byte overload)
        {
            string str = ReadConstantString(overload);
            stack.Push(StackValue.FromString(str));
        }

        private string ReadConstantString(byte overload)
        {
            var result = new System.Text.StringBuilder();
            switch (overload)
            {
                case 0:
                    {
                        StackValue length = stack.Pop();
                        StackValue location = stack.Pop();

                        uint pos = location.UIntValue;
                        uint count = length.UIntValue;
                        for (uint i = 0; i < count; i++)
                        {
                            result.Append((char)constants[pos+i]);
                        }
                    }
                    break;

                case 1:
                    {
                        StackValue content = stack.Pop();
                        byte lsb = (byte)(content.UIntValue & 0xFF);
                        byte msb = (byte)(content.UIntValue >> 8);
                        result.Append((char)lsb); ;
                        if (msb != 0)
                        {
                            result.Append((char)msb); ;
                        }
                    }
                    break;
                default:
                    throw new NotImplementedException();
            }
            return result.ToString();
        }
        private void StringLengthGet(byte overload)
        {
            StackValue top = stack.Pop();
            string content = top.RefValue as string;
            stack.Push(StackValue.FromUInt((ushort)(content.Length)));
        }
        private void StringGetChar(byte overload)
        {
            ushort index = (ushort)stack.Pop().UIntValue;
            StackValue top = stack.Pop();
            string content = top.RefValue as string;
            stack.Push(StackValue.FromChar(content[index]));
        }
        private void StringAppend(byte overload)
        {
            string result = String.Empty;
            switch (overload)
            {
                case 0:
                    {
                        StackValue top = stack.Pop();
                        StackValue next = stack.Pop();
                        result = (next.RefValue as string) + (top.RefValue as string);
                    }
                    break;
                default:
                    throw new NotImplementedException();
            }
            stack.Push(StackValue.FromString(result));
        }

        private void StringBuildFront(byte overload)
        {
            switch (overload)
            {
                case 0:
                    {
                        char more = (char)stack.Pop().UIntValue;
                        StackValue reference = stack.Pop();
                        ushort address = (ushort)reference.UIntValue;
                        StackValue build = stack.GetAt(address);
                        stack.SetAt(address, StackValue.FromString(more + (build.RefValue as string)));
                    }
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        #endregion

        #region Time System Calls

        

        private void TimeMillisGet(byte overload)
        {
            DateTime now = DateTime.Now;
            TimeSpan elapsed = now - Program.LaunchTime;
            stack.Push(StackValue.FromLong((long)elapsed.TotalMilliseconds));
        }

        #endregion

        #region UInt System Calls

        private void UIntToLong(byte overload)
        {
            StackValue top = stack.Pop();
            stack.Push(StackValue.FromLong(top.UIntValue));
        }
        #endregion

        #region Long System Calls

        private void LongNew(byte overload)
        {
            stack.Push(StackValue.FromLong(0));
        }
        private void LongToFloat(byte overload)
        {
            StackValue top = stack.Pop();
            float f = top.LongValue;
            stack.Push(StackValue.FromFloat(f));
        }

        private void LongAdd(byte overload)
        {
            StackValue top = stack.Pop();
            StackValue next = stack.Pop();
            stack.Push(StackValue.FromLong(next.LongValue + top.LongValue));
        }

        #endregion

        #region Float System Calls

        private void FloatNew(byte overload)
        {
            stack.Push(StackValue.FromFloat(0));
        }
        private void FloatNewFromConstant(byte overload)
        {
            int location = (int)stack.Pop().UIntValue;
            
            float f = BitConverter.ToSingle(constants, location);
            stack.Push(StackValue.FromFloat(f));
        }
        private void FloatToString(byte overload)
        {
            StackValue top = stack.Pop();
            stack.Push(StackValue.FromString(top.FloatValue.ToString("G9")));
        }

        private void FloatAdd(byte overload)
        {
            StackValue top = stack.Pop();
            StackValue next = stack.Pop();
            stack.Push(StackValue.FromFloat(next.FloatValue + top.FloatValue));
        }

        private void FloatDiv(byte overload)
        {
            StackValue top = stack.Pop();
            StackValue next = stack.Pop();
            stack.Push(StackValue.FromFloat(next.FloatValue / top.FloatValue));
        }

        #endregion

        #region Screen System Calls

        private void ScreenPrint(byte overload)
        {
            // Stack contains: [text, foreColor, backColor] (top to bottom)
            var backColor = stack.Pop();
            var foreColor = stack.Pop();
            var text      = stack.Pop();

            screenUnit.Print(text.RefValue?.ToString() ?? "", 
                           foreColor.UIntValue, 
                           backColor.UIntValue);
        }

        private void ScreenPrintLn(byte overload)
        {
            screenUnit.PrintLn();
        }

        #endregion

        #region Future System Call Implementations

        // TODO: Keyboard system calls
        /*
        private void KeyboardIsAvailable(byte overload)
        {
            bool available = keyboardUnit.IsAvailable;
            stack.Push(StackValue.FromBool(available));
        }

        private void KeyboardReadKey(byte overload)
        {
            var key = keyboardUnit.ReadKey();
            stack.Push(StackValue.FromUInt((uint)key));
        }
        */

        // TODO: File system calls
        /*
        private void FileOpen(byte overload)
        {
            var path = stack.Pop().RefValue?.ToString() ?? "";
            var file = fileUnit.Open(path);
            stack.Push(StackValue.FromRef(file));
        }
        */

        // TODO: Time system calls
        /*
        private void TimeMillis(byte overload)
        {
            long millis = timeUnit.Millis;
            stack.Push(StackValue.FromLong(millis));
        }
        */

        #endregion
    }

    /// <summary>
    /// Represents a single system call with its name and implementation
    /// </summary>
    public class SystemCall
    {
        public string Name { get; }
        public Action<byte> Execute { get; }

        public SystemCall(string name, Action<byte> implementation)
        {
            Name = name;
            Execute = implementation;
        }

        public override string ToString() => Name;
    }

    
}