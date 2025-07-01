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
        public void HandleSyscall(byte syscallId, byte param)
        {
            if (systemCalls.TryGetValue(syscallId, out SystemCall? syscall))
            {
                syscall.Execute(param);
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
            
            // Screen Unit - 0x20-0x3F range  
            systemCalls[0x29] = new SystemCall("Screen.Print", ScreenPrint);
            systemCalls[0x2A] = new SystemCall("Screen.PrintLn", ScreenPrintLn);

            systemCalls[0x1D] = new SystemCall("Long.New", LongNew);

            systemCalls[0x25] = new SystemCall("Time.MillisGet", TimeMillisGet);
        }

        #region String System Calls

        private void StringNew(byte param)
        {
            string str = System.String.Empty;
            stack.Push(StackValue.FromString(str));
        }

        private void StringNewFromConstant(byte param)
        {
            string str = ReadConstantString(param);
            stack.Push(StackValue.FromString(str));
        }

        private string ReadConstantString(byte offset)
        {
            var result = new System.Text.StringBuilder();
            int pos = offset;
            while (pos < constants.Length && constants[pos] != 0)
            {
                result.Append((char)constants[pos]);
                pos++;
            }
            return result.ToString();
        }

        #endregion

        #region Time System Calls

        static DateTime startTime = DateTime.Now;
        private void TimeMillisGet(byte param)
        {
            DateTime now = DateTime.Now;
            TimeSpan elapsed = now - startTime;
            stack.Push(StackValue.FromLong((long)elapsed.TotalMilliseconds));
        }

        #endregion

        #region Long System Calls

        private void LongNew(byte param)
        {
            stack.Push(StackValue.FromLong(0));
        }

        #endregion

        #region Screen System Calls

        private void ScreenPrint(byte param)
        {
            // Stack contains: [text, foreColor, backColor] (top to bottom)
            var backColor = stack.Pop();
            var foreColor = stack.Pop();
            var text      = stack.Pop();

            screenUnit.Print(text.RefValue?.ToString() ?? "", 
                           foreColor.UIntValue, 
                           backColor.UIntValue);
        }

        private void ScreenPrintLn(byte param)
        {
            screenUnit.PrintLn();
        }

        #endregion

        #region Future System Call Implementations

        // TODO: Keyboard system calls
        /*
        private void KeyboardIsAvailable(byte param)
        {
            bool available = keyboardUnit.IsAvailable;
            stack.Push(StackValue.FromBool(available));
        }

        private void KeyboardReadKey(byte param)
        {
            var key = keyboardUnit.ReadKey();
            stack.Push(StackValue.FromUInt((uint)key));
        }
        */

        // TODO: File system calls
        /*
        private void FileOpen(byte param)
        {
            var path = stack.Pop().RefValue?.ToString() ?? "";
            var file = fileUnit.Open(path);
            stack.Push(StackValue.FromRef(file));
        }
        */

        // TODO: Time system calls
        /*
        private void TimeMillis(byte param)
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