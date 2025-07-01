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
        private readonly Stack<StackValue> stack;
        private readonly byte[] constants;

        public SystemCallDispatcher(Stack<StackValue> vmStack, byte[] constantData, 
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
            }
        }

        /// <summary>
        /// Register all system calls with their implementations
        /// </summary>
        private void RegisterSystemCalls()
        {
            // String Unit - 0x00-0x0F range
            systemCalls[0x05] = new SystemCall("String.NewFromConstant", StringNewFromConstant);
            
            // Screen Unit - 0x20-0x3F range  
            systemCalls[0x29] = new SystemCall("Screen.Print", ScreenPrint);
            systemCalls[0x2A] = new SystemCall("Screen.PrintLn", ScreenPrintLn);
            
            // TODO: Add more system calls as needed
            // Keyboard Unit - 0x40-0x4F range
            // File Unit - 0x50-0x5F range
            // Time Unit - 0x60-0x6F range
            // etc.
        }

        #region String System Calls

        private void StringNewFromConstant(byte param)
        {
            string str = ReadConstantString(param);
            stack.Push(StackValue.FromRef(str));
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

        #region Screen System Calls

        private void ScreenPrint(byte param)
        {
            // Stack contains: [text, foreColor, backColor] (top to bottom)
            var backColor = stack.Pop();
            var foreColor = stack.Pop();
            var text = stack.Pop();
            
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

    /// <summary>
    /// System call ID ranges for organization
    /// This helps prevent ID conflicts as we add more system calls
    /// </summary>
    public static class SystemCallRanges
    {
        // String operations: 0x00-0x0F
        public const byte StringNewFromConstant = 0x05;
        
        // Screen operations: 0x20-0x3F
        public const byte ScreenPrint = 0x29;
        public const byte ScreenPrintLn = 0x2A;
        public const byte ScreenClear = 0x2B;
        public const byte ScreenSetCursor = 0x2C;
        
        // Keyboard operations: 0x40-0x4F
        public const byte KeyboardIsAvailable = 0x40;
        public const byte KeyboardReadKey = 0x41;
        
        // File operations: 0x50-0x5F
        public const byte FileOpen = 0x50;
        public const byte FileRead = 0x51;
        public const byte FileWrite = 0x52;
        public const byte FileClose = 0x53;
        
        // Time operations: 0x60-0x6F
        public const byte TimeMillis = 0x60;
        public const byte TimeDelay = 0x61;
        
        // Math operations: 0x70-0x7F
        public const byte MathSin = 0x70;
        public const byte MathCos = 0x71;
        public const byte MathSqrt = 0x72;
        
        // List operations: 0x80-0x8F
        public const byte ListAppend = 0x80;
        public const byte ListGetItem = 0x81;
        public const byte ListSetItem = 0x82;
        public const byte ListCount = 0x83;
        
        // Dictionary operations: 0x90-0x9F
        public const byte DictSet = 0x90;
        public const byte DictGet = 0x91;
        public const byte DictContains = 0x92;
        public const byte DictCount = 0x93;
        
        // Runtime operations: 0xA0-0xAF
        public const byte RuntimeInDebugger = 0xA0;
        public const byte RuntimeUserCode = 0xA1;
        
        // System operations: 0xB0-0xBF
        public const byte SystemBeep = 0xB0;
        public const byte SystemArguments = 0xB1;
        
        // Reserved for future use: 0xC0-0xFF
    }
}