using System;
using System.Collections.Generic;

namespace HopperRuntime.Core
{
    /// <summary>
    /// Hopper VM value types - corresponds to the type system in the Hopper language
    /// </summary>
    public enum ValueType : byte
    {
        Undefined = 0x00,
        Char = 0x01,
        Int = 0x02,
        Byte = 0x03,
        UInt = 0x04,
        Reference = 0x05,
        Bool = 0x06,
        Type = 0x0C,
        Float = 0x0D,
        Long = 0x0E,
        String = 0x0F,
        Pair = 0x10,
        Array = 0x12,
        Dictionary = 0x13,
        Variant = 0x14,
        File = 0x15,
        Directory = 0x16,
        List = 0x19,
        ListItem = 0x1A,
    }

    /// <summary>
    /// Helper methods for working with Hopper value types
    /// </summary>
    public static class ValueTypeHelper
    {
        /// <summary>
        /// Check if a type is a reference type (stored on heap)
        /// </summary>
        public static bool IsReferenceType(ValueType type)
        {
            return (byte)type >= 0x0D;
        }

        /// <summary>
        /// Check if a type is a value type (stored directly on stack)
        /// </summary>
        public static bool IsValueType(ValueType type)
        {
            return !IsReferenceType(type);
        }

        /// <summary>
        /// Get the display name for a value type
        /// </summary>
        public static string GetTypeName(ValueType type)
        {
            return type switch
            {
                ValueType.Undefined => "undefined",
                ValueType.Char => "char",
                ValueType.Int => "int",
                ValueType.Byte => "byte",
                ValueType.UInt => "uint",
                ValueType.Reference => "reference",
                ValueType.Bool => "bool",
                ValueType.Type => "type",
                ValueType.Float => "float",
                ValueType.Long => "long",
                ValueType.String => "string",
                ValueType.Pair => "pair",
                ValueType.Array => "array",
                ValueType.Dictionary => "dictionary",
                ValueType.Variant => "variant",
                ValueType.File => "file",
                ValueType.Directory => "directory",
                ValueType.List => "list",
                ValueType.ListItem => "listitem",
                _ => $"unknown({(byte)type:X2})"
            };
        }
    }
    /// <summary>
    /// Represents a stack value with type information for the Hopper VM
    /// </summary>
    public struct StackValue
    {
        public uint UIntValue;
        public int IntValue;
        public long LongValue;
        public float FloatValue;
        public object? RefValue;
        public ValueType Type;

        public static StackValue FromUInt(uint value) => new StackValue
        {
            UIntValue = value,
            Type = ValueType.UInt
        };

        public static StackValue FromInt(int value) => new StackValue
        {
            IntValue = value,
            Type = ValueType.Int
        };

        public static StackValue FromBool(bool value) => new StackValue
        {
            UIntValue = value ? 1u : 0u,
            Type = ValueType.Bool
        };

        public static StackValue FromChar(char value) => new StackValue
        {
            UIntValue = (uint)value,
            Type = ValueType.Char
        };

        public static StackValue FromByte(byte value) => new StackValue
        {
            UIntValue = value,
            Type = ValueType.Byte
        };

        public static StackValue FromFloat(float value) => new StackValue
        {
            FloatValue = value,
            Type = ValueType.Float
        };

        public static StackValue FromLong(long value) => new StackValue
        {
            LongValue = value,
            Type = ValueType.Long
        };

        public static StackValue FromReference(object value, ValueType type) => new StackValue
        {
            RefValue = value,
            Type = type
        };

        public static StackValue FromRef(object value) => new StackValue
        {
            RefValue = value,
            Type = ValueType.Reference
        };

        public static StackValue FromString(string value) => new StackValue
        {
            RefValue = value,
            Type = ValueType.String
        };

        public override string ToString()
        {
            return Type switch
            {
                ValueType.UInt => $"UInt({UIntValue})",
                ValueType.Int => $"Int({IntValue})",
                ValueType.Bool => $"Bool({(UIntValue != 0)})",
                ValueType.Char => $"Char('{(char)UIntValue}')",
                ValueType.Byte => $"Byte({(byte)UIntValue})",
                ValueType.Float => $"Float({FloatValue})",
                ValueType.Long => $"Long({LongValue})",
                ValueType.String => $"String({RefValue ?? "null"})",
                _ => $"{Type}({RefValue ?? UIntValue})"
            };
        }
    }

    /// <summary>
    /// Virtual Machine Stack - provides indexed access and traditional stack operations
    /// Designed for the Hopper VM where globals are at absolute positions and locals are relative to BP
    /// </summary>
    public class VMStack
    {
        private readonly List<StackValue> stack = new List<StackValue>();

        #region Properties

        /// <summary>
        /// Number of items on the stack
        /// </summary>
        public int Count => stack.Count;

        /// <summary>
        /// Check if the stack is empty
        /// </summary>
        public bool IsEmpty => stack.Count == 0;

        #endregion

        #region Stack Operations

        /// <summary>
        /// Push a value onto the top of the stack
        /// </summary>
        public void Push(StackValue value)
        {
            stack.Add(value);
        }

        /// <summary>
        /// Pop a value from the top of the stack
        /// </summary>
        public StackValue Pop()
        {
            if (stack.Count == 0)
                throw new InvalidOperationException("Stack underflow");

            var value = stack[stack.Count - 1];
            stack.RemoveAt(stack.Count - 1);
            return value;
        }

        /// <summary>
        /// Peek at the top stack value without removing it
        /// </summary>
        public StackValue Peek()
        {
            if (stack.Count == 0)
                throw new InvalidOperationException("Stack underflow");

            return stack[stack.Count - 1];
        }

        /// <summary>
        /// Pop multiple values from the stack
        /// </summary>
        public void PopMultiple(int count)
        {
            for (int i = 0; i < count; i++)
            {
                if (stack.Count > 0)
                    stack.RemoveAt(stack.Count - 1);
                else
                    break;
            }
        }

        /// <summary>
        /// Clear all values from the stack
        /// </summary>
        public void Clear()
        {
            stack.Clear();
        }

        #endregion

        #region Indexed Access

        /// <summary>
        /// Get stack value at absolute index (0 = bottom of stack)
        /// </summary>
        public StackValue GetAt(int index)
        {
            if (index < 0 || index >= stack.Count)
                throw new ArgumentOutOfRangeException(nameof(index), $"Index {index} out of range [0, {stack.Count - 1}]");

            return stack[index];
        }

        /// <summary>
        /// Set stack value at absolute index (0 = bottom of stack)
        /// </summary>
        public void SetAt(int index, StackValue value)
        {
            if (index < 0 || index >= stack.Count)
                throw new ArgumentOutOfRangeException(nameof(index), $"Index {index} out of range [0, {stack.Count - 1}]");

            stack[index] = value;
        }

        /// <summary>
        /// Ensure the stack has at least the specified number of elements
        /// Fills with zero values if needed
        /// </summary>
        public void EnsureSize(int minSize)
        {
            while (stack.Count < minSize)
            {
                stack.Add(StackValue.FromUInt(0));
            }
        }

        #endregion

        #region Global Variable Access

        /// <summary>
        /// Get a global variable by offset (globals are at absolute stack positions)
        /// </summary>
        public StackValue GetGlobal(UInt16 offset)
        {
            if (offset < stack.Count)
                return stack[offset];
            else
                return StackValue.FromUInt(0); // Default value for uninitialized global
        }

        /// <summary>
        /// Set a global variable by offset (globals are at absolute stack positions)
        /// </summary>
        public void SetGlobal(UInt16 offset, StackValue value)
        {
            // Ensure stack is large enough for this global
            EnsureSize(offset + 1);
            stack[offset] = value;
        }

        #endregion

        #region Local Variable Access

        /// <summary>
        /// Get a local variable or parameter relative to the base pointer
        /// </summary>
        public StackValue GetLocal(uint basePointer, short offset)
        {
            int stackPosition = (int)basePointer + offset;

            if (stackPosition >= 0 && stackPosition < stack.Count)
                return stack[stackPosition];
            else
                throw new InvalidOperationException($"Invalid local/parameter access: offset {offset} from BP {basePointer}");
        }

        /// <summary>
        /// Set a local variable or parameter relative to the base pointer
        /// </summary>
        public void SetLocal(uint basePointer, short offset, StackValue value)
        {
            int stackPosition = (int)basePointer + offset;

            if (stackPosition >= 0 && stackPosition < stack.Count)
                stack[stackPosition] = value;
            else
                throw new InvalidOperationException($"Invalid local/parameter access: offset {offset} from BP {basePointer}");
        }

        #endregion

        #region System Call Support

        /// <summary>
        /// Pop a specified number of arguments for system calls
        /// Returns them in the order they were pushed (first pushed = index 0)
        /// </summary>
        public StackValue[] PopArguments(int count)
        {
            if (count > stack.Count)
                throw new InvalidOperationException($"Cannot pop {count} arguments, only {stack.Count} items on stack");

            var args = new StackValue[count];

            // Pop in reverse order to get them in push order
            for (int i = count - 1; i >= 0; i--)
            {
                args[i] = Pop();
            }

            return args;
        }

        /// <summary>
        /// Push multiple values onto the stack
        /// </summary>
        public void PushMultiple(params StackValue[] values)
        {
            foreach (var value in values)
            {
                Push(value);
            }
        }

        #endregion

        #region Debugging Support

        /// <summary>
        /// Get a string representation of the current stack state
        /// </summary>
        public string GetStackTrace(int maxItems = 10)
        {
            if (stack.Count == 0)
                return "Stack: [empty]";

            var items = new List<string>();
            int startIndex = Math.Max(0, stack.Count - maxItems);

            for (int i = startIndex; i < stack.Count; i++)
            {
                string marker = (i == stack.Count - 1) ? " <- TOP" : "";
                items.Add($"  [{i}]: {stack[i]}{marker}");
            }

            if (startIndex > 0)
                items.Insert(0, $"  ... ({startIndex} more items)");

            return "Stack:\n" + string.Join("\n", items);
        }

        /// <summary>
        /// Get the raw stack contents as an array (for debugging)
        /// </summary>
        public StackValue[] ToArray()
        {
            return stack.ToArray();
        }

        #endregion
    }
}