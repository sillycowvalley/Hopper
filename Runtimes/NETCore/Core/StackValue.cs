namespace HopperRuntime.Core
{
    /// <summary>
    /// Defines the types that can be stored in a stack value.
    /// All primitive types are stored as values, while complex types are references.
    /// </summary>
    public enum ValueType : byte
    {
        UInt,
        Int,
        Long,
        Float,
        Byte,
        Char,
        Bool,
        Reference  // string, lists, arrays, etc.
    }

    /// <summary>
    /// Represents a value on the Hopper VM stack.
    /// Uses 32-bit slots for efficiency on modern architectures.
    /// All primitive types are stored as values, complex types as references to managed heap.
    /// </summary>
    public struct StackValue
    {
        public uint UIntValue;
        public int IntValue;
        public long LongValue;
        public float FloatValue;
        public object? RefValue;
        public ValueType Type;

        #region Factory Methods

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

        public static StackValue FromLong(long value) => new StackValue
        {
            LongValue = value,
            Type = ValueType.Long
        };

        public static StackValue FromFloat(float value) => new StackValue
        {
            FloatValue = value,
            Type = ValueType.Float
        };

        public static StackValue FromByte(byte value) => new StackValue
        {
            UIntValue = value,
            Type = ValueType.Byte
        };

        public static StackValue FromChar(char value) => new StackValue
        {
            UIntValue = value,
            Type = ValueType.Char
        };

        public static StackValue FromBool(bool value) => new StackValue
        {
            UIntValue = value ? 1u : 0u,
            Type = ValueType.Bool
        };

        public static StackValue FromRef(object? value) => new StackValue
        {
            RefValue = value,
            Type = ValueType.Reference
        };

        #endregion

        #region Type Conversion Helpers

        /// <summary>
        /// Get the value as a byte, regardless of how it's stored internally
        /// </summary>
        public byte AsByte => Type switch
        {
            ValueType.Byte => (byte)UIntValue,
            ValueType.UInt => (byte)UIntValue,
            ValueType.Int => (byte)IntValue,
            ValueType.Char => (byte)UIntValue,
            ValueType.Bool => (byte)UIntValue,
            _ => throw new InvalidCastException($"Cannot convert {Type} to byte")
        };

        /// <summary>
        /// Get the value as a char, regardless of how it's stored internally
        /// </summary>
        public char AsChar => Type switch
        {
            ValueType.Char => (char)UIntValue,
            ValueType.Byte => (char)UIntValue,
            ValueType.UInt => (char)UIntValue,
            _ => throw new InvalidCastException($"Cannot convert {Type} to char")
        };

        /// <summary>
        /// Get the value as a uint, regardless of how it's stored internally
        /// </summary>
        public uint AsUInt => Type switch
        {
            ValueType.UInt => UIntValue,
            ValueType.Byte => UIntValue,
            ValueType.Char => UIntValue,
            ValueType.Bool => UIntValue,
            ValueType.Int => (uint)IntValue,
            _ => throw new InvalidCastException($"Cannot convert {Type} to uint")
        };

        /// <summary>
        /// Get the value as a bool, regardless of how it's stored internally
        /// </summary>
        public bool AsBool => Type switch
        {
            ValueType.Bool => UIntValue != 0,
            ValueType.Byte => UIntValue != 0,
            ValueType.UInt => UIntValue != 0,
            ValueType.Int => IntValue != 0,
            _ => throw new InvalidCastException($"Cannot convert {Type} to bool")
        };

        /// <summary>
        /// Get the value as a reference type
        /// </summary>
        public T? AsRef<T>() where T : class => Type switch
        {
            ValueType.Reference => RefValue as T,
            _ => throw new InvalidCastException($"Cannot convert {Type} to reference type")
        };

        #endregion

        #region Validation

        /// <summary>
        /// Check if this value is compatible with the specified Hopper type
        /// </summary>
        public bool IsCompatibleWith(ValueType hopperType) => Type switch
        {
            ValueType.Byte => hopperType == ValueType.Byte,
            ValueType.Char => hopperType == ValueType.Char,
            ValueType.UInt => hopperType == ValueType.UInt,
            ValueType.Int => hopperType == ValueType.Int,
            ValueType.Long => hopperType == ValueType.Long,
            ValueType.Float => hopperType == ValueType.Float,
            ValueType.Bool => hopperType == ValueType.Bool,
            ValueType.Reference => hopperType == ValueType.Reference,
            _ => false
        };

        /// <summary>
        /// Check if this value represents a numeric type that can be used in arithmetic
        /// </summary>
        public bool IsNumeric => Type switch
        {
            ValueType.Byte or ValueType.UInt or ValueType.Int or
            ValueType.Long or ValueType.Float => true,
            _ => false
        };

        /// <summary>
        /// Check if this value is zero/empty/null
        /// </summary>
        public bool IsZero => Type switch
        {
            ValueType.Byte or ValueType.UInt or ValueType.Bool => UIntValue == 0,
            ValueType.Char => UIntValue == 0,
            ValueType.Int => IntValue == 0,
            ValueType.Long => LongValue == 0,
            ValueType.Float => FloatValue == 0.0f,
            ValueType.Reference => RefValue == null,
            _ => false
        };

        #endregion

        #region String Representation

        public override string ToString()
        {
            return Type switch
            {
                ValueType.UInt => UIntValue.ToString(),
                ValueType.Int => IntValue.ToString(),
                ValueType.Long => LongValue.ToString(),
                ValueType.Float => FloatValue.ToString("F6"),
                ValueType.Byte => $"0x{(byte)UIntValue:X2}",
                ValueType.Char => $"'{(char)UIntValue}'",
                ValueType.Bool => (UIntValue != 0).ToString().ToLower(),
                ValueType.Reference => RefValue?.ToString() ?? "null",
                _ => "unknown"
            };
        }

        /// <summary>
        /// Get a detailed string representation including type information
        /// </summary>
        public string ToDetailedString()
        {
            return $"{Type}({ToString()})";
        }

        #endregion

        #region Equality

        public override bool Equals(object? obj)
        {
            if (obj is not StackValue other) return false;

            if (Type != other.Type) return false;

            return Type switch
            {
                ValueType.UInt or ValueType.Byte or ValueType.Char or ValueType.Bool => UIntValue == other.UIntValue,
                ValueType.Int => IntValue == other.IntValue,
                ValueType.Long => LongValue == other.LongValue,
                ValueType.Float => Math.Abs(FloatValue - other.FloatValue) < float.Epsilon,
                ValueType.Reference => ReferenceEquals(RefValue, other.RefValue),
                _ => false
            };
        }

        public override int GetHashCode()
        {
            return Type switch
            {
                ValueType.UInt or ValueType.Byte or ValueType.Char or ValueType.Bool => HashCode.Combine(Type, UIntValue),
                ValueType.Int => HashCode.Combine(Type, IntValue),
                ValueType.Long => HashCode.Combine(Type, LongValue),
                ValueType.Float => HashCode.Combine(Type, FloatValue),
                ValueType.Reference => HashCode.Combine(Type, RefValue),
                _ => HashCode.Combine(Type)
            };
        }

        public static bool operator ==(StackValue left, StackValue right) => left.Equals(right);
        public static bool operator !=(StackValue left, StackValue right) => !left.Equals(right);

        #endregion
    }
}