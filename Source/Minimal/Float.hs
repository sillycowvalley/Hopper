unit Float
{
    uses "Diagnostics" // for Die
    uses "Int"         // for Int.ToString
    uses "Long"        // so we can use Long for the 23 bit mantissa
    
    friend UInt;       // so UInt.ToFloat can access Float.normalize and Float.combineComponents
    friend  Int;       // so Int.ToFloat  can access Float.normalize and Float.combineComponents
    friend Long;       // so Long.ToFloat can access Float.normalize and Float.combineComponents
    
    byte GetByte(float this, byte index) system;
    float FromBytes(byte b0, byte b1, byte b2, byte b3) system;

    const float Pi = 3.1415926535;

    float Add(float a, float b)
    {
        byte signA = getSign(a);
        byte exponentA = getExponent(a);
        long mantissaA = getMantissa(a);

        byte signB = getSign(b);
        byte exponentB = getExponent(b);
        long mantissaB = getMantissa(b);

        // Add the implicit leading bit
        mantissaA = Long.or(mantissaA, 0x00800000);
        mantissaB = Long.or(mantissaB, 0x00800000);

        // Align exponents
        if (exponentA > exponentB)
        {
            int shift = exponentA - exponentB;
            mantissaB = Long.shiftRight(mantissaB, shift);
            exponentB = exponentA;
        }
        else if (exponentA < exponentB)
        {
            int shift = exponentB - exponentA;
            mantissaA = Long.shiftRight(mantissaA, shift);
            exponentA = exponentB;
        }

        long resultMantissa;
        if (signA == signB)
        {
            resultMantissa = Long.Add(mantissaA, mantissaB); // Same sign: addition
        }
        else
        {
            if (Long.GreaterThanOrEqual(mantissaA, mantissaB))
            {
                resultMantissa = Long.Subtract(mantissaA, mantissaB); // Different signs: subtraction
            }
            else
            {
                resultMantissa = Long.Subtract(mantissaB, mantissaA);
                signA = signB;
            }
        }

        if (Long.GreaterThanOrEqual(resultMantissa, 0x01000000))
        {
            resultMantissa = Long.shiftRight(resultMantissa, 1);
            exponentA++;
        }
        else
        {
            normalize(ref resultMantissa, ref exponentA);
        }

        return combineComponents(signA, exponentA, resultMantissa);
    }

    float Subtract(float a, float b)
    {
        byte signB = getSign(b);
        signB = signB ^ 1; // Flip the sign of b
        float negativeB = combineComponents(signB, getExponent(b), getMantissa(b));

        return Add(a, negativeB);
    }

    float Multiply(float a, float b)
    {
        byte signA = getSign(a);
        byte exponentA = getExponent(a);
        long mantissaA = getMantissa(a);

        byte signB = getSign(b);
        byte exponentB = getExponent(b);
        long mantissaB = getMantissa(b);

        // Add the implicit leading bit
        mantissaA = Long.or(mantissaA, 0x00800000);
        mantissaB = Long.or(mantissaB, 0x00800000);

        long resultMantissa = Long.shiftRight(Long.Multiply(mantissaA, mantissaB), 23); // Multiply mantissas
        int resultExponent = (int)exponentA + (int)exponentB - 127; // Add exponents
        byte resultSign = signA ^ signB; // Determine the sign

        if (Long.GreaterThanOrEqual(resultMantissa, 0x01000000))
        {
            resultMantissa = Long.shiftRight(resultMantissa, 1);
            resultExponent++;
        }
        else
        {
            normalize(ref resultMantissa, ref resultExponent);
        }

        return combineComponents(resultSign, (byte)resultExponent, resultMantissa);
    }

    float Divide(float a, float b)
    {
        byte signA = getSign(a);
        byte exponentA = getExponent(a);
        long mantissaA = getMantissa(a);

        byte signB = getSign(b);
        byte exponentB = getExponent(b);
        long mantissaB = getMantissa(b);

        // Add the implicit leading bit
        mantissaA = Long.or(mantissaA, 0x00800000);
        mantissaB = Long.or(mantissaB, 0x00800000);

        if (Long.Equal(mantissaB, 0))
        {
            Die(0x04); // Division by zero
        }

        long resultMantissa = Long.shiftRight(Long.Divide(Long.shiftLeft(mantissaA, 23), mantissaB), 0); // Divide mantissas
        int resultExponent = (int)exponentA - (int)exponentB + 127; // Subtract exponents
        byte resultSign = signA ^ signB; // Determine the sign

        if (Long.GreaterThanOrEqual(resultMantissa, 0x01000000))
        {
            resultMantissa = Long.shiftRight(resultMantissa, 1);
            resultExponent++;
        }
        else
        {
            normalize(ref resultMantissa, ref resultExponent);
        }

        return combineComponents(resultSign, (byte)resultExponent, resultMantissa);
    }

    bool Equal(float a, float b)
    {
        return (GetByte(a, 0) == GetByte(b, 0)) &&
               (GetByte(a, 1) == GetByte(b, 1)) &&
               (GetByte(a, 2) == GetByte(b, 2)) &&
               (GetByte(a, 3) == GetByte(b, 3));
    }

    bool NotEqual(float a, float b)
    {
        return !Equal(a, b);
    }

    bool LessThan(float a, float b)
    {
        byte signA = getSign(a);
        byte signB = getSign(b);

        if (signA != signB)
        {
            return signA > signB;
        }

        byte exponentA = getExponent(a);
        byte exponentB = getExponent(b);

        if (exponentA != exponentB)
        {
            return (signA == 0) ? (exponentA < exponentB) : (exponentA > exponentB);
        }

        long mantissaA = getMantissa(a);
        long mantissaB = getMantissa(b);

        return (signA == 0) ? (mantissaA < mantissaB) : (mantissaA > mantissaB);
    }

    bool GreaterThan(float a, float b)
    {
        return !LessThanOrEqual(a, b);
    }

    bool LessThanOrEqual(float a, float b)
    {
        return LessThan(a, b) || Equal(a, b);
    }

    bool GreaterThanOrEqual(float a, float b)
    {
        return GreaterThan(a, b) || Equal(a, b);
    }

    string ToString(float value)
    {
        if (Equal(value, FromBytes(0, 0, 0, 0)))
        {
            return "0";
        }

        bool isNegative = getSign(value) == 1;
        if (isNegative)
        {
            value = Negate(value);
        }

        int integerPart = value.ToInt();
        float fractionalPart = Subtract(value, integerPart.ToFloat());

        string integerPartStr = IntToString(integerPart);
        string fractionalPartStr = fractionToString(fractionalPart);

        string result = integerPartStr + "." + fractionalPartStr;

        if (isNegative)
        {
            result = "-" + result;
        }

        return result;
    }

    int ToInt(float f)
    {
        byte sign = getSign(f);
        byte exponent = getExponent(f);
        long mantissa = getMantissa(f);

        int result = int(Long.shiftRight(mantissa, 8));
        int shift = exponent - 127 - 23;

        if (shift > 0)
        {
            result <<= shift;
        }
        else if (shift < 0)
        {
            result >>= -shift;
        }

        return sign == 1 ? -result : result;
    }

    uint ToUInt(float f)
    {
        int intValue = f.ToInt();
        if (intValue < 0)
        {
            Die(0x0D); // Overflow
        }
        return uint(intValue);
    }

    long ToLong(float f)
    {
        byte sign = getSign(f);
        byte exponent = getExponent(f);
        long mantissa = getMantissa(f);

        long result = Long.shiftRight(mantissa, 8);
        int shift = exponent - 127 - 23;

        if (shift > 0)
        {
            result = Long.shiftLeft(result, shift);
        }
        else if (shift < 0)
        {
            result = Long.shiftRight(result, -shift);
        }

        return sign == 1 ? -result : result;
    }
    
    byte getSign(float f)
    {
        return (GetByte(f, 3) >> 7) & 1;
    }

    byte getExponent(float f)
    {
        return (GetByte(f, 3) & 0x7F) << 1 | (GetByte(f, 2) >> 7);
    }

    long getMantissa(float f)
    {
        return (long(GetByte(f, 2) & 0x7F) << 16) | (long(GetByte(f, 1)) << 8) | long(GetByte(f, 0));
    }

    float combineComponents(byte sign, byte exponent, long mantissa)
    {
        byte b0 = byte(mantissa & 0xFF);
        byte b1 = byte((mantissa >> 8) & 0xFF);
        byte b2 = byte((mantissa >> 16) & 0x7F);
        b2 |= byte((exponent & 1) << 7);
        byte b3 = byte((exponent >> 1) & 0x7F);
        b3 |= byte(sign << 7);

        return FromBytes(b0, b1, b2, b3);
    }

    normalize(ref long mantissa, ref int exponent)
    {
        while ((mantissa & 0x00800000) == 0)
        {
            mantissa = Long.shiftLeft(mantissa, 1);
            exponent--;
        }
        mantissa &= 0x007FFFFF; // Remove the implicit leading bit
    }

    string IntToString(int value)
    {
        string result = "";
        bool isNegative = false;

        if (value == 0)
        {
            return "0";
        }

        if (value < 0)
        {
            isNegative = true;
            value = -value;
        }

        while (value > 0)
        {
            int digit = value % 10;
            result = (char)('0' + digit) + result;
            value /= 10;
        }

        if (isNegative)
        {
            result = "-" + result;
        }

        return result;
    }

    string fractionToString(float fractionalPart)
    {
        string result = "";
        int precision = 6; // Number of digits after the decimal point

        while (precision > 0)
        {
            fractionalPart = Multiply(fractionalPart, Int.ToFloat(10));
            int digit = fractionalPart.ToInt();
            result += (char)('0' + digit);
            fractionalPart = Subtract(fractionalPart, Int.ToFloat(digit));
            precision--;
        }

        return result;
    }

    float Negate(float value)
    {
        byte sign = getSign(value) ^ 1;
        byte exponent = getExponent(value);
        long mantissa = getMantissa(value);

        return combineComponents(sign, exponent, mantissa);
    }
    /*
    <byte> ToBytes(float this)
    {
        <byte> bytes;
        bytes.Append(this.GetByte(0));
        bytes.Append(this.GetByte(1));
        bytes.Append(this.GetByte(2));
        bytes.Append(this.GetByte(3));
        return bytes
    }
    */
}
