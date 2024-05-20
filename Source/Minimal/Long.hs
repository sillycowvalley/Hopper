unit Long
{
    uses "Float"
    
    friend Float, Int;
    
    byte GetByte(long this, byte index) system;
    long FromBytes(byte b0, byte b1, byte b2, byte b3) system;
        
    long Add(long a, long b)
    {
        // Extract bytes from both longs
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);
    
        byte b0 = GetByte(b, 0);
        byte b1 = GetByte(b, 1);
        byte b2 = GetByte(b, 2);
        byte b3 = GetByte(b, 3);
    
        // Perform addition byte-by-byte with carry
        byte result0;
        byte result1;
        byte result2;
        byte result3;
    
        uint carry = 0;
    
        // Add the lowest byte with carry
        uint temp = uint(a0) + uint(b0) + carry;
        result0 = byte(temp & 0xFF);
        carry = (temp > 0xFF) ? 1 : 0;
    
        // Add the second byte with carry
        temp = uint(a1) + uint(b1) + carry;
        result1 = byte(temp & 0xFF);
        carry = (temp > 0xFF) ? 1 : 0;
    
        // Add the third byte with carry
        temp = uint(a2) + uint(b2) + carry;
        result2 = byte(temp & 0xFF);
        carry = (temp > 0xFF) ? 1 : 0;
    
        // Add the highest byte with carry
        temp = uint(a3) + uint(b3) + carry;
        result3 = byte(temp & 0xFF);
        carry = (temp > 0xFF) ? 1 : 0;
    
        // Create new long from result bytes
        return FromBytes(result0, result1, result2, result3);
    }
        
        
    long Sub(long a, long b)
    {
        // Extract bytes from both longs
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);
    
        byte b0 = GetByte(b, 0);
        byte b1 = GetByte(b, 1);
        byte b2 = GetByte(b, 2);
        byte b3 = GetByte(b, 3);
    
        // Perform subtraction byte-by-byte with borrow
        byte result0;
        byte result1;
        byte result2;
        byte result3;
    
        uint borrow = 0;
    
        // Subtract the lowest byte with borrow
        uint temp = uint(a0) - uint(b0) - borrow;
        result0 = byte(temp & 0xFF);
        borrow = (temp > 0xFF) ? 1 : 0;
    
        // Subtract the second byte with borrow
        temp = uint(a1) - uint(b1) - borrow;
        result1 = byte(temp & 0xFF);
        borrow = (temp > 0xFF) ? 1 : 0;
    
        // Subtract the third byte with borrow
        temp = uint(a2) - uint(b2) - borrow;
        result2 = byte(temp & 0xFF);
        borrow = (temp > 0xFF) ? 1 : 0;
    
        // Subtract the highest byte with borrow
        temp = uint(a3) - uint(b3) - borrow;
        result3 = byte(temp & 0xFF);
        borrow = (temp > 0xFF) ? 1 : 0;
    
        // Create new long from result bytes
        return FromBytes(result0, result1, result2, result3);
    }
        
     
    long Mul(long a, long b)
    {
        // Extract bytes from both longs
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);
        
        byte b0 = GetByte(b, 0);
        byte b1 = GetByte(b, 1);
        byte b2 = GetByte(b, 2);
        byte b3 = GetByte(b, 3);
        
        // Perform multiplication byte-by-byte with carry
        uint carry = 0;
        
        // Multiply the lowest bytes
        uint result0 = uint(a0) * uint(b0);
        
        // Multiply the second bytes and add carry from previous result
        uint result1 = uint(a0) * uint(b1) + uint(a1) * uint(b0) + (result0 >> 8);
        
        // Multiply the third bytes and add carry from previous result
        uint result2 = uint(a0) * uint(b2) + uint(a1) * uint(b1) + uint(a2) * uint(b0) + (result1 >> 8);
        
        // Multiply the highest bytes and add carry from previous result
        uint result3 = uint(a0) * uint(b3) + uint(a1) * uint(b2) + uint(a2) * uint(b1) + uint(a3) * uint(b0) + (result2 >> 8);
        
        // Handle carry for the highest part
        uint result4 = uint(a1) * uint(b3) + uint(a2) * uint(b2) + uint(a3) * uint(b1) + (result3 >> 8);
        uint result5 = uint(a2) * uint(b3) + uint(a3) * uint(b2) + (result4 >> 8);
        uint result6 = uint(a3) * uint(b3) + (result5 >> 8);
        
        // Extract final result bytes
        byte finalResult0 = byte(result0 & 0xFF);
        byte finalResult1 = byte(result1 & 0xFF);
        byte finalResult2 = byte(result2 & 0xFF);
        byte finalResult3 = byte(result3 & 0xFF);
        byte finalResult4 = byte(result4 & 0xFF);
        byte finalResult5 = byte(result5 & 0xFF);
        byte finalResult6 = byte(result6 & 0xFF);
        
        // Create the final 32-bit result long
        long finalResultLow = FromBytes(finalResult0, finalResult1, finalResult2, finalResult3);
        long finalResultHigh = FromBytes(finalResult4, finalResult5, finalResult6, 0);
    
        // Combine low and high parts to get the final result
        return Add(finalResultLow, finalResultHigh);
    }
    
    long divMod(long dividend, long divisor, ref long remainder)
    {
        long zero = FromBytes(0, 0, 0, 0);

        if (EQ(divisor, zero))
        {
            Die(0x04); // division by zero attempted
        }

        long one = FromBytes(1, 0, 0, 0);
        long quotient = zero;
        remainder = zero;

        for (int i = 31; i >= 0; i--)
        {
            // Shift the remainder left by 1 and bring down the next bit of the dividend
            remainder = remainder.shiftLeft(1);
            remainder = remainder.or((dividend.shiftRight(i)).and(one));

            // If the remainder is greater than or equal to the divisor
            if (GE(remainder, divisor))
            {
                remainder = remainder.Sub(divisor);
                quotient = quotient.or(one.shiftLeft(i));
            }
        }

        return quotient;
    }

    long Div(long dividend, long divisor)
    {
        long remainder = 0;
        return divMod(dividend, divisor, ref remainder);
    }

    long Mod(long dividend, long divisor)
    {
        long remainder = 0;
        _ = divMod(dividend, divisor, ref remainder);
        return remainder;
    }

    bool EQ(long left, long right)
    {
        for (byte i = 0; i < 4; i++)
        {
            if (GetByte(left, i) != GetByte(right, i))
            {
                return false;
            }
        }
        return true;
    }

    bool GT(long left, long right)
    {
        byte leftSignByte = GetByte(left, 3);
        byte rightSignByte = GetByte(right, 3);

        // Compare the sign bytes first
        if (leftSignByte > rightSignByte)
        {
            return true;
        }
        else if (leftSignByte < rightSignByte)
        {
            return false;
        }

        // If sign bytes are equal, compare the rest
        for (int i = 2; i >= 0; i--)
        {
            byte leftByte = GetByte(left, byte(i));
            byte rightByte = GetByte(right, byte(i));

            if (leftByte > rightByte)
            {
                return true;
            }
            else if (leftByte < rightByte)
            {
                return false;
            }
        }
        return false; // They are equal
    }

    bool LT(long left, long right)
    {
        return !GE(left, right);
    }

    bool GE(long left, long right)
    {
        return GT(left, right) || EQ(left, right);
    }

    bool LE(long left, long right)
    {
        return LT(left, right) || EQ(left, right);
    }

    long Abs(long value)
    {
        long zero = FromBytes(0, 0, 0, 0); // 0 as a long
        return GreaterThanOrEqual(value, zero) ? value : Negate(value);
    }

    long Negate(long value)
    {
        long zero = FromBytes(0, 0, 0, 0); // 0 as a long
        return Sub(zero, value);
    }

    long Max(long a, long b)
    {
        return GT(a, b) ? a : b;
    }

    long Min(long a, long b)
    {
        return LT(a, b) ? a : b;
    }
    
    string ToString(long value)
    {
        long zero = FromBytes(0, 0, 0, 0); // 0 as a long
        long ten = FromBytes(10, 0, 0, 0); // 10 as a long
        string result = "";

        if (EQ(value, zero))
        {
            String.BuildFront(ref result, '0');
            return result;
        }

        bool isNegative = false;
        if (LT(value, zero))
        {
            isNegative = true;
            value = Negate(value);
        }

        while (!EQ(value, zero))
        {
            long remainder = 0;
            value = divMod(value, ten, ref remainder);
            char c = (char)(GetByte(remainder, 0) + '0');
            String.BuildFront(ref result, c);
        }

        if (isNegative)
        {
            String.BuildFront(ref result, '-');
        }

        return result;
    }

    string ToBinaryString(long this, byte digits)
    {
        char c;
        string result = "";
        for (int i = digits - 1; i >= 0; i--)
        {
            byte currentByte = GetByte(this, i / 8);
            byte bit = (currentByte >> (i % 8)) & 1;
            c = (bit == 1) ? '1' : '0';
            String.BuildFront(ref result, c);
        }
        return result;
    }

    string ToHexString(long this, byte digits)
    {
        char c;
        string result = "";
        for (int i = 0; i < digits; i++)
        {
            byte currentByte = GetByte(this, i);
            c = Byte.ToHex(currentByte);
            String.BuildFront(ref result, c);
        }
        return result;
    }

    int ToInt(long l)
    {
        long intMax = FromBytes(0xFF, 0xFF, 0x7F, 0x00); // Max value for int (32767)
        long intMin = FromBytes(0x00, 0x00, 0x80, 0xFF); // Min value for int (-32768)

        if (GreaterThan(l, intMax) || LessThan(l, intMin))
        {
            Die(0x0D); // numeric type out of range / overflow
        }

        return (int)((GetByte(l, 0) << 8) | GetByte(l, 1));
    }

    uint ToUInt(long l)
    {
        long uintMax = FromBytes(0xFF, 0xFF, 0x00, 0x00); // Max value for uint (65535)
        long uintMin = FromBytes(0x00, 0x00, 0x00, 0x00); // Min value for uint (0)

        if (GreaterThan(l, uintMax) || LessThan(l, uintMin))
        {
            Die(0x0D); // numeric type out of range / overflow
        }

        return (uint)((GetByte(l, 0) << 8) | GetByte(l, 1));
    }
    
    bool TryParse(string content, ref long returnValue)
    {
        long result;
        bool makeNegative;
        if (content.Length < 1)
        {
            return false;
        }
        if (content.StartsWith("0x"))
        {
            return tryParseHex(content, ref returnValue);
        }
        if (content.StartsWith('+'))
        {
            String.Substring(ref content, 1);
        }
        else if (content.StartsWith('-'))
        {
            String.Substring(ref content, 1);
            makeNegative = true;
        }
        foreach (var c in content)
        {
            result = result * 10;
            if (!c.IsDigit())
            {
                return false;
            }
            result = result + (byte(c) - 48); // 48 is ASCII for '0'
        }
        if (makeNegative)
        {
            result = -result;
        }
        returnValue = result;
        return true;
    }
    bool tryParseHex(string content, ref long returnValue)
    {
        bool success;
        uint length;
        uint i;
        char c;
        loop
        {
            returnValue = 0;
            if (!content.StartsWith("0x"))
            {
                break;
            }
            length = content.Length;
            if (length < 3)
            {
                break;
            }
            success = true;
            for (i=0; i < length-2; i++)
            {
                returnValue = returnValue * 16;
                c = content.GetChar(i+2);
                if (c.IsDigit())
                {
                    returnValue = returnValue + (byte(c) - 48); // 48 is ASCII for '0'
                }
                else if (c.IsHexDigit())
                {
                    returnValue = returnValue + (byte(c.ToLower()) - 87); // 97 is ASCII for 'a', -97+10 = -87
                }
                else
                {
                    success = false;
                    break;
                }
            }
            break;
        }
        return success;
    }
    
    float ToFloat(long l)
    {
        if (l == 0)
        {
            return Float.FromBytes(0, 0, 0, 0);
        }

        byte sign = (l < 0) ? 1 : 0;
        if (sign == 1)
        {
            l = -l;
        }

        byte exponent = 127 + 23;
        long mantissa = shiftLeft(l, 8);

        Float.normalize(ref mantissa, ref exponent);

        return Float.combineComponents(sign, exponent, mantissa);
    }
    
    long shiftLeft(long value, int bits)
    {
        // Shifts the long value left by the specified number of bits
        long result = value;
        for (int i = 0; i < bits; i++)
        {
            result = result.shiftLeftOne();
        }
        return result;
    }

    long shiftLeftOne(long value)
    {
        // Shift the value left by 1 bit
        long result = FromBytes(
            (GetByte(value, 1) << 1) | (GetByte(value, 0) >> 7),
            (GetByte(value, 2) << 1) | (GetByte(value, 1) >> 7),
            (GetByte(value, 3) << 1) | (GetByte(value, 2) >> 7),
            (GetByte(value, 3) >> 7)
        );
        return result;
    }

    long shiftRight(long value, int bits)
    {
        // Shifts the long value right by the specified number of bits
        long result = value;
        for (int i = 0; i < bits; i++)
        {
            result = result.shiftRightOne();
        }
        return result;
    }

    long shiftRightOne(long value)
    {
        // Shift the value right by 1 bit
        long result = FromBytes(
            (GetByte(value, 0) >> 1),
            (GetByte(value, 1) >> 1) | ((GetByte(value, 0) & 1) << 7),
            (GetByte(value, 2) >> 1) | ((GetByte(value, 1) & 1) << 7),
            (GetByte(value, 3) >> 1) | ((GetByte(value, 2) & 1) << 7)
        );
        return result;
    }

    long or(long left, long right)
    {
        return FromBytes(
            GetByte(left, 0) | GetByte(right, 0),
            GetByte(left, 1) | GetByte(right, 1),
            GetByte(left, 2) | GetByte(right, 2),
            GetByte(left, 3) | GetByte(right, 3)
        );
    }

    long and(long left, long right)
    {
        return FromBytes(
            GetByte(left, 0) & GetByte(right, 0),
            GetByte(left, 1) & GetByte(right, 1),
            GetByte(left, 2) & GetByte(right, 2),
            GetByte(left, 3) & GetByte(right, 3)
        );
    }

    long xor(long a, long b)
    {
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);

        byte b0 = GetByte(b, 0);
        byte b1 = GetByte(b, 1);
        byte b2 = GetByte(b, 2);
        byte b3 = GetByte(b, 3);

        byte result0 = a0 ^ b0;
        byte result1 = a1 ^ b1;
        byte result2 = a2 ^ b2;
        byte result3 = a3 ^ b3;

        return FromBytes(result0, result1, result2, result3);
    }

    long not(long a)
    {
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);

        byte result0 = ~a0;
        byte result1 = ~a1;
        byte result2 = ~a2;
        byte result3 = ~a3;

        return FromBytes(result0, result1, result2, result3);
    }
    /*
    <byte> ToBytes(long this)
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
