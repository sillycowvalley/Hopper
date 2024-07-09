unit Long
{
    uses "Float"
    
    friend Float, Int, UInt;
    
    byte GetByte(long this, byte index) system;
    long FromBytes(byte b0, byte b1, byte b2, byte b3) system;
    

    long Add(long a, long b) system;
    long Sub(long a, long b) system;
    long Mul(long ai, long bi) system;
    long Div(long dividend, long divisor) system;
    long Mod(long dividend, long divisor) system;
    long Negate(long value) system;
    
    /*
    long Add(long a, long b)
    {
        byte result0;
        byte result1;
        byte result2;
        byte result3;
    
        uint carry;
    
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
        byte result0;
        byte result1;
        byte result2;
        byte result3;
    
        uint borrow;
    
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
    long Div(long dividend, long divisor)
    {
        long remainder = 0;
        return divMod(dividend, divisor, ref remainder);
    }
    long Mod(long dividend, long divisor)
    {
        long remainder;
        _ = divMod(dividend, divisor, ref remainder);
        return remainder;
    }
    long Negate(long value)
    {
        return Sub(long(0), value);
    }
    long Mul(long ai, long bi)
    {
        long a = ai;
        long b = bi;
        if (EQ(a, long(0)))
        {
            return a;
        }
        if (EQ(b, long(0)))
        {
            return b;
        }
        if (EQ(a, long(1)))
        {
            return b;
        }
        if (EQ(b, long(1)))
        {
            return a;
        }
        
        // Determine the signs of the operands
        bool isNegativeA = (GetByte(a, 3) & 0x80) != 0;
        bool isNegativeB = (GetByte(b, 3) & 0x80) != 0;
        
        // If the values are negative, convert them to positive equivalents
        if (isNegativeA)
        {
            a = Negate(a);
        }
        if (isNegativeB)
        {
            b = Negate(b);
        }
        
        // Extract bytes from both longs
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);
    
        byte b0 = GetByte(b, 0);
        byte b1 = GetByte(b, 1);
        byte b2 = GetByte(b, 2);
        byte b3 = GetByte(b, 3);
    
        // Perform multiplication using 8-bit parts and cast results to long
        long result0 = long(uint(a0) * uint(b0));
        long result1 = long(uint(a0) * uint(b1)) + long(uint(a1) * uint(b0));
        long result2 = long(uint(a0) * uint(b2)) + long(uint(a1) * uint(b1)) + long(uint(a2) * uint(b0));
        long result3 = long(uint(a0) * uint(b3)) + long(uint(a1) * uint(b2)) + long(uint(a2) * uint(b1)) + long(uint(a3) * uint(b0));
        long result4 = long(uint(a1) * uint(b3)) + long(uint(a2) * uint(b2)) + long(uint(a3) * uint(b1));
        long result5 = long(uint(a2) * uint(b3)) + long(uint(a3) * uint(b2));
        long result6 = long(uint(a3) * uint(b3));
    
        // Split results into bytes and handle carries
        uint r0 = result0.GetByte(0);
        uint r1 = result1.GetByte(0);
        uint r2 = result2.GetByte(0);
        uint r3 = result3.GetByte(0);
        uint r4 = result4.GetByte(0);
        uint r5 = result5.GetByte(0);
        uint r6 = result6.GetByte(0);
    
        uint carry0 = (result0.GetByte(1) + r1) >> 8;
        r1 += result0.GetByte(1);
        uint carry1 = (result1.GetByte(1) + r2 + carry0) >> 8;
        r2 += result1.GetByte(1) + carry0;
        uint carry2 = (result2.GetByte(1) + r3 + carry1) >> 8;
        r3 += result2.GetByte(1) + carry1;
        uint carry3 = (result3.GetByte(1) + r4 + carry2) >> 8;
        r4 += result3.GetByte(1) + carry2;
        uint carry4 = (result4.GetByte(1) + r5 + carry3) >> 8;
        r5 += result4.GetByte(1) + carry3;
        uint carry5 = (result5.GetByte(1) + r6 + carry4) >> 8;
        r6 += result5.GetByte(1) + carry4;
    
        long finalResult = Long.FromBytes(byte(r0), byte(r1), byte(r2), byte(r3));
        finalResult = Long.Add(finalResult, Long.shiftLeft(Long.FromBytes(byte(r4), byte(r5), byte(r6), byte(carry5)), 16));
    
        if ((isNegativeA || isNegativeB) && (isNegativeA != isNegativeB))
        {
            finalResult = Negate(finalResult);
        }
        return finalResult;
    }
    
    long divMod(long dividendi, long divisori, ref long remainder)
    {
        long dividend = dividendi;
        long divisor  = divisori;
        if (EQ(divisor, long(0)))
        {
            Die(0x04); // division by zero attempted
        }
        
        // Determine the signs of the operands
        bool isNegativeA = (GetByte(dividend, 3) & 0x80) != 0;
        bool isNegativeB = (GetByte(divisor, 3) & 0x80) != 0;
    
        // If the values are negative, convert them to positive equivalents
        if (isNegativeA)
        {
            dividend = Negate(dividend);
        }
        if (isNegativeB)
        {
            divisor = Negate(divisor);
        }

        long one = 1;
        long quotient;
        remainder = 0;

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
        if ((isNegativeA || isNegativeB) && (isNegativeA != isNegativeB))
        {
            quotient = Negate(quotient);
        }
        return quotient;
    }
    */

    

    bool EQ(long left, long right) system;
    bool LT(long left, long right) system;
    bool GT(long left, long right) system;
    bool GE(long left, long right) system;
    bool LE(long left, long right) system;
    /*
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
    bool LT(long left, long right)
    {
        return !GE(left, right);
    }
    bool GT(long left, long right)
    {
        byte leftSignByte = GetByte(left, 3);
        byte rightSignByte = GetByte(right, 3);
    
        // Compare the sign bytes first
        if (((leftSignByte & 0x80) == 0) && ((rightSignByte & 0x80) != 0))
        {
            // Left is positive and right is negative
            return true;
        }
        else if (((leftSignByte & 0x80) != 0) && ((rightSignByte & 0x80) == 0))
        {
            // Left is negative and right is positive
            return false;
        }
    
        // If both are positive, or both are negative, we need to compare accordingly
        bool bothNegative = ((leftSignByte & 0x80) != 0) && ((rightSignByte & 0x80) != 0);
    
        // Compare the rest of the bytes
        for (int i = 3; i >= 0; i--)
        {
            byte leftByte = GetByte(left, byte(i));
            byte rightByte = GetByte(right, byte(i));
            
            if (leftByte > rightByte)
            {
                return bothNegative ? false : true;
            }
            else if (leftByte < rightByte)
            {
                return bothNegative ? true : false;
            }
        }
    
        // They are equal
        return false;
    }

    bool GE(long left, long right)
    {
        return GT(left, right) || EQ(left, right);
    }

    bool LE(long left, long right)
    {
        return LT(left, right) || EQ(left, right);
    }
    */
    long Abs(long value)
    {
        return (value >= 0) ? value : -value;
    }

    long Max(long a, long b)
    {
        return (a > b) ? a : b;
    }

    long Min(long a, long b)
    {
        return (a < b) ? a : b;
    }
    
    string ToString(long this)
    {
        long ten = 10;
        long value = this;
        string result;

        if (value == 0)
        {
            String.BuildFront(ref result, '0');
            return result;
        }

        bool isNegative = false;
        if (value < 0)
        {
            isNegative = true;
            value = Negate(value);
        }

        while (value != 0)
        {
            long remainder = value % ten;
            value = value / ten;
            /*
            long remainder;
            value = divMod(value, ten, ref remainder);
            */
            char c = char(GetByte(remainder, 0) + '0');
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
        string result;
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
        string result;
        for (byte i = 0; i < digits; i++)
        {
            byte currentByte = GetByte(this, (i >> 1));
            if (i % 2 == 1)
            {
                currentByte = currentByte >> 4;
            }
            c = Byte.ToHex(currentByte & 0x0F); // Masking the lower nibble
            String.BuildFront(ref result, c);
        }
        return result;
    }

    int ToInt(long this)
    {
        long l = this;
        
        if ((l > 32767) || (l < -32768))
        {
            WriteLn(l.ToString() + " ");
            Die(0x0D); // numeric type out of range / overflow
        }
    
        int result;
        if (l < 0) // Check if the value is negative
        {
            l = -l; // Get the positive equivalent
            byte lowByte  = Long.GetByte(l, 0);
            byte highByte = Long.GetByte(l, 1);
            result = -Int.FromBytes(lowByte, highByte); // Negate the result
        }
        else
        {
            byte lowByte  = Long.GetByte(l, 0);
            byte highByte = Long.GetByte(l, 1);
            result = Int.FromBytes(lowByte, highByte); // Direct conversion for positive values
        }
        
        return result;
    }
    
    

    uint ToUInt(long l)
    {
        if ((l > 65535) || (l < 0))
        {
            Die(0x0D); // numeric type out of range / overflow
        }
        return UInt.FromBytes(GetByte(l, 0), GetByte(l, 1));
    }
    
    bool TryParse(string input, ref long returnValue)
    {
        long result;
        bool makeNegative;
        string content = input;
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
    
    float ToFloat(long this)
    {
        long l = this;
        if (l == 0)
        {
            return Float.FromBytes(0, 0, 0, 0);
        }
    
        byte sign = (l < 0) ? 1 : 0;
        if (sign == 1)
        {
            l = -l;
        }
    
        int exponent = 127 + 23; // Bias + initial shift for normalization
    
        // Normalize the mantissa using countLeadingZeros
        byte leadingZeros = Float.countLeadingZeros(l);
        long mantissa;
    
        if (leadingZeros <= 8) // if leadingZeros <= 8, we need to shift right
        {
            mantissa = Long.shiftRight(l, 8 - leadingZeros);
            exponent += (8 - leadingZeros);
        }
        else // if leadingZeros > 8, we need to shift left
        {
            mantissa = Long.shiftLeft(l, leadingZeros - 8);
            exponent -= (leadingZeros - 8);
        }
    
        // Remove the implicit leading bit to the mantissa
        mantissa = Long.and(mantissa, Long.FromBytes(0xFF, 0xFF, 0x7F, 0));
    
        // Handle exponent overflow/underflow
        if (exponent <= 0) 
        {
            // Underflow: result is too small to be represented
            exponent = 0;
            mantissa = 0;
        }
        else if (exponent >= 255) 
        {
            // Overflow: result is too large to be represented
            exponent = 255;
            mantissa = 0;
        }
    
        float result = Float.combineComponents(sign, byte(exponent), mantissa);
        return result;
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
        long result = FromBytes(
            (GetByte(value, 0) << 1),
            (GetByte(value, 1) << 1) | (GetByte(value, 0) >> 7),
            (GetByte(value, 2) << 1) | (GetByte(value, 1) >> 7),
            (GetByte(value, 3) << 1) | (GetByte(value, 2) >> 7)
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
            (GetByte(value, 0) >> 1) | ((GetByte(value, 1) & 1) << 7),
            (GetByte(value, 1) >> 1) | ((GetByte(value, 2) & 1) << 7),
            (GetByte(value, 2) >> 1) | ((GetByte(value, 3) & 1) << 7),
            (GetByte(value, 3) >> 1)
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

    long and(long a, long b)
    {
        byte a0 = GetByte(a, 0);
        byte a1 = GetByte(a, 1);
        byte a2 = GetByte(a, 2);
        byte a3 = GetByte(a, 3);
    
        byte b0 = GetByte(b, 0);
        byte b1 = GetByte(b, 1);
        byte b2 = GetByte(b, 2);
        byte b3 = GetByte(b, 3);
    
        byte result0 = a0 & b0;
        byte result1 = a1 & b1;
        byte result2 = a2 & b2;
        byte result3 = a3 & b3;
    
        return FromBytes(result0, result1, result2, result3);
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
}
