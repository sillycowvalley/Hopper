unit Long
{
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

        // Combine bytes into 16-bit ints
        int lowA = Int.FromBytes(a0, a1);
        int highA = Int.FromBytes(a2, a3);

        int lowB = Int.FromBytes(b0, b1);
        int highB = Int.FromBytes(b2, b3);

        // Add low parts and handle carry
        int lowResult = lowA + lowB;
        int carry = (lowResult > 0xFFFF) ? 1 : 0; // Check for carry
        lowResult = lowResult & 0xFFFF; // Keep only the lower 16 bits

        // Add high parts with carry
        int highResult = highA + highB + carry;

        // Extract bytes from the result
        byte result0 = Int.GetByte(lowResult, 0);
        byte result1 = Int.GetByte(lowResult, 1);
        byte result2 = Int.GetByte(highResult, 0);
        byte result3 = Int.GetByte(highResult, 1);

        // Create new long from result bytes
        return FromBytes(result0, result1, result2, result3);
    }

    long Subtract(long a, long b)
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

        // Combine bytes into 16-bit ints
        int lowA = Int.FromBytes(a0, a1);
        int highA = Int.FromBytes(a2, a3);

        int lowB = Int.FromBytes(b0, b1);
        int highB = Int.FromBytes(b2, b3);

        // Subtract low parts and handle borrow
        int lowResult = lowA - lowB;
        int borrow = (lowResult < 0) ? 1 : 0; // Check for borrow
        lowResult = lowResult & 0xFFFF; // Keep only the lower 16 bits

        // Subtract high parts with borrow
        int highResult = highA - highB - borrow;

        // Extract bytes from the result
        byte result0 = Int.GetByte(lowResult, 0);
        byte result1 = Int.GetByte(lowResult, 1);
        byte result2 = Int.GetByte(highResult, 0);
        byte result3 = Int.GetByte(highResult, 1);

        // Create new long from result bytes
        return FromBytes(result0, result1, result2, result3);
    }

    long Multiply(long a, long b)
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

        // Combine bytes into 16-bit ints
        int lowA = Int.FromBytes(a0, a1);
        int highA = Int.FromBytes(a2, a3);

        int lowB = Int.FromBytes(b0, b1);
        int highB = Int.FromBytes(b2, b3);

        // Multiply parts
        int lowResult = lowA * lowB;
        int middleResult1 = lowA * highB;
        int middleResult2 = highA * lowB;
        int highResult = highA * highB;

        // Combine results to form a 32-bit long result
        // Here we shift and add the intermediate results appropriately
        long combinedLow = FromBytes(
            Int.GetByte(lowResult, 0),
            Int.GetByte(lowResult, 1),
            Int.GetByte(middleResult1, 0),
            Int.GetByte(middleResult1, 1)
        );

        long combinedHigh = FromBytes(
            Int.GetByte(middleResult2, 0),
            Int.GetByte(middleResult2, 1),
            Int.GetByte(highResult, 0),
            Int.GetByte(highResult, 1)
        );

        // Sum the combined low and high results to form the final long result
        // Note: This simplified approach assumes that there will be no overflow, which may need to be handled separately.
        long finalResult = Add(combinedLow, combinedHigh);

        return finalResult;
    }
    
    long divMod(long dividend, long divisor, ref long remainder)
    {
        long zero = FromBytes(0, 0, 0, 0);
        
        if (equal(divisor, zero))
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
            if (greaterThanOrEqual(remainder, divisor))
            {
                remainder = remainder.Subtract(divisor);
                quotient = quotient.or(one.shiftLeft(i));
            }
        }

        return quotient;
    }

    long Divide(long dividend, long divisor)
    {
        long remainder = 0;
        return divMod(dividend, divisor, ref remainder);
    }

    long Modulo(long dividend, long divisor)
    {
        long remainder = 0;
        divMod(dividend, divisor, ref remainder);
        return remainder;
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
    
    bool Equal(long left, long right)
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

    bool GreaterThan(long left, long right)
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
        for (byte i = 2; i >= 0; i--)
        {
            byte leftByte = GetByte(left, i);
            byte rightByte = GetByte(right, i);

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

    bool LessThan(long left, long right)
    {
        return !GreaterThanOrEqual(left, right);
    }

    bool GreaterThanOrEqual(long left, long right)
    {
        return GreaterThan(left, right) || Equal(left, right);
    }

    bool LessThanOrEqual(long left, long right)
    {
        return LessThan(left, right) || Equal(left, right);
    }

    bool NotEqual(long left, long right)
    {
        return !Equal(left, right);
    }
    
    long Abs(long value)
    {
        long zero = FromBytes(0, 0, 0, 0); // 0 as a long
        return GreaterThanOrEqual(value, zero) ? value : Negate(value);
    }

    long Negate(long value)
    {
        long zero = FromBytes(0, 0, 0, 0); // 0 as a long
        return Subtract(zero, value);
    }
    
    long Max(long a, long b)
    {
        return GreaterThan(a, b) ? a : b;
    }
    long Min(long a, long b)
    {
        return LessThan(a, b) ? a : b;
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

}
