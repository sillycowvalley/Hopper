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
    
#ifdef FAST_6502_RUNTIME    
    float Add(float a, float b) system;
    float Sub(float a, float b) system;
    float Mul(float a, float b) system;
    float Div(float a, float b) system;
    
    bool EQ(float a, float b) system;
    bool LT(float a, float b) system;
    long ToLong(float f) system;
#else

    byte countLeadingZeros(long result)
    {
        byte count;
        long one = 1; // 0x00000001 as a long
    
        for (int i = 31; i >= 0; i--)
        {
            if (!Long.EQ(Long.and(result, Long.shiftLeft(one, i)), Long.FromBytes(0, 0, 0, 0)))
            {
                return count;
            }
            count++;
        }
        return count; // all zero, return 32
    }

    mantissaDivide(long dividend, long divisor, ref long quotient, ref long remainder)
    {
        long zero;      // 0x00000000 in 32-bit
        long one  = 1;  // 0x00000001 in 32-bit
        
        quotient  = zero;
        remainder = zero;
    
        for (int i = 47; i >= 0; i--)
        {
            // Shift remainder left by 1 and combine with the next bit from extendedDividendHigh or extendedDividendLow
            remainder = remainder.shiftLeftOne();
            if (i >= 24)
            {
                remainder = Long.or(remainder, Long.and(Long.shiftRight(dividend, i-16), one));
            }
            else
            {
                remainder = Long.or(remainder, Long.and(Long.shiftLeft(dividend, 24-i), one));
            }
            // If the remainder is greater than or equal to the divisor, subtract the divisor from the remainder
            if (remainder >= divisor)
            {
                remainder = remainder - divisor;
                quotient = Long.or(quotient, Long.shiftLeft(one, i));
            }
        }
    }
         
    float Div(float a, float b)
    {
        byte signA = getSign(a);
        byte signB = getSign(b);
        byte resultSign = signA ^ signB;
        if (isZero(a))
        {
            return Float.FromBytes(0,0,0, (resultSign << 7));
        }
        if (EQ(b, float(1)))
        {
            return a;
        }
        if (isZero(b))
        {
            Die(0x04); // Division by zero
        }
        
        byte exponentA = getExponent(a);
        long mantissaA = getMantissa(a);
        byte exponentB = getExponent(b);
        long mantissaB = getMantissa(b);
        // Add the implicit leading bit
        mantissaA = Long.or(mantissaA, Long.FromBytes(0, 0, 0x80, 0)); // 0x00800000 in 32-bit
        mantissaB = Long.or(mantissaB, Long.FromBytes(0, 0, 0x80, 0)); // 0x00800000 in 32-bit
        
        long quotient;
        long remainder;
        mantissaDivide(mantissaA, mantissaB, ref quotient, ref remainder); // Divide mantissas
        
        int resultExponent = int(exponentA) - int(exponentB) + 127; // Subtract exponents
        
        long resultMantissa = quotient;
        
        // Normalize the result
        byte leadingZeros = countLeadingZeros(resultMantissa);
        if (leadingZeros > 8)
        {
            resultMantissa = Long.shiftLeft(resultMantissa, leadingZeros - 8);
        }
        else if (leadingZeros < 8)
        {
            resultMantissa = Long.shiftRight(resultMantissa, 8 - leadingZeros);
        }
        resultMantissa = Long.and(resultMantissa, Long.FromBytes(0xFF, 0xFF, 0x7F, 0)); // remove the implicit leading 1
        
        // Minimal adjustment for leadingZeros == 16
        if (leadingZeros == 16)
        {
            resultExponent -= 1;
        }
        
        // Handle exponent overflow/underflow
        if (resultExponent <= 0) 
        {
            // Underflow: result is too small to be represented
            resultExponent = 0;
            resultMantissa  = 0;
        }
        else if (resultExponent >= 255) 
        {
            // Overflow: result is too large to be represented
            resultExponent = 255;
            resultMantissa  = 0;
        }
        
        float result = combineComponents(resultSign, byte(resultExponent), resultMantissa);
        return result;
    }
    
    float Add(float a, float b)
    {
        if (a == 0) { return b; }
        if (a == 0) { return a; }
        
        byte signA     = getSign(a);
        int exponentA  = getExponent(a);  // Change to int for processing
        long mantissaA = getMantissa(a);
        
        byte signB     = getSign(b);
        int exponentB  = getExponent(b);  // Change to int for processing
        long mantissaB = getMantissa(b);
        
        // Add the implicit leading bit
        mantissaA = Long.or(mantissaA, Long.FromBytes(0, 0, 0x80, 0)); // 0x00800000 in 32-bit
        mantissaB = Long.or(mantissaB, Long.FromBytes(0, 0, 0x80, 0)); // 0x00800000 in 32-bit
    
        // Align exponents
        if (exponentA > exponentB) {
            int shift = exponentA - exponentB;
            mantissaB = Long.shiftRight(mantissaB, shift);
            exponentB = exponentA;
        }
        else if (exponentA < exponentB) {
            int shift = exponentB - exponentA;
            mantissaA = Long.shiftRight(mantissaA, shift);
            exponentA = exponentB;
        }
    
        long resultMantissa;
        if (signA == signB) {
            resultMantissa = mantissaA + mantissaB; // Same sign: addition
        }
        else {
            if (mantissaA >= mantissaB) {
                resultMantissa = mantissaA - mantissaB; // Different signs: subtraction
            }
            else {
                resultMantissa = mantissaB - mantissaA;
                signA = signB;
            }
            
            // Check for zero mantissa (cancellation)
            if (resultMantissa == 0) {
                return 0; // Return +0.0
            }
        }
        
        // Normalize the result
        byte leadingZeros = countLeadingZeros(resultMantissa);
        if (leadingZeros < 8) {
            resultMantissa = Long.shiftRight(resultMantissa, 8 - leadingZeros);
        }
        else {
            resultMantissa = Long.shiftLeft(resultMantissa, leadingZeros - 8);
        }
        exponentA = exponentA + 8 - leadingZeros;
        
        // Handle exponent overflow/underflow
        if (exponentA <= 0) {
            // Underflow: result is too small to be represented
            exponentA = 0;
            resultMantissa  = 0;
        }
        else if (exponentA >= 255) {
            // Overflow: result is too large to be represented
            exponentA = 255;
            resultMantissa  = 0;
        }
    
        // Remove the implicit leading bit
        resultMantissa = Long.and(resultMantissa, Long.FromBytes(0xFF, 0xFF, 0x7F, 0));
    
        // Convert exponent back to byte for combining
        float result = combineComponents(signA, byte(exponentA), resultMantissa); 
        return result;
    }
    float Sub(float a, float b)
    {
        byte signB = getSign(b);
        signB = signB ^ 1; // Flip the sign of b
        float negativeB = combineComponents(signB, getExponent(b), getMantissa(b));
        return a + negativeB;
    }
    
    mantissaMultiply(long mantissaA, long mantissaB, ref long resultHigh, ref long resultLow)
    {
        // Extract the higher and lower parts of the mantissas
        long aHigh = Long.shiftRight(mantissaA, 12);
        long aLow = Long.and(mantissaA, Long.FromBytes(0xFF, 0x0F, 0x00, 0x00)); // Bottom 12 bits
        long bHigh = Long.shiftRight(mantissaB, 12);
        long bLow = Long.and(mantissaB, Long.FromBytes(0xFF, 0x0F, 0x00, 0x00)); // Bottom 12 bits
    
        // Perform the multiplications
        long highHigh = Long.Mul(aHigh, bHigh);  // Bits 24-47
        long highLow  = Long.Mul(aHigh, bLow);   // Bits 12-35
        long lowHigh  = Long.Mul(aLow, bHigh);   // Bits 12-35
        long lowLow   = Long.Mul(aLow, bLow);    // Bits 0-23
    
        // Initialize lowerLong with the lower 16 bits of lowLow
        long lowerLong = Long.and(lowLow, Long.FromBytes(0xFF, 0xFF, 0, 0));
    
        // Add the overlapping bits from highLow and lowHigh
        long highLowOverlap = Long.and(highLow, Long.FromBytes(0x0F, 0x00, 0, 0)); // Extract bits 12..15 (0x0000000F)
        long lowHighOverlap = Long.and(lowHigh, Long.FromBytes(0x0F, 0x00, 0, 0)); // Extract bits 12..15 (0x0000000F)
        
        // Shift these overlaps to the right position and add them to lowerLong
        lowerLong = Long.Add(lowerLong, Long.shiftLeft(highLowOverlap, 12));
        lowerLong = Long.Add(lowerLong, Long.shiftLeft(lowHighOverlap, 12));
    
        // Check for carry to middleLong
        long middleLong = Long.shiftRight(lowerLong, 16);
        
        // Mask lowerLong to 16 bits after adding
        lowerLong = Long.and(lowerLong, Long.FromBytes(0xFF, 0xFF, 0, 0));
    
        // Accumulate the 16..31 bits
        
        // Add bits 16..31 of lowLow
        middleLong = Long.Add(middleLong, Long.shiftRight(lowLow, 16)); 
        
        // Add the overlapping bits from highLow and lowHigh
        
        // Extract bits 16..31 from 12..35 (0x000FFFF0)
        highLowOverlap  = Long.and(highLow, Long.FromBytes(0xF0, 0xFF, 0x0F, 0)); 
        // Extract bits 16..31 from 12..35 (0x000FFFF0)
        lowHighOverlap  = Long.and(lowHigh, Long.FromBytes(0xF0, 0xFF, 0x0F, 0)); 
        // Extract bits 24..31 from 24..47 (0x000000FF)
        long highHighOverlap = Long.and(highHigh, Long.FromBytes(0xFF, 0, 0, 0)); 
    
        // Add bits 16..31 of highLow
        middleLong = Long.Add(middleLong, Long.shiftRight(highLowOverlap, 4));  
        // Add bits 16..31 of lowHigh
        middleLong = Long.Add(middleLong, Long.shiftRight(lowHighOverlap, 4));  
        // Add bits 24..31 of highHigh
        middleLong = Long.Add(middleLong, Long.shiftLeft (highHighOverlap, 8)); 
    
        // Check for carry to upperLong
        long upperLong = Long.shiftRight(middleLong, 16);
    
        // Mask middleLong to 16 bits after adding
        middleLong = Long.and(middleLong, Long.FromBytes(0xFF, 0xFF, 0, 0));
    
        // Accumulate the 32..47 bits
        
        // Extract bits 32..35 (0x0000000F)
        highLowOverlap = Long.and(Long.shiftRight(highLow, 20), Long.FromBytes(0x0F, 0x00, 0, 0));
        // Extract bits 32..35 (0x0000000F)
        lowHighOverlap = Long.and(Long.shiftRight(lowHigh, 20), Long.FromBytes(0x0F, 0x00, 0, 0)); 
        // Extract bits 32..47
        highHighOverlap = Long.shiftRight(highHigh, 8);                                            
        
        upperLong = Long.Add(upperLong, highLowOverlap);  // Add bits 32..47 of highLow
        upperLong = Long.Add(upperLong, lowHighOverlap);  // Add bits 32..47 of lowHigh
        upperLong = Long.Add(upperLong, highHighOverlap); // Add bits 32..47 of highHigh
    
        // Mask upperLong to 16 bits after adding
        upperLong = Long.and(upperLong, Long.FromBytes(0xFF, 0xFF, 0, 0));
    
        // Combine results into final mantissa
        resultLow = Long.or(lowerLong, Long.shiftLeft(middleLong, 16));
        resultHigh = upperLong;
    }
    
    byte countLeadingZeros(long resultHigh, long resultLow)
    {
        byte count = countLeadingZeros(resultHigh);
        if (count != 32)
        {
            return count;
        }
        count += countLeadingZeros(resultLow);
        return count; // If both resultHigh and resultLow are zero, return 64
    }
    
    shiftRight64Bit(ref long resultHigh, ref long resultLow, byte shift)
    {
        if (shift >= 32)
        {
            resultLow = Long.shiftRight(resultHigh, shift - 32);
            resultHigh = 0;
        }
        else
        {
            resultLow  = Long.shiftRight(resultLow, shift);
            resultLow  = Long.or(resultLow, Long.shiftLeft(resultHigh, 32 - shift));
            resultHigh = Long.shiftRight(resultHigh, shift);
        }
    }
    
    float Mul(float a, float b)
    {
        byte signA = getSign(a);
        byte signB = getSign(b);
        byte resultSign = signA ^ signB;
        if (isZero(a)) 
        {
            return Float.FromBytes(0,0,0,(resultSign << 7));
        }
        if (isZero(b)) 
        {
            return Float.FromBytes(0,0,0,(resultSign << 7));
        }
        
        int exponentA = getExponent(a);
        long mantissaA = getMantissa(a);
        int exponentB = getExponent(b);
        long mantissaB = getMantissa(b);
        
        // Add the implicit leading bit
        mantissaA = Long.or(mantissaA, Long.FromBytes(0, 0, 0x80, 0)); // 0x00800000 in 32-bit
        mantissaB = Long.or(mantissaB, Long.FromBytes(0, 0, 0x80, 0)); // 0x00800000 in 32-bit
        
        // Perform the multiplication
        long resultHigh;
        long resultLow;
        mantissaMultiply(mantissaA, mantissaB, ref resultHigh, ref resultLow);
        
        // Normalize the result
        byte leadingZeros = countLeadingZeros(resultHigh, resultLow);
        if (leadingZeros < 40)
        {
            shiftRight64Bit(ref resultHigh, ref resultLow, 40 - leadingZeros);
        }
        else
        {
            resultLow = Long.shiftLeft(resultLow, leadingZeros - 40);
        }
        
        // Now the mantissa of interest is in resultLow
        long resultMantissa = Long.and(resultLow, Long.FromBytes(0xFF, 0xFF, 0x7F, 0));  // remove the implicit leading 1
        
        int resultExponent = exponentA + exponentB - 127;
        if (leadingZeros == 16)
        {
            resultExponent++;
        }
    
        // Handle exponent overflow/underflow
        if (resultExponent <= 0) 
        {
            // Underflow: result is too small to be represented
            resultExponent = 0;
            resultMantissa = 0;
        }
        else if (resultExponent >= 255) 
        {
            // Overflow: result is too large to be represented
            resultExponent = 255;
            resultMantissa = 0;
        }
        
        float result = combineComponents(resultSign, byte(resultExponent), resultMantissa);
        return result;
    }
    bool EQ(float a, float b)
    {
        if (isZero(a) && isZero(b))
        {
            return true;
        }
        return (GetByte(a, 0) == GetByte(b, 0)) &&
               (GetByte(a, 1) == GetByte(b, 1)) &&
               (GetByte(a, 2) == GetByte(b, 2)) &&
               (GetByte(a, 3) == GetByte(b, 3));
    }

    long ToLong(float f)
    {
        long result;
        byte sign = getSign(f);
        int exponent = getExponent(f) - 127; // Bias adjustment
        loop
        {
            if (exponent < 0)
            {
                // exponent is -ve so fraction only
                break;
            }
            long mantissa = getMantissa(f);
            mantissa = Long.or(mantissa, Long.FromBytes(0, 0, 0x80, 0)); // Add the implicit leading bit
        
            
            if (exponent > 23)
            {
                result = Long.shiftLeft(mantissa, exponent - 23);
            }
            else if (exponent < 23)
            {
                result = Long.shiftRight(mantissa, 23 - exponent);
            }
            else
            {
                result = mantissa;
            }
            if (sign == 1)
            {
                result = -result;
            }
            break;
        }
        return result;
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
        return Long.FromBytes(GetByte(f, 0), GetByte(f, 1), GetByte(f, 2) & 0x7F, 0);
    }
    bool isZero(float this)
    {
        return (GetByte(this, 0) == 0) && (GetByte(this, 1) == 0) && (GetByte(this, 2) == 0) && ((GetByte(this, 3) & 0x7F) == 0);
    }
    float negate(float value)
    {
        byte sign = getSign(value) ^ 1;
        byte exponent = getExponent(value);
        long mantissa = getMantissa(value);
        return combineComponents(sign, exponent, mantissa);
    }
    float combineComponents(byte sign, byte exponent, long mantissa)
    {
        // Extract bytes from mantissa
        byte b0 = Long.GetByte(mantissa, 0);
        byte b1 = Long.GetByte(mantissa, 1);
        byte b2 = Long.GetByte(mantissa, 2) & 0x7F; // Only take the 7 least significant bits
        b2 |= byte((exponent & 1) << 7); // Set the least significant bit of the exponent in b2
    
        byte b3 = byte((exponent >> 1) & 0x7F); // Take the next 7 bits of the exponent
        b3 |= byte(sign << 7); // Set the sign bit in b3
    
        return FromBytes(b0, b1, b2, b3);
    }
    
    bool LT(float a, float b)
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
#endif
    string ToString(float this)
    {
        float value = this;
        byte b3 = value.GetByte(3);
        bool isNegative = (b3 & 0x80) != 0;
        if ((b3 == 0) && (value.GetByte(0) == 0) && (value.GetByte(1) == 0) && (value.GetByte(2) == 0))
        {
            return isNegative ? "-0" : "0";
        }
        if (isNegative)
        {
            byte b0 = value.GetByte(0);
            byte b1 = value.GetByte(1);
            byte b2 = value.GetByte(2);
            value = Float.FromBytes(b0, b1, b2, (b3 & 0x7F));
        }
        
        long   integerPart = long(value);
        string result      = integerPart.ToString();
        
        float  fractionalPart    = value - float(integerPart);
        string fractionalPartStr = fractionToString(fractionalPart);
        
        // If the fractional part is not empty, append it
        if ((fractionalPartStr.Length == 1) && (fractionalPartStr[0] == '0'))
        {
        }
        else
        {
            String.Build(ref result, '.');
            String.Build(ref result, fractionalPartStr);
        }
        
        if (isNegative)
        {
            String.BuildFront(ref result, '-');
        }
        return result;
    }
    
    string fractionToString(float input)
    {
        uint length;
        int digit;
        uint i;
        char ch;
        int precision = 6; // Number of digits after the decimal point
        string result;
        float fractionalPart = input;
        while (precision > 0)
        {
            fractionalPart = fractionalPart * 10.0;
            digit = int(long(fractionalPart));
            ch = char(byte('0') + digit);
            String.Build(ref result, ch);
            fractionalPart = fractionalPart - float(digit);
            precision--;
        }
        
        // Remove trailing zeros
        length = result.Length;
        while ((length > 1) && (result[length-1] == '0'))
        {
            length--;
        }
        if (length != result.Length)
        {
            string tmp = result;
            String.Build(ref result);
            for (i=0; i < length; i++)
            {
                String.Build(ref result, tmp[i]);
            }
        }
        return result;
    }
    
    bool GT(float a, float b)
    {
        return !LE(a, b);
    }
    
    bool LE(float a, float b)
    {
        return LT(a, b) || EQ(a, b);
    }
    
    bool GE(float a, float b)
    {
        return GT(a, b) || EQ(a, b);
    }
        
    int ToInt(float f)
    {
        long longValue = f.ToLong();
        if ((longValue < -32768) || (longValue > 32767))
        {
            Die(0x0D); // Overflow
        }
        return int(longValue);
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
    
    string ToHexString(float f)
    {
        string result;
        result += Byte.ToHexString(Float.GetByte(f, 3), 2);
        result += Byte.ToHexString(Float.GetByte(f, 2), 2);
        result += Byte.ToHexString(Float.GetByte(f, 1), 2);
        result += Byte.ToHexString(Float.GetByte(f, 0), 2);
        return result;
    }
    
    bool TryParse(string content, ref float returnValue)
    {
        bool success;
        uint iDot;
        long longValue;
        float floatValue;
        string digits;
        loop
        {
            if (content.Contains('E') || content.Contains('e'))
            {
                <string> parts;
                if (content.Contains('E'))
                {
                    parts = content.Split('E');
                }
                else if (content.Contains('e'))
                {
                    parts = content.Split('e');
                }
                if (parts.Count == 2)
                {
                    int exponent;
                    if (Float.TryParse(parts[0], ref floatValue) && Int.TryParse(parts[1], ref exponent))
                    {
                        // 4E+07
                        if (exponent == 0)
                        {
                            // 3.141E+00
                        }
                        else if (exponent < 0)
                        {
                            exponent = -exponent;
                            while (exponent != 0)
                            {
                                floatValue = floatValue / 10;
                                exponent--;
                            }    
                        }
                        else
                        {
                            while (exponent != 0)
                            {
                                floatValue = floatValue * 10;
                                exponent--;
                            }    
                        }
                        success = true;
                        break;
                    }
                }    
            }
            if (content.IndexOf('.', ref iDot))
            {
                digits = content.Substring(0, iDot);
                if (!Long.TryParse(digits, ref longValue))
                {
                    break;
                }
                bool negative = (longValue < 0);
                floatValue = longValue.ToFloat();
                digits = content.Substring(iDot+1);
                uint length = digits.Length;
                if (length > 0)
                {
                    if (!Long.TryParse(digits, ref longValue))
                    {
                        break;
                    }
                    float decimalValue = longValue.ToFloat();
                    while (length > 0)
                    {
                        decimalValue = decimalValue / 10.0;
                        length--;
                    }
                    if (negative)
                    {
                        floatValue = floatValue - decimalValue;
                    }
                    else
                    {
                        floatValue = floatValue + decimalValue;    
                    }
                }
                success = true;
            }
            else
            {
                if (Long.TryParse(content, ref longValue))
                {
                    floatValue = longValue.ToFloat();
                    success = true;
                }
            }
            break;
        }
        if (success)
        {
            returnValue = floatValue;
        }
        return success;
    }
    
    
    // Function to calculate the square root using the Newton-Raphson method
    float Sqrt(float number)
    {
        if (number == 0) { return 0; }
        if (number == 1) { return 1; }
        if (number < 0)
        {
            Die(0x0D); // numeric type out of range / overflow
        }

        float guess = number;
        float epsilon = 0.000001; // Precision of the result
        loop
        {
            float quotient = number / guess;
            float average = (guess + quotient) / 2;
            if (Float.Abs(guess - average) < epsilon) { break; }
            guess = average;
        }
        return guess;
    }
            
    float Radians(float angle) { return angle * Pi / 180.0; }
    float Degrees(float angle) { return angle * 180.0 / Pi; }
    
    float Abs(float value) { return (value >= 0) ? value : -value; }
    float Min(float a, float b) { return (a < b) ? a : b; }
    float Max(float a, float b) { return (a > b) ? a : b; }
    
    
    float Random()
    {
        uint ri = UInt.Random();
        return ri / 65535.0;
    }
}

