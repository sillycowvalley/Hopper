unit Int
{
    byte GetByte(int this, byte index) system;
    int FromBytes(byte b0, byte b1) system;
    
    bool TryParse(string content, ref int returnValue)
    {
        bool success;
        uint length;
        byte b;
        bool makeNegative;
        loop
        {
            length = content.Length;
            if (length < 1)
            {
                break;
            }
            if (content[0] == '+')
            {
                String.Substring(ref content, 1);
                length--;
            }
            else if (content[0] == '-')
            {
                String.Substring(ref content, 1);
                length--;
                makeNegative = true;
            }
            if (length < 1)
            {
                break;
            }
            if (content.Length < 5) // -9999..+9999 : limited range supported without 'long'
            {
                returnValue = 0;
                success = true;
                for (uint i=0; i < length; i++)
                {
                    b = byte(content[i]);
                    returnValue = returnValue * 10;
                    if (b < 48)
                    {
                        success = false;
                        break;
                    }
                    b = b - 48; // 48 is ASCII for '0'
                    if (b > 9)
                    {
                        success = false;
                        break;
                    }
                    returnValue = returnValue + b; 
                }
                if (makeNegative)
                {
                    returnValue = 0 - returnValue;
                }
            }
            else
            {
#ifndef MINIMAL_RUNTIME                
                long rv;
                if (Long.TryParse(content, ref rv))
                {
                    if ((rv >= -32768) && (rv <= 32767))
                    {
                        returnValue = int(rv);
                        success = true;
                    }
                }
#endif
            }
            break;
        }
        return success;
    }
    string ToString(int this)
    {
        uint digit;
        uint uthis;
        char c;
        bool negative;
        string result;
        if (this < 0)
        {
            negative = true;
            this = 0 - this;
        }
        else if (this == 0)
        {
            String.Build(ref result, '0');
        }
        uthis = uint(this);
        while (uthis != 0)
        {
            digit = uthis % 10;
            c = Byte.ToDigit(byte(digit));
            String.BuildFront(ref result, c);
            uthis = uthis / 10;
        }
        if (negative)
        {
            String.BuildFront(ref result, '-');
        }
        return result;
    }
    string ToHexString(int this, byte digits)
    {
        int digit;
        char c;
        string result;
        for (int i = digits; i > 0; i--)
        {
            digit = this % 16;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 16;
        }
        return result;
    }
    
    int Abs(int value) { return (value >= 0) ? value : -value; }
    int Min(int a, int b) { return (a < b) ? a : b; }
    int Max(int a, int b) { return (a > b) ? a : b; }
    Swap(ref int a, ref int b) { int t = a; a = b; b = t; }
    
    float ToFloat(int i)
    {
        if (i == 0)
        {
            return FromBytes(0, 0, 0, 0);
        }

        byte sign = (i < 0) ? 1 : 0;
        if (sign == 1)
        {
            i = -i;
        }

        byte exponent = 127 + 23;
        long mantissa = long(i) << 8;

        Float.normalize(ref mantissa, ref exponent);

        return Float.combineComponents(sign, exponent, mantissa);
    }
}
