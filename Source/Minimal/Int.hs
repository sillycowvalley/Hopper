unit Int
{
    byte GetByte(int this, byte index) system;
    int  FromBytes(byte b0, byte b1) system;
    
    bool TryParse(string input, ref int returnValue)
    {
        bool success;
        uint length;
        byte b;
        bool makeNegative;
        string content = input;
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
            // -29999..+29999 : limited range supported without 'long'
            if ((length < 5) ||
                ((length == 5) && ((content[0] == '0') || (content[0] == '1') || (content[0] == '2')))
               ) 
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
                long rv;
                if (Long.TryParse(content, ref rv))
                {
                    if ((rv >= -32768) && (rv <= 32767))
                    {
                        returnValue = int(rv);
                        success = true;
                    }
                }
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
    
    int Sqrt(int number)
    {
        if (number == 0) { return 0; }
        if (number == 1) { return 1; }
        if (number < 0)
        {
            Die(0x0D); // numeric type out of range / overflow
        }
        int guess = number / 2 + 1; // Initial guess
        int result = (guess + number / guess) / 2;
        while (result < guess)
        {
            guess = result;
            result = (guess + number / guess) / 2;
        }
        return guess;
    }
    
#if defined(FAST_6502_RUNTIME)
    long ToLong(int value) system;
#else
    long ToLong(int value)
    {
        // Extract bytes from the int
        byte lowByte = GetByte(value, 0);
        byte highByte = GetByte(value, 1);

        // Sign-extend the int to 32-bit long
        byte highHighByte = (value < 0) ? 0xFF : 0x00;

        // Create new long from int bytes
        return Long.FromBytes(lowByte, highByte, highHighByte, highHighByte);
    }
#endif
    
    float ToFloat(int i)
    {
        return Long.ToFloat(Int.ToLong(i));
    }
}
