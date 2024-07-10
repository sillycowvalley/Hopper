unit UInt
{
    
    byte GetByte(uint this, byte index) system;
    uint FromBytes(byte b0, byte b1) system;
    /*
    byte GetByte(uint this, byte index) system;
    {
        byte result;
        if (index == 1)
        {
            result = byte(this >> 8);
        }
        else
        {
            result = byte(this & 0xFF);
        }
        return result;
    }
    uint FromBytes(byte b0, byte b1)
    {
        return b0 + b1 << 8;
    }
    */
    bool TryParse(string input, ref uint returnValue)
    {
        bool success;
        uint length;
        byte b;
        uint i;
        string content = input;
        if (content.StartsWith("0x"))
        {
            success = tryParseHex(content, ref returnValue);
        }
        else if (content.StartsWith("0b"))
        {
            success = tryParseBinary(content, ref returnValue);
        }
        else 
        {
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
                if (length < 1)
                {
                    break;
                }
                // 0..+59999 : limited range supported without 'long'
                if ((length < 5) ||
                    ((length == 5) && ((content[0] == '0') || (content[0] == '1') || (content[0] == '2') || (content[0] == '3') || (content[0] == '4') || (content[0] == '5')))
                   )
                { 
            
                    returnValue = 0;
                    success = true;
                    for ( ; i < length; i++)
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
                }
                else 
                {
                    success = false; // TODO
                }
                break;
            }
        }
        return success;
    }

    bool tryParseHex(string content, ref uint returnValue)
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
            for ( ; i < length-2; i++)
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
        } // loop
        return success;
    }
    bool tryParseBinary(string content, ref uint returnValue)
    {
        char c;
        uint length;
        bool success = true;
        uint i;
        returnValue = 0;
        if (!content.StartsWith("0b"))
        {
            success = false;
        }
        else
        {
            length = content.Length;
            if (length < 3)
            {
                success = false;
            }
            else
            {
                for ( ; i < length-2; i++)
                {
                    returnValue = returnValue * 2;
                    c = content.GetChar(i+2);
                    if (c == '1')
                    {
                        returnValue = returnValue + 1;
                    }
                    else if (c == '0')
                    {
                    }
                    else
                    {
                        success = false;
                        break;
                    }
                }
            }
        }
        return success;
    }

    string ToString(uint this)
    {
        string result;
        uint value = this;
        if (value == 0)
        {
            String.Build(ref result, '0');
        }
        while (value != 0)
        {
            String.BuildFront(ref result, char((value % 10) + 48));
            value = value / 10;
        }
        return result;
    }

    string ToHexString(uint this, byte digits)
    {
        string result;
        uint value = this;
        for (; digits > 0; digits--)
        {
            String.BuildFront(ref result, Byte.ToHex(byte(value % 16)));
            value = value / 16;
        }
        return result;
    }
    string ToBinaryString(uint this, byte digits)
    {
        string result = UInt.ToBinaryString(this);
        if (result.Length > digits)
        {
            result = result.Substring(result.Length-digits);
        }
        return result;
    }
    string ToBinaryString(uint this)
    {
        uint digit;
        char c;
        string result;
        uint value = this;
        for (uint i = 16; i > 0; i--)
        {
            digit = value % 2;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            value = value / 2;
        }
        return result;
    }
    uint Min(uint a, uint b)
    { 
       if (a < b)
       {
           return a;
       }
       return b;
    }
    uint Max(uint a, uint b) 
    { 
        if (a > b)
        {
            return a;
        }
        return b; 
    }
    Swap(ref uint a, ref uint b) { uint t = a; a = b; b = t; }
    
    uint Sqrt(uint number)
    {
        if (number == 0) { return 0; }
        if (number == 1) { return 1; }

        uint guess = number / 2 + 1; // Initial guess
        uint result = (guess + number / guess) / 2;

        while (result < guess)
        {
            guess = result;
            result = (guess + number / guess) / 2;
        }
        return guess;
    }
    
    float ToFloat(uint i)
    {
        return Long.ToFloat(UInt.ToLong(i));
    }

    
#if defined(FAST_6502_RUNTIME)
    long ToLong(uint i) system;
#else
    long ToLong(uint i)
    {
        return Long.FromBytes(UInt.GetByte(i, 0), UInt.GetByte(i, 1), 0, 0);
    }
#endif
    
    uint gRnd;
    uint Random()
    {
        gRnd = gRnd ^ (gRnd << 7);
        gRnd = gRnd ^ (gRnd >> 9);
        gRnd = gRnd ^ (gRnd << 8);
        return gRnd & 0x7FFF;
    }
    
    Seed(uint seed) 
    {
        if (seed == 0)
        {
            seed = 1;
        }
        gRnd = seed;
    }
    Seed()
    {
        long seed = Millis;
        Seed(UInt.FromBytes(seed.GetByte(0), seed.GetByte(1)));
    }
}
