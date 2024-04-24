unit UInt
{
    byte GetByte(uint this, byte index)
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
    bool TryParse(string content, ref uint returnValue)
    {
        bool success;
        uint length;
        uint i;
        byte b;
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
                if (length < 5) // 0..+9999
                {
                    returnValue = 0;
                    success = true;
                    for (i=0; i < length; i++)
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
        } // loop
        return success;
    }
    bool tryParseBinary(string content, ref uint returnValue)
    {
        char c;
        uint length;
        uint i;
        bool success = true;
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
        if (this == 0)
        {
            String.Build(ref result, '0');
        }
        while (this != 0)
        {
            String.BuildFront(ref result, char((this % 10) + 48));
            this = this / 10;
        }
        return result;
    }
    string ToHexString(uint this, byte digits)
    {
        string result;
        for (; digits > 0; digits--)
        {
            String.BuildFront(ref result, Byte.ToHex(byte(this % 16)));
            this = this / 16;
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
        uint i;
        string result;
        for (i = 16; i > 0; i--)
        {
            digit = this % 2;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 2;
        }
        return result;
    }
    uint Min(uint a, uint b) { return (a < b) ? a : b; }
    uint Max(uint a, uint b) { return (a > b) ? a : b; }
    Swap(ref uint a, ref uint b) { uint t = a; a = b; b = t; }
}
