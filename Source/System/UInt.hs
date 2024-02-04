unit UInt
{

    bool TryParse(string content, ref uint returnValue)
    {
        bool success;
        uint length;
        uint i;
        byte b;
        if (content.StartsWith("0x"))
        {
            return tryParseHex(content, ref returnValue);
        }
        else if (content.StartsWith("0b"))
        {
            return tryParseBinary(content, ref returnValue);
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
                }
                else 
                {
                    long rv;
                    if (Long.TryParse(content, ref rv))
                    {
                        if ((rv >= 0) && (rv <= 65535))
                        {
                            returnValue = uint(rv);
                            success = true;
                        }
                    }
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
            for (uint i=0; i < length-2; i++)
            {
                returnValue = returnValue * 16;
                char c = content.GetChar(i+2);
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
        returnValue = 0;
        if (!content.StartsWith("0b"))
        {
            return false;
        }
        length = content.Length;
        if (length < 3)
        {
            return false;
        }
        for ( ; i < length-2; i++)
        {
            returnValue = returnValue * 2;
            c = content.GetChar(i+2);
            if (c == '1')
            {
                returnValue = returnValue + 1;
            }
        }
        return true;
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
    ToString(uint this, ref string result)
    {
        String.Build(ref result);
        if (this == 0)
        {
            String.Build(ref result, '0');
        }
        while (this != 0)
        {
            String.BuildFront(ref result, char((this % 10) + 48));
            this = this / 10;
        }
    }
    long  ToLong(uint this) system;
    float ToFloat(uint this) system;
    
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
    <byte> ToBytes(uint this)
    {
        <byte> lst;
        lst.Append(byte(this & 0xFF));
        lst.Append(byte(this >> 8));
        return lst;
    }
    byte GetByte(uint this, byte index)
    {
        switch (index)
        {
            case 0:
            {
                return byte(this & 0xFF);
            }
            case 1:
            {
                return byte(this >> 8);
            }
            default:
            {
                Diagnostics.SetError(0x02); // array index out of range
            }
        }
        return 0;
    }
    uint FromBytes(byte b0, byte b1)
    {
        return b0 + b1 << 8;
    }
    
    uint Min(uint a, uint b) { return (a < b) ? a : b; }
    uint Max(uint a, uint b) { return (a > b) ? a : b; }
    Swap(ref uint a, ref uint b) { uint t = a; a = b; b = t; }
}
