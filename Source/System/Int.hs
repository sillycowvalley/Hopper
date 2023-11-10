unit Int
{

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
#ifndef TINYHOPPER
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
            c = Char.ToDigit(byte(digit));
            String.BuildFront(ref result, c);
            uthis = uthis / 10;
        }
        if (negative)
        {
            String.BuildFront(ref result, '-');
        }
        return result;
    }
    float ToFloat(int this) system;
    long ToLong(int this) system;
    
    string ToHexString(int this, byte digits)
    {
        int digit;
        char c;
        int i;
        string result;
        for (i = digits; i > 0; i--)
        {
            digit = this % 16;
            c = Char.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 16;
        }
        return result;
    }
    
#ifdef PORTABLE    
    <byte> ToBytes(int this)
    {
        <byte> lst;
        lst.Append(GetByte(this, 0));
        lst.Append(GetByte(this, 1));
        return lst;
    }
#else
    <byte> ToBytes(int this) system;
#endif

    byte GetByte(int this, byte index) system;
    int FromBytes(byte b0, byte b1) system;
    
}
