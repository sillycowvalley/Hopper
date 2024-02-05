unit Float
{
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
                    if (Float.TryParse(parts[0], ref returnValue) && Int.TryParse(parts[1], ref exponent))
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
                                returnValue = returnValue / 10;
                                exponent--;
                            }    
                        }
                        else
                        {
                            while (exponent != 0)
                            {
                                returnValue = returnValue * 10;
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
    string ToString(float this) system;
    
    <byte> ToBytes(float this) system;
    
    byte GetByte(float this, byte index) system;
    float FromBytes(byte b0, byte b1, byte b2, byte b3) system;
    
    float Abs(float value) { return (value >= 0) ? value : -value; }
    float Min(float a, float b) { return (a < b) ? a : b; }
    float Max(float a, float b) { return (a > b) ? a : b; }
}
