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
            if (content.IndexOf('.', ref iDot))
            {
                digits = content.Substring(0, iDot);
                if (!Long.TryParse(digits, ref longValue))
                {
                    break;
                }
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
                    if (floatValue > 0)
                    {
                        floatValue = floatValue + decimalValue;
                    }
                    else
                    {
                        floatValue = floatValue - decimalValue;
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
}
