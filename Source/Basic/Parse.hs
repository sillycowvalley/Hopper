unit Parse
{
    uses "/Source/Basic/Commands"
    
    const uint LineLimit = 9999;
    
    bool TryParseLong(string content, ref long returnValue)
    {
        returnValue = 0;
        bool makeNegative = false;
        if (content.Length < 1)
        {
            return false;
        }
        if (content.StartsWith('+'))
        {
            content = content.Substring(1);
        }
        else if (content.StartsWith('-'))
        {
            content = content.Substring(1);
            makeNegative = true;
        }
        foreach (var c in content)
        {
            returnValue = returnValue * 10;
            if (!c.IsDigit())
            {
                return false;
            }
            returnValue = returnValue + (byte(c) - 48); // 48 is ASCII for '0'
        }
        if (makeNegative)
        {
            returnValue = -returnValue;
        }
        return true;
    }
    
    bool tryParseShort(string content, ref int returnValue)
    {
        returnValue = 0;
        bool makeNegative = false;
        if (content.Length < 1)
        {
            return false;
        }
        if (content.StartsWith('+'))
        {
            content = content.Substring(1);
        }
        else if (content.StartsWith('-'))
        {
            content = content.Substring(1);
            makeNegative = true;
        }
        foreach (var c in content)
        {
            returnValue = returnValue * 10;
            if (!c.IsDigit())
            {
                return false;
            }
            returnValue = returnValue + (byte(c) - 48); // 48 is ASCII for '0'
        }
        if (makeNegative)
        {
            returnValue = -returnValue;
        }
        return true;
    }
    bool tryParseUShort(string content, ref uint returnValue)
    {
        returnValue = 0;
        if (content.Length < 1)
        {
            return false;
        }
        if (content.StartsWith('+'))
        {
            content = content.Substring(1);
        }
        else if (content.StartsWith('-'))
        {
            return false;
        }
        foreach (var c in content)
        {
            returnValue = returnValue * 10;
            if (!c.IsDigit())
            {
                return false;
            }
            returnValue = returnValue + (byte(c) - 48); // 48 is ASCII for '0'
        }
        return true;
    }
    
    bool TryParseInt(string content, ref int returnValue)
    {
        bool success = false;
        if (content.Length < 5) // -999..9999
        {
            if (tryParseShort(content, ref returnValue))
            {
                success = true;
            }
        }
        else 
        {
            long rv;
            if (TryParseLong(content, ref rv))
            {
                if ((rv >= -32768) && (rv <= 32767))
                {
                    returnValue = int(rv);
                    success = true;
                }
            }
        }
        return success;
    }
    bool TryParseUInt(string content, ref uint returnValue)
    {
        bool success = false;
        if (content.Length < 5) // 0..9999
        {
            if (tryParseUShort(content, ref returnValue))
            {
                success = true;
            }
        }
        else
        {
            long rv;
            if (TryParseLong(content, ref rv))
            {
                if ((rv >= 0) && (rv <= 65535))
                {
                    returnValue = uint(rv);
                    success = true;
                }
            }
        }
        return success;
    }
    bool TryParseLineNumber(string content, ref uint lineNumber)
    {
        bool result = false;
        loop
        {
            if (TryParseUInt(content, ref lineNumber))
            {
                if ((lineNumber < 1) || (lineNumber > LineLimit))
                {
                    Error(3, content); // Illegal line number
                    break;
                }
                result = true;
            }
            break;
        }
        return result;
    }
                
}
