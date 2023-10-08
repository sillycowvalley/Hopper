unit Parse
{
    uses "/Source/Basic/Commands"
    
    const uint LineLimit = 9999;

    bool TryParseLineNumber(string content, ref uint lineNumber)
    {
        bool result = false;
        loop
        {
            if (UInt.TryParse(content, ref lineNumber))
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
