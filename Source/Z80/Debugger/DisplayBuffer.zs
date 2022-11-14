unit DisplayBuffer
{
    
    // DisplayBuffer 
    //    textBuffer: < string >
    
    <string> textBuffer;
    
    Initialize()
    {
        <string> tb;
        textBuffer = tb; // initialize with correct type
        textBuffer.Append(""); // start with a single empty line
    }
    
    AddFile(file textFile)
    {
        loop
        {
            string ln = textFile.ReadLine();
            if (ln.Length == 0)
            {
                if (!textFile.IsValid())
                {
                    break;
                }
            }
            textBuffer.Append(ln);
        }
        uint count = GetLineCount();
        if (count == 0)
        {
            // empty file
            textBuffer.Append(""); // start with a single empty line
        }
    }
    
    string GetLine(uint lineIndex)
    {
        string ln = textBuffer.GetItem(lineIndex);
        return ln;
    }
    uint GetLineCount()
    {
        uint count = textBuffer.Length;
        return count;
    }
    uint GetLineLength(uint lineIndex)
    {
        string ln = GetLine(lineIndex);
        return ln.Length;
    }
}
