unit Source
{
    // Knobs and dials
    const uint lineLimit    =  9999; // makes parsing in TryParseLineNumber(..) trivial
    
    // #### globals at the top of the file so we can keep track of them:
    uint            lastLine;         // highest current line number
    <uint, string>  sourceLines;
    bool[10003]     sourceLineExists; // mainly to make a fast search for the next non-empty line (1 bit per flag = 1250 bytes)
    // #### end of globals
    
    uint LastLine   { get { return lastLine; } }
    uint LineLimit  { get { return lineLimit;   } }
    
    Clear() // clear the current program (called by 'NEW')
    {
        sourceLines.Clear();
        for (uint i=1; i <= lastLine; i++)
        {
            sourceLineExists[i] = false;
        }
        sourceLineExists[10000] = false;
        sourceLineExists[10001] = false;
        sourceLineExists[10002] = false;
        lastLine = 0;
        HopperCode.Clear();
    }
    uint GetSize()
    {
        uint size = 0;
        foreach(var kv in sourceLines)
        {
            string ln = kv.value;
            size = size + ln.Length;
            size = size + 8 + 8; // at least 8 bytes for the string header and another 8 for the list item
        }
        return size;
    }
    
    bool LineExists(uint i)
    {
        return sourceLineExists[i];
    }
    string GetLine(uint i)
    {
        return sourceLines[i];
    }
    uint GetNextLine(uint lineNumber)
    {
        loop
        {
            lineNumber++;
            if (lineNumber > lastLine)
            {
                break;
            }
            if (sourceLineExists[lineNumber])
            {
                break;
            }
        }
        return lineNumber;
    }
    
    Add(uint lineNumber, string sourceLine)
    {
        if (lineNumber <= lineLimit)
        {
            HopperCode.Clear(); // discard current tokenized program when source changes
        }
        sourceLines[lineNumber] = sourceLine;
        if (sourceLine.Length > 0)
        {
            sourceLineExists[lineNumber] = true;
            if ((lineNumber > lastLine)  && (lineNumber <= lineLimit))
            {
                lastLine = lineNumber;
            }
        }
        else
        {
            sourceLineExists[lineNumber] = false;
            if (lastLine == lineNumber)
            {
                while (lastLine > 0)
                {
                    lastLine--;
                    if (sourceLineExists[lastLine])
                    {
                        break;
                    }
                }
            }
        }
    }
    
}
