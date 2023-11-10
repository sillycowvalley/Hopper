unit SourceStream
{
    <string> sourceStream;
    <string> headerStream;
    
    New()
    {
        sourceStream.Clear();
        headerStream.Clear();
    }
    
    <string> CurrentStream { get { return sourceStream; } }
    
    NewLine()
    {
        uint depth = BlockDepth();
        if (!InMain && (depth > 0))
        {
            depth = depth - 1;
        }
        string indent;
        indent = indent.Pad(' ', depth * 4);
        sourceStream.Append(indent);
    }
    Append(string str, int trim)
    {
        string line = sourceStream[sourceStream.Length-1];
        if ((trim == -1) && line.StartsWith("    "))
        {
            line = line.Substring(4);
        }
        line = line + str;
        sourceStream[sourceStream.Length-1] = line;    
    }
    Append(string str)
    {
        string line = sourceStream[sourceStream.Length-1];
        line = line + str;
        sourceStream[sourceStream.Length-1] = line;
    }
    AppendHeader(string str)
    {
        headerStream.Append(str);
    }
    
    bool Export(string path)
    {
        bool success = false;
        File.Delete(path);
        loop
        {
            file codeFile = File.Create(path);
            if (!codeFile.IsValid())
            {
                break;
            }
            codeFile.Append("// method definitions" + char(0x0A));
            foreach (var line in headerStream)
            {
                codeFile.Append(line + char(0x0A));
                if (!codeFile.IsValid())
                {
                    break;
                }
            }
            codeFile.Append("" + char(0x0A));
            foreach (var line in sourceStream)
            {
                codeFile.Append(line + char(0x0A));
                if (!codeFile.IsValid())
                {
                    break;
                }
            }
            codeFile.Flush();
            if (!codeFile.IsValid())
            {
                break;
            }
            success = true;
            break;
        }
        return success;
    }
}
