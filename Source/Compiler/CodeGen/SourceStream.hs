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
        string line = sourceStream[sourceStream.Count-1];
        if ((trim == -1) && line.StartsWith("    "))
        {
            line = line.Substring(4);
        }
        line = line + str;
        sourceStream[sourceStream.Count-1] = line;    
    }
    Append(string str)
    {
        string line = sourceStream[sourceStream.Count-1];
        line = line + str;
        sourceStream[sourceStream.Count-1] = line;
    }
    AppendHeader(string str)
    {
        headerStream.Append(str);
    }
    
    bool Export(string headerpath, string sourcepath)
    {
        bool success = false;
        File.Delete(headerpath);
        File.Delete(sourcepath);
        loop
        {
            file headerFile = File.Create(headerpath);
            if (!headerFile.IsValid())
            {
                break;
            }
            
            file codeFile = File.Create(sourcepath);
            if (!codeFile.IsValid())
            {
                break;
            }
            
            
            headerFile.Append("// method definitions" + Char.EOL);
            foreach (var line in headerStream)
            {
                headerFile.Append(line + Char.EOL);
                if (!headerFile.IsValid())
                {
                    break;
                }
            }
            headerFile.Flush();
            if (!headerFile.IsValid())
            {
                break;
            }
            
            codeFile.Append("" + Char.EOL);
            foreach (var line in sourceStream)
            {
                codeFile.Append(line + Char.EOL);
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
