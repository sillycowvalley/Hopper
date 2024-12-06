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
            
            headerFile.Append("#ifndef HOPPERRUNTIME_H" + Char.EOL);
            headerFile.Append("#define HOPPERRUNTIME_H" + Char.EOL);
            headerFile.Append("" + Char.EOL);
            headerFile.Append("#include <Arduino.h>" + Char.EOL);
            headerFile.Append("#include \"HopperConfiguration.h\"" + Char.EOL);
            headerFile.Append("" + Char.EOL);
            headerFile.Append("typedef unsigned char  Byte;" + Char.EOL);
            headerFile.Append("typedef unsigned char  Char;" + Char.EOL);
            headerFile.Append("typedef unsigned short UInt;" + Char.EOL);
            headerFile.Append("typedef   signed short Int;" + Char.EOL);
            headerFile.Append("typedef   signed char  Int8;" + Char.EOL);
            headerFile.Append("typedef           bool Bool;" + Char.EOL);
            headerFile.Append("" + Char.EOL);
            headerFile.Append("typedef Bool (*InstructionDelegate)();" + Char.EOL);
            headerFile.Append("typedef UInt PinISRDelegate;" + Char.EOL);
            headerFile.Append("typedef UInt TimerISRDelegate;" + Char.EOL);
            headerFile.Append("typedef UInt HandlerDelegate;" + Char.EOL);
            headerFile.Append("" + Char.EOL);
            
            foreach (var line in headerStream)
            {
                headerFile.Append(line + Char.EOL);
                if (!headerFile.IsValid())
                {
                    break;
                }
            }
            
            headerFile.Append("" + Char.EOL);
            headerFile.Append("#endif // HOPPERRUNTIME_H" + Char.EOL);
            
            headerFile.Flush();
            if (!headerFile.IsValid())
            {
                break;
            }
            
            codeFile.Append("#include \"Platform.h\"" + Char.EOL);
            codeFile.Append("#include \"HopperFile.h\"" + Char.EOL);
            codeFile.Append("#include \"HopperWiFi.h\"" + Char.EOL);
            codeFile.Append("#include \"HopperTimer.h\"" + Char.EOL);
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
