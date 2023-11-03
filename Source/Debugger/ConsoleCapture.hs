unit ConsoleCapture
{
    uses "/Source/System/File"
    
    uses "/Source/Debugger/Output"
    
    string capturePath;
    string captureBuffer;
    
    file logFile;
    SetPath(string currentIHexPath)
    {
        capturePath = Path.GetFileName(currentIHexPath);
        capturePath = capturePath.ToLower();
        capturePath = capturePath.Replace(".hex", ".log");
        capturePath = Path.Combine("/Debug", capturePath);
    }
    ClearLog()
    {
        if (DebugOptions.IsCaptureConsoleMode)
        {
            if (logFile.IsValid())
            {
                logFile.Flush();
            }
            File.Delete(capturePath);
        }
    }
    FlushLog()
    {
        if (logFile.IsValid())
        {
            if (captureBuffer.Length > 0)
            {
                logFile.Append(captureBuffer);
                String.Build(ref captureBuffer);
            }
            logFile.Flush();
        }
    }
    AppendLineToLog(string str)
    {
        foreach(var ch in str)
        {
            AppendToLog(ch);
        }
        AppendToLog(char(0x0D));
    }
    AppendToLog(char ch)
    {
        if (!logFile.IsValid())
        {
            logFile = File.Create(capturePath);
        }
        if (ch == char(0x08))
        {
            // ignore backspace
        }
        else
        {
            String.Build(ref captureBuffer, ch);
            if (captureBuffer.Length >= 1024)
            {
                logFile.Append(captureBuffer);
                String.Build(ref captureBuffer);
                logFile.Flush();
            }
        }
    }
}
