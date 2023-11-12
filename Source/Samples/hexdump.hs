program HexDump
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    
    {
        
        <string> arguments = System.Arguments;
        
        string filePath;
        bool showHelp;
        
        foreach (var argument in arguments)
        {
            if (filePath.Length > 0)
            {
                showHelp = true;
                break;
            }
            else
            {
                filePath = argument;
            }
        }
        
        if (filePath.Length == 0)
        {
            showHelp = true;
        }
        
        loop
        {
            if (!showHelp)
            {
                // check the file
                if (!File.Exists(filePath))
                {
                    string fullPath = Path.Combine(System.CurrentDirectory, filePath);
                    if (!File.Exists(fullPath))
                    {
                        PrintLn(fullPath);
                        PrintLn("File '" + filePath + "' not found.");
                        showHelp = true;
                    }
                    else
                    {
                        filePath = fullPath;
                    }
                }
            }
            
            if (showHelp)
            {
                PrintLn("HEXDUMP <filepath>");
                break;
            }
            file binFile = File.Open(filePath);
            if (!binFile.IsValid())
            {
                PrintLn("Error attempting to open file '" + filePath +"'", Red, Black);
                break;
            }
            
            uint address = 0;
            loop
            {
                byte b = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                if (address % 16 == 0)
                {
                    PrintLn();
                    Print("0x" + address.ToHexString(4));
                }
                Print(' ');
                if (address % 8 == 0)
                {
                    Print(' ');
                }
                Print(b.ToHexString(2));
                address++;
                if (address > 0x200)
                {
                    break;
                }
            }
            PrintLn();
            
            
            break;
        }
        
    }
}
