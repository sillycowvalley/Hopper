program LittleFS
{
    #define SERIALCONSOLE // use Serial, not Screen for Write, WriteLn, etc.
    
    #define RP2040
    uses "/Source/Library/MCU"
    
    ListDirectory(string directoryPath, string indent)
    {
        WriteLn(indent + directoryPath);
        directory dir = Directory.Open(directoryPath);
        uint subfolderCount = dir.GetDirectoryCount();
        uint fileCount      = dir.GetFileCount();
        indent += "  ";
        for (uint i=0; i < fileCount; i++)
        {
            string filePath = dir.GetFile(i);
            string fileName = Path.GetFileName(filePath);
            Write(indent + fileName.Pad(' ', 16));
            long fileSize = File.GetSize(filePath);
            Write(fileSize.ToString() + " bytes");
            WriteLn();
        }
        for (uint i=0; i < subfolderCount; i++)
        {
            string subfolder = dir.GetDirectory(i);
            ListDirectory(subfolder, indent);
        }
    }
    DeleteDirectory(string directoryPath, string indent)
    {
        directory dir = Directory.Open(directoryPath);
        uint subfolderCount = dir.GetDirectoryCount();
        uint fileCount      = dir.GetFileCount();
        indent += "  ";
        <string> filePaths;
        for (uint i=0; i < fileCount; i++)
        {
            string filePath = dir.GetFile(i);
            filePaths.Append(filePath);
            // deleting here would make the index 'i' incorrect for the next file
        }
        foreach (var filePath in filePaths)
        {
            string fileName = Path.GetFileName(filePath);
            Write(indent + fileName.Pad(' ', 16));
            File.Delete(filePath);
            WriteLn(" deleted.");
        }
        <string> subfolders;
        for (uint i=0; i < subfolderCount; i++)
        {
            string subfolder = dir.GetDirectory(i);
            DeleteDirectory(subfolder, indent);
            subfolders.Append(subfolder);
        }
        foreach (var subfolder in subfolders)
        {
            Write(subfolder + subfolder.Pad(' ', 16));
            Directory.Delete(subfolder);
            WriteLn(" deleted.");
        }
    }
    
    ShowFile(string path)
    {
        file f = File.Open(path);
        loop
        {
            char c = char(f.Read());    
            if (!f.IsValid())
            {
                break;
            }
            Write(c);
        }
    }
    {
        ListDirectory("/", "");
        //DeleteDirectory("/", ""); // delete everything
        //ShowFile("/Data/Mandelbrot.txt");
    }
}
