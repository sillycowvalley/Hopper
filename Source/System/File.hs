unit File
{
    
    bool Exists(string path) system;
    Delete(string path) system;
    long GetSize(string path) system;
    long GetTime(string path) system;
        
    bool IsValid(file this) system;
    
    file Open(string fullpath) system;
    string ReadLine(file this) system;
    byte Read(file this) system;
    byte Read(file this, long seekPosition) system;
    
    file Create(string fullpath) system;
    Append(file this, byte content) system;
    Append(file this, string content) system;
    Flush(file this) system;
    
    bool Exists(ref string filePath, ref string extension, string searchFolder)
    {
        string originalfilePath = filePath;
        if (!File.Exists(filePath))
        {    
            string ext = Path.GetExtension(originalfilePath);
            if (ext == ".")
            {
                // first try appending the extension
                filePath = originalfilePath + extension;
                filePath = filePath.Replace("..", "."); // in case it was ending in a trailing .
                if (!File.Exists(filePath))
                {
                    // try the searchFolder with the extension
                    filePath = Path.Combine(searchFolder, filePath);
                    if (!File.Exists(filePath))
                    {
                        filePath = originalfilePath;
                        return false;
                    }
                }
            }
            else
            {
                // try the searchFolder
                filePath = Path.Combine(searchFolder, originalfilePath);
                if (!File.Exists(filePath))
                {
                    filePath = originalfilePath;
                    return false;
                }
            }
        }
        extension = Path.GetExtension(filePath);
        return true;
    }
    bool TryReadAllText(string path, ref string content)
    {
        content = "";
        file tf = File.Open(path);
        if (!tf.IsValid())
        {
            return false;
        }
        loop
        {
            char ch = char(tf.Read());
            if (!tf.IsValid()) { break; }
            content = content + ch;
        }
        return true;
    }
    uint CRC16(string filepath)
    {
        uint crc = 0xFFFF;
        file f = File.Open(filepath);
        loop
        {
            byte data = f.Read();
            if (!f.IsValid()) { break; }
            
            crc = crc ^ data;
            for (byte k = 0; k < 8; k++)
            {
                crc = (crc & 1 != 0) ? ((crc >> 1) ^ 0xA001) : (crc >> 1);
            }
        }
        return crc;
    }
    bool Copy(string sourcePath, string destinationPath)
    {
        return Copy(sourcePath, destinationPath, false);
    }
    bool Copy(string sourcePath, string destinationPath, bool overwrite)
    {
        if (File.Exists(destinationPath))
        {
            if (!overwrite)
            {
                return false;
            }
            File.Delete(destinationPath);   
        }
        
        bool success;
        loop
        {
            file sf = File.Open(sourcePath);
            if (!sf.IsValid()) { break; }
            file df = File.Create(destinationPath);
            if (!df.IsValid()) { break; }
            loop
            {
                byte data = sf.Read();
                if (!sf.IsValid()) { break; }
                df.Append(data);
                if (!df.IsValid()) { break; }
            }
            df.Flush();
            success = df.IsValid();
            break;
        }
        return success;
    }
}
