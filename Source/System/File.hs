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
        if (!File.Exists(filePath))
        {
            string rawPath = filePath;
            string ext = Path.GetExtension(rawPath);
            if (ext == ".")
            {
                // first try appending the extension
                filePath = rawPath + extension;
                if (!File.Exists(filePath))
                {
                    // try the searchFolder with the extension
                    filePath = Path.Combine(searchFolder, rawPath + extension);
                    if (!File.Exists(filePath))
                    {
                        return false;
                    }
                }
            }
            else
            {
                // try the searchFolder
                filePath = Path.Combine(searchFolder, rawPath);
                if (!File.Exists(filePath))
                {
                    return false;
                }
            }
        }
        extension = Path.GetExtension(filePath);
        return true;
    }
    bool TryReadAllText(string path, ref string content)
    {
        file tf = File.Open(path);
        if (!tf.IsValid())
        {
            return false;
        }
        content = "";
        loop
        {
            char ch = char(tf.Read());
            if (!tf.IsValid()) { break; }
            content = content + ch;
        }
        return true;
    }
}
