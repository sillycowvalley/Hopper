unit File
{
    
    uses "/Source/Library/File/SerialEEPROM"
    uses "/Source/Library/File/BlockFileSystem"
    
    record File
    {
        // private members
        bool isValid;
        bool isReading;
        bool isWriting;
        string path;
        uint pos;        // 64K file size limit
        string buffer;   // string object as buffer of bytes
        uint size;       // 64K file size limit
    }
    
    // Check if a file exists at the given path.
    bool Exists(string path)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return false;
    }
    
    // Delete the file at the given path.
    Delete(string path)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
    }
    
    // Get the size of the file at the given path.
    long GetSize(string path)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return 0;
    }
    
    // Check if the file object is valid.
    bool IsValid(File this)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return false;
    }
    
    // Open a file at the given path.
    File Open(string fullpath)
    {
        // Stub implementation
        File fileInstance
        Diagnostics.Die(0x0A); // not implemented
        return fileInstance;
    }
    
    // Read a line from the file.
    string ReadLine(File this)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return "";
    }
    
    // Read a byte from the file.
    byte Read(File this)
    {
        // Stub implementation
        return 0;
    }
    
    // Read a byte from the file at the specified seek position.
    byte Read(File this, uint seekPosition)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return 0;
    }
    
    // Read data from the file into a byte array.
    uint Read(File this, byte[] data, uint bufferSize)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return 0;
    }
    
    // Create a new file at the given path.
    File Create(string fullpath)
    {
        // Stub implementation
        File fileInstance;
        Diagnostics.Die(0x0A); // not implemented
        return fileInstance;
    }
    
    // Append a byte to the file.
    Append(File this, byte content)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
    }
    
    // Append a string to the file.
    Append(File this, string content)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
    }
    
    // Flush the file buffer to the storage media.
    Flush(File this)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
    }
    
    // Check if a file exists with the given extension in the specified folder.
    bool Exists(ref string filePath, ref string extension, string searchFolder)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
        return false;
    }
    
    // Try to read all text from a file into a string.
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
