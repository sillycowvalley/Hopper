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
        string fullPath = FileSystem.getFullPath(path);
        byte[4] fileHandle = FileSystem.fOpen(fullPath, "r");
        if (fileHandle[0] == 0)
        {
            return false; // File not found
        }
        return true;
    }
    
    // Delete the file at the given path.
    Delete(string path)
    {
        _ = FileSystem.remove(path); // error check?
    }
    
    // Get the size of the file at the given path.
    long GetSize(string path)
    {
        // Stub implementation
        File current;
        current = File.Open(path);
        if (current.isValue)
        {
            return current.size;
        }
        return 0;
    }
    
    // Check if the file object is valid.
    bool IsValid(File this)
    {
        return this.isValid;
    }
    
    // Read a line from the file.
    string ReadLine(File this)
    {
        string line;
        if (!this.isReading || this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return line;
        }
        
        if (!this.isReading || this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return line;
        }
        if (this.pos >= this.size)
        {
            this.isValid = false;
            return line;
        }
        uint count;
        string buffer = this.buffer;
        while (this.pos < this.size)
        {
            char ch = buffer[this.pos];
            this.pos = this.pos + 1;
            if (ch == Char.EOL)
            {
                break;
            }
            line += ch;
            count++;
        }
        return line;
    }
    
    // Read a byte from the file.
    byte Read(File this)
    {
        if (!this.isReading || this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return 0;
        }
        if (this.pos >= this.size)
        {
            this.isValid = false;
            return 0;
        }
        byte data = byte(this.buffer[pos]);
        this.pos = this.pos + 1;
        return data;
    }
    
    // Read a byte from the file at the specified seek position.
    byte Read(File this, uint seekPosition)
    {
        this.pos = seekPosition;
        return Read(this);
    }
    
    // Read data from the file into a byte array.
    uint Read(File this, byte[] data, uint bufferSize)
    {
        uint count;
        if (!this.isReading || this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return count;
        }
        while ((this.pos < this.size) && (count < bufferSize))
        {
            data[count] = byte(buffer[this.pos]);
            this.pos = this.pos + 1;
            count++;
        }
        return count;
    }
    
    // Append a byte to the file.
    Append(File this, byte content)
    {
        if (this.isReading || !this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return;
        }
        this.buffer += char(content);
    }
    
    // Append a string to the file.
    Append(File this, string content)
    {
        if (this.isReading || !this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return;
        }
        this.buffer += content;
    }
    
    // Try to read all text from a file into a string.
    bool TryReadAllText(string path, ref string content)
    {
        content = "";
        file current = File.Open(path);
        if (!current.IsValid())
        {
            return false;
        }
        content = current.buffer;
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
            if (0 != FileSystem.remove(destinationPath))
            {
                return false;
            }
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
    
    // Create a new file at the given path.
    File Create(string path)
    {
        string fullPath = FileSystem.getFullPath(path);
        File fileInstance;
        if (File.Exists(fullPath))
        {
            if (0 != FileSystem.remove(fullPath))  
            {
                return fileInstance;
            }
        }
        fileInstance.isWriting = true;
        fileInstance.isValid = true;
        fileInstance.path = fullPath;
        return fileInstance;
    }
    
    
    // Open a file at the given path (and read all content into buffer)
    File Open(string path)
    {
        File fileInstance;
        string fullPath = FileSystem.getFullPath(path);
        fileInstance.path = fullPath;
        fileInstance.isReading = true;
        
        byte[4] fileHandle = FileSystem.fOpen(fullPath, "r");
        if (fileHandle[0] == 0)
        {
            fileInstance.isValid = false;
            return fileInstance; // File not found
        }
        loop
        {
            byte[128] buffer;
            uint count = FileSystem.fRead(buffer, 128, 1, fileHandle);
            if (count == 0)
            {
                break;
            }
            for (uint i=0; i < count; i++)
            {
                fileInstance.buffer = fileInstance.buffer + char(buffer[i]);
            }
        }
        fileInstance.size = (fileInstance.buffer).Length;
        fileInstance.pos = 0;
        fileInstance.isValid = (0 == FileSystem.fClose(fileHandle));
        return fileInstance;
    }
    
    // Flush the file buffer to the storage media.
    Flush(File this)
    {
        if (this.isReading || !this.isWriting || !this.isValid)
        {
            this.isValid = false;
            return;
        }
        
        byte[4] fileHandle = FileSystem.fOpen(this.path, "w");
        if (fileHandle[0] == 0)
        {
            this.isValid = false;
            return; // failed to create
        }
        string content = this.buffer;
        byte[128] buffer;
        uint pos = 0;
        loop
        {
            uint i;
            while ((pos < content.Length) && (i < 128))
            {
                buffer[i] = byte(content[pos]);
                pos++;
                i++;
            }
            if (i == 0)
            {
                break;
            }
            _ = FileSystem.fWrite(buffer, i, 1, fileHandle);
        }
        this.isValid = (0 == FileSystem.fClose(fileHandle));
    }
    
}
