unit Directory
{
    
    uses "/Source/Library/File/SerialEEPROM"
    uses "/Source/Library/File/BlockFileSystem"
    
    record Directory
    {
        // private members
        bool   isValid;
        string path;
    }
    
    bool IsValid(Directory this)
    {
        // Check if the directory handle is valid
        return this.isValid;
    }
    
    // Check if a directory exists at the given path.
    bool Exists(string path)
    {
        // Stub implementation
        string fullPath = FileSystem.getFullPath(path);
        byte[2] dirHandle = FileSystem.openDir(fullPath);
        if (dirHandle[0] == 0)
        {
            return false; // Directory not found
        }
        return true;
    }
    
    Delete(string path)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
    }
    Create(string path)
    {
        // Stub implementation
        Diagnostics.Die(0x0A); // not implemented
    }
    
    uint GetDirectoryCount(Directory this)
    {
        uint count;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle);
                if (entry[0] == 0) 
                {
                    break;
                }
                if (FileSystem.isDirectory(entry))
                {
                    // directory
                    count++;
                }
            }
        }
        return count;
    }
    
    uint GetFileCount(Directory this)
    {
        uint count;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle);
                if (entry[0] == 0) 
                {
                    break;
                }
                if (!FileSystem.isDirectory(entry))
                {
                    // file
                    count++;
                }
            }
        }
        return count;
    }
    
    string GetDirectory(Directory this, uint index)
    {
        string result;
        uint count;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle);
                if (entry[0] == 0) 
                {
                    break;
                }
                if (FileSystem.isDirectory(entry))
                {
                    // directory
                    if (count == index)
                    {
                        result = FileSystem.getName(entry);
                        result = Path.Combine(this.path, result);
                        break;
                    }
                    count++;
                }
            }
        }
        return result;
    }
    
    string GetFile(Directory this, uint index)
    {
        string result;
        uint count;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle);
                if (entry[0] == 0) 
                {
                    break;
                }
                if (!FileSystem.isDirectory(entry))
                {
                    // file
                    if (count == index)
                    {
                        result = FileSystem.getName(entry);
                        result = Path.Combine(this.path, result);
                        break;
                    }
                    count++;
                }
            }
        }
        return result;
    }
    
    Directory Open(string path)
    {
        Directory dir;
        string fullPath = FileSystem.getFullPath(path);
        if (Directory.Exists(fullPath))
        {
            dir.path = fullPath;
            dir.isValid = true;
        }
        return dir;
    }
    
    
}
