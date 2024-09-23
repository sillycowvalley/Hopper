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
        _ = FileSystem.rmDir(path); // error check?
    }
    Create(string path)
    {
        _ = FileSystem.mkDir(path); // error check?
    }
    
    uint GetDirectoryCount(Directory this)
    {
        uint count;
        bool done;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle, ref done);
                if (done)
                {
                    break;
                }
                if (entry[0] == 0) 
                {
                    continue; // skip empty slot
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
        bool done;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle, ref done);
                if (done)
                {
                    break;
                }
                if (entry[0] == 0) 
                {
                    continue; // skip empty slot
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
        bool done;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle, ref done);
                if (done)
                {
                    break;
                }
                if (entry[0] == 0) 
                {
                    continue; // skip empty slot
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
        bool done;
        if (Directory.IsValid(this))
        {
            byte[2] dirHandle = FileSystem.openDir(this.path);
            loop
            {
                byte[] entry =  FileSystem.readDir(dirHandle, ref done);
                if (done)
                {
                    break;
                }
                if (entry[0] == 0) 
                {
                    continue; // skip empty slot
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
