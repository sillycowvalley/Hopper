unit Directory
{
    uses "/Source/Library/StorageMedia"
    
    record Directory
    {
        // private members
        bool   isValid;
        string path;
    }
    
    bool Exists(string path)
    {
        byte[StorageMedia.SectorSize] buffer;
        
        // Extract the parent directory and the directory name
        string parentDirName = Path.GetDirectoryName(path);
        string dirName = Path.GetFileName(path);

        // Open the parent directory
        Directory parentDir = Directory.Open(parentDirName);
        if (!Directory.IsValid(parentDir))
        {
            return false;
        }

        // Scan the parent directory entries
        uint skipped;
        uint dirCount = Directory.GetDirectoryCount(parentDir, ref skipped);
        for (uint i = 0; i < dirCount; i++)
        {
            string existingDirName = Directory.GetDirectory(parentDir, i);
            if (existingDirName == dirName)
            {
                return true;
            }
        }
        return false;
    }

    bool Create(string path)
    {
        byte[StorageMedia.SectorSize] buffer;

        // Extract the parent directory and the directory name
        string parentDirName = Path.GetDirectoryName(path);
        string dirName = Path.GetFileName(path);

        // Open the parent directory
        Directory parentDir = Directory.Open(parentDirName);
        if (!IsValid(parentDir))
        {
            return false;
        }

        // Find an empty entry in the parent directory
        uint skipped;
        uint dirCount = Directory.GetDirectoryCount(parentDir, ref skipped);
        for (uint i = 0; i < dirCount; i++)
        {
            string existingDirName = Directory.GetDirectory(parentDir, i);
            if (existingDirName == "")
            {
                // Found an empty entry, create the new directory
                // Fill in the new directory entry
                // The actual implementation details for FAT12 will be within StorageMedia

                // Initialize directory entry in buffer
                // This would typically involve setting the directory name, attributes, and starting cluster
                
                // For the purpose of this implementation, we're assuming `writeDirectoryEntry` is a private method in StorageMedia
                if (!StorageMedia.writeDirectoryEntry(parentDir, i, dirName, true))
                {
                    return false;
                }

                // Create the new directory's own sector
                // Initialize directory sector (empty entries)
                for (uint j = 0; j < StorageMedia.SectorSize; j++)
                {
                    buffer[j] = 0;
                }

                // Write the new directory sector to storage
                uint newDirSector = StorageMedia.allocateSector(); // Assuming this helper method exists
                if (!StorageMedia.WriteSector(newDirSector, buffer))
                {
                    return false;
                }

                return true;
            }
        }
        return false;
    }

    Directory Open(string path)
    {
        byte[StorageMedia.SectorSize] buffer;

        // Extract the parent directory and the directory name
        string parentDirName = Path.GetDirectoryName(path);
        string dirName = Path.GetFileName(path);
        
        Directory result;

        // Open the parent directory
        Directory parentDir = Directory.Open(parentDirName);
        if (!Directory.IsValid(parentDir))
        {
            return result;
        }

        // Scan the parent directory entries
        uint skipped;
        uint dirCount = Directory.GetDirectoryCount(parentDir, ref skipped);
        for (uint i = 0; i < dirCount; i++)
        {
            string existingDirName = Directory.GetDirectory(parentDir, i);
            if (existingDirName == dirName)
            {
                // Open the directory (for now, returning the path as a simple handle)
                result.isValid = true;
                result.path = dirName;
                break;
            }
        }
        return result;
    }

    bool IsValid(Directory this)
    {
        // Check if the directory handle is valid
        return this.isValid;
    }

    uint GetFileCount(Directory this)
    {
        byte[StorageMedia.SectorSize] buffer;

        // Read the directory sector
        uint index = 0; // this.Index TODO
        if (!StorageMedia.ReadSector(index, ref buffer))
        {
            return 0;
        }

        // Count the number of valid file entries
        uint fileCount = 0;
        for (uint i = 0; i < StorageMedia.SectorSize; i += 32) // Assuming each directory entry is 32 bytes
        {
            if (buffer[i] != 0) // Check if the entry is not empty
            {
                fileCount++;
            }
        }
        return fileCount;
    }

    // Stubs for the missing methods to ensure the code compiles
    Delete(string path)
    {
        Diagnostics.Die(0x0A); // not implemented
    }

    string GetDirectory(Directory this, uint index)
    {
        Diagnostics.Die(0x0A); // not implemented
        return "";
    }

    string GetFile(Directory this, uint index)
    {
        Diagnostics.Die(0x0A); // not implemented
        return "";
    }

    string GetTime(string path)
    {
        Diagnostics.Die(0x0A); // not implemented
        return "";
    }

    string GetDate(string path)
    {
        Diagnostics.Die(0x0A); // not implemented
        return "";
    }

    uint GetDirectoryCount(Directory this, ref uint skipped)
    {
        Diagnostics.Die(0x0A); // not implemented
        return 0;
    }

    uint GetFileCount(Directory this, ref uint skipped)
    {
        Diagnostics.Die(0x0A); // not implemented
        return 0;
    }
}

