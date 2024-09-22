unit FileSystem
{
    #if !defined(BLOCKFILESYSTEM)
        #define BLOCKFILESYSTEM
    #endif
    
    friend Directory, File, System;
    
    /*
    File System Structure:
    -----------------------
    1. ChainLists:
       - Contains chains of blocks for files and directories that span multiple blocks.
       - Located at page 0.
    2. Root Directory:
       - The starting point of the file system, storing the root directory entries.
       - Located at page 1.
    Descriptor Record Structure (within Directory Pages):
    -----------------------------------------------------
    - Filename:           Offset 0, 12 bytes (null-terminated if shorter)
    - File Type:          Offset 12, 1 byte (high nibble: type, low nibble: name length)
                          - 0x00: File
                          - 0x10: Directory
    - Start Block:        Offset 13, 1 byte (block number where the file/directory starts, 1 if empty or end of chain)
    - File Size:          Offset 14, 2 bytes (size of the file in bytes)
    File Handle Structure (byte array):
    -----------------------------------
       [0] - Directory block number (byte)
       [1] - File descriptor index (0 to 15) (byte)
       [2] - Current position LSB in file (byte)
       [3] - Current position MSB in file (byte)
       Total: 4 bytes
    Directory Handle Structure (byte array):
    ----------------------------------------
       [0] - Directory descriptor index (byte)
       [1] - Current position in directory (byte)
       Total: 2 bytes
    Directory Entry Structure (byte array):
    ---------------------------------------
       [0] - Entry type (0 for file, 1 for directory) (byte)
       [1-12] - Entry name (up to 12 bytes, null-terminated if shorter)
       Total: 13 bytes
    */
    
    // Constants
    const byte SEEK_SET = 0;
    const byte SEEK_CUR = 1;
    const byte SEEK_END = 2;

    const uint blockSize  = 256;
    const uint totalPages = 256;
    const byte chainBlock = 0;
    const byte rootDirStartBlock = 1;
    const byte fileTypeDirectory = 0x10;
    const byte fileTypeFile = 0x00;
    const byte filenameOffset = 0;
    const byte fileTypeOffset = 12;
    const byte startBlockOffset = 13;
    const byte fileSizeOffset = 14;
    const byte descriptorSize = 16;
    const uint descriptorsPerBlock = blockSize / descriptorSize;
    
    string currentDirectory;

    // ### private Helper Functions
    
    // Reads a block from EEPROM.
    // blockNum: The block number to read.
    // buffer: The buffer to store the read data.
    readBlock(byte blockNum, byte[] buffer) 
    {
        BlockStorage.ReadBlock(blockNum, buffer);
    }
    
    
    // Writes a block to EEPROM.
    // blockNum: The block number to write.
    // buffer: The buffer containing the data to write.
    writeBlock(byte blockNum, byte[] buffer)
    {
        BlockStorage.WriteBlock(blockNum, buffer);
    }

    // Finds a free page in the ChainList.
    // Returns the index of the free page or 0 if no free page is found.
    byte findFreePage(byte[] theChainBlock)
    {
        for (uint i = 2; i < totalPages; i++)
        {
            if (theChainBlock[i] == 0)
            {
                return byte(i);
            }
        }
        return 0; // No free page found
    }

    // Validates a path for correct length and valid characters.
    // Returns true if the path is valid, false otherwise.
    bool isValidPath(string path) 
    {
        uint len = path.Length;
        if ((len == 0) || (len > 255))
        {
            return false;
        }
        for (uint i = 0; i < len; i++)
        {
            if (!Path.IsValidPathCharacter(path[i]))
            {
                return false;
            }
        }
        return true;
    }

    splitPath(string fullPath, ref string parentDir, ref string fileName) 
    {
        uint iSlash;
        parentDir = "";
        if (fullPath.LastIndexOf('/', ref iSlash))
        {
            if (iSlash == 0)
            {
                parentDir = "/";
            }
            else
            {
                parentDir = fullPath.Substring(0, iSlash);
            }
            fileName  = fullPath.Substring(iSlash + 1);
        }
        else 
        {
            fileName = fullPath;
        }
        if (parentDir.Length == 0)
        {
            parentDir = "/";
        }
    }
    
    string resolveRelativePath(string path)
    {
        string resolved;
        
        // Handle the root path separately
        if (path == "/") 
        {
            resolved = "/";
            return resolved;
        }
        
        <string> tokens = path.Split('/');
        foreach (var token in tokens)
        {
            if (token == ".") 
            {
                // Skip
            }
            else if (token == "..")
            {
                // Remove last directory in resolved
                uint iSlash;
                if (resolved.LastIndexOf('/', ref iSlash) && (iSlash != 0))
                {
                    resolved = resolved.Substring(0, iSlash);
                }
                else
                {
                    // Avoid removing the root slash
                    resolved = resolved.Substring(0, 1);
                }
            }
            else
            {
                if (resolved.Length > 1)
                {
                    resolved += '/';
                }
                resolved += token;
            }
        }
        return resolved;
    }
    string getFullPath(string path)
    {
        string result;
        string resolvedPath;
        if (currentDirectory.Length == 0) 
        {
            currentDirectory = "/"; // initialize before first use
        }
        if ((path.Length > 0) && (path[0] == '/'))
        {
            // Absolute path
            result = path;
        }
        else 
        {
            // Relative path
            result = currentDirectory;
            if (!result.EndsWith('/')) 
            {
                result += '/';
            }
            result += path;
        }
        
        // Resolve relative components like '.' and '..'
        resolvedPath = resolveRelativePath(result);
        
        result = resolvedPath;
        
        // Ensure the path starts with a root '/'
        if (result[0] != '/')
        {
            result = "/" + result;
        }
        return result;
    }
    
    
    bool compareDirEntry(byte[] dirEntry, string name, byte entryType)
    {
        bool found;
        uint length = name.Length;
        loop
        {
            if (dirEntry[startBlockOffset] == 0) 
            {
                break; // empty dirEntry
            }
            if ((dirEntry[fileTypeOffset] & 0xF0) != entryType)
            {
                break; // not the correct type of record
            }
            if (length != (dirEntry[fileTypeOffset] & 0x0F))
            {
                break; // not the correct length
            }
            for (uint i=0; i < length; i++)
            {
                if (dirEntry[filenameOffset+i] != byte(name[i]))
                {
                    break; // name does not match
                }
            }
            found = true; // name match
            break;
        }
        return found;
    }
    
    // Compacts a directory by moving entries from later blocks to empty slots in earlier blocks.
    compactDirectory(byte startBlock, byte currentBlock)
    {
        byte[blockSize] dirBlock;
        byte[blockSize] theChainBlock;
        byte[blockSize] nextDirBlock;
        byte parentNextBlock;
        uint i;
        uint j;
        bool blockMoved;
        bool isEmpty;
        byte prevBlock;
        
        readBlock(chainBlock, theChainBlock);
        loop
        {
            blockMoved = false;
            readBlock(currentBlock, dirBlock);
            for (i = 0; i < blockSize; i += descriptorSize)
            {
                if (dirBlock[i + filenameOffset] == 0) // Find empty slot
                { 
                    // Look for entries in later blocks
                    parentNextBlock = theChainBlock[currentBlock];
                    while (parentNextBlock != 1) 
                    {
                        readBlock(parentNextBlock, nextDirBlock);
                        for (j = 0; j < blockSize; j += descriptorSize) 
                        {
                            if (nextDirBlock[j + filenameOffset] != 0) // Find used slot
                            { 
                                // Move the entry to the empty slot
                                for (uint k=0; k < descriptorSize; k++)
                                {
                                    dirBlock[i+k] = nextDirBlock[j+k];
                                }
                                for (uint k=0; k < descriptorSize; k++)
                                {
                                    nextDirBlock[j+k] = 0;
                                }
                                writeBlock(currentBlock, dirBlock);
                                writeBlock(parentNextBlock, nextDirBlock);
                                blockMoved = true;
                                // Debug output for moving an entry
                                break;
                            }
                        }
                        if (j < blockSize) 
                        {
                            break;
                        }
                        parentNextBlock = theChainBlock[parentNextBlock];
                    }
                }
            }
            
            // Check if the current block is completely empty
            isEmpty = true;
            for (i = 0; i < blockSize; i += descriptorSize) 
            {
                if (dirBlock[i + filenameOffset] != 0) 
                {
                    isEmpty = false;
                    break;
                }
            }
            
            // If the current block is empty and not the first block, remove it from the chain
            if (isEmpty && (currentBlock != startBlock))
            {
                prevBlock = startBlock;
                while (theChainBlock[prevBlock] != currentBlock)
                {
                    prevBlock = theChainBlock[prevBlock];
                    if ((prevBlock == 1) || (prevBlock == 0))
                    {
                        break;
                    }
                }
                theChainBlock[prevBlock] = theChainBlock[currentBlock];
                theChainBlock[currentBlock] = 0;
                writeBlock(chainBlock, theChainBlock);
                break;
            }
            if (!blockMoved)
            {
                break;
            }
            
            currentBlock = theChainBlock[currentBlock];
            if (currentBlock == 1) { break; }
        }
    }
    
    // ### end of private helper functions
    
    bool isDirectory(byte[] dirEntry)
    {
        return (dirEntry[fileTypeOffset] & 0xF0) == fileTypeDirectory;
    }
    
    // Formats the drive, initializing the file system.
    Format() 
    {
        // Initialize global block chain page (no chains yet, 0 and 1 reserved)
        byte[blockSize] blockChainPage;
        blockChainPage[0] = 1;
        blockChainPage[1] = 1;
        writeBlock(chainBlock, blockChainPage);
        
        // Initialize the root directory (empty initially)
        byte[blockSize] rootDir;
        writeBlock(rootDirStartBlock, rootDir);
    }
    
    // Closes an open directory.
    // dirHandle: The handle of the directory to close.
    // Returns 0 on success, or -1 on error.
    closeDir(byte[] dirHandle) {
        dirHandle[0] = 0; // so it cannot be used
        dirHandle[1] = 0;
    }  
    
    // Closes an open file or directory.
    // fileHandle: The handle of the file or directory to close.
    // Returns 0 on success, or -1 on error.
    int fClose(byte[] fileHandle) {
        if (fileHandle[0] != 0)
        {
            fileHandle[0] = 0; // Directory block number
            fileHandle[1] = 0; // File descriptor index
            return 0; // Success
        }
        return -1; // Invalid handle
    }
      
    int mkDir(string dirname) 
    {
        byte[blockSize] dirBlock;
        byte[descriptorSize] descriptor;
        byte[blockSize] theChainBlock;
        byte newBlock;
        string fullPath;
        uint i;
        
        byte[2] dirHandle = openDir(dirname);
        if (dirHandle[0] != 0)
        {
            return -1; // Directory already exists
        }
        
        fullPath = getFullPath(dirname);
        if (!isValidPath(fullPath)) 
        {
            return -1; // Invalid path
        }
        
        readBlock(chainBlock, theChainBlock);
        
        // Find a free block for the new directory
        newBlock = findFreePage(theChainBlock);
        if (newBlock == 0) 
        {
            return -1; // Disk full (no available pages)
        }
        
        // Create a local copy of dirname to modify
        string localParent;
        string localName;
        // Initialize the directory descriptor
        splitPath(fullPath, ref localParent, ref localName);
        
        for (i = 0; i < localName.Length; i++)
        {
            descriptor[filenameOffset+i] = byte(localName[i]);
        }
        descriptor[fileTypeOffset] = byte(fileTypeDirectory | (localName.Length & 0x0F));
        descriptor[startBlockOffset] = newBlock;
        
        // Read the parent directory block to find an empty slot
        byte[] parentDirHandle = openDir(localParent);
        if (parentDirHandle[0] == 0)
        {
            return -1; // Failed to open parent directory
        }
        readBlock(parentDirHandle[0], dirBlock);
        byte currentBlock = parentDirHandle[0];
        closeDir(parentDirHandle);
        
        loop
        {
            for (i = 0; i < blockSize; i += descriptorSize)
            {
                if (dirBlock[i + filenameOffset] == 0)
                {
                    for (uint j = 0; j < descriptorSize; j++)
                    {
                        dirBlock[i+j] = descriptor[j];
                    }
                    writeBlock(currentBlock, dirBlock);
                    
                    // Create new empty block for the directory and update ChainBlock
                    byte [blockSize] newDirBlock;
                    writeBlock(newBlock, newDirBlock);
                    theChainBlock[newBlock] = 1; // Mark as end of chain
                    writeBlock(chainBlock, theChainBlock);
                    return 0; // Success
                }
            }
            // If no empty slot found in the current block, follow the chain
            if (theChainBlock[currentBlock] == 1)
            {
                // No more blocks in the chain, add a new block
                byte newDirBlockInChain = findFreePage(theChainBlock);
                if (newDirBlockInChain == 0)
                {
                    return -1; // Disk full (no available pages)
                }
                theChainBlock[currentBlock] = newDirBlockInChain;
                theChainBlock[newDirBlockInChain] = 1; // Mark as end of chain
                writeBlock(chainBlock, theChainBlock);
                
                // Initialize the new directory block
                byte [blockSize] newDirBlock;
                writeBlock(newDirBlockInChain, newDirBlock);
                currentBlock = newDirBlockInChain;
                readBlock(currentBlock, dirBlock);
            }
            else
            {
                // Move to the next block in the chain
                currentBlock = theChainBlock[currentBlock];
                readBlock(currentBlock, dirBlock);
            }
        }
        return -1; // No empty slot found
    }
    
    byte[2] openDir(string dirname)
    {
        byte[2] dirHandle;
        byte[blockSize] dirBlock;
        byte[blockSize] theChainBlock;
        string fullPath;
        byte currentBlock;
        bool found;
        uint i;
        
        fullPath = getFullPath(dirname);
        if (!isValidPath(fullPath))
        {
            return dirHandle; // Invalid path
        }
        
        readBlock(chainBlock, theChainBlock);
        
        if (fullPath == "/")
        {
            dirHandle[0] = rootDirStartBlock;  // Root directory descriptor index
            //dirHandle[1] = 0;                // Current position in directory
            return dirHandle;
        }
        
        // Split the path into components
        currentBlock = rootDirStartBlock;
        <string> tokens = fullPath.Split('/');
        foreach (var token in tokens)
        {
            found = false;
            readBlock(currentBlock, dirBlock);
            loop
            {
                for (i = 0; i < blockSize; i += descriptorSize)
                {
                    byte[descriptorSize] dirEntry;
                    for (uint j = 0; j < descriptorSize; j++)
                    {
                        dirEntry[j] = dirBlock[i+j];
                    }
                    if (compareDirEntry(dirEntry, token, fileTypeDirectory))
                    {
                        currentBlock = dirBlock[i + startBlockOffset];
                        found = true;
                        break;
                    }
                }
                if (found || (theChainBlock[currentBlock] == 1))
                {
                    break;
                }
                currentBlock = theChainBlock[currentBlock];
                readBlock(currentBlock, dirBlock);
            }
            
            if (!found)
            {
                dirHandle[0] = 0;
                //dirHandle[1] = 0;
                return dirHandle; // Directory not found
            }
        }
        
        dirHandle[0] = currentBlock;
        //dirHandle[1] = 0;
        return dirHandle;
    }
    
    string getName(byte[] dirEntry)
    {
        string name;
        uint length = dirEntry[fileTypeOffset] & 0x0F;
        for (uint i=0; i < length; i++)
        {
            name = name + char(dirEntry[filenameOffset+i]);
        }
        return name;
    }
    
    byte[descriptorSize] readDir(byte[2] dirHandle)
    {
        byte[descriptorSize] dirEntry;
        
        if (dirHandle[1] == blockSize - descriptorSize) 
        {
            // Previous entry was the last one in that page
            // Follow the chain to the next page for this directory
            byte[blockSize] theChainBlock;
            readBlock(chainBlock, theChainBlock);
            
            dirHandle[0] = theChainBlock[dirHandle[0]];
            if (dirHandle[0] == 1) // that was the last page in the chain
            {
                return dirEntry;  
            }
            dirHandle[1] = 0;
        }
        
        // Read the block where the current directory entry is
        byte[blockSize] dirBlock;
        readBlock(dirHandle[0], dirBlock);
        
        // Copy the directory entry
        for (uint i=0; i < descriptorSize; i++)
        {
            dirEntry[i] = dirBlock[dirHandle[1]+i];
        }
        if (dirEntry[startBlockOffset] == 0)
        {
            // empty slot
            return dirEntry;
        }
            
        // Update the directory handle position for the next iteration
        dirHandle[1] = dirHandle[1] + descriptorSize;
        return dirEntry;
    }
    
    
    int rmDir(string path)
    {
        byte[2] dirHandle;
        
        byte[blockSize] dirBlock;
        byte[blockSize] theChainBlock;
        byte[descriptorSize] descriptor;
        string fullPath;
        string parentDir;
        string dirName;
        byte currentBlock;
        uint i;
        int result = -1; // Default to failure
        
        loop
        {
            fullPath = getFullPath(path);
            
            if (!isValidPath(fullPath)) 
            {
                break; // Invalid path
            }
            
            // Open the directory to ensure it exists
            dirHandle = openDir(fullPath);
            if (dirHandle[0] == 0) 
            {
                break; // Directory not found
            }
            
            // Check if the directory is empty
            readBlock(dirHandle[0], dirBlock);
            for (i = 0; i < blockSize; i += descriptorSize) 
            {
                if (dirBlock[i + filenameOffset] != 0) 
                {
                    closeDir(dirHandle);
                    break; // Directory not empty
                }
            }
            if (i < blockSize) 
            {
                break; // Directory not empty
            }
            currentBlock = dirHandle[0];
            closeDir(dirHandle);
            
            // Free the blocks used by the directory
            readBlock(chainBlock, theChainBlock);
            while (currentBlock != 1) 
            {
                byte nextBlock = theChainBlock[currentBlock];
                theChainBlock[currentBlock] = 0;
                currentBlock = nextBlock;
            }
            writeBlock(chainBlock, theChainBlock);
            
            // Find and clear the directory descriptor in the parent directory
            splitPath(fullPath, ref parentDir, ref dirName);
            dirHandle = openDir(parentDir);
            if (dirHandle[0] == 0) 
            {
                break; // Failed to open parent directory
            }
            
            readBlock(dirHandle[0], dirBlock);
            loop
            {
                for (i = 0; i < blockSize; i += descriptorSize)
                {
                    for (uint j=0; j < descriptorSize; j++)
                    {
                        descriptor[j] = dirBlock[i+j];
                    }
                    
                    if (compareDirEntry(descriptor, dirName, fileTypeDirectory))
                    {
                        // Clear the descriptor
                        for (uint j=0; j < descriptorSize; j++)
                        {
                            dirBlock[i+j] = 0;
                        }
                        writeBlock(dirHandle[0], dirBlock);
                        
                        compactDirectory(dirHandle[0], dirHandle[0]); // Compact the directory
                        
                        result = 0; // Success
                        break; // Exit the loop
                    }
                }
                if ((result == 0) || (theChainBlock[dirHandle[0]] == 1))
                {
                    break; // Exit the loop if success or no more blocks in the chain
                }
                dirHandle[0] = theChainBlock[dirHandle[0]];
                readBlock(dirHandle[0], dirBlock);
            }
            
            closeDir(dirHandle);
            break; // Exit the main for loop
        }
        return result; // Return the result
    }
    
    // Returns the current file position indicator for the file.
    // fileHandle: The handle of the file.
    // Returns the current file position as a word, or 0 on error.
    uint fTell(byte[4] fileHandle)
    {
        // Extract the current position from the file handle
        uint position = (fileHandle[2] << 8) | fileHandle[3];
        return position;
    }
    
    string getCwd()
    {
        string copy;
        if (currentDirectory == "")
        {
            currentDirectory = "/"; // initialize before first use
        }
        copy = currentDirectory;
        return copy;
    }
    
    int chDir(string path)
    {
        byte[2] dirHandle;
        string fullPath = getFullPath(path);
        
        if (!isValidPath(fullPath))
        {
            return -1; // Invalid path
        }
        dirHandle = openDir(fullPath);
        if (dirHandle[0] == 0)
        {
            return -1; // Directory not found
        }
        closeDir(dirHandle);
        currentDirectory = fullPath;
        return 0; // Success
    }
    
    // Opens a file
    // filename: Name of the file or directory to open.
    // mode: Mode in which to open the file (e.g., "r" for read, "w" for write, etc.).
    // Returns a file handle if successful, or null if an error occurs.
    byte[4] fOpen(string path, string mode)
    {
        byte[blockSize] dirBlock;
        byte[blockSize] newDirBlock;
        byte[blockSize] theChainBlock;
        byte[descriptorSize] descriptor;
        byte currentBlock;
        uint i;
        string parentDir;
        string fileName;
        byte[2] parentDirHandle;
        byte[4] fileHandle;
        byte fileBlock;
        byte nextBlock;
        byte newDirBlockInChain;
        
        loop
        {
            string fullPath = getFullPath(path);
            if (!isValidPath(fullPath)) 
            {
                break; // Invalid path
            }
            if ((mode != "w") && (mode != "r"))
            {
                break; // Invalid mode
            }
            readBlock(chainBlock, theChainBlock);
            splitPath(fullPath, ref parentDir, ref fileName);
            
            parentDirHandle = openDir(parentDir);
            if (parentDirHandle[0] == 0)
            {
                break; // directory not found
            }
            // Locate the file descriptor
            loop
            {
                readBlock(parentDirHandle[0], dirBlock);
                for (i = 0; i < blockSize; i += descriptorSize)
                {
                    byte[descriptorSize] dirEntry;
                    for (uint j = 0; j < descriptorSize; j++)
                    {
                        dirEntry[j] = dirBlock[i+j];
                    }
                    if (compareDirEntry(dirEntry, fileName, fileTypeFile))
                    {
                         // If mode is "w", truncate the file
                        if (mode == "w")
                        {
                            // Free the blocks used by the file if it already exists
                            fileBlock = dirBlock[i + startBlockOffset];
                            while (fileBlock != 1)
                            {
                                nextBlock = theChainBlock[fileBlock];
                                theChainBlock[fileBlock] = 0;
                                fileBlock = nextBlock;
                            }
                            // Reset the descriptor
                            dirBlock[i + startBlockOffset] = 1; // end of chain
                            dirBlock[i + fileSizeOffset] = 0; // file size to 0
                            dirBlock[i + fileSizeOffset + 1] = 0; // file size to 0
                            writeBlock(parentDirHandle[0], dirBlock);
                            writeBlock(chainBlock, theChainBlock);
                        }
                        fileHandle[0] = parentDirHandle[0]; // Directory block number
                        fileHandle[1] = byte(i / descriptorSize); // File descriptor index
                        //fileHandle[2] = 0; // Current position LSB in file (Tigger C : automatic zero initialization)
                        //fileHandle[3] = 0; // Current position MSB in file (Tigger C : automatic zero initialization)
                        closeDir(parentDirHandle);
                        break; // Success
                    }
                } // for
                if (fileHandle[0] != 0)
                {
                    break; // Success
                }
                // Follow the chain if no descriptor found in current block
                if (theChainBlock[parentDirHandle[0]] == 1)
                {
                    break; // No more blocks in the chain
                }
                parentDirHandle[0] = theChainBlock[parentDirHandle[0]];
            } // while
            closeDir(parentDirHandle);
            if (fileHandle[0] != 0)
            {
                break; // Success
            }
            // If the file doesn't exist and mode is "w", create a new file
            if (mode == "w") 
            {
                // Initialize the file descriptor
                uint length = fileName.Length;
                for (uint j = 0; j < length; j++)
                {
                    descriptor[filenameOffset+j] = byte(fileName[j]);
                }
                
                descriptor[fileTypeOffset] = byte(fileTypeFile | (length & 0x0F));
                descriptor[startBlockOffset]   = 1; // end of chain
                descriptor[fileSizeOffset]     = 0; // Empty file LSB
                descriptor[fileSizeOffset + 1] = 0; // Empty file MSB
                // Read the parent directory block to find an empty slot
                parentDirHandle = openDir(parentDir);
                if (parentDirHandle[0] == 0) 
                {
                    break; // Failed to open parent directory
                }
                readBlock(parentDirHandle[0], dirBlock);
                currentBlock = parentDirHandle[0];
                closeDir(parentDirHandle);
                loop
                {
                    for (i = 0; i < blockSize; i += descriptorSize)
                    {
                        if (dirBlock[i + filenameOffset] == 0) 
                        {
                            for (uint j = 0; j < descriptorSize; j++)
                            {
                                dirBlock[i+j] = descriptor[j];
                            }
                            writeBlock(currentBlock, dirBlock);
                            fileHandle[0]   = currentBlock; // Directory block number
                            fileHandle[1]   = byte(i / descriptorSize); // File descriptor index
                            //fileHandle[2] = 0; // Current position LSB in file (Tigger C : automatic zero initialization)
                            //fileHandle[3] = 0; // Current position MSB in file (Tigger C : automatic zero initialization)
                            break; // Success
                        }
                    } // for
                    if (fileHandle[0] != 0)
                    {
                        break; // Success
                    }
                    // If no empty slot found in the current block, follow the chain
                    if (theChainBlock[currentBlock] == 1) 
                    {
                        // No more blocks in the chain, add a new block
                        newDirBlockInChain = findFreePage(theChainBlock);
                        if (newDirBlockInChain == 0)
                        {
                            break; // Disk full (no available pages)
                        }
                        theChainBlock[currentBlock] = newDirBlockInChain;
                        theChainBlock[newDirBlockInChain] = 1; // Mark as end of chain
                        writeBlock(chainBlock, theChainBlock);
                        // Initialize the new directory block
                        writeBlock(newDirBlockInChain, newDirBlock);
                        currentBlock = newDirBlockInChain;
                        readBlock(currentBlock, dirBlock);
                    }
                    else
                    {
                        // Move to the next block in the chain
                        currentBlock = theChainBlock[currentBlock];
                        readBlock(currentBlock, dirBlock);
                    }
                } // while
            } // if "w"
            break;
        } // loop
        return fileHandle; // Return file handle or null if not found/created
    }
    
    int remove(string path)
    {
        byte[blockSize] dirBlock;
        byte[blockSize] theChainBlock;
        byte[descriptorSize] descriptor;
        string fullPath;
        string parentDir;
        string fileName;
        byte[2] dirHandle;
        byte currentBlock;
        uint i;
        byte fileBlock;
        byte nextBlock;
        int result = -1; // Default to failure
    
        loop
        {
            fullPath = getFullPath(path);
            if (!isValidPath(fullPath))
            {
                break; // Invalid path
            }
    
            splitPath(fullPath, ref parentDir, ref fileName);
            dirHandle = openDir(parentDir);
            if (dirHandle[0] == 0)
            {
                break; // Failed to open parent directory
            }
    
            readBlock(chainBlock, theChainBlock);
            
            // Locate the file descriptor
            loop
            {
                readBlock(dirHandle[0], dirBlock);
                for (i = 0; i < blockSize; i += descriptorSize)
                {
                    for (uint j=0; j < descriptorSize; j++)
                    {
                        descriptor[j] = dirBlock[i + j];
                    }
                    
                    if (compareDirEntry(descriptor, fileName, fileTypeFile))
                    {
                        fileBlock = descriptor[startBlockOffset];
                        
                        // Clear the file descriptor
                        for (uint j=0; j < descriptorSize; j++)
                        {
                            descriptor[j] = 0; // Clearing the descriptor array itself
                            dirBlock[i+j] = 0; // Clearing the block position
                        }
                        writeBlock(dirHandle[0], dirBlock);
                        
                        // Free the blocks used by the file
                        while ((fileBlock != 1) && (fileBlock != 0))
                        {
                            nextBlock = theChainBlock[fileBlock];
                            theChainBlock[fileBlock] = 0;
                            fileBlock = nextBlock;
                        }
                        writeBlock(chainBlock, theChainBlock);
                        result = 0; // Success
    
                        // Call to compact the directory
                        compactDirectory(dirHandle[0], dirHandle[0]);
                        break; // Exit the loop
                    }
                }
    
                if ((result == 0) || (theChainBlock[dirHandle[0]] == 1))
                {
                    break; // Exit the loop if success or no more blocks in the chain
                }
                dirHandle[0] = theChainBlock[dirHandle[0]];
            }
    
            closeDir(dirHandle);
            break; // Exit the main for loop
        }
        return result; // Return the result
    }
    
    uint fWrite(byte[] buffer, uint size, uint count, byte[4] fileHandle)
    {
        byte[blockSize] fileBlock;
        byte[blockSize] theChainBlock;
        byte[blockSize] dirBlock;
        uint bytesToWrite = size * count;
        uint bytesWritten = 0; // Explicitly setting it to 0 for clarity
        uint toWrite;
        uint position = (fileHandle[2] << 8) | fileHandle[3];
        byte currentBlock;
        byte newBlock;
        uint offset = position;
        byte dirBlockNumber = fileHandle[0]; // Directory block number
        byte dirIndex = fileHandle[1]; // Directory entry index
    
        readBlock(chainBlock, theChainBlock);
        readBlock(dirBlockNumber, dirBlock);
        currentBlock = dirBlock[dirIndex * descriptorSize + startBlockOffset];
    
        // Handle new empty file scenario
        if (currentBlock == 1)
        {
            currentBlock = findFreePage(theChainBlock);
            if (currentBlock == 0)
            {
                return 0; // Disk full, no blocks available
            }
            theChainBlock[currentBlock] = 1; // Mark as end of chain
            writeBlock(chainBlock, theChainBlock);
            // Update directory entry with new block number
            dirBlock[dirIndex * descriptorSize + startBlockOffset] = currentBlock;
            writeBlock(dirBlockNumber, dirBlock);
        }
    
        // Walk the current file data block chain until you get to the block that contains the current position
        while (offset >= blockSize)
        {
            if (theChainBlock[currentBlock] == 1)
            {
                newBlock = findFreePage(theChainBlock);
                if (newBlock == 0)
                {
                    return 0; // Disk full, no blocks available
                }
                theChainBlock[currentBlock] = newBlock;
                theChainBlock[newBlock] = 1; // Mark as end of chain
                writeBlock(chainBlock, theChainBlock);
                currentBlock = newBlock;
            }
            else
            {
                currentBlock = theChainBlock[currentBlock];
            }
            offset -= blockSize;
        }
    
        while (bytesToWrite > 0)
        {
            readBlock(currentBlock, fileBlock);
            offset = position % blockSize;
            toWrite = blockSize - offset;
            if (toWrite > bytesToWrite)
            {
                toWrite = bytesToWrite;
            }
            for (uint i=0; i < toWrite; i++)
            {
                fileBlock[offset+i] = buffer[bytesWritten+i];
            }
            writeBlock(currentBlock, fileBlock);
            position += toWrite;
            bytesWritten += toWrite;
            bytesToWrite -= toWrite;
    
            if (bytesToWrite > 0)
            {
                if (theChainBlock[currentBlock] == 1)
                {
                    newBlock = findFreePage(theChainBlock);
                    if (newBlock == 0)
                    {
                        return bytesWritten / size; // Disk full, return number of elements written
                    }
                    theChainBlock[currentBlock] = newBlock;
                    theChainBlock[newBlock] = 1; // Mark as end of chain
                    writeBlock(chainBlock, theChainBlock);
                    currentBlock = newBlock;
                }
                else
                {
                    currentBlock = theChainBlock[currentBlock];
                }
            }
        }
    
        // Update position in file handle
        fileHandle[2] = byte((position >> 8) & 0xFF);
        fileHandle[3] = byte(position & 0xFF);
        // Update the file size to the current position in directory entry
        dirBlock[dirIndex * descriptorSize + fileSizeOffset]     = byte((position >> 8) & 0xFF);
        dirBlock[dirIndex * descriptorSize + fileSizeOffset + 1] = byte(position & 0xFF);
        writeBlock(dirBlockNumber, dirBlock);
    
        return bytesWritten / size;
    }
       
    uint fRead(byte[] buffer, uint size, uint count, byte[4] fileHandle) 
    {
        byte[blockSize] fileBlock;
        byte[blockSize] theChainBlock;
        byte[blockSize] dirBlock;
        uint bytesToRead = size * count;
        uint bytesRead = 0; // Explicitly setting it to 0 for clarity
        uint toRead;
        uint position = (fileHandle[2] << 8) | fileHandle[3];
        byte currentBlock;
        uint offset = position;
        byte dirBlockNumber = fileHandle[0]; // Directory block number
        byte dirIndex = fileHandle[1]; // Directory entry index
        uint fileSize;
    
        readBlock(chainBlock, theChainBlock);
        readBlock(dirBlockNumber, dirBlock);
        currentBlock = dirBlock[dirIndex * descriptorSize + startBlockOffset];
        fileSize   = ((dirBlock[dirIndex * descriptorSize + fileSizeOffset]) << 8) |
                       dirBlock[dirIndex * descriptorSize + fileSizeOffset + 1];
    
        // Handle empty file scenario
        if ((currentBlock == 1) || (position >= fileSize))
        {
            return 0; // Nothing to read
        }
    
        // Adjust bytesToRead if it exceeds the remaining file size
        if (bytesToRead > (fileSize - position))
        {
            bytesToRead = fileSize - position;
        }
    
        // Walk the current file data block chain until you get to the block that contains the current position
        while (offset >= blockSize)
        {
            if (theChainBlock[currentBlock] == 1)
            {
                return bytesRead / size; // End of file reached
            }
            currentBlock = theChainBlock[currentBlock];
            offset -= blockSize;
        }
    
        while (bytesToRead > 0)
        {
            readBlock(currentBlock, fileBlock);
            offset = position % blockSize;
            toRead = blockSize - offset;
            if (toRead > bytesToRead)
            {
                toRead = bytesToRead;
            }
            for (uint i = 0; i < toRead; i++)
            {
                buffer[bytesRead+i] = fileBlock[offset+i];
            }
            position += toRead;
            bytesRead += toRead;
            bytesToRead -= toRead;
    
            if (bytesToRead > 0)
            {
                if (theChainBlock[currentBlock] == 1)
                {
                    break; // End of file reached
                }
                currentBlock = theChainBlock[currentBlock];
            }
        }
    
        // Update position in file handle
        fileHandle[2] = byte((position >> 8) & 0xFF);
        fileHandle[3] = byte(position & 0xFF);
    
        return bytesRead / size;
    }
    
    // Sets the file position indicator for the file.
    // fileHandle: The handle of the file.
    // offset: Number of bytes to offset from whence.
    // whence: Position from where offset is applied (0: beginning, 1: current position, 2: end of file).
    // Returns 0 on success, or -1 on error.
    int fSeek(byte[4] fileHandle, int offset, byte whence)
    {
        Diagnostics.Die(0x0A); // TODO
        return -1;
    }
}
