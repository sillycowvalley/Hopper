unit FileSystem
{
    #if !defined(BLOCKFILESYSTEM)
        #define BLOCKFILESYSTEM
    #endif
    
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
    
    byte[descriptorSize] dirEntry; // Global used by readdir
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
    byte findFreePage(byte[] chainBlock)
    {
        for (uint i = 2; i < TotalPages; i++)
        {
            if (chainBlock[i] == 0)
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
            parentDir = fullPath.Substring(0, iSlash-1);
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
    
    // ### end of private helper functions
    
    
    // ### internal c-like methods
    
    
    // ### end of internal c-like methods
    
    
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
    
    
}
