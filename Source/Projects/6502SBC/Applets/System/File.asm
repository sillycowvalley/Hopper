unit File
{
    // File type constants for Exists() and StartLoad()
    enum FileType
    {
        Any        = 2,  // DirWalkAction.FindFile - any file type
        Executable = 3,  // DirWalkAction.FindExecutable - .PRG files only
    }
    
    // Check if file exists
    // Input:  ZP.STR = pointer to filename (null-terminated, uppercase)
    //         A = FileType.Any or FileType.Executable
    // Output: C set if exists, clear if not found
    // Note:   Filename must be 1-13 chars, alphanumeric only
    Exists()
    {
        LDX # SysCall.FileExists
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Delete file
    // Input:  ZP.STR = pointer to filename (null-terminated, uppercase)
    // Output: C set on success, clear on error
    // Note:   File must exist, directory compaction performed automatically
    Delete()
    {
        LDX # SysCall.FileDelete
        JMP [ZP.BIOSDISPATCH]
    }
    
    // List directory contents
    // Input:  None
    // Output: C set on success (directory printed to serial)
    // Note:   Shows filename, size, type (* = executable)
    Dir()
    {
        LDX # SysCall.FileDir
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Open file for writing
    // Input:  ZP.STR = pointer to filename (null-terminated, uppercase)
    // Output: C set on success, clear on error
    // Note:   Overwrites existing file if present
    //         Must call EndSave() to finalize
    StartSave()
    {
        LDX # SysCall.FileStartSave
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Write data chunk to open file
    // Input:  ZP.FS0/FS1 (SectorSource) = pointer to data
    //         ZP.FS2/FS3 (TransferLength) = number of bytes (16-bit)
    // Output: C set on success, clear if disk full
    // Note:   Call repeatedly to write large files
    //         Maximum 65535 bytes per file
    AppendStream()
    {
        LDX # SysCall.FileAppendStream
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Close and finalize file being saved
    // Input:  A = 0x80 for executable, 0x00 for data file
    // Output: C set on success, clear on error
    // Note:   Must be called after StartSave()/AppendStream()
    //         Writes FAT and directory to EEPROM
    EndSave()
    {
        LDX # SysCall.FileEndSave
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Open file for reading
    // Input:  ZP.STR = pointer to filename (null-terminated, uppercase)
    //         A = FileType.Any or FileType.Executable
    // Output: C set on success, clear if not found
    // Note:   Prepares file for NextStream() calls
    StartLoad()
    {
        LDX # SysCall.FileStartLoad
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Read next chunk from open file
    // Input:  None (file must be open via StartLoad())
    // Output: C set if data available, clear if EOF
    //         ZP.FS2/FS3 (TransferLength) = bytes read (max 256)
    //         Data in FileDataBuffer (0x0600-0x06FF)
    // Note:   Call repeatedly until C clear (EOF)
    NextStream()
    {
        LDX # SysCall.FileNextStream
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Format EEPROM filesystem
    // Input:  None
    // Output: C set on success, clear on error
    // Warning: Erases all files! User confirmation recommended
    Format()
    {
        LDX # SysCall.FileFormat
        JMP [ZP.BIOSDISPATCH]
    }
}