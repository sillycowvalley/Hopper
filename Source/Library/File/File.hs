unit File
{
    
    uses "/Source/Library/File/SerialEEPROM"
    uses "/Source/Library/File/BlockFileSystem"
    
    bool Exists(string path) system;
    Delete(string path) system;
    long GetSize(string path) system;
        
    bool IsValid(file this) system;
    
    file Open(string fullpath) system;
    string ReadLine(file this) system;
    byte Read(file this) system;
    byte Read(file this, long seekPosition) system;
    uint Read(file this, byte[] data, uint bufferSize) library;
    
    file Create(string fullpath) system;
    Append(file this, byte content) system;
    Append(file this, string content) system;
    Flush(file this) system;
    
}
