unit File
{
    
    bool Exists(string path) system;
    Delete(string path) system;
    bool IsValid(file this) system;
    file Open(string fullpath) system;
    file Create(string fullpath) system;
    string ReadLine(file this) system;
    byte Read(file this) system;
    byte Read(file this, long seekPosition) system;
    Append(file this, byte content) system;
    Append(file this, string content) system;
    Flush(file this) system;
    long GetSize(string path) system;
    
}
