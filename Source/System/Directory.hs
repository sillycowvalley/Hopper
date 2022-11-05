unit Directory
{
    bool Exists(string path) system;
    bool IsValid(directory this) system;
    directory Open(string fullpath) system;
    uint GetDirectoryCount(directory this) system;
    uint GetFileCount(directory this) system;
    string GetDirectory(directory this, uint index) system;
    string GetFile(directory this, uint index) system;
}