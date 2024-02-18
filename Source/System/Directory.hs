unit Directory
{
    bool Exists(string path) system;
    Delete(string path) system;
    bool IsValid(directory this) system;
    directory Open(string fullpath) system;
    Create(string path) system;
    uint GetDirectoryCount(directory this) system;
    uint GetDirectoryCount(directory this, ref uint skipped) system;
    uint GetFileCount(directory this) system;
    uint GetFileCount(directory this, ref uint skipped) system;
    string GetDirectory(directory this, uint index) system;
    string GetFile(directory this, uint index) system;
    long GetTime(string path) system;
}
