unit TinyCode
{
    file codeFile;
    string codePath;
    
    uint lastGlobal;
    <string,uint> globalIndex;
    <string,string> globalType;
    <string,string> globalValue;
    
    uint lastFunction;
    <string,uint> functionIndex;
    
    
    Initialize(string path)
    {
        string extension = Path.GetExtension(path);
        codePath = path.Replace(extension, ".hs");
        File.Delete(codePath);
        
        string name = Path.GetFileName(path);
        name = name.Replace(extension, "");
        name = name.ToUpper();
        
        codeFile = File.Create(codePath);
        codeFile.Append("program " + name + Char.EOL);
        codeFile.Append("{" + Char.EOL);
        
    }
    Flush()
    {
        codeFile.Append("}" + Char.EOL);
        codeFile.Flush();
    }
    
    
}
