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
    
    
    DefineFunction(string name)
    {
        functionIndex[name] = lastFunction;
        lastFunction++;
    }
    
    // Placeholder methods for code generation
    DefineLocalVar(string tp, string name)
    {
        // TODO : Implement the code generation logic for local variables here
    }

    DefineAssignment(string name)
    {
        // TODO : Implement the code generation logic for assignments here
    }
}
