unit TinyCode
{
    uses "TinySymbols"
    
    file codeFile;
    string codePath;
    
    uint lastGlobal;
    <string,uint> globalIndex;
    <string,string> globalType;
    <string,string> globalValue;
    
    uint lastFunction;
    <string,uint> functionIndex;
    
    uint line;
    uint extra;
    
    PadOut(string text, uint delta)
    {
        line++;
        if (text.Length != 0)
        {
            text = ("").Pad(' ', (BlockLevel+extra+delta) * 4) + text;
        }
        codeFile.Append(text + Char.EOL);
    }
    
    Initialize(string path)
    {
        string extension = Path.GetExtension(path);
        codePath = path.Replace(extension, ".asm");
        File.Delete(codePath);
        
        string name = Path.GetFileName(path);
        name = name.Replace(extension, "");
        name = name.ToUpper();
        
        codeFile = File.Create(codePath);
        PadOut("program " + name, 0);
    }
    Append(string line)
    {
        PadOut(line, 0);
    }
    Flush()
    {
        codeFile.Flush();
    }
    
    Function(string functionName)
    {
        TinyCode.PadOut("", 0);
        string name = "|" + functionName + "()";
        name = name.Replace("|main()", "Hopper()").Replace("|","");
        TinyCode.PadOut(name, 0);
    }
    If(string comment)
    {
        PadOut("if (NZ) // " + comment, 0);
    }
    IfExit(string comment)
    {
        PadOut("if (Z) // " + comment, 0);
        PadOut("{", 0);
        PadOut("break;", 1);
        PadOut("}", 0);
    }
    Else()
    {
        PadOut("else", 0);
    }
    Loop(string comment)
    {
        PadOut("loop // " + comment, 0);
        TinyConstant.EnterBlock();
        TinySymbols.EnterBlock();
    }
    EndLoop(string comment)
    {
        TinySymbols.LeaveBlock(comment);
        TinyConstant.LeaveBlock();
    }
    Break(string comment)
    {
        if (comment.Length != 0)
        {
            PadOut("break; // " + comment, 0);
        }
        else
        {
            PadOut("break;", 0);
        }
    }
    Continue()
    {
        PadOut("continue;", 0);
    }
}
