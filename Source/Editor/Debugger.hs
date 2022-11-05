unit Debugger
{
    uses "/Source/System/Diagnostics"
    uses "/Source/Editor/Editor"
    uses "/Source/Compiler/JSON/Code"
    uses "/Source/Compiler/Tokens/Parser"
        
    InitializeSymbols()
    {
        
        string path = Editor.GetProjectPath();
        string fileName = Path.GetFileName(path);
        string extension = Path.GetExtension(fileName);
        extension = extension.ToLower();
        if (extension == ".hs")
        {
            fileName = fileName.Replace(extension, "");
            string codePath = "/Debug/Obj/" + fileName + ".code";
            if (File.Exists(codePath))
            {
                Parser.SetInteractive(true);
                Editor.SetStatusBarText("Loading '" + codePath + "' symbols");
                if (ParseCode(codePath, false, true))
                {
                    Editor.SetStatusBarText("");
                }
            }
        }
    }
}
