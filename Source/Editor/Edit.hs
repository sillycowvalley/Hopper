program Edit
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Editor/Panel"
    uses "/Source/Editor/MenuBar"
    uses "/Source/Editor/StatusBar"
    uses "/Source/Editor/Editor"
    uses "/Source/Editor/Commands"
    uses "/Source/Compiler/Tokens/Parser" // SetInteractive
    
    string optionsPath;
    string OptionsPath { get { return optionsPath; } }
    
    {
        <string> arguments = System.Arguments;
        
        string filePath;
        bool showHelp;
        
        Editor.Locate(0, 0, Screen.Columns, Screen.Rows);
        Parser.SetInteractive(Editor.Left+1, Editor.Top + Editor.Height-1);
        
        foreach (var argument in arguments)
        {
            if (filePath.Length > 0)
            {
                showHelp = true;
                break;
            }
            else
            {
                filePath = argument;
            }
        }
        
        if (filePath.Length == 0)
        {
            showHelp = true;
        }

        loop
        {
            bool doNew = false;
            if (!showHelp)
            {
                loop
                {
                    // check the file
                    string fullPath;
                    if (File.Exists(filePath))
                    {
                        break;
                    }
                    fullPath = Path.Combine(System.CurrentDirectory, filePath);
                    if (File.Exists(fullPath))
                    {
                        filePath = fullPath;
                        break;
                    }
                    string extension = Path.GetExtension(filePath);
                    if (extension == ".")
                    {
                        string filePathExt = filePath + ".hs";
                        if (File.Exists(filePathExt))
                        {
                            filePath = filePathExt;
                            break;
                        }
                        string fullPathExt = Path.Combine(System.CurrentDirectory, filePathExt);
                        if (File.Exists(fullPathExt))
                        {
                            filePath = fullPathExt;
                            break;
                        }
                    }
                    if (!File.Exists(fullPath))
                    {
                        if (!filePath.Contains('/'))
                        {
                            filePath = fullPath;
                        }
                        doNew = true;
                    }
                    break;
                } // loop
            }
            if (showHelp)
            {
                PrintLn("EDIT <filepath>");
                break;
            }
            
            optionsPath = Path.MakeOptions(filePath);
            
            Screen.Clear();
            Commands.Initialize();
           
            <string, variant> menubar   = MenuBar.New(); 
            <string, variant> statusbar = StatusBar.New(); 
            
            Editor.New(statusbar, menubar); 

            if (!doNew)
            {            
                Editor.LoadFile(filePath);
            }
            
            MenuBar.Draw(menubar);
            StatusBar.Draw(statusbar);
            Editor.Draw();

            if (doNew)
            {
                Editor.SetNewPath(filePath);
                Commands.Execute("New");
                if (!File.Exists(Editor.GetCurrentPath()))
                {
                    ExitCommand.Execute();
                    break;
                }
            }
            
            loop
            {
                Key key = ReadKey();
               
                // offer the key to the menus
                if (MenuBar.OnKey(menubar, key))
                {
                    // consumed by menus
                }
                else if (StatusBar.OnKey(statusbar, key))
                {
                    // consumed by status bar
                }
                else
                {
                    string commandName = Commands.KeyToCommand(key);
                    if (commandName.Length > 0) // checks IsEnabled too
                    {
                        Commands.Execute(commandName);
                    }
                    else if (Editor.OnKey(key)) // to Editor last since it is greedy about eating keys
                    {
                        // consumed by editor
                    }
                }
                if (ExitCommand.IsExiting())
                {
                    break;
                }
            } // loop
            break;
        } // loop
    }
}
