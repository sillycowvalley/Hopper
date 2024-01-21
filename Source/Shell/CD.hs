program CD
{

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    {
        <string> arguments = Arguments;
        loop
        {
            bool found = false;
            if (arguments.Length != 1) 
            {
                // zero arguments or spaces between more than one part
                PrintLn("Invalid arguments for CD.");
                break;
            }
            string path = arguments[0];
            if (!path.EndsWith('/'))
            {
                path = path + '/';
            }
            
            if (path == "./")
            {
                break;
            }
            if (path == "../")
            {
                string upPath = CurrentDirectory;
                upPath = Path.GetDirectoryName(upPath);
                if (upPath.Length == 0)
                {
                    break;
                }
                else if (Directory.Exists(upPath))
                {
                    path = upPath;
                    found = true;
                }
                else
                {
                    break; // do nothing?
                }
            }
            if (!found && !path.StartsWith('/'))
            {
                string fullPath = Path.Combine(CurrentDirectory, path);
                if (Directory.Exists(fullPath))
                {
                    path = fullPath;
                    found = true;
                }
            }
            if (!found && Directory.Exists(path))
            {
                found = true;
            }
            if (found)
            {
                CurrentDirectory = path;
            }
            else
            {
                PrintLn("Invalid arguments for CD."); // System.Beep();
            }
            break;
        }
        
    }
}
