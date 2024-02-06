unit Path
{
    bool IsValidPathCharacter(char c)
    {
        bool valid;
        valid = IsLetterOrDigit(c);
        if (!valid)
        {
            switch (c)
            {
                case '/':
                case '.':
                {
                    valid = true;
                }
            }
        }
        return valid;
    }
    string GetFileName(string path)
    {
        uint iSlash;
        if (path.LastIndexOf('/', ref iSlash))
        {
            path = path.Substring(iSlash+1);
        }
        return path;
    }
    
    string GetDirectoryName(string fullPath)
    {
        uint startPosition;
        uint iLastSlash;
        string resultPath;
        loop
        {
            if ((fullPath.Length == 0) || (fullPath == "/"))
            {
                break;
            }
            startPosition = fullPath.Length - 1;
            if (fullPath[startPosition] == '/')
            {
                startPosition--; // ignore trailing slash
            }
            
            if (!fullPath.LastIndexOf('/', startPosition, ref iLastSlash))
            {
                // there is no slash (except for a possible trailing one) so the entire string is the tail
                break;
            }
            if (iLastSlash < 0)
            {
                break;
            }
            resultPath = fullPath.Substring(0, iLastSlash+1);
            break;
        }
        return resultPath;
    }
    
    string Combine(string partOne, string partTwo)
    {
        uint length;
        string resultPath;
        resultPath = partOne;
        length = resultPath.Length;
        if ((length > 0) && (resultPath[length - 1] == '/'))
        {
            // already has trailing slash
        }
        else
        {
            resultPath = resultPath + '/'; // append trailing slash
        }
        if ((0 != partTwo.Length) && (partTwo[0] == '/'))
        {
            // has leading slash so skip it
            if (partTwo.Length > 1)
            {
                resultPath = resultPath + partTwo.Substring(1);
            }
        }
        else
        {
            resultPath = resultPath + partTwo;
        }
        return resultPath;
    }
    string GetExtension(string path)
    {
        uint iDot;
        uint iSlash;
                
        string extension;
        loop
        {
            extension = "."; // empty "."
            if (path.LastIndexOf('.', ref iDot))
            {
                if (path.LastIndexOf('/', ref iSlash))
                {
                    if (iSlash > iDot)
                    {
                        break; // first '.' is in a folder name (should not be possible in Hopper)
                    }
                }
                extension = path.Substring(iDot);
            }
            break;
        }
        return extension;
    }
    // Returns the full path for a valid file (including correct case)
    string GetFullPath(string path)
    {
        uint fCount;
        uint iFile;
        string dir;
        directory dr;
        string name;
        string correctCaseName;
        string fpath;
        dir = GetDirectoryName(path);
        if (dir == "")
        {
            dir = CurrentDirectory;
        }
        dr = Directory.Open(dir);
        if (dr.IsValid())
        {
            name = Path.GetFileName(path);
            fCount = dr.GetFileCount();
            for (iFile = 0; iFile < fCount; iFile++)
            {
                fpath = dr.GetFile(iFile);
                correctCaseName = Path.GetFileName(fpath);
                if (correctCaseName.ToLower() == name.ToLower())
                {
                    path = fpath;
                    break;
                }
            }
        }
        return path;
    }
    string MakeOptions(string filePath)
    {
        Directory.Create("/Bin/Options/"); // make sure the /Bin/Options directory exists
        filePath = Path.GetFileName(filePath);
        string extension = Path.GetExtension(filePath);
        filePath = filePath.Replace(extension, ".options");
        filePath = Path.Combine("/Bin/Options/", filePath);
        return filePath;
    }
}
