unit Path
{
    bool IsValidPathCharacter(char c)
    {
        bool valid = IsLetterOrDigit(c);
        if (!valid)
        {
            switch (c)
            {
                case '/':
                {
                    valid = true;
                }
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
        string resultPath;
        loop
        {
            if ((0 == fullPath.Length) || (fullPath == "/"))
            {
                break;
            }
            uint startPosition = fullPath.Length - 1;
            if (fullPath[startPosition] == '/')
            {
                startPosition--; // ignore trailing slash
            }
            
            uint iLastSlash;
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
        string resultPath = partOne;
        uint length = resultPath.Length;
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
        string extension;
        loop
        {
            extension = "."; // empty "."
            uint iDot;
            if (path.LastIndexOf('.', ref iDot))
            {
                uint iSlash;
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
        string dir = GetDirectoryName(path);
        if (dir == "")
        {
            dir = CurrentDirectory;
        }
        directory dr = Directory.Open(dir);
        if (dr.IsValid())
        {
            string name = Path.GetFileName(path);
            uint fCount = dr.GetFileCount();
            for (uint iFile = 0; iFile < fCount; iFile++)
            {
                string fpath = dr.GetFile(iFile);
                string correctCaseName = Path.GetFileName(fpath);
                if (correctCaseName.ToLower() == name.ToLower())
                {
                    path = fpath;
                    break;
                }
            }
        }
        return path;
    }
}
