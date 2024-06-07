program TCPreprocess
{
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/Compiler/Tokens/Token"
    
    uses "/Source/Compiler/Tokens/Parser" // for SetInteractive
    
    bool isExperimental;
    bool IsExperimental { get { return isExperimental; } set { isExperimental = value; } }
    
    <string, bool> includesParsed;
    <string> includesQueue;
    <string,bool> definedSymbols;
    
    AddInclude(string path)
    {
        string lowerPath = path.ToLower();
        if (!includesParsed.Contains(lowerPath))
        {
            includesQueue.Append(lowerPath);
            includesParsed[lowerPath] = false;
        }
    }
    IncludeDone(string path)
    {
        string lowerPath = path.ToLower();
        if (includesQueue[0] != lowerPath)
        {
            Die(0x0B);
        }
        includesQueue.Remove(0);
        includesParsed[lowerPath] = true;
    }
    
    Error(string path, uint line, string message)
    {
        string errorMessage = "[" + path;
        if (line != 0)
        {
            errorMessage = errorMessage + ":" + line.ToString();
        }
        // ,col
        errorMessage += "]";
        errorMessage += " ";
        errorMessage += message;
        Parser.EmitError(errorMessage);
    }
    
    string ResolvePath(string sourcePath, uint line, string includePath)
    {
        loop
        {
            string includePathLower = includePath.ToLower();
            
            string includeExtension = Path.GetExtension(includePathLower);
            if (includeExtension == ".")
            {
                includeExtension = ".tc";
                includePath = includePath + includeExtension;
            }
            if (!File.Exists(includePath))
            {
                string tryFile = includePath;
                uint removeLevels = 0;
                if (tryFile.StartsWith("./"))
                {
                    tryFile = tryFile.Substring(2);
                }
                while (tryFile.StartsWith("../"))
                {
                    tryFile = tryFile.Substring(3);
                    removeLevels++;
                }
                
                if (!tryFile.StartsWith("/"))
                {
                    // first try relative to current source file:
                    string currentDirectory = Path.GetDirectoryName(sourcePath);
                    while (removeLevels > 0)
                    {
                        currentDirectory = Path.GetDirectoryName(currentDirectory);
                        removeLevels--;
                    }
                    string tryPath = Path.Combine(currentDirectory, tryFile);
                    if (File.Exists(tryPath))
                    {
                        includePath = tryPath;
                    }
                    else
                    {
                        // then try relative to main project file
                        string projectDirectory = Path.GetDirectoryName(projectPath);
                        tryPath = Path.Combine(projectDirectory, tryFile);
                        if (File.Exists(tryPath))
                        {
                            includePath = tryPath;
                        }
                    }
                }
            }
            if (!File.Exists(includePath))
            {
                Error(sourcePath, line, "'" + includePath + "' not found");
                String.Build(ref includePath);
                break;
            }
            else
            {
                // relative path from '/'? add '/' to be canonical (and avoid multiple references to the same unit)
                if (!includePath.StartsWith('/') && (CurrentDirectory == "/"))
                {
                    includePath = "/" + includePath;
                }
            }
            
            includePathLower = includePath.ToLower();
            if (!includesParsed.Contains(includePathLower))
            {
                includesParsed[includePathLower] = false; // false means we're aware of it but we haven't parsed it yet
            }
            break;
        }
        return includePath;
    }
    BadArguments()
    {
        PrintLn("Invalid arguments for TCPP:");
        PrintLn("  TCPP [args] <source file>");    
        PrintLn("    -g <c> <r>  : called from GUI, not console");
        PrintLn("    -x          : use experimental features");
        PrintLn("    -d <symbol> : define conditional compilation symbols");
    }
    
    bool processFile(string sourcePath, file preFile)
    {
        bool success;
        Parser.ProgressTick("p"); // preprocess
    
        file sourceFile = File.Open(sourcePath);
        if (!sourceFile.IsValid())
        {
            Error(sourcePath, 0, "failed to open '" + sourcePath + "'");
            return success;
        }
        IncludeDone(sourcePath);
        
        success = true;
        uint line = 1;
        bool inComment;
        
        loop
        {
            string content = sourceFile.ReadLine();
            if (!sourceFile.IsValid()) { break; }
            
            string result;
            bool inString;
            char stringChar;
            
            for (uint i = 0; i < content.Length; i++)
            {
                char c = content[i];
                
                if (inString)
                {
                    result += c;
                    if (c == stringChar)
                    {
                        inString = false;
                    }
                    else if ((c == '\\') && (i + 1 < content.Length) && (content[i + 1] == stringChar))
                    {
                        i++; result += content[i]; // skip escaped quote
                    }
                }
                else if (inComment)
                {
                    if ((i + 1 < content.Length) && (c == '*') && (content[i + 1] == '/'))
                    {
                        inComment = false;
                        i++; // skip */
                    }
                }
                else if ((c == '"') || (c == '\''))
                {
                    inString = true;
                    stringChar = c;
                    result += c;
                }
                else if ((i + 1 < content.Length) && (c == '/') && (content[i + 1] == '/'))
                {
                    break; // stop processing rest of the line
                }
                else if ((i + 1 < content.Length) && (c == '/') && (content[i + 1] == '*'))
                {
                    inComment = true;
                    i++; // skip /*
                }
                else
                {
                    result += c;
                }
            }
    
            if (!inComment) // Only add line if not within a multi-line comment
            {
                result = result.TrimRight();
                preFile.Append(sourcePath + ":" + line.ToString() + Char.Tab + result + Char.EOL);
            }
            line++;
        }
    
        return success;
    }
    
    
    
    bool preProcess(string projectPath, string prePath)
    {
        bool success;
        loop
        {
            file preFile = File.Create(prePath);
            if (!preFile.IsValid())
            {
                Parser.EmitError("failed to create '" + prePath + "'");
                break;
            }
            AddInclude(projectPath);
            
            success = true;
            while (includesQueue.Count != 0)
            {
                if (!processFile(includesQueue[0], preFile))
                {
                    success = false;
                    break;
                }
            }
            
            preFile.Flush();
            break;
        }
        return success;
    }
    
    Hopper()
    {
        bool success = false;
        loop
        {
          <string> rawArgs = System.Arguments;
          <string> args;
          
          for (uint iArg = 0; iArg < rawArgs.Count; iArg++)
          {
              string arg = rawArgs[iArg];
              if ((arg.Length == 2) && (arg[0] == '-'))
              {
                  arg = arg.ToLower();
                  switch (arg)
                  {
                      case "-g":
                      {
                          uint col;
                          uint row;
                          iArg++;
                          if (UInt.TryParse(rawArgs[iArg], ref col))
                          {
                          }
                          iArg++;
                          if (UInt.TryParse(rawArgs[iArg], ref row))
                          {
                          }
                          Parser.SetInteractive(byte(col), byte(row));
                      }
                      case "-d":
                      {
                          iArg++;
                          definedSymbols[rawArgs[iArg]] = true;
                      }
                      case "-x":
                      {
                          isExperimental = true;   
                      }
                      default:
                      {
                          args.Clear();
                          break;
                      }
                  }
              }
              else
              {
                  args.Append(arg);
              }
          }
          
          if (args.Count != 1)
          {
              BadArguments();
              break;
          }
          bool sourceFound;
          string projectPath = args[0];
          string ext = ".tc";
          string codePath = args[0];
          
          <string> sourceFolders;
          sourceFolders.Append(System.CurrentDirectory);
          sourceFolders.Append("/Source/Languages/Tiny6502");
          sourceFolders.Append("/Source/Languages/Tiny6502/Samples");
          sourceFolders.Append("/Source/Languages/Tiny6502/Testing");
          foreach (var sourceFolder in sourceFolders)
          {
              if (File.Exists(ref projectPath, ref ext, sourceFolder))
              {
                  sourceFound = true;
                  break;
              }
          }
          if (!sourceFound)
          {
              BadArguments();
              break;
          }
          projectPath = Path.GetFullPath(projectPath);
          
          long startTime = Millis;
          loop
          {
              string extension = Path.GetExtension(projectPath);
              string prePath  = projectPath.Replace(extension, ".tc");
              prePath = Path.GetFileName(prePath);
              prePath = Path.Combine("/Debug/Obj/", prePath);
              if (File.Exists(prePath))
              {
                  // delete previous so no output on error
                  File.Delete(prePath); 
              }
              
              Token.InitializeTinyC();
              
              if (!preProcess(projectPath, prePath))
              {
                 // error!
                 break;
              }
              
              if (!Parser.IsInteractive())
              {
                  Print("Success. ", Colour.ProgressText, Colour.ProgressFace);
                  long elapsedTime = Millis - startTime;
                  float seconds = elapsedTime / 1000.0;
                  PrintLn("  " +seconds.ToString() + "s", Colour.ProgressHighlight, Colour.ProgressFace); 
              }
              else
              {
                  Parser.ProgressDone();
              }
              success = true;
              break;
          } // main loop
          break;
        } // arguments loop
        if (!success)
        {
            Diagnostics.SetError(0x0E);
        }
      
    }
}

