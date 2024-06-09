program TCPreprocess
{
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    
    uses "/Source/Compiler/Tokens/Parser" // for SetInteractive
    
    bool isExperimental;
    bool IsExperimental { get { return isExperimental; } set { isExperimental = value; } }
    
    <string, bool>   includesDone;
    <string,bool>    definedSymbols;
    <bool>           ifdefStack;
    
    string projectPath;
    
    bool AddInclude(string path)
    {
        string lowerPath = path.ToLower();
        if (!includesDone.Contains(lowerPath))
        {
            includesDone[lowerPath] = false;
        }
        return !includesDone[lowerPath]; // not included yet?
    }
    IncludeDone(string path)
    {
        string lowerPath = path.ToLower();
        includesDone[lowerPath] = true;
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
                    while (removeLevels != 0)
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
    
    bool isValidPreprocessorSymbol(string symbol)
    {
        // Check if the symbol is empty
        if (symbol.Length == 0)
        {
            return false;
        }
    
        // Check if the first character is a letter or underscore
        char firstChar = symbol[0];
        if (!Char.IsLetter(firstChar) && (firstChar != '_'))
        {
            return false;
        }
    
        // Check the rest of the characters
        for (uint i = 1; i < symbol.Length; i++)
        {
            char c = symbol[i];
            if (!Char.IsLetterOrDigit(c) && (c != '_'))
            {
                return false;
            }
        }
        return true;
    }
    
    bool evaluateSymbol(string sourcePath, uint line, string symbol, ref bool value)
    {
        symbol = symbol.Trim();
        if (symbol.StartsWith("defined("))
        {
            if (!symbol.EndsWith(")"))
            {
                Error(sourcePath, line, "invalid expression: missing closing parenthesis in " + symbol);
                return false;
            }
        
            string symbolName = symbol.Substring(8, symbol.Length - 9).Trim();
            if (!isValidPreprocessorSymbol(symbolName))
            {
                Error(sourcePath, line, "invalid symbol name in #if expression");
                return false;
            }
            value = definedSymbols.Contains(symbolName) && definedSymbols[symbolName];
            return true;
        }
        else
        {
            Error(sourcePath, line, "invalid expression: " + symbol);
            return false;
        }
    }
    
    bool evaluateExpression(string sourcePath, uint line, string expression, ref bool isDefined)
    {
        bool success = true;
        uint index = 0;
    
        loop
        {
            if (!parseExpression(sourcePath, line, expression, ref index, ref isDefined))
            {
                success = false;
                break;
            }
    
            skipWhitespace(expression, ref index);
    
            if (index != expression.Length)
            {
                Error(sourcePath, line, "unexpected characters in #if expression");
                success = false;
                break;
            }
    
            break;
        }
    
        return success;
    }
    
    skipWhitespace(string expression, ref uint index)
    {
        while (index < expression.Length && Char.IsWhitespace(expression[index]))
        {
            index++;
        }
    }
    
    // Parse a factor: "defined(SYMBOL)" | "(" expression ")" | "!" factor
    bool parseFactor(string sourcePath, uint line, string expression, ref uint index, ref bool value)
    {
        skipWhitespace(expression, ref index);
    
        if ((index < expression.Length) && (expression[index] == '!'))
        {
            // "!" factor
            index++;
            bool factorValue = false;
            if (!parseFactor(sourcePath, line, expression, ref index, ref factorValue))
            {
                return false;
            }
            value = !factorValue;
            return true;
        }
        else if ((index < expression.Length) && (expression[index] == '('))
        {
            // "(" expression ")"
            index++;
            if (!parseExpression(sourcePath, line, expression, ref index, ref value))
            {
                return false;
            }
            skipWhitespace(expression, ref index);
            if ((index >= expression.Length) || (expression[index] != ')'))
            {
                Error(sourcePath, line, "mismatched parentheses in #if expression");
                return false;
            }
            index++;
            return true;
        }
        else
        {
            // "defined(SYMBOL)"
            uint startIndex = index;
            bool parenSeen;
            while ((index < expression.Length) && !Char.IsWhitespace(expression[index]) && (expression[index] != '|') && (expression[index] != '&'))
            {
                if (expression[index] == ')')
                {
                    if (parenSeen) { break; }
                    // include the first ')' seen in the symbol
                    parenSeen = true;
                }
                index++;
            }
            string symbol = expression.Substring(startIndex, index - startIndex);
            return evaluateSymbol(sourcePath, line, symbol, ref value);
        }
    }
    
    // Parse a term: factor { "&&" factor }
    bool parseTerm(string sourcePath, uint line, string expression, ref uint index, ref bool value)
    {
        if (!parseFactor(sourcePath, line, expression, ref index, ref value))
        {
            return false;
        }
    
        skipWhitespace(expression, ref index);
    
        while ((index + 1 < expression.Length) && (expression.Substring(index, 2) == "&&"))
        {
            index += 2;
            bool factorValue = false;
            if (!parseFactor(sourcePath, line, expression, ref index, ref factorValue))
            {
                return false;
            }
            value = (value && factorValue);
            skipWhitespace(expression, ref index);
        }
        return true;
    }
    
    // Parse an expression: term { "||" term }
    bool parseExpression(string sourcePath, uint line, string expression, ref uint index, ref bool value)
    {
        if (!parseTerm(sourcePath, line, expression, ref index, ref value))
        {
            return false;
        }
    
        skipWhitespace(expression, ref index);
    
        while ((index + 1 < expression.Length) && (expression.Substring(index, 2) == "||"))
        {
            index += 2;
            bool termValue = false;
            if (!parseTerm(sourcePath, line, expression, ref index, ref termValue))
            {
                return false;
            }
            value = (value || termValue);
            skipWhitespace(expression, ref index);
        }
        return true;
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
        
        bool skipBlock = false; // Indicates if the current block should be skipped
    
        loop
        {
            string content = sourceFile.ReadLine();
            if (!sourceFile.IsValid()) { break; }
    
            string result;
            bool inString;
            char stringChar;
    
            content = content.Replace(Char.Tab, ' ');
    
            // Process comments first
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
    
            // If still in comment, skip the rest of the processing
            if (inComment)
            {
                line++;
                continue;
            }
    
            // Trim leading whitespace after comment processing
            string trimmedContent = result.TrimLeft();
    
            // Process preprocessor directives
            if (trimmedContent.StartsWith("#"))
            {
                // Split the line by whitespace to isolate the directive name
                <string> parts = trimmedContent.Split(' ');
                if (parts.Count != 0)
                {
                    string directiveName = (parts[0]).Substring(1); // Remove the leading '#'
                    string directiveContent = parts.Count > 1 ? trimmedContent.Substring(parts[0].Length).Trim() : "";
    
                    switch (directiveName)
                    {
                        case "include":
                            // Process #include directive
                            {
                                if (skipBlock) { break; }
                                string includePath = directiveContent.Trim();
                                if (!includePath.StartsWith('"') || !includePath.EndsWith('"'))
                                {
                                    Error(sourcePath, line, "syntax error");
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                includePath = includePath.Substring(1, includePath.Length - 2); // trim quotes
                                includePath = ResolvePath(sourcePath, line, includePath);
                                if (includePath.Length == 0)
                                {
                                    success = false;
                                    break; // exit the method loop on error (there was an error resolving the path)
                                }
                                if (AddInclude(includePath))
                                {
                                    // file has not been included yet (only inline once, the first time seen)
    
                                    // inline the include file
                                    if (!processFile(includePath, preFile))
                                    {
                                        success = false;
                                        break; // exit the method loop on error
                                    }
                                }
                            }
    
                        case "define":
                        case "undef":
                            {
                                // Process #define and #undef directives
                                if (skipBlock) { break; }
                                string symbol = directiveContent.Trim();
                                if (!isValidPreprocessorSymbol(symbol))
                                {
                                    Error(sourcePath, line, "invalid preprocessor symbol");
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                definedSymbols[symbol] = (directiveName == "define");
                                if (directiveName == "define")
                                {
                                    if (symbol.StartsWith("CPU_") || symbol.StartsWith("ROM_"))
                                    {
                                        preFile.Append(sourcePath + ":" + line.ToString() + Char.Tab + "#define " + symbol + Char.EOL);
                                    }
                                }
                            }
    
                        case "ifdef":
                            {
                                // Process #ifdef directive
                                string symbol = directiveContent.Trim();
                                if (!isValidPreprocessorSymbol(symbol))
                                {
                                    Error(sourcePath, line, "invalid preprocessor symbol");
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                bool isDefined = definedSymbols.Contains(symbol) && definedSymbols[symbol];
                                ifdefStack.Append(isDefined);
                                skipBlock = !isDefined;
                            }
    
                        case "ifndef":
                            {
                                // Process #ifndef directive
                                string symbol = directiveContent.Trim();
                                if (!isValidPreprocessorSymbol(symbol))
                                {
                                    Error(sourcePath, line, "invalid preprocessor symbol");
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                bool isNotDefined = !definedSymbols.Contains(symbol) || !definedSymbols[symbol];
                                ifdefStack.Append(isNotDefined);
                                skipBlock = !isNotDefined;
                            }
    
                        case "else":
                            {
                                // Process #else directive
                                if (ifdefStack.Count == 0)
                                {
                                    Error(sourcePath, line, "misplaced #else");
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                // Toggle the current block's skip state
                                skipBlock = !skipBlock;
                            }
    
                        case "endif":
                            {
                                // Process #endif directive
                                if (ifdefStack.Count == 0)
                                {
                                    Error(sourcePath, line, "misplaced #endif");
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                ifdefStack.Remove(ifdefStack.Count-1); // Remove the top state
                                // Set the skip state to the state of the current top of the stack (if any)
                                skipBlock = ifdefStack.Count != 0 ? !ifdefStack[ifdefStack.Count-1] : false;
                            }
                            
                        case "if":
                            {
                                // Process #if directive
                                bool isDefined;
                                if (!evaluateExpression(sourcePath, line, directiveContent, ref isDefined))
                                {
                                    success = false;
                                    break; // exit the method loop on error
                                }
                                ifdefStack.Append(isDefined);
                                skipBlock = !isDefined;
                            }
    
                        default:
                            {
                                Error(sourcePath, line, "unknown preprocessor directive: " + directiveName);
                                success = false;
                                break; // exit the method loop on error
                            }
                    }
                }
            }
            else
            {
                if (!inComment && !skipBlock) // Only add line if not within a multi-line comment and not skipping the block
                {
                    result = result.TrimRight();
                    if (result.Length != 0) // Only append if there is content after trimming
                    {
                        preFile.Append(sourcePath + ":" + line.ToString() + Char.Tab + result + Char.EOL);
                    }
                }
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
            _ = AddInclude(projectPath);
            
            success = processFile(projectPath, preFile);
            
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
          projectPath = args[0];
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

