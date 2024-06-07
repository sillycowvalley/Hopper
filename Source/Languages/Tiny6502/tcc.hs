program TCCompile
{
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    
    uses "TinyToken"
    uses "TinyScanner"
    
    uses "/Source/Compiler/Tokens/Parser" // for SetInteractive
    
    bool isExperimental;
    bool IsExperimental { get { return isExperimental; } set { isExperimental = value; } }
    bool isOptimized;
    bool IsOptimized { get { return isOptimized; } set { isOptimized = value; } }
    
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
    
    BadArguments()
    {
        PrintLn("Invalid arguments for TCC:");
        PrintLn("  TCC [args] <preprocessed source file>");    
        PrintLn("    -g <c> <r>  : called from GUI, not console");
        PrintLn("    -x          : use experimental features");
        PrintLn("    -o          : optimizations and less runtime checks");
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
                      case "-o":
                      {
                          isOptimized = true;
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
          
          long startTime = Millis;
          loop
          {
              string extension = Path.GetExtension(projectPath);
              projectPath  = projectPath.Replace(extension, ".tc");
              projectPath = Path.GetFileName(projectPath);
              projectPath = Path.Combine("/Debug/Obj/", projectPath);
              
              PrintLn(projectPath);
              TinyToken.Initialize();
              TinyScanner.Restart(projectPath);
              
              Token token;
              loop
              {
                  token = TinyScanner.Current();    
                  Print(" " + TinyToken.ToString(token.Type) + "('" + token.Lexeme + "')");
                  if (token.Type == TokenType.EOF)
                  {
                      break;
                  }
                  TinyScanner.Advance();
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

