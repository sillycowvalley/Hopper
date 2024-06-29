program TCCompile
{
    // Serial EEPROM page size needs to be defined in /Source/Runtime/6502/Devices/SerialEEPROM, defaults to 128 bytes for 24AA512 (64K) and 24AA1026 (128K)
   
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    
    uses "TCToken"
    uses "TCScanner"
    uses "TCCompile"
    
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
        PrintLn("    -z          : use Zero Page for global variables");
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
                      case "-z":
                      {
                          ZeroPageGlobals = true;
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
              if (!projectPath.Contains('.'))
              {
                  projectPath = projectPath + ".tc";
              }
              string extension = Path.GetExtension(projectPath);
              projectPath = projectPath.Replace(extension, ".tc");
              projectPath = Path.GetFileName(projectPath);
              projectPath = Path.Combine("/Debug/Obj/", projectPath);

              // First Pass:
              TCToken.Initialize();
              FirstPass = true;     
              TCScanner.Restart(projectPath);
              TCCode.Initialize(projectPath);
              if (!TCCompile.Compile())
              {
                  break;
              }
              
              PrintLn(); // TODO REMOVE
              
              // Second Pass:
              FirstPass = false;
              Compiling = true;
              TCCode.Initialize(projectPath);
              TCSymbols.Reset();
              TCCompile.Reset(); // globalDefinitions
              TCGen.Initialize();
              TCScanner.Restart(projectPath);
              if (!TCCompile.Compile())
              {
                  break;
              }
              TCCode.Flush();
              
              if (false && IsExperimental)
              {
                  TCGen.EmitTrips();
                  TCGen.EmitPairs();
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

