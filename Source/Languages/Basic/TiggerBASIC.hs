program TiggerBASIC
{
    // User knobs and dials:
    //#define TERSE      // errors are numbers only, no text messages
    //#define CHECKED    // runtime checks like stack overflow/underflow and division by zero (also <ctrl><X> functionality)
    #define OPTIMIZER    // peephole optimizer for the byte code generator (slower compile times, smaller faster code)
    #define DYNAMICSTACK // <uint> vs [uint] for GOSUB call stack (use dynamic/list if you don't need extreme GOSUB call depth)
    
    // Development only:
    //#define DEBUG        // verbose listings showing the IL code, internal system checks
    
    #define MCU            // running on a microcontroller (CALLI vs CALL for example)
    #define SERIAL_CONSOLE  // no Keyboard
    //#define USELCD
    
    uses "/Source/System/System"    
#ifdef MCU    
    uses "/Source/Library/Boards/PiPicoW"
    uses "/Source/Library/Screen"
#else
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
#endif
    
    uses "/Source/System/IO"         // write either to Screen or Screen and Serial, read from Keyboard or Keyboard and Serial
    
    uses "Errors"
    
    uses "HopperCode"  // code related to Hopper VM byte code
    uses "Memory"      // support for TOP, ?160, and the PEEK and POKE functionality (! and ?)
    uses "Platform"    // helper methods called from the VM byte code
    
    uses "Source"      // source code lines
    uses "Expression"  // recursive descent expression parser
    uses "Tokenizer"   // the rest of the parser

    bool Debug // Hopper optimizer does constant folding and dead code removal using this:
    {
        get
        {
#ifdef DEBUG 
            return true;
#else
            return false;
#endif
        }
    }

    getInputLine(ref string currentLine)
    {
        String.Build(ref currentLine);
        uint clength = 0;
        bool refresh = true;
        Write('>');
        loop
        {
            char ch = IO.Read();
            if (ch == char(0x0D))
            {
                break;
            }
            else if (ch == char(0x1B))
            {
                // clear the current line
                foreach (var c in currentLine)
                {
                    Write(char(0x08));
                    Write(' ');
                    Write(char(0x08));
                }
                String.Build(ref currentLine);
                clength = 0;
            }
            else if (ch == char(0x08))
            {
                if (clength != 0)
                {
                    currentLine = currentLine.Substring(0, clength-1);
                    clength--;
                    // backspace
                    Write(char(0x08));
                    Write(' ');
                    Write(char(0x08));
                }
            }
            else // alphanumeric
            {
                if (clength < IO.LineMax)
                {
                    String.Build(ref currentLine, ch);
                    clength++;
                    Write(ch);
                }
            } // alphanumeric
            
        } // loop
        WriteLn();
        if (clength != 0)
        {
            if ((currentLine[0] == ' ') || (currentLine[clength-1] == ' '))
            {
                currentLine = currentLine.Trim();
            }
        }
    }
    
    bool tryParseLineNumber(string content, ref uint lineNumber)
    {
        // variation on UInt.TryParse(..) that does a lot less (faster, no need for 'long')
        uint length;
        byte b;
        bool success;
        loop
        {
            lineNumber = 0;
            length = content.Length;
            if (length == 0)
            {
                break;
            }
            if (length < 5) // 0..9999
            {
                success = true;
                for (uint i=0; i < length; i++)
                {
                    b = byte(content[i]);
                    lineNumber = lineNumber * 10;
                    if (b < 48)
                    {
                        success = false;
                        break;
                    }
                    b = b - 48; // 48 is ASCII for '0'
                    if (b > 9)
                    {
                        success = false;
                        break;
                    }
                    lineNumber = lineNumber + b; 
                }
            }
            break;
        } // loop
        if (success && ((lineNumber == 0) || (lineNumber > Source.LineLimit)) )
        {
            Error(2); // Bad line number
            success = false;
        }
        return success;
    }
    
    welcome()
    {
        IO.Clear();
        WriteLn("Tigger BASIC (GIBL variant running on the Hopper VM)");
        HopperCode.New();
        Source.Clear();
        WriteLn("Ok");
    }
    
    execute(uint lineNumber)
    {
        HopperCode.Run(lineNumber);
        if (Condition == Conditions.Error)
        {
            // Error(..) already emitted the error
        }
        else  if (Condition == Conditions.Break)
        {
            WriteLn(); WriteLn("BREAK");
        }
    }
    
    compile() // copy of run(..) below
    {
        // reset variable values
        Platform.Clear();
        if (CurrentCodeSize == 0)
        {
            Tokenize();
            if (Condition != Conditions.None)
            {
#ifndef DEBUG
                HopperCode.Clear(); // discard current incompletely tokenized program
#endif
                return;
            }
        }
    }
    run()
    {
        // reset variable values
        Platform.Clear();
        if (CurrentCodeSize == 0)
        {
            Tokenize();
            if (Condition != Conditions.None)
            {
                HopperCode.Clear(); // discard current incompletely tokenized program
                return;
            }
        }
        // start from the first valid line after line '0' 
#ifdef MCU        
        LED = true;
#endif
        execute(Source.GetNextLine(0));
#ifdef MCU
        LED = false;
#endif
    }
    
    bool validateName(ref string name, bool mustExist)
    {
        foreach (var c in name)
        {
            if (!Path.IsValidPathCharacter(c))
            {
                return false;
            }
        }
        if (!name.Contains('.'))
        {
            name = name + ".BAS";
        }
        string extension = Path.GetExtension(name);
        if (extension != ".BAS")
        {
            return false;
        }
        if (mustExist)
        {
            if (!File.Exists(name))
            {
                string path = Path.Combine(CurrentDirectory, name);
                if (!File.Exists(path))
                {
                    path = Path.Combine("/BASIC/", name);
                    if (!File.Exists(path))
                    {
                        return false;
                    }
                }    
                name = path;
            }
        }
        else
        {
            string folder = Path.GetDirectoryName(name);
            if (folder.Length == 0)
            {
                name = Path.Combine("/BASIC/", name);           
            }
        }
        return true;
    }
    dir()
    {
        directory dir = Directory.Open("/BASIC/");
        if (!dir.IsValid()) { return; }
        uint files = dir.GetFileCount();
        for (uint i = 0; i < files; i++)
        {
            string path = (dir.GetFile(i)).ToUpper();
            if (Path.GetExtension(path) == ".BAS")
            {
                WriteLn(path);
            } 
        }
    }
    load(string name)
    {
        if (validateName(ref name, true))
        {
            Source.Clear();   // NEW
            Platform.Clear(); // CLEAR
            Write("Loading " + name);
            file textFile = File.Open(name);
            if (!textFile.IsValid())
            {
                WriteLn();
                Write("  Error attempting to open file '" + name +"'");
            }
            else
            {
                loop
                {
                    string ln = textFile.ReadLine();
                    if (ln.Length == 0)
                    {
                        if (!textFile.IsValid())
                        {
                            break;
                        }
                    }
                    processLine(ln);
                }
            }
            WriteLn();
        }
        else
        {
            WriteLn("'" + name + "' invalid.");
        }
    }
    save(string name)
    {
        if (validateName(ref name, false))
        {
            Write("Saving " + name);       
            WriteLn();
        }
        else
        {
            WriteLn("'" + name + "' invalid.");
        }
    }
    immediate(string inputLine)
    {
        loop
        {
            if (CurrentCodeSize == 0) // not compiled
            {
                compile();
                if (Condition != Conditions.None) 
                { 
                    break; 
                }
            }
            Source.Add(10000,"END"); // so the real program doesn't run into the immediate line
            Source.Add(10001, inputLine);
            Source.Add(10002, "END");
            TokenizeImmediate();
            if (Condition != Conditions.None) 
            { 
                break; 
            }
            execute(10001);
            break;
        } // loop
    }
    
    listing()
    {
        for (uint i=1; i <= Source.LineLimit; i++)
        {
            if (Source.LineExists(i))
            {
                string content = i.ToString() + " " + Source.GetLine(i);
                WriteLn(content);
#ifndef MCU
                Diagnostics.OutputDebug(content);
#endif
#ifdef DEBUG
                HopperCode.Disassemble(i);
#endif
            }
            if (i >= Source.LastLine)
            {
                break;
            }
        }

#ifdef DEBUG        
        if (Source.LineExists(10000))
        {
            WriteLn(Source.GetLine(10000));
            HopperCode.Disassemble(10000);
            WriteLn(Source.GetLine(10001));
            HopperCode.Disassemble(10001);
            WriteLn(Source.GetLine(10002));
            HopperCode.Disassemble(10002);
        }   
#endif        
    }
    processLine(string inputLine)
    {
        inputLine = inputLine.Trim();
        switch (inputLine.ToUpper())
        {
            case "NEW": { Source.Clear();                  }
            default:
            {
                loop
                {
                    string numberString;
                    uint nCount;
                    uint lineNumber;
                    loop
                    {
                        // find the first non-digit in the line
                        if (nCount == inputLine.Length)
                        {
                            break;
                        }
                        char ch = inputLine[nCount];
                        if (!ch.IsDigit())
                        {
                            break;    
                        }
                        nCount++;
                    }
                    if (nCount != 0)
                    {
                        numberString = inputLine.Substring(0, nCount);
                        inputLine = inputLine.Substring(nCount);
                    }
                    inputLine = inputLine.Trim();
                    if (tryParseLineNumber(numberString, ref lineNumber))
                    {
                        // good line number, could be blank line
                        Source.Add(lineNumber, inputLine); // calls HopperCode.Clear()
                        break;
                    }
                    if ((Condition == Conditions.None) && (inputLine.Length != 0))
                    {
                        immediate(inputLine);
                    }
                    break;
                } // loop
            } // default
        } // switch
    }
    
    bool execute(ref string inputLine)
    {
        
        Condition = Conditions.None;
        inputLine = inputLine.Trim();
        loop
        {
            string command = inputLine.ToUpper();
            string arguments;
            uint iSpace;
            if (command.IndexOf(' ', ref iSpace))
            {
                arguments = (command.Substring(iSpace)).Trim();
                command = command.Substring(0, iSpace);
            }
            switch (command)
            {
                case "BYE":     { return true;                     } // exit
                case "CLEAR":   { Platform.Clear();                } // reset variables in Platform
                case "RUN":     { run();                           }
                case "LIST":    { listing();                       }
                case "COMPILE": { compile();                       } // tokenize without executing
                case "BRON":    { HopperCode.BreakCheck(true);     }
                case "BROFF":   { HopperCode.BreakCheck(false);    }
                case "LOAD":    { load(arguments);                 }
                case "SAVE":    { save(arguments);                 }
                case "DIR":     { dir();                           }
                
                //case "NEW":
                default:
                {
                    processLine(inputLine);
                    break;
                } // default
            } // switch
            break;
        } // loop 
        return false; // next command
    }
    
    bool DisplayILI9341xPiPico28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(17, 21);
        ConfigureReset(20);
        DisplayState result = Graphics.Begin();
        return (result == DisplayState.OK);
    }
    
    {
#ifdef USELCD        
        IO.EchoToLCD = true;
        if (!DisplayILI9341xPiPico28())
        {
            WriteLn("Failed to initialize Display");
            return;
        }
        //Graphics.FlipDisplay(true);
        //Graphics.InvertDisplay(false);
#endif
        
#ifdef MCU
        LED = false;
#else
        Memory.Initialize(0x1000, 0xEF00); // fraction of the 64K data segment to use for our heap
#endif
        HopperCode.BreakCheck(false); // BROFF by default
        welcome();
        string inputLine;
        loop
        {
            getInputLine(ref inputLine);
            if (inputLine.Length != 0)
            {
                if (execute(ref inputLine))
                {
                    break;
                }
            }
        }
    }
}

