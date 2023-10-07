program Debug
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Runtime"
    
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/JSON/Code"
    uses "/Source/Compiler/CodeGen/Instructions"
    
    <byte> code;
    
    string VariantToString(variant stackVariant)
    {
        string variantValue;
        switch (typeof(stackVariant))
        {
            case string:
            {
                variantValue = stackVariant;
                variantValue = '"' + variantValue + '"';
            }
            default:
            {
                variantValue = "[reference]";
            }
        }
        return variantValue;
    }
    string ValueToString(uint stackValue, type valueType)
    {
        string valueValue;
        switch (valueType)
        {
            case char:
            {
                valueValue = "'" + char(stackValue) + "'";
            }
            default:
            {
                valueValue = stackValue.ToString();
            }
        }
        return valueValue;
    }
    DumpValueStack()
    {
        PrintLn("Value Stack:");
        uint bp = Runtime.BP;
        uint sp = Runtime.SP;
        //PrintLn("  BP=" + bp.ToString());
        //PrintLn("  SP=" + sp.ToString());
        while (sp != bp)
        {
            sp = sp - 2;
            type stackType = Runtime.GetStackType(sp);
            string stackLine = "0x" + sp.ToHexString(4);
            if (IsValueType(stackType.ToString()))
            {
                uint stackWord = Runtime.GetStackWord(sp);
                string valueValue = ValueToString(stackWord, stackType);
                stackLine = " " + valueValue + " (" + stackType.ToString() + ")";
            }
            else
            {
                variant stackVariant = Runtime.GetStackVariant(sp);
                string variantValue = VariantToString(stackVariant);
                stackLine = " " + variantValue + " (" + stackType.ToString() + ")";
            }
            uint offset = sp - bp;
            stackLine = "(BP+" + offset.ToString() + ")" + stackLine;
            PrintLn(stackLine);
        }
    }
    DumpCallStack()
    {
        uint csp = Runtime.CSP;
        <uint> locations;
        <uint> bps;
        locations.Append(Runtime.PC);
        bps.Append(Runtime.BP);
        loop
        {
            if (csp == 0)
            {
                break;
            }
            csp--; // Pop BP
            uint bp = Runtime.GetCallStackWord(csp);
            if (csp == 0)
            {
                break;
            }
            csp--; // return address
            uint address = Runtime.GetCallStackWord(csp);
            if (address == 0)
            {
                break;
            }
            locations.Append(address);
            bps.Append(bp);
        }
        uint sp = Runtime.SP;
        
        uint pc = Runtime.PC;
        
        string sourceIndex = GetSourceIndex(pc);
        
        PrintLn("Call Stack:");
        uint ibp = 0;
        foreach (var location in locations)
        {
            uint bp = bps[ibp];
            ibp++;
            <string> argumentLines;
            uint methodIndex = Code.LocationToIndex(location);
            string stackLine = "0x" + location.ToHexString(4) + " 0x" + methodIndex.ToHexString(4);
            
            <string,variant> methodSymbols = Code.GetMethodSymbols("0x" + methodIndex.ToHexString(4));
            string methodName = methodSymbols["name"];
            String.Build(ref stackLine, "  " + methodName + "(");
            if (methodSymbols.Contains("arguments"))
            {
                <string,string> arguments = methodSymbols["arguments"];
                bool first = true;
                foreach (var kv in arguments)
                {
                    if (!first)
                    {
                        String.Build(ref stackLine, ", ");
                    }
                    
                    int bpOffset;
                    //PrintLn(kv.key);
                    if (Token.TryParseInt(kv.key, ref bpOffset))
                    {
                        uint stackAddress = uint(long(bp) + bpOffset);
                        
                        type stackType = Runtime.GetStackType(stackAddress);
                        if (IsValueType(stackType.ToString()))
                        {
                            uint stackWord = Runtime.GetStackWord(stackAddress);
                            string valueValue = ValueToString(stackWord, stackType);
                            argumentLines.Append(kv.value + "=" + valueValue + " (" + stackType.ToString() + ")");
                        }
                        else
                        {
                            variant stackVariant = Runtime.GetStackVariant(stackAddress);
                            string variantValue = VariantToString(stackVariant);
                            argumentLines.Append(kv.value + "=" + variantValue + " (" + stackType.ToString() + ")");
                        }
                            
                    }
                
                    String.Build(ref stackLine, kv.value);
                    first = false;
                }
            }
            String.Build(ref stackLine, "), ");
            
            if (sourceIndex.Length > 0)
            {
                String.Build(ref stackLine, sourceIndex);
                sourceIndex = "";
            }
            else
            {
                string src = methodSymbols["source"];
                String.Build(ref stackLine, src);
                String.Build(ref stackLine, ':');
                string ln = methodSymbols["line"];
                String.Build(ref stackLine, ln);
            }
            
            
            PrintLn(stackLine);
        
            if (argumentLines.Length > 0)
            {
                foreach (var ln in argumentLines)
                {
                    PrintLn("  " + ln);
                }
            }    
            //foreach (var kv in methodSymbols)
            //{
            //    PrintLn("  " + kv.key);
            //}
        }
        
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for DEBUG:");
        PrintLn("  DEBUG <hexe file>");
    }
    
    string ExtendPath(string command)
    {
        string extension = Path.GetExtension(command);
        extension = extension.ToLower();
        if (extension == ".hexe")
        {
            string binaryname = command; // full path?
            if (File.Exists(binaryname))
            {
                // good
            }
            else
            {
                binaryname = Path.Combine(CurrentDirectory, command);
                if (File.Exists(binaryname))
                {
                    command = binaryname;
                }
                else
                {
                    binaryname = Path.Combine("/bin", command);
                    if (File.Exists(binaryname))
                    {
                        command = binaryname;
                    }
                }
            }
        }
        else if (extension == ".")
        {
            string binaryname = command; // full path?
            if (File.Exists(binaryname + hexeExtension))
            {
                 command = binaryname + hexeExtension;
            }
            else
            {
                binaryname = Path.Combine(CurrentDirectory, command);
                if (File.Exists(binaryname + hexeExtension))
                {
                    command = binaryname + hexeExtension;
                }
                else
                {
                    binaryname = Path.Combine("/bin", command);
                    if (File.Exists(binaryname + hexeExtension))
                    {
                        command = binaryname + hexeExtension;
                    }
                }
            }
        }
        return command;
    }
    
    bool Load(string command, <string> arguments)
    {
        bool success = false;
        Print("Loading '" + command + "'");
        success = Runtime.Load(command, arguments);
        PrintLn();
        if (success)
        {
            code = LoadRawCode(command);
            
            uint bytesLoaded = Runtime.BytesLoaded;
            PrintLn(bytesLoaded.ToString() + " bytes loaded.");
            
            string extension = Path.GetExtension(command);
            string symbolsPath  = command.Replace(extension, ".code");
            symbolsPath = Path.GetFileName(symbolsPath);
            symbolsPath = Path.Combine("/Debug/Obj", symbolsPath);
            
            if (File.Exists(symbolsPath))
            {
                if (Code.ParseCode(symbolsPath, false, true, false))
                {
                    uint methods = Code.GetMethodSymbolsCount();
                    PrintLn();
                    PrintLn("Symbols loaded for " + methods.ToString() + " methods.");
                    LoadStatements();
                }
            }
            SysCalls.New(); // for the comments
            
            Runtime.SetStepping(true);
            Runtime.SetVisibility(true);
            Runtime.Run();
        }
        return success;
    }
    
    DefineBreakpoint()
    {
        Print("  :");
        uint x = CursorX;
        uint y = CursorY;
            
        string hex;
        bool setBreakpoint = false;
        loop
        {
            Key key = ReadKey();
            Key unmaskedKey = (key & Key.Mask);
            switch (key)
            {
                case Key.Escape:
                {
                    Print(" Cancelled");
                    break;
                }
                case Key.Backspace:
                {
                    if (hex.Length > 0)
                    {
                        hex = hex.Substring(0, hex.Length-1);
                    }
                    SetCursor(x, y);
                    Print("                ");
                    SetCursor(x, y);
                    Print(hex);
                }
                case Key.Enter:
                {
                    setBreakpoint = hex.Length > 0;
                    break;
                }
                default:
                {
                    uint k = uint(key);
                    if ((k > 31) && (k < 128))
                    {
                        char c = char(k);
                        if (IsHexDigit(c))
                        {
                            c = c.ToUpper();
                            Print(c);
                            String.Build(ref hex, c);
                        }
                    }
                }
            }
        } // loop
        
        if (setBreakpoint)
        {
            bool success = false;
            loop
            {
                uint address;
                if (TryParseHex("0x" + hex, ref address))
                {
                    string sourceIndex = GetSourceIndex(address);
                    if (sourceIndex.Length > 0)
                    {
                        Print(" // " + sourceIndex);
                        Runtime.SetBreakpoint(address);
                        success = true;
                    }
                }
                break;
            }
            if (!success)
            {
                Print(" Invalid");
            }
        }
        PrintLn();
    }
    
    Debug()
    {
        loop
        {
            Print(".");
            if (!Runtime.Halted)
            {
                PrintLn();
                break;
            }
            Delay(10);
        }
        loop
        {
            loop
            {
                if (Runtime.Halted)
                {
                    break;
                }
                if (Runtime.Waiting)
                {
                    break; // runtime is waiting for debugger instruction
                }
                Delay(10);
            } // loop
            if (Runtime.Halted)
            {
                PrintLn("* Halted");
                break;
            }
            uint previousPC = 0;
            uint x = CursorX;
            uint y = CursorY;
            loop
            {
                loop
                {
                    if (Runtime.Waiting)
                    {
                        break; // runtime is waiting for debugger instruction
                    }
                    Delay(10);
                } // loop
                uint pc = Runtime.PC;
                uint address = pc;
                string content = "0x" + address.ToHexString(4);
                if ((address > 0) && (address < code.Length))
                {
                    content = Instructions.Disassemble(code, ref address, 0);
                }
                
                string sourceIndex = Code.GetSourceIndex(pc);
                if (previousPC != pc)
                {
                    SetCursor(x, y);
                    if (sourceIndex.Length > 0)
                    {
                        string sourceLine = Code.GetSourceLine(sourceIndex);
                        content = sourceLine + " // " + sourceIndex;   
                    }
                    content = content.Pad(' ', Columns-2);
                    PrintLn(content, MatrixBlue, Black);
                    Print("* ");
                    previousPC = pc;
                }
                if (Keyboard.IsAvailable)
                {
                    break;
                }
                if (Runtime.Halted)
                {
                    PrintLn("Halted");
                    break;
                }
            } // loop
            if (Runtime.Halted)
            {
                break;
            }
            Key k = ReadKey();
            switch (k)
            {
                case Key.F5:
                {
                    PrintLn("Run");
                    Runtime.StepRun();
                }
                case Key.F11:
                {
                    PrintLn("Step Into");
                    Runtime.StepInto();
                }
                case Key.F10:
                {
                    PrintLn("Step Over");
                    Runtime.StepOver();
                }
                case Key.Escape:
                {
                    PrintLn("Exit");
                    Runtime.Halted = true;
                    break;
                }
                case Key.c:
                case Key.C:
                {
                    DumpCallStack();
                }
                case Key.s:
                case Key.S:
                {
                    DumpValueStack();
                }
                case Key.x:
                case Key.X:
                {
                    PrintLn("Clear Breakpoints");
                    Runtime.ClearBreakpoints();
                }
                case Key.b:
                case Key.B:
                {
                    PrintLn("Breakpoint:");
                    DefineBreakpoint();
                }
                default:
                {
                    PrintLn(KeyToString(k)); // TODO: remove
                }
            } // switch (k)
        } // loop
        Runtime.SetVisibility(false);
    }
    
    {
        loop
        {
            <string> rawArgs = System.Arguments;
            if (rawArgs.Length < 1)
            {
                BadArguments();
                break;
            }
            string codePath;  
            <string> args;
            bool first = true;
            foreach (var arg in rawArgs)
            {
                if ((arg.Length == 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
                        //case "-g":
                        //{
                        //    Parser.SetInteractive(true);    
                        //}
                        default:
                        {
                            args.Append(arg); // not for me
                            break;
                        }
                    }
                }
                else if (first)
                {
                    first = false;
                    codePath = arg;
                }
                else
                {
                    args.Append(arg);
                }
            }
          
            string ext;
            codePath = ExtendPath(codePath);
            if (!File.Exists(codePath))
            {
                BadArguments();
                break;
            }
            
            // load the .hexe
            if (!Load(codePath, args))
            {
                PrintLn("Failed to load " + codePath);
                break;
            }
            
            Debug();
            
            break;
        } // loop
    }
}
