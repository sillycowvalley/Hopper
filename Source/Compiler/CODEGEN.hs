program CODEGEN
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/JSON/Code"
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    
    long codeSize = 0;
    
    WriteCode(file hexeFile, <byte> code)
    {
        foreach (var b in code)
        {
            byte c = b;
            hexeFile.Append(c);
        }
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for CODEGEN:");
        PrintLn("  CODEGEN <code file>");
        PrintLn("    -g : called from GUI, not console");
    }
    
    {
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            foreach (var arg in rawArgs)
            {
                if ((arg.Length == 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
                        case "-g":
                        {
                            Parser.SetInteractive(true);    
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
          
            if (args.Length != 1)
            {
                BadArguments();
                break;
            }
            string codePath = args[0];
            if (!File.Exists(codePath))
            {
                string ext = Path.GetExtension(codePath);
                if (ext == ".")
                {
                    codePath = codePath + ".code";
                }
                if (!File.Exists(codePath))
                {
                    BadArguments();
                    break;
                }
            }
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(codePath);
                string hexePath  = codePath.Replace(extension, ".hexe2");
                hexePath = Path.GetFileName(hexePath);
                hexePath = Path.Combine("/Bin/", hexePath);
                File.Delete(hexePath);

                file hexeFile = File.Create(hexePath);
                if (!hexeFile.IsValid())
                {
                    PrintLn("Failed to create '" + hexePath + "'");
                    break;
                }
                
                if (!ParseCode(codePath, true, false))
                {
                    break;
                }
                                
                
                // binary format version number
                hexeFile.Append(byte(0));
                hexeFile.Append(byte(0));
                
                // figure out header table
                uint methodCount = Code.GetMethodCount();
                uint tableSize = ((methodCount-1) * 4);
                
                uint constOffset = tableSize+6;
                uint lsb = (constOffset & 0xFF);
                uint msb = (constOffset >> 8);
                
                // offset of location on constant data
                hexeFile.Append(byte(lsb));
                hexeFile.Append(byte(msb));
                
                <byte> constantData = Code.GetConstantData();
                
                uint mainOffset = constOffset + constantData.Length;
                
                lsb = (mainOffset & 0xFF);
                msb = (mainOffset >> 8);
                
                // offset of location on "main"
                hexeFile.Append(byte(lsb));
                hexeFile.Append(byte(msb));
                
                uint entryIndex = Code.GetEntryIndex();
                uint offset = mainOffset + Code.GetMethodSize(entryIndex);                
                
                <uint, uint> methodSizes = Code.GetMethodSizes();
                foreach (var sz in methodSizes)
                {
                    uint index = sz.key;
                    if (index == entryIndex)
                    {
                        continue;
                    }
                    
                    lsb = (index & 0xFF);
                    msb = (index >> 8);
                    hexeFile.Append(byte(lsb));
                    hexeFile.Append(byte(msb));
                    
                    lsb = (offset & 0xFF);
                    msb = (offset >> 8);
                    hexeFile.Append(byte(lsb));
                    hexeFile.Append(byte(msb));
                    
                    offset = offset + sz.value;
                }
                Parser.ProgressTick(".");
                
                // emit data
                WriteCode(hexeFile, constantData);
                Parser.ProgressTick(".");
                <byte> methodCode = Code.GetMethodCode(entryIndex);
                WriteCode(hexeFile, methodCode);
                Parser.ProgressTick(".");
                foreach (var sz in methodSizes)
                {
                    uint index = sz.key;
                    if (index == entryIndex)
                    {
                        continue;
                    }
                    methodCode = Code.GetMethodCode(index);
                    WriteCode(hexeFile, methodCode);   
                    Parser.ProgressTick(".");
                }
                                
                hexeFile.Flush();
                if (!Parser.IsInteractive())
                {
                    codeSize = File.GetSize(hexePath);
                    PrintLn();
                    Print("Success, " + codeSize.ToString() + " bytes, ", Color.DarkGreen, Color.LightGray);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() +"s", Color.MatrixBlue, Color.LightGray);
                }
                else
                {
                    Parser.ProgressDone();
                }
                break;
            }
            break;
        }
    }
}
