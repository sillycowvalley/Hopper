program CODEGEN
{
    #define JSONEXPRESS // .code and .json are generated by us so assume .json files have no errors
    
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/JSON/Code"
    
    uses "/Source/Compiler/CodeGen/Instructions"
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    
    long codeSize = 0;
    bool extendedCodeSegment;
    
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
        PrintLn("    -g <c> <r> : called from GUI, not console");
        PrintLn("    -extended  : full 64K for code, assumes 32 bit runtime");
        PrintLn("    -ihex      : generate an Intel HEX file from the .hexe");
    }
    
    byte hexCheckSum(string values)
    {
        uint sum = 0;
        for (uint i = 0; i < values.Length / 2; i++)
        {
            string substr = values.Substring(i * 2, 2);
            uint b = 0;
            if (UInt.TryParse("0x" + substr, ref b))
            {
            }
            sum = sum + b;
        }
        sum = sum % 256;
        byte chk = byte(sum);
        chk = ~chk;
        chk++;
        return chk;
    }
  
    emitBuffer(file ihexFile, uint address, string buffer)
    {
        uint bytes = buffer.Length / 2;
        string ln = bytes.ToHexString(2) + address.ToHexString(4) + "00" + buffer;
        byte chk = hexCheckSum(ln);
        ihexFile.Append(":" + ln + chk.ToHexString(2) + char(0x0A));
    }
    
    ConvertToIHex(string hexePath)
    {
        // https://en.wikipedia.org/wikie/Intel_HEX#Format
        // convert the .hexe into IHEX
        string extension = Path.GetExtension(hexePath);
        string ihexPath = hexePath.Replace(extension, ".hex");
        if (File.Exists(ihexPath))
        {
            File.Delete(ihexPath);
        }
        
        File.Delete(ihexPath);
        file ihexFile = File.Create(ihexPath);
        file binFile = File.Open(hexePath);
        uint byteCount = 0;
        
        byte currentTick = 0;
        string progressTicks = "-\\|/-\\|/";
        
        string buffer;
        uint emitAddress = 0x0000;
        loop
        {
            byte cb = binFile.Read();
            
            if (!binFile.IsValid())
            {
                // done
                break;
            }
            buffer = buffer + cb.ToHexString(2);
            if (buffer.Length == 32)
            {
                emitBuffer(ihexFile, emitAddress, buffer);
                emitAddress = emitAddress + 16;
                buffer = "";
            }
            
            byteCount++;
            if (byteCount % 32 == 0)
            {
                Parser.ProgressTick("x");
            }
        }
        if (buffer.Length > 0)
        {
            emitBuffer(ihexFile, emitAddress, buffer);
        }
        ihexFile.Append(":00000001FF" + char(0x0A)); // eof
        ihexFile.Flush();
    }
    
    {
        bool success = false;
        loop
        {
            bool doIHex = false;
            <string> rawArgs = System.Arguments;
            <string> args;
            for (uint iArg = 0; iArg < rawArgs.Length; iArg++)
            {
                string arg = rawArgs[iArg];
                if ((arg.Length >= 2) && (arg[0] == '-'))
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
                        case "-ihex":
                        {
                            doIHex = true;
                        }
                        case "-extended":
                        {
                            extendedCodeSegment = true;
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
            string ext = ".code";
            string codePath = args[0];
            if (!File.Exists(ref codePath, ref ext, "/Debug/Obj/"))
            {
                BadArguments();
            }
            
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(codePath);
                //string hexePath  = codePath.Replace(extension, hexeExtension);
                string hexePath  = codePath.Replace(extension, ".hexe");
                
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
                                
                byte versionLSB = extendedCodeSegment ? 0x01 : 0x00;
                hexeFile.Append(versionLSB);
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
                uint offset = Code.GetMethodSize(entryIndex);
                if (!extendedCodeSegment)
                {
                    offset += mainOffset;
                }
                
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
                
                if (doIHex)
                {
                    ConvertToIHex(hexePath);
                }
                
                if (!Parser.IsInteractive())
                {
                    codeSize = File.GetSize(hexePath);
                    if (extendedCodeSegment)
                    {
                        codeSize -= mainOffset;
                    }
                    PrintLn();
                    Print("Success, " + codeSize.ToString() + " bytes of code, ", Color.ProgressText, Color.ProgressFace);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() +"s", Color.ProgressHighlight, Color.ProgressFace);
                }
                else
                {
                    Parser.ProgressDone();
                }
                success = true;
                break;
            }
            break;
        }
        if (!success)
        {
            Diagnostics.SetError(0x0E);
        }
    }
}
