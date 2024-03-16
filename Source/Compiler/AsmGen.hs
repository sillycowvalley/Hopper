program ASMGEN
{
    #define JSON_EXPRESS // .code and .json are generated by us so assume .json files have no errors
    #define ASSEMBLER
    
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "JSON/JSON"
    uses "JSON/Code"
    
    uses "Tokens/Token"
    uses "Tokens/Scanner"
    uses "Tokens/Parser"
    
    uses "Symbols"
    
    uses "CODEGEN/AsmStream"
    
    uint romSize = 0;
    long codeSize = 0;
    
    <byte> output;
    <uint,uint> patches; // <callLocation,methodIndex>
    <uint,uint> methods; // <methodIndex,address>
    
    bool NoPackedInstructions { get { return false; } }
    
    badArguments()
    {
        PrintLn("Invalid arguments for ASMGEN:");
        PrintLn("  ASMGEN <code file>");
        PrintLn("    -g <c> <r> : called from GUI, not console");
    }
    
    writeMethod(uint methodIndex, <byte> code, uint romAddress)
    {
        methods[methodIndex] = output.Count + romAddress;
        
        byte callInstruction;
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            callInstruction = GetJSRInstruction();
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            callInstruction = GetCALLInstruction();
        }
        
        uint index = 0;
        loop
        {
            if (index == code.Count) { break; }
            byte instruction = code[index];
            byte instructionLength = AsmStream.GetInstructionLength(instruction);
            if (instruction == callInstruction)
            {
                uint address = code[index+1] + code[index+2] << 8;
                patches[output.Count+1] = address;
            }
            for (uint i=0; i < instructionLength; i++)
            {
                output.Append(code[index+i]);
            }
            index += instructionLength;
        }
    }
    doCallPatches()
    {
        // <uint,uint> patches; // <callLocation,methodIndex>
        // <uint,uint> methods; // <methodIndex,address>
        foreach (var kv in patches)
        {
            uint patchAddress  = kv.key;
            uint targetMethod  = kv.value;
            uint targetAddress = methods[targetMethod];
            output.SetItem(patchAddress,   byte(targetAddress & 0xFF));
            output.SetItem(patchAddress+1, byte(targetAddress >> 8));
        }
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
    
    writeIHex(file ihexFile, uint romAddress, <byte> output, <byte> vectors)
    {
        // https://en.wikipedia.org/wikie/Intel_HEX#Format
        
        uint byteCount = 0;
        uint index = 0;
        
        byte currentTick = 0;
        string progressTicks = "-\\|/-\\|/";
        
        string buffer;
        uint emitAddress = 0;
        loop
        {
            if (index == output.Count)
            {
                // done
                break;
            }
            byte cb = output[index]; index++;
            
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
        if (buffer.Length != 0)
        {
            emitBuffer(ihexFile, emitAddress, buffer);
            buffer = "";
        }
        
        // 6502 Vectors:
        foreach (var vb in vectors)
        {
            buffer = buffer + vb.ToHexString(2);
        }
        emitBuffer(ihexFile, 0xFFFA - romAddress, buffer);
        
        
        ihexFile.Append(":00000001FF" + char(0x0A)); // eof
        ihexFile.Flush();
    }
    
    {
        bool success = false;
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            for (uint iArg = 0; iArg < rawArgs.Count; iArg++)
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
                badArguments();
                break;
            }
            string ext = ".code";
            string codePath = args[0];
            if (!File.Exists(ref codePath, ref ext, "/Debug/Obj/"))
            {
                badArguments();
            }
            
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(codePath);
                string ihexPath  = codePath.Replace(extension, ".hex");
                string jsonPath  = codePath.Replace(extension, ".json");
                
                ihexPath = Path.GetFileName(ihexPath);
                ihexPath = Path.Combine("/Bin/", ihexPath);
                File.Delete(ihexPath);
                
                Symbols.New();
                if (!Symbols.Import(jsonPath))
                {
                    break;
                }
                if (DefineExists("CPU_6502"))
                {
                    Architecture = CPUArchitecture.M6502;
                }
                if (DefineExists("CPU_65C02"))
                {
                    Architecture = CPUArchitecture.W65C02;
                }
                if (DefineExists("CPU_Z80A"))
                {
                    Architecture = CPUArchitecture.Z80A;
                }
                if (DefineExists("ROM_32K"))
                {
                    romSize = 0x8000;
                }
                if (DefineExists("ROM_16K"))
                {
                    romSize = 0x4000;
                }
                if (DefineExists("ROM_8K"))
                {
                    romSize = 0x2000;
                }
                if (DefineExists("ROM_4K"))
                {
                    romSize = 0x1000;
                }
                
                long startAddress = 0x10000 - romSize; 
                uint romAddress = uint(startAddress);
                
                file ihexFile = File.Create(ihexPath);
                if (!ihexFile.IsValid())
                {
                    PrintLn("Failed to create '" + ihexPath + "'");
                    break;
                }
                
                if (!ParseCode(codePath, true, false))
                {
                    break;
                }
                
                uint methodCount = Code.GetMethodCount();
                
                <byte> constantData = Code.GetConstantData();
                if (constantData.Count != 0)
                {
                    PrintLn("Unexpected constant data");
                    break;
                }
                
                uint entryIndex = Code.GetEntryIndex();
                
                <uint, uint> methodSizes = Code.GetMethodSizes();
                
                Parser.ProgressTick(".");
                byte arch = byte(Architecture);
                output.Append(0); // version
                output.Append(arch);
                output.Append(byte(romAddress & 0xFF));
                output.Append(byte(romAddress >> 8));
                
                
                
                <byte> methodCode = Code.GetMethodCode(entryIndex);
                writeMethod(entryIndex, methodCode, romAddress);
                Parser.ProgressTick(".");
                uint indexMax = 0;
                foreach (var sz in methodSizes)
                {
                    if (sz.key > indexMax)
                    {
                        indexMax = sz.key;
                    }
                }
                // if we emit the methods in increasing order of indices
                // then we can find them again in the binary (for debug info)
                for (uint index = 0; index <= indexMax; index++)
                {
                    if (index == entryIndex) { continue; }
                    if (!methodSizes.Contains(index)) { continue; }   
                    methodCode = Code.GetMethodCode(index);
                    writeMethod(index, methodCode, romAddress);   
                    Parser.ProgressTick(".");
                }
                doCallPatches();
                Parser.ProgressTick(".");
                
                uint iIndex;
                uint nIndex;
                if (!Symbols.GetFunctionIndex("IRQ", ref iIndex))
                {
                    PrintLn("6502 should have an 'IRQ()' method for the isr vector destination");
                    break;
                }
                if (!Symbols.GetFunctionIndex("NMI", ref nIndex))
                {
                    PrintLn("6502 should have an 'NMI()' method for the isr vector destination");
                    break;    
                }
                <uint> mOverloads = Symbols.GetFunctionOverloads(iIndex);
                iIndex = mOverloads[0];
                mOverloads = Symbols.GetFunctionOverloads(nIndex);
                nIndex = mOverloads[0];
                
                // 6502 vectors
                <byte>vectors;
                vectors.Append(byte(methods[nIndex] & 0xFF));
                vectors.Append(byte(methods[nIndex] >> 8));
                vectors.Append(byte(methods[entryIndex] & 0xFF));
                vectors.Append(byte(methods[entryIndex] >> 8));
                vectors.Append(byte(methods[iIndex] & 0xFF));
                vectors.Append(byte(methods[iIndex] >> 8));
                
                writeIHex(ihexFile, romAddress, output, vectors);
                
                if (!Parser.IsInteractive())
                {
                    codeSize = File.GetSize(ihexPath);
                    
                    PrintLn();
                    Print("Success, " + codeSize.ToString() + " bytes of code, ", Colour.ProgressText, Colour.ProgressFace);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() +"s", Colour.ProgressHighlight, Colour.ProgressFace);
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
