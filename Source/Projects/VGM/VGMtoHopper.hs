program VGMtoHopper
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uint keepK = 4;
    
    BadArguments()
    {
        PrintLn("Invalid arguments for VGMtoHopper:");
        PrintLn("  VGMtoHopper <vgm file>");
        PrintLn("    -c         : count in K to keep (default is 4)");
    }
    
    appendByte(file hsFile, byte data, uint count)
    {
        if (count % 16 == 0)
        {
            hsFile.Append(Char.EOL + "        ");
        }
        hsFile.Append("0x" + data.ToHexString(2) + ", ");
        /*
        if (count % 32 == 0)
        {
            PrintLn();
            Print(count.ToHexString(4) + " ");
        }
        Print(" " + data.ToHexString(2));
        */
    }
    bool convert(string name, file vgmFile, file hsFile)
    {
        bool success;
        bool ended;
        uint count;
        uint headerSize = 256;
        uint bytesToKeep = keepK * 1024 - 1; // -1 for END
        byte versionMSB;
        byte versionLSB;  
        
        hsFile.Append("unit VGMSRC { // " +name + " (" + keepK.ToString() + " KB)" + Char.EOL);
        hsFile.Append("    const byte[] VGMDATA = {");
        loop
        {
            byte data = vgmFile.Read();
            if (vgmFile.IsValid())
            {
                if (count < headerSize)
                {
                    // header
                    if (count % 32 == 0)
                    {
                        PrintLn();
                        Print(count.ToHexString(4) + " ");
                    }
                    Print(" " + data.ToHexString(2), Colour.Ocean, Colour.Black);
                    
                    /* assuming header is always 0x100 bytes:
                    switch(count)
                    {
                        case 0x08: { versionLSB = data; }
                        case 0x09: 
                        { 
                            versionMSB = data; 
                            if ((versionMSB == 0x01) && (versionLSB == 0x50))
                            {
                                //headerSize = 64;
                            }
                            else if ((versionMSB == 0x01) && (versionLSB == 0x51))
                            {
                                //headerSize = 128;
                            }
                            else
                            {
                                PrintLn();
                                PrintLn("Header size for version " + versionMSB.ToHexString(2) + "." + versionLSB.ToHexString(2) + " not implemented");
                                break;
                            }
                        }
                    }
                    */
                    
                    count++;
                }
                else
                {
                    // data
                    uint arguments;
                    switch (data)
                    {
                        case 0x50:
                        { arguments = 1; }
                            
                        case 0x61: 
                        { arguments = 2; }
                            
                        case 0x62:
                        case 0x70 .. 0x7F:
                        {} // no arguments
                        
                        case 0x66:
                        { ended = true; } // no arguments
                            
                        default:
                        {
                            PrintLn();
                            PrintLn(" Command 0x" + data.ToHexString(2) + " not implemented ");
                            break;
                        }
                    }
                    appendByte(hsFile, data, count);
                    count++;
                    while (arguments != 0)
                    {
                        data = vgmFile.Read();
                        appendByte(hsFile, data, count);
                        count++;
                        arguments--;
                    }
                }
                
                if (!ended)
                {
                    if ((count < bytesToKeep + headerSize))
                    {
                        continue;
                    }
                    hsFile.Append("0x66" + Char.EOL); // END
                }
            }
            hsFile.Append("    };" + Char.EOL);
            hsFile.Append("}" + Char.EOL);
            
            if (!hsFile.IsValid())
            {
                break;
            }
            hsFile.Flush();
            success = true;
            break;
        }    
        return success;
    }
    
    Hopper()
    {
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
                        case "-c":
                        {
                            iArg++;
                            if (!UInt.TryParse(rawArgs[iArg], ref keepK))
                            {
                                args.Clear();
                                break;
                            }
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
            string ext = ".vgz";
            string vgmPath = args[0];
            if (!File.Exists(ref vgmPath, ref ext, "/Source/Projects/VGM/Samples/"))
            {
                BadArguments();
            }
            file vgmFile = File.Open(vgmPath);
            if (!vgmFile.IsValid())
            {
                PrintLn("Failed to open '" + vgmPath + "'");
                BadArguments();
                break;
            }
            
            
            string extension = Path.GetExtension(vgmPath);
            string hsPath  = vgmPath.Replace(extension, ".hs");
            File.Delete(hsPath);

            file hsFile = File.Create(hsPath);
            if (!hsFile.IsValid())
            {
                PrintLn("Failed to create '" + hsPath + "'");
                break;
            }
            if (convert(Path.GetFileName(hsPath), vgmFile, hsFile))
            {
                PrintLn();
                PrintLn("Successfully created '" + hsPath + "'");
            }
            else
            {
                PrintLn();
                PrintLn("Failure converting '" + vgmPath + "' to '" + hsPath + "'", Colour.MatrixRed, Colour.Black);
            }
            break;
        } // loop
    }
}
