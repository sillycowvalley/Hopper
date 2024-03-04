unit Vectors
{
    uses "/Source/Library/Display"
    int width;
    int height;
    uint backColour;
    
    int Width { get { return width; } }
    int Height { get { return height; } }
    uint BackColour { get { return backColour; } }
    
    string vectorsPath;
    bool binaryMode;
    
    
    bool Header(string path)
    {
        bool success = false;
        loop
        {
            vectorsPath = path;
            binaryMode = path.EndsWith(".bin");
            file f = File.Open(path);
            if (!f.IsValid()) { break; }
            
            if (binaryMode)
            {
                byte[16] buffer;
                uint bytesRead = f.Read(buffer, 16);
                width  = int(buffer[0] + (buffer[1] << 8));
                height = int(buffer[2] + (buffer[3] << 8));
                if (!f.IsValid()) { break; }
                char recordType = char(buffer[4]);
                if ((recordType == 'p') && (bytesRead > 10))
                {
                    backColour = uint(buffer[9] + (buffer[10] << 8));
                }
                else if ((recordType == 'h') && (bytesRead > 12))
                {
                    backColour = uint(buffer[11] + (buffer[12]  << 8));
                }
                else
                {
                    break;
                }
                
                if (!f.IsValid()) { break; }
            }
            else
            {
                string header = f.ReadLine();
                if (!f.IsValid()) { break; }
                if (!header.StartsWith("sz:")) { break; }
                header = header.Substring(3);
                <string> parts = header.Split(',');
                if ((parts.Count != 2) || !Int.TryParse(parts[0], ref width) || !Int.TryParse(parts[1], ref height)) { break; }
                
                string first  = f.ReadLine();
                if (!f.IsValid()) { break; }
                
                if (first.StartsWith("p:"))
                {
                    parts = first.Split(',');
                    if ((parts.Count != 3) || !UInt.TryParse(parts[2], ref backColour)) { break; }
                }
                else if (first.StartsWith("h:"))
                {
                    parts = first.Split(',');
                    if ((parts.Count != 4) || !UInt.TryParse(parts[3], ref backColour)) { break; }
                }
                else
                {
                    break;
                }
            }
            
            success = true;
            break;
        }
        return success;
    }
    
    uint bufferBytes;
    uint bufferCurrent;
    byte[1024] readBuffer;
    
    byte GetBufferByte(file f)
    {
        if (bufferCurrent >= bufferBytes)
        {
            bufferBytes = f.Read(readBuffer, 1024);
            bufferCurrent = 0;
            if (bufferBytes == 0)
            {
                return 0;
            }
        }
        byte b = readBuffer[bufferCurrent];
        bufferCurrent++;
        return b;
    }
    
    Render(int dx, int dy)
    {
        file f = File.Open(vectorsPath);
        int sx; int ex; int sy;
        uint colour;
        
        
        if (binaryMode)
        {
            _ = GetBufferByte(f);
            _ = GetBufferByte(f);
            _ = GetBufferByte(f);
            _ = GetBufferByte(f);
            loop
            {
                char recordType = char(GetBufferByte(f));
                if (recordType == 'p')
                {
                    sx = int(GetBufferByte(f) + GetBufferByte(f) << 8);
                    ex = sx;
                    sy = int(GetBufferByte(f) + GetBufferByte(f) << 8);
                }
                else if (recordType == 'h')
                {
                    sx = int(GetBufferByte(f) + GetBufferByte(f) << 8);
                    ex = int(GetBufferByte(f) + GetBufferByte(f) << 8);
                    sy = int(GetBufferByte(f) + GetBufferByte(f) << 8);
                }
                else
                {
                    break;
                }
                colour = uint(GetBufferByte(f) + (GetBufferByte(f) << 8));
                if (!f.IsValid()) { break; }
                Display.HorizontalLine(sx+dx, sy+dy, ex+dx, colour);
            }
        }
        else
        {
            _ = f.ReadLine(); // header
            loop
            {
                string command = f.ReadLine();
                if (!f.IsValid()) { break; }
                uint iColon;
                if (command.IndexOf(':', ref iColon))
                {
                    string arguments = command.Substring(iColon+1);
                    command = command.Substring(0, iColon);
                    <string> parts = arguments.Split(',');
                    switch (command)
                    {
                        case "p":
                        {
                            _ = Int.TryParse(parts[0], ref sx);                        
                            _ = Int.TryParse(parts[1], ref sy);
                            ex = sx;
                            _ = UInt.TryParse(parts[2], ref colour);
                        }
                        case "h":
                        {
                            _ = Int.TryParse(parts[0], ref sx);                        
                            _ = Int.TryParse(parts[1], ref ex);                        
                            _ = Int.TryParse(parts[2], ref sy);
                            _ = UInt.TryParse(parts[3], ref colour);
                        }
                        default:
                        {
                            continue;
                        }
                    }
                    Display.HorizontalLine(sx+dx, sy+dy, ex+dx, colour);
                }
            }
        }
    }
}
