unit Vectors
{
    uses "/Source/Library/Display"
    uses "/Source/Library/Graphics/Canvas"
    
    bool Load(string path, ref <char> commands, ref < <float> > coords)
    {
        bool success;
        commands.Clear();
        coords.Clear();
        loop
        {
            file dataFile = File.Open(path);
            loop
            {
                string content = dataFile.ReadLine();
                if (!dataFile.IsValid()) { break; }
                if (content.Length == 0) { break; }
                success = true;
                char command = content[0];
                content = content.Substring(1);
                String.Trim(ref content);
                <float> arguments;
                switch (command)
                {
                    case 'c':
                    case 'm':
                    case 'M':
                    case 'h':
                    case 'H':
                    case 'v':
                    case 'V':
                    case 'l':
                    case 'L':
                    {
                        <string> parts = content.Split(" ,");
                        foreach (var part in parts)
                        {
                            if (part == ",") { continue; }
                            float fl;
                            if (!Float.TryParse(part, ref fl))
                            {
                                success = false;
                                break;
                            }
                            arguments.Append(fl);
                        }
                    }
                    case 'z':
                    case 'Z':
                    {
                    }
                    default:
                    {
                        success = false;
                        break; // unsupported command
                    }
                }
                commands.Append(command);
                coords.Append(arguments);
            }
            success = true;
            break;
        }
        return success;
    }
    Extents(< <float> > coordsList, ref float minX, ref float minY, ref float maxX, ref float maxY)
    {
        bool first = true;
        foreach (var coords in coordsList)
        {
            uint length = coords.Length;
            if (length == 0) { continue; }
            if (first)
            {
                minX = coords[0];
                maxX = coords[0];
                minY = coords[1];
                maxY = coords[1];
                first = false;
            }
            for (uint i = 2; i < length; i+= 2)
            {
                float x = coords[i];
                float y = coords[i+1];
                
                minX = Float.Min(minX, x);
                minY = Float.Min(minY, y);
                maxX = Float.Max(maxX, x);
                maxY = Float.Max(maxY, y);
            }    
        }
    }
    Line(float x0, float y0, float x1, float y1)
    {
        Display.Line(Canvas.ToX(x0), Canvas.ToY(y0), Canvas.ToY(x0), Canvas.ToY(y0), Screen.ForeColour);
    }
    Render(<char> commandsList, < <float> > coordsList)
    {
        uint count = commandsList.Length;
        float currentX;
        float currentY;
        float startX;
        float startY;
        for (uint i = 0; i < count; i++)
        {
            char   command = commandsList[i];
            <float> coords = coordsList[i];
            switch (command)
            {
                case 'm':
                {
                    currentX += coords[0];
                    currentY += coords[1];
                }
                case 'M':
                {
                    currentX = coords[0];
                    currentY = coords[1];
                }
                case 'L':
                {
                    Vectors.Line(currentX, currentY, coords[0], coords[1]);
                    currentX = coords[0];
                    currentY = coords[1];
                }
                case 'l':
                {
                    Vectors.Line(currentX, currentY, currentX+coords[0], currentY+coords[1]);
                    currentX += coords[0];
                    currentY += coords[1];
                }
                case 'H':
                {
                    Vectors.Line(currentX, currentY, coords[0], currentY);
                    currentX = coords[0];
                }
                case 'h':
                {
                    Vectors.Line(currentX, currentY, currentX+coords[0], currentY);
                    currentX += coords[0];
                }
                case 'V':
                {
                    Vectors.Line(currentX, currentY, currentX, coords[0]);
                    currentY = coords[0];
                }
                case 'v':
                {
                    Vectors.Line(currentX, currentY, currentX, currentY+coords[0]);
                    currentY += coords[0];
                }
                case 'C':
                {
                    uint length = coords.Length;
                    startX = coords[0];
                    startY = coords[1];
                    uint i = 0;
                    loop
                    {
                        if (i == length) { break; }
                        
                        Vectors.Line(currentX, currentY, coords[i], coords[i+1]);
                        currentX = coords[i];
                        currentY = coords[i+1];
                    
                        i += 2;
                    }
                }
                case 'c':
                {
                    uint length = coords.Length;
                    startX = currentX + coords[0];
                    startY = currentY + coords[1];
                    uint i = 0;
                    loop
                    {
                        if (i == length) { break; }
                        
                        Vectors.Line(currentX, currentY, currentX+coords[i], currentY+coords[i+1]);
                        currentX += coords[i];
                        currentY += coords[i+1];
                    
                        i += 2;
                    }
                }
                case 'z':
                case 'Z':
                {
                    Vectors.Line(currentX, currentY, startX, startY);
                    currentX = startX;
                    currentY = startY;
                }
            }
        }
    }
}
