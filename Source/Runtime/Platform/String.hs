unit HRString
{
    // String memory map:
    //   0000 heap allocator size
    //   0F   type = tString
    //   00   GC reference count
    //   0000 string length n
    //   00   first char in string
    //   ..
    //   <n>  last char in string
    
    const int siLength = 2;
    const int siChars  = 4;

    uint New()
    {
        uint address = GC.New(2, Type.String);
        WriteWord(address+2, 0); // length=0
        return address;
    }
    uint NewFromConstant0(uint location, uint length)
    {
        uint address = GC.New(2 + length, Type.String);
        WriteWord(address+2, length);
        for (uint i = 0; i < length; i++)
        {
            WriteByte(address+4+i, ReadByte(location+i));
        }
        return address;
    }
    uint NewFromConstant1(uint doubleChar)
    {
        byte lsb = byte(doubleChar & 0xFF);
        byte msb = byte(doubleChar >> 8);
        uint address = GC.New((msb == 0) ? 3 : 4, Type.String);
        WriteWord(address+siLength, (msb == 0) ? 1 : 2); // length
        WriteByte(address+siChars, lsb);
        if (msb != 0)
        {
            WriteByte(address+5, msb);
        }
        return address;
    }
    uint Clone(uint original)
    {
        return clone(original, 0);
    }
    uint clone(uint original, uint extra)
    {
        uint length = ReadWord(original+2);
        uint address = GC.New(4+length+extra, Type.String);
        WriteWord(address+2, length);
        for (uint i = 0; i < length; i++)
        {
            WriteByte(address+4+i, ReadByte(original+4+i));
        }
        return address;    
    }
    uint getCapacity(uint this)
    {
        return ReadWord(this-2) - 6;
    }
    uint GetLength(uint this)
    {
        return ReadWord(this+2);
    }
    char GetChar(uint this, uint index)
    {
        uint length = GetLength(this);
#ifdef CHECKED        
        if (index >= length)
        {
            Error = 0x05;
            return char(0);
        }
#endif
        return char(ReadByte(this+4+index));     
    }
    uint Append(uint this, uint append)
    {
        uint length0 = GetLength(this);
        uint length1 = GetLength(append);
        uint result = GC.New(2 + length0 + length1, Type.String);
        WriteWord(result+2, length0 + length1);
        for (uint i = 0; i < length0; i++)
        {
            WriteByte(result+4+i, ReadByte(this+4+i));
        }
        for (uint i = 0; i < length1; i++)
        {
            WriteByte(result+4+i+length0, ReadByte(append+4+i));
        }
        return result;
    }
    uint Substring(uint this, uint start, uint limit)
    {
        uint length0 = GetLength(this);
        if (start >= length0)
        {
            start = length0;
        }
        uint length1 = length0 - start;
        uint result = GC.New(2 + length1, Type.String);
        uint newLength = 0;
        for (uint i = 0; i < length1; i++)
        {
            if (newLength == limit)
            {
                break;
            }
            WriteByte(result+4+i, ReadByte(this+4+i+start));
            newLength++;
        }
        WriteWord(result+2, newLength);
        return result;
    }
    uint Substring(uint this, uint start)
    {
        uint limit = GetLength(this);
        return Substring(this, start, limit);
    }
    uint InsertChar(uint this, uint index, char ch)
    {
        uint length = GetLength(this);
        uint result = GC.New(2 + length + 1, Type.String);
        uint j = 0;
        for (uint i = 0; i < length; i++)
        {
            if (i == index)
            {
                WriteByte(result+4+j, byte(ch));
                j++;
            }
            WriteByte(result+4+j, ReadByte(this+4+i));
            j++;
        }
        if ((length == 0) || (index >= length))
        {
            WriteByte(result+4+j, byte(ch));    
        }
        WriteWord(result+2, length+1);
        return result;
    }
    uint Append(uint this, char ch)
    {
        uint length = GetLength(this);
        uint result = clone(this, 1);
        WriteByte(result+4+length, byte(ch));
        WriteWord(result+2, length+1);
        return result;
    }
    
    uint Replace(uint this, uint pattern, uint replace)
    {
        uint result;
        uint patternLength = GetLength(pattern);
        if (patternLength == 0)
        {
            result = clone(this, 0);
            return result;
        }
        uint originalLength = GetLength(this);
        uint replaceLength  = GetLength(replace);
        if (replaceLength <= patternLength)
        {
            result = clone(this, 0);
        }
        else
        {
            result = clone(this, (replaceLength-patternLength) * originalLength); // add maximum possible delta
        }
        uint i;
        uint j;
        loop
        {
            if (i == originalLength) { break; }
            bool match = false;
            if (i + patternLength <= originalLength)
            {
                // match pattern at 'i'
                match = true;
                for (uint n=0; n < patternLength; n++)
                {
                    if (ReadByte(this+4+i+n) != ReadByte(pattern+4+n))
                    {
                        match = false;
                        break;
                    }
                }
            }
            if (match)
            {
                i = i + patternLength;
                for (uint n = 0; n < replaceLength; n++)
                {
                    WriteByte(result+4+j, ReadByte(replace+4+n));
                    j++;
                }
            }
            else
            {
                WriteByte(result+4+j, ReadByte(this+4+i));
                j++; i++;
            }
        }
        WriteWord(result+2, j);
        return result;
    }
    
    uint Replace(uint this, char from, char to)
    {
        uint length = GetLength(this);
        uint result = clone(this, 0);
        for (uint i = 0; i < length; i++)
        {
            char ch = char(ReadByte(this+4+i));
            if (ch == from)
            {
                WriteByte(result+4+i, byte(to));
            }
        }
        return result;
    }
    Build(ref uint this)
    {
        WriteWord(this+2, 0); // length = 0
    }
    Build(ref uint this, uint append)
    {
        uint capacity = getCapacity(this);
        uint length0   = GetLength(this);
        uint length1   = GetLength(append);
        if (length1 > 0)
        {
            if (capacity < length0+length1)
            {
                uint copy = clone(this, length0+length1 - capacity);
                GC.Release(this);
                this = copy;
            }
            for (uint i = 0; i < length1; i++)
            {
                WriteByte(this+4+length0+i, ReadByte(append+4+i));
            }
            WriteWord(this+2, length0+length1);
        }
    }
    Build(ref uint this, char ch)
    {
        uint capacity = getCapacity(this);
        uint length   = GetLength(this);
        if (capacity < length+1)
        {
            uint copy = clone(this, 1);
            GC.Release(this);
            this = copy;
        }
        WriteByte(this+4+length, byte(ch));
        WriteWord(this+2, length+1);
    }
    BuildFront(ref uint this, char ch)
    {
        uint capacity = getCapacity(this);
        uint length = GetLength(this);
        if (capacity < length+1)
        {
            uint copy = clone(this, 1);
            GC.Release(this);
            this = copy;
        }
        uint i = length;
        loop
        {
            WriteByte(this+4+i, ReadByte(this+3+i));
            i--;
            if (i == 0)
            {
                break;
            }
        }
        WriteByte(this+4, byte(ch));
        WriteWord(this+2, length+1);
    }
    Substring(ref uint this, uint start)
    {
        uint length = GetLength(this);
        if (start == 0)
        {
            return;
        }
        if (start >= length)
        {
            WriteWord(this+2, 0); // new length
            return;
        }
        
        WriteWord(this+2, length-start); // new length
        uint i = start;
        uint j = 0;
        loop
        {
            WriteByte(this+4+j, ReadByte(this+4+i));  
            i++;
            j++;
            if (i == length)
            {
                break;
            }
        }
    
    }
    uint TrimLeft(uint this)
    {
        uint copy = Clone(this);
        TrimLeft(ref copy);
        return copy;
    }
    uint Trim(uint this)
    {
        uint copy = Clone(this);
        TrimRight(ref copy);
        TrimLeft(ref copy);
        return copy;
    }
    TrimLeft(ref uint this)
    {
        uint length = GetLength(this);
        uint i = 0;
        loop
        {
            if (i == length) { break; }
            char ch = char(ReadByte(this+4+i));
            if (ch != ' ')
            {
                break;
            }
            i++;
        }
        Substring(ref this, i);
    }
    TrimRight(ref uint this)
    {
        uint length = GetLength(this);
        if (length == 0)
        {
            return;
        }
        uint i = length-1;
        loop
        {
            char ch = char(ReadByte(this+4+i));
            if (ch != ' ')
            {
                WriteWord(this+2, i+1); // new length    
                break;
            }
            if (i == 0) 
            { 
                WriteWord(this+2, 0); // new length    
                break; 
            }
            i--;
        }
    }
    
    bool EndsWith(uint this, char with)
    {
        uint length = GetLength(this);
        if (length == 0)
        {
            return false;
        }
        return (char(ReadByte(this+4+length-1)) == with);
    }
    bool EndsWith(uint this, uint with)
    {
        uint length0 = GetLength(this);
        uint length1 = GetLength(with);
        if (length0 < length1)
        {
            return false;
        }
        if (length1 == 0)
        {
            return true; // EndsWith(xxx, "") == true?
        }
        uint i = 1;
        loop
        {
            char w = char(ReadByte(with+4+length1-i));   
            char t = char(ReadByte(this+4+length0-i));
            if (w != t) { return false; }
            if (i == length1)
            {
                break;
            }
            i++;
        }
        return true;
    }
    int Compare(uint left, uint right) // returns -1, 0, +1
    {
        uint i;
        int result;
        uint ll = GetLength(left);
        uint rl = GetLength(right);
        loop
        {
            if (i >= ll)
            {
                break;
            }
            if (i >= rl)
            {
                break;
            }
            if (ReadByte(left+siChars+i) != ReadByte(right+siChars+i))
            {
                break;
            }
            i++;
        }
        loop
        {
            if ((ll == 0) && (rl == 0))
            {
                break; // " == "
            }
            
            // at this point the strings are identical up to left[i-1] == right[i-1]
            if ((i < ll) && (i < rl))
            {
                if (int(ReadByte(left+siChars+i)) > int(ReadByte(right+siChars+i)))
                {
                    result = 1;
                }
                else
                {
                    result = -1;
                }
                break;
            }
            if (i >= ll)
            {
                if (i >= rl)
                {
                    break; // they were equal to this point
                }
                // left string is shorter but they are equal to [i-1]
                result = -1;
                break;
            }
            result = 1;
            break;
        }
        return result;
    }

    Dump(uint address, uint indent)
    {
        for (uint i = 0; i < indent; i++)
        {
            IO.Write(' ');
        }
        
        IO.Write(char(0x27)); // single quote
        uint length = ReadWord(address+2);
        if (length > 40)
        {
            length = 40;
        }
        for (uint i = 0; i < length; i++)
        {
            IO.Write(char(ReadByte(address+4+i)));
        }
        IO.Write(char(0x27)); // single quote
    }

}
