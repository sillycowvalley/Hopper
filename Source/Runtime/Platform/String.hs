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

    uint new(uint size)
    {
        uint blockSize = 6 + size;
        blockSize = (blockSize + 15) & 0xFFF0; // round up string allocations to 16 byte boundaries
        return GC.New(blockSize-4, Type.String);
    }
    uint clone(uint original, uint extra)
    {
        uint length = ReadWord(original+siLength);
        uint address = HRString.new(length+extra);
        WriteWord(address+siLength, length);
        for (uint i = 0; i < length; i++)
        {
            WriteByte(address+siChars+i, ReadByte(original+siChars+i));
        }
        return address;    
    }
    uint New()
    {
        uint address = HRString.new(0);
        WriteWord(address+siLength, 0); // length=0
        return address;
    }
    uint NewFromConstant0(uint location, uint length)
    {
        uint address = HRString.new(length);
        WriteWord(address+siLength, length);
        for (uint i = 0; i < length; i++)
        {
            WriteByte(address+siChars+i, ReadCodeByte(location+i));
        }
        return address;
    }
    uint NewFromConstant1(uint doubleChar)
    {
        byte lsb = byte(doubleChar & 0xFF);
        byte msb = byte(doubleChar >> 8);
        uint address = HRString.new((msb == 0) ? 1 : 2);
        WriteWord(address+siLength, (msb == 0) ? 1 : 2); // length
        WriteByte(address+siChars, lsb);
        if (msb != 0)
        {
            WriteByte(address+siChars+1, msb);
        }
        return address;
    }
    uint Clone(uint original)
    {
        return clone(original, 0);
    }
    
    uint getCapacity(uint this)
    {
        return ReadWord(this-2) - 6;
    }
    uint GetLength(uint this)
    {
        return ReadWord(this+siLength);
    }
    char GetChar(uint this, uint index)
    {
        uint length = GetLength(this);
        if (index >= length)
        {
            Error = 0x05;
            return char(0);
        }
        return char(ReadByte(this+siChars+index));     
    }
    uint Substring(uint this, uint start, uint limit)
    {
        uint length0 = GetLength(this);
        if (start >= length0)
        {
            start = length0;
        }
        uint length1 = length0 - start;
        uint result = HRString.new(length1);
        uint newLength = 0;
        for (uint i = 0; i < length1; i++)
        {
            if (newLength == limit)
            {
                break;
            }
            WriteByte(result+siChars+i, ReadByte(this+siChars+i+start));
            newLength++;
        }
        WriteWord(result+siLength, newLength);
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
        uint result = HRString.new(length+1);
        uint j = 0;
        for (uint i = 0; i < length; i++)
        {
            if (i == index)
            {
                WriteByte(result+siChars+j, byte(ch));
                j++;
            }
            WriteByte(result+siChars+j, ReadByte(this+siChars+i));
            j++;
        }
        if ((length == 0) || (index >= length))
        {
            WriteByte(result+siChars+j, byte(ch));    
        }
        WriteWord(result+siLength, length+1);
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
                    if (ReadByte(this+siChars+i+n) != ReadByte(pattern+siChars+n))
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
                    WriteByte(result+siChars+j, ReadByte(replace+siChars+n));
                    j++;
                }
            }
            else
            {
                WriteByte(result+siChars+j, ReadByte(this+siChars+i));
                j++; i++;
            }
        }
        WriteWord(result+siLength, j);
        return result;
    }
    
    uint Replace(uint this, char from, char to)
    {
        uint length = GetLength(this);
        uint result = clone(this, 0);
        for (uint i = 0; i < length; i++)
        {
            char ch = char(ReadByte(this+siChars+i));
            if (ch == from)
            {
                WriteByte(result+siChars+i, byte(to));
            }
        }
        return result;
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
            WriteWord(this+siLength, 0); // new length
            return;
        }
        
        WriteWord(this+siLength, length-start); // new length
        uint i = start;
        uint j = 0;
        loop
        {
            WriteByte(this+siChars+j, ReadByte(this+siChars+i));  
            i++;
            j++;
            if (i == length)
            {
                break;
            }
        }
    
    }
    uint Trim(uint this)
    {
        uint copy = Clone(this);
        TrimRight(ref copy);
        TrimLeft(ref copy);
        return copy;
    }
    uint TrimLeft(uint this)
    {
        uint copy = Clone(this);
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
            char ch = char(ReadByte(this+siChars+i));
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
            char ch = char(ReadByte(this+siChars+i));
            if (ch != ' ')
            {
                WriteWord(this+siLength, i+1); // new length    
                break;
            }
            if (i == 0) 
            { 
                WriteWord(this+siLength, 0); // new length    
                break; 
            }
            i--;
        }
    }
    bool IndexOf(uint this, char pattern, ref uint index)
    {
        uint length = GetLength(this);
        for (uint i = 0; i < length; i++)
        {
            if (char(ReadByte(this+siChars+i)) == pattern)
            {
                index = i;
                return true;
            }
        }
        return false;
    }
    bool IndexOf(uint this, char pattern, uint searchIndex, ref uint index)
    {
        uint length = GetLength(this);
        loop
        {
            if (searchIndex >= length)
            {
                break;
            }
            if (char(ReadByte(this+siChars+searchIndex)) == pattern)
            {
                index = searchIndex;
                return true;
            }
            searchIndex++;
        }
        return false;
    }
    bool Contains(uint this, char needle)
    {
        uint i;
        uint length = GetLength(this);
        for ( ; i < length; i++)
        {
            if (char(ReadByte(this+siChars+i)) == needle)
            {
                return true;
            }
        }
        return false;
    }
    bool StartsWith(uint this, char with)
    {
        uint length = GetLength(this);
        if (length == 0)
        {
            return false;
        }
        return (char(ReadByte(this+siChars)) == with);
    }
    bool StartsWith(uint this, uint with)
    {
        uint length0 = GetLength(this);
        uint length1 = GetLength(with);
        if (length0 < length1)
        {
            return false;
        }
        if (length1 == 0)
        {
            return true; // StartsWith(xxx, "") == true?
        }
        uint i = 0;
        loop
        {
            if (i == length1)
            {
                break;
            }
            char w = char(ReadByte(with+siChars+i));   
            char t = char(ReadByte(this+siChars+i));
            if (w != t) { return false; }
            i++;
        }
        return true;
    }
    
    bool EndsWith(uint this, char with)
    {
        uint length = GetLength(this);
        if (length == 0)
        {
            return false;
        }
        return (char(ReadByte(this+siChars+length-1)) == with);
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
            char w = char(ReadByte(with+siChars+length1-i));   
            char t = char(ReadByte(this+siChars+length0-i));
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
        uint length = ReadWord(address+siLength);
        if (length > 40)
        {
            length = 40;
        }
        for (uint i = 0; i < length; i++)
        {
            IO.Write(char(ReadByte(address+siChars+i)));
        }
        IO.Write(char(0x27)); // single quote
    }
    
    uint Append(uint this, uint append)
    {
        uint length0 = GetLength(this);
        uint length1 = GetLength(append);
        uint result = HRString.new(length0 + length1);
        WriteWord(result+siLength, length0 + length1);
        for (uint i = 0; i < length0; i++)
        {
            WriteByte(result+siChars+i, ReadByte(this+siChars+i));
        }
        for (uint i = 0; i < length1; i++)
        {
            WriteByte(result+siChars+i+length0, ReadByte(append+siChars+i));
        }
        return result;
    }
    uint Append(uint this, char ch)
    {
        uint length = GetLength(this);
        uint result = clone(this, 1);
        WriteByte(result+siChars+length, byte(ch));
        WriteWord(result+siLength, length+1);
        return result;
    }
    
    BuildClear(ref uint this)
    {
        WriteWord(this+siLength, 0); // length = 0
    }
    BuildChar(ref uint this, char ch)
    {
        uint capacity = getCapacity(this);
        uint length   = GetLength(this);
        if (capacity < length+1)
        {
            uint copy = clone(this, 1);
            GC.Release(this);
            this = copy;
        }
        WriteByte(this+siChars+length, byte(ch));
        WriteWord(this+siLength, length+1);
    }
    BuildString(ref uint this, uint append)
    {
        uint length1 = GetLength(append);
        if (length1 > 0)
        {
            uint capacity = getCapacity(this);
            uint length0  = GetLength(this);
            if (capacity < length0+length1)
            {
                uint copy = clone(this, length1);
                GC.Release(this);
                this = copy;
            }
            for (uint i = 0; i < length1; i++)
            {
                WriteByte(this+siChars+length0+i, ReadByte(append+siChars+i));
            }
            WriteWord(this+siLength, length0+length1);
        }
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
            if (i == 0)
            {
                break;
            }
            WriteByte(this+siChars+i, ReadByte(this+siChars+i-1));
            i--;
        }
        WriteByte(this+siChars, byte(ch));
        WriteWord(this+siLength, length+1);
    }
    
    uint ToUpper(uint this)
    {
        uint copy = Clone(this);
        ToUpper(ref copy);
        return copy;
    }
    ToUpper(ref uint this)
    {
        uint length = GetLength(this);
        uint i = 0;
        loop
        {
            if (i == length) { break; }
            char ch = char(ReadByte(this+siChars+i));
            WriteByte(this+siChars+i, byte(HRChar.ToUpper(ch)));
            i++;
        }
    }
    uint ToLower(uint this)
    {
        uint copy = Clone(this);
        ToLower(ref copy);
        return copy;
    }
    ToLower(ref uint this)
    {
        uint length = GetLength(this);
        uint i = 0;
        loop
        {
            if (i == length) { break; }
            char ch = char(ReadByte(this+siChars+i));
            WriteByte(this+siChars+i, byte(HRChar.ToLower(ch)));
            i++;
        }
    }

}
