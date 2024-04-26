unit String
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
        uint this = GC.Create(Type.String, 12); // default size is 16 less header size (blocksize, ref and type)
        return this;
    }
    uint NewFromConstant0(uint location, uint length)
    {
        uint this;
        uint size;
        uint i;
        uint source;
        uint destination;
        uint constantLocation;
        size = length + 6;                     // +6 for blocksize, ref, type, length 
        if ((size & 0x000F) != 0)
        {
            size = (size + 15) & 0xFFF0;       // round up to nearest 16 byte boundary
        }
        this = GC.Create(Type.String, size-4); // -4 (header added by Create and Allocate)
        destination = this + siChars;
        constantLocation = ReadWord(ConstantStart) + 2;
        source      = constantLocation + location;
        for (i=0; i < length; i++)
        {
            WriteByte(destination, ReadByte(source));
            destination++;
            source++;
        }
        WriteWord(this + siLength, length);
        return this;
    }
    uint NewFromConstant1(uint chch)
    {
        byte lch = byte(chch & 0xFF);
        byte mch = byte(chch >> 8);
        uint this;
        this = GC.Create(Type.String, 12); // 16-4 (header added by Create and Allocate)
        WriteByte(this + siChars, lch);
        if (mch != 0)
        {
            WriteByte(this + siChars + 1, mch);
        }
        uint length = 2;
        if (mch == 0)
        {
            length = 1;
        }
        WriteWord(this + siLength, length);
        return this;
    }
    uint GetLength(uint this)
    {
        return ReadWord(this+siLength);
    }
    uint GetChar(uint this, uint index)
    {
        uint value;
        uint length = ReadWord(this+siLength);
        if (index >= length)
        {
            Die(0x05); // string index out of range
        }
        value = ReadByte(this + siChars + index);
        return value;
    }
    
    BuildChar(ref uint str, char append)
    {
        uint capacity = ReadWord(str-2) - 6; // -6 for blocksize, ref, type, length
        uint length = ReadWord(str+siLength);
        uint strExpanded;
        uint i;
        uint source;
        uint destination;
        byte references;
        if (length >= capacity)
        {
            // expand 
            references  = ReadByte(str + 1);                     // GC reference count
            strExpanded = GC.Create(Type.String, capacity + 18); // capacity + 6 + 16 - 4 (-4 for header added by Create and Allocate)  
            source      = str + siChars;
            destination = strExpanded + siChars;
            for (i=0; i < length; i++)
            {
                WriteByte(destination, ReadByte(source));
                destination++;
                source++;
            }
            GC.Release(str);
            str = strExpanded;
            WriteByte(str + 1, references);
        }    
        WriteByte(str + siChars + length, byte(append));
        WriteWord(str + siLength,          length+1);
    }
    BuildString(ref uint str, uint append)
    {
        uint capacity = ReadWord(str-2) - 6; // -6 for blocksize, ref, type, length
        uint length   = ReadWord(str+siLength);
        uint length2  = ReadWord(append+siLength);
        uint strExpanded;
        uint size;
        uint i;
        uint source;
        uint destination;
        byte references;
        if (length+length2 >= capacity)
        {
            // expand 
            references  = ReadByte(str + 1);              // GC reference count
            size = length + length2 + 6;                  // +6 for blocksize, ref, type, length 
            if ((size & 0x000F) != 0)
            {
                size = (size + 15) & 0xFFF0;              // round up to nearest 16 byte boundary
            }
            strExpanded = GC.Create(Type.String, size-4); // -4 (header added by Create and Allocate)
            source      = str + siChars;
            destination = strExpanded + siChars;
            for (i=0; i < length; i++)
            {
                WriteByte(destination, ReadByte(source));
                destination++;
                source++;
            }
            GC.Release(str);
            str = strExpanded;
            WriteByte(str + 1, references);
        }    
        source      = append + siChars;
        destination = str + siChars + length;
        for (i=0; i < length2; i++)
        {
            WriteByte(destination, ReadByte(source));
            destination++;
            source++;
        }
        WriteWord(str + siLength, length+length2);
    }
    BuildFront(ref uint str, char append)
    {
        uint capacity = ReadWord(str-2) - 6; // -6 for blocksize, ref, type, length
        uint length = ReadWord(str+siLength);
        uint strExpanded;
        uint i;
        uint source;
        uint destination;
        byte references;
        if (length >= capacity)
        {
            // expand 
            references  = ReadByte(str + 1);                     // GC reference count
            strExpanded = GC.Create(Type.String, capacity + 18); // capacity + 6 + 16 - 4 (-4 for header added by Create and Allocate)     
            source      = str + siChars;
            destination = strExpanded + siChars;
            for (i=0; i < length; i++)
            {
                WriteByte(destination, ReadByte(source));
                destination++;
                source++;
            }
            GC.Release(str);
            str = strExpanded;
            WriteByte(str + 1, references);
        }    
        source      = str + siChars + length - 1; // last character
        destination = source + 1;
        for (i=0; i < length; i++)
        {
            WriteByte(destination, ReadByte(source));
            destination--;
            source--;
        }
        WriteByte(str + siChars,  byte(append));
        WriteWord(str + siLength, length+1);
    }
    BuildClear(ref uint str)
    {
        WriteWord(str + siLength, 0);
    }

}
