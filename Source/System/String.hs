unit String
{
    string InsertChar(string this, uint index, char append) system; // just for simulation for now
#ifndef ZOPPER
    uint Length { get system; }

    string Replace(string this, string pattern, string replace) system;
    string Replace(string this, char pattern, char replace) system;
    
    string Substring(string this, uint start) system;
    string Substring(string this, uint start, uint length) system;
    
    bool EndsWith(string this, char pattern) system;
    bool EndsWith(string this, string pattern) system;
    
    int Compare(string left, string right) system; // returns -1, 0, +1
    
    
#endif
    char GetChar(string this, uint index)
    {
        char c = this[index];
        return c;
    }
    bool IsEmpty { get { return this.Length == 0; } }
    
    bool IndexOf(string this, char pattern, ref uint index)
    {
        bool found = false;
        uint length = this.Length;
        for (uint i=0; i < length; i++)
        {
            if (this[i] == pattern)
            {
                index = i;
                found = true;
                break;
            }
        }
        return found;
    }
    bool IndexOf(string this, string pattern, ref uint index)
    {
        bool found = false;
        uint length = this.Length;
        uint pLength = pattern.Length;
        uint pIndex = 0;
        loop
        {
            if (pIndex + pLength > length)
            {
                break;
            }
            found = true;
            for (uint i=0; i < pLength; i++)
            {
                if (this[i+pIndex] != pattern[i])
                {
                    found = false;
                    break;
                }
            }
            if (found)
            {
                index = pIndex;
                break;
            }
            pIndex = pIndex + 1;
        }
        return found;
    }

    bool IndexOf(string this, string pattern, uint startIndex, ref uint index)
    {
        bool found = false;
        loop
        {
            if (startIndex >= this.Length-1)
            {
                break;
            }
            string src = this.Substring(startIndex);
            if (src.IndexOf(pattern, ref index))
            {
                index = index + startIndex;
                found = true;
            }
            break;
        }
        return found;
    }
    
    bool LastIndexOf(string this, char pattern, ref uint index)
    {
        bool found = false;
        loop
        {
            uint length = this.Length;
            if (length == 0)
            {
                break;
            }
            uint i = length-1;
            loop
            {
                if (this[i] == pattern)
                {
                    index = i;
                    found = true;
                    break;
                }
                if (i == 0)
                {
                    break;
                }
                i = i - 1;
            }
            break;
        }
        return found;
    }
    
    bool LastIndexOf(string this, char pattern, uint startIndex, ref uint index)
    {
        bool found = false;
        uint length = this.Length;
        if (startIndex < length)
        {
            uint i = startIndex;
            loop
            {
                if (this[i] == pattern)
                {
                    index = i;
                    found = true;
                    break;
                }
                if (i == 0)
                {
                    break;
                }
                i = i - 1;
            }
        }
        return found;
    }
    
    string Append(string this, string append)
    {
        string result = this + append;
        return result;
    }
    string Append(string this, char append)
    {
        string result = this + append;
        return result;
    }
    
    string Pad(string this, char append, uint width)
    {
        uint length = this.Length;
        if (width > length)
        {
            uint padding = width - length;
            while (padding > 0)
            {
                this = this + append;
                padding--;
            }
        }
        return this;
    }
    
    string LeftPad(string this, char append, uint width)
    {
        uint length = this.Length;
        if (width > length)
        {
            uint padding = width - length;
            while (padding > 0)
            {
                this = this.InsertChar(0, append);
                padding--;
            }
        }
        return this;
    }
    
    string ToUpper(string this)
    {
        string result;
        foreach (var c in this)
        {
            result = result + c.ToUpper();
        }
        return result;
    }
    string ToLower(string this)
    {
        string result;
        foreach (var c in this)
        {
            result = result + c.ToLower();
        }
        return result;
    }
    
    
    bool StartsWith(string this, char pattern)
    {
        return (this.Length > 0) && (this[0] == pattern);
    }
    bool StartsWith(string this, string pattern)
    {
        uint length = pattern.Length;
        if (length <= this.Length)
        {
            for (uint i=0; i < length; i++)
            {
                if (pattern[i] != this[i])
                {
                    return false;
                }
            }
            return true;
        }
        return false;
    }
    
    bool Contains(string this, char needle)
    {
        uint index;
        return IndexOf(this, needle, ref index);
    }
    bool Contains(string this, string needle)
    {
        uint index;
        return IndexOf(this, needle, ref index);
    }
    
    string Trim(string this)
    {
        string result;
        uint length = this.Length;
        if (length > 0)
        {
            uint iFirstNonSpace;
            bool firstFound = false;
            uint iLastNonSpace;
            for (uint i=0; i < length; i++)
            {
                if (this[i] != ' ')
                {
                    iFirstNonSpace = i;
                    firstFound = true;
                    break;
                }
            }
            if (firstFound)
            {
                for (uint i=length-1; i > 0; i--)
                {
                    if (this[i] != ' ')
                    {
                        iLastNonSpace = i;
                        // if there was a firstFound, there will always be a lastFound
                        break;
                    }
                }
                result = this.Substring(iFirstNonSpace, iLastNonSpace-iFirstNonSpace+1);
            }
        }
        return result;
    }
 
    <string> Split(string this, string delimiters)
    {
        <string> stringList;

        uint length = this.Length;
        uint dlength = delimiters.Length;
        string accumulator;
        for (uint i = 0; i < length; i++)
        {
            char c = this[i];
            bool delim = false;
            for (uint di = 0; di < dlength; di++)
            {
                char d = delimiters[di];
                if (c == d)
                {
                    delim = true;
                    break;
                }
            }
            if (delim)
            {
                if (accumulator.Length > 0)
                {
                    stringList.Append(accumulator);
                    accumulator = "";
                }
            }
            else
            {
                accumulator = accumulator + c;
            }
        }
        if (accumulator.Length > 0)
        {
            stringList.Append(accumulator);
        }
        return stringList;
    }
    <string> Split(string this, char delimiter)
    {
        string delimiters = delimiter.ToString();
        return Split(this, delimiters);
    }
}
