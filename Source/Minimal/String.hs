unit String
{
#ifndef CPU_Z80    
    uint Length { get system; }
    
    char GetChar(string this, uint index) system; // syntax shortcut would be :  ch = this[index]
    
    Build(ref string build, string append) system;
    Build(ref string build, char append) system;
    Build(ref string build) system;
    BuildFront(ref string build, char insert) system;
#endif
    
    bool IsEmpty { get { return this.Length == 0; } }
    
    string Append(string this, string append)
    {
        string result = this;
        Build(ref result, append);
        return result;
    }
    string Append(string this, char append)
    {
        string result = this;
        Build(ref result, append);
        return result;
    }
    
    string InsertChar(string this, uint index, char append)
    {
        if (index >= this.Length)
        {
            string result = this;
            Build(ref result, append); // Append the character at the end
            return result;
        }
        else
        {
            string result = this.Substring(0, index); // Get the substring before the insert position
            Build(ref result, append); // Append the character at the insert position
            Build(ref result, this.Substring(index)); // Append the rest of the original string
            return result;
        }
    }
    
    
    
    bool IndexOf(string this, char pattern, ref uint index)
    {
        uint i = 0;
        while (i < this.Length)
        {
            if (this[i] == pattern)
            {
                index = i;
                return true;
            }
            i++;
        }
        return false;
    }
    bool IndexOf(string this, char pattern, uint searchIndex, ref uint index)
    {
        if (searchIndex >= this.Length || (this.Length == 0))
        {
            return false;
        }
       uint i = searchIndex;
        while (i < this.Length)
        {
            if (this[i] == pattern)
            {
                index = i;
                return true;
            }
            i++;
        }
       return false;
    }
    
    bool Contains(string this, char needle)
    {
        foreach (var c in this)
        {
            if (c == needle)
            {
                return true;
            }
        }
        return false;
    }
    bool StartsWith(string this, char pattern)
    {
        if (this.Length == 0)
        {
            return false;
        }

        return this[0] == pattern;
    }
    string Replace(string original, string pattern, string replace)
    {
        // Handle the case where the pattern is empty
        if (pattern.Length == 0)
        {
            return original; // Return a copy of the original string
        }
        
        string result;
        uint index;
        loop
        {
            uint patternIndex;
            if (!original.IndexOf(pattern, index, ref patternIndex))
            {
                // Pattern not found, append the remaining original string and break
                Build(ref result, original.Substring(index));
                break;
            }

            // Append the substring before the pattern
            Build(ref result, original.Substring(index, patternIndex - index));
            
            // Append the replacement string
            Build(ref result, replace);

            // Move the index past the pattern
            index = patternIndex + pattern.Length;
        }
       return result;
    }
    string Replace(string original, char pattern, char replace)
    {
        string result;
       foreach (var c in original)
        {
            if (c == pattern)
            {
                Build(ref result, replace);
            }
            else
            {
                Build(ref result, c);
            }
        }
       return result;
    }
    bool EndsWith(string original, char pattern)
    {
        if (original.Length == 0)
        {
            return false;
        }
       return original[original.Length - 1] == pattern;
    }
    bool EndsWith(string original, string pattern)
    {
        if (original.Length < pattern.Length)
        {
            return false;
        }
        return original.Substring(original.Length - pattern.Length) == pattern;
    }
    bool Contains(string this, string needle)
    {
        if (needle.Length == 0)
        {
            return true; // An empty string is always contained in any string
        }
        for (uint i = 0; i <= this.Length - needle.Length; i++)
        {
            bool found = true;
            for (uint j = 0; j < needle.Length; j++)
            {
                if (this[i + j] != needle[j])
                {
                    found = false;
                    break;
                }
            }
            if (found)
            {
                return true;
            }
        }
       return false;
    }
    bool StartsWith(string this, string pattern)
    {
        if (pattern.Length > this.Length)
        {
            return false;
        }
        for (uint i = 0; i < pattern.Length; i++)
        {
            if (this[i] != pattern[i])
            {
                return false;
            }
        }
        return true;
    }
    
    bool IndexOf(string this, string pattern, ref uint index)
    {
        if (pattern.Length == 0)
        {
            return false;
        }
       for (uint i = 0; i <= this.Length - pattern.Length; i++)
        {
            bool found = true;
            for (uint j = 0; j < pattern.Length; j++)
            {
                if (this[i + j] != pattern[j])
                {
                    found = false;
                    break;
                }
            }
            if (found)
            {
                index = i;
                return true;
            }
        }
       return false;
    }
    
    bool IndexOf(string this, string pattern, uint startIndex, ref uint index)
    {
        if ((pattern.Length == 0) || (startIndex >= this.Length))
        {
            return false;
        }
        for (uint i = startIndex; i <= this.Length - pattern.Length; i++)
        {
            bool found = true;
            for (uint j = 0; j < pattern.Length; j++)
            {
                if (this[i + j] != pattern[j])
                {
                    found = false;
                    break;
                }
            }
            if (found)
            {
                index = i;
                return true;
            }
        }
       return false;
    }
    bool LastIndexOf(string this, char pattern, ref uint index)
    {
        if (this.Length == 0)
        {
            return false;
        }
        for (uint i = this.Length - 1; i > 0; i--)
        {
            if (this[i] == pattern)
            {
                index = i;
                return true;
            }
        }
        // Check the first character separately
        if (this[0] == pattern)
        {
            index = 0;
            return true;
        }
       return false;
    }
    bool LastIndexOf(string this, char pattern, uint startIndex, ref uint index)
    {
        if ((this.Length == 0) || (startIndex >= this.Length))
        {
            return false;
        }
        for (uint i = UInt.Min(startIndex, this.Length - 1); i > 0; i--)
        {
            if (this[i] == pattern)
            {
                index = i;
                return true;
            }
        }
        // Check the first character separately
        if ((startIndex == 0) && (this[0] == pattern))
        {
            index = 0;
            return true;
        }
       return false;
    }
    string Pad(string this, char append, uint width)
    {
        if (this.Length >= width)
        {
            return this;
        }
        string padded = this;
        uint paddingLength = width - this.Length;
        for (uint i = 0; i < paddingLength; i++)
        {
            Build(ref padded, append);
        }
        return padded;
    }
    string LeftPad(string this, char append, uint width)
    {
        if (this.Length >= width)
        {
            return this;
        }
        string padded = "";
        uint paddingLength = width - this.Length;
        for (uint i = 0; i < paddingLength; i++)
        {
            Build(ref padded, append);
        }
        Build(ref padded, this);
        return padded;
    }
    <string> Split(string this, string delimiters)
    {
        <string> stringList;
        string current = "";
        foreach (char c in this)
        {
            if (delimiters.Contains(c))
            {
                if (!current.IsEmpty)
                {
                    stringList.Append(current);
                    current = "";
                }
            }
            else
            {
                current = current + c;
            }
        }
        if (!current.IsEmpty)
        {
            stringList.Append(current);
        }
        return stringList;
    }
    
    string Substring(string this, uint start) 
    {
        string result;
        for (uint i = start; i < this.Length; i++)
        {
            Build(ref result, this[i]);
        }
        return result;
    }
    string Substring(string this, uint start, uint length) 
    {
        string result;
        for (uint i = start; i < start + length && i < this.Length; i++)
        {
            Build(ref result, this[i]);
        }
        return result;
    }
    string TrimLeft(string this) 
    {
        uint start = 0;
        while (start < this.Length && Char.IsWhitespace(this[start]))
        {
            start++;
        }
        return this.Substring(start);
    }

    string TrimRight(string this) 
    {
        uint end = this.Length;
        while (end > 0 && Char.IsWhitespace(this[end - 1]))
        {
            end--;
        }
        return this.Substring(0, end);
    }

    string Trim(string this) 
    {
        uint start = 0;
        uint end = this.Length;

        // Trim leading spaces
        while (start < end && Char.IsWhitespace(this[start]))
        {
            start++;
        }

        // Trim trailing spaces
        while (end > start && Char.IsWhitespace(this[end - 1]))
        {
            end--;
        }
        return this.Substring(start, end - start);
    }    
    
    int Compare(string left, string right) 
    {
        uint minLength = UInt.Min(left.Length, right.Length);
        for (uint i = 0; i < minLength; i++)
        {
            if (left[i] < right[i])
            {
                return -1;
            }
            else if (left[i] > right[i])
            {
                return 1;
            }
        }
       if (left.Length < right.Length)
        {
            return -1;
        }
        else if (left.Length > right.Length)
        {
            return 1;
        }
       return 0;
    }
        
    Substring(ref string build, uint start)
    {
        build = build.Substring(start);
    }
    Trim(ref string build)
    {
        build = build.Trim();
    }
    TrimLeft(ref string build)
    {
        build = build.TrimLeft();
    }
    TrimRight(ref string build)
    {
        build = build.TrimRight();
    }
    string ToUpper(string this)
    {
        string result;
        foreach (var ch in this)
        {
            String.Build(ref result, ch.ToUpper());
        }
        return result;
    }
    string ToLower(string this)
    {
        string result;
        foreach (var ch in this)
        {
            String.Build(ref result, ch.ToLower());
        }
        return result;
    }
    ToUpper(ref string str)
    {
        str = str.ToUpper();
    }
    ToLower(ref string str)
    {
        str = str.ToLower();
    }
}
