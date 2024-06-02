unit String
{
    uint Length { get system; }
    
    char GetChar(string this, uint index) system; // syntax shortcut would be :  ch = this[index]
    
    Build(ref string build, string append) system;
    Build(ref string build, char append) system;
    Build(ref string build) system;
    BuildFront(ref string build, char insert) system;
    
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
        string result;
        if (index >= this.Length)
        {
            result = this;
            Build(ref result, append); // Append the character at the end
            
        }
        else
        {
            result = this.Substring(0, index); // Get the substring before the insert position
            Build(ref result, append); // Append the character at the insert position
            Build(ref result, this.Substring(index)); // Append the rest of the original string
        }
        return result;
    }
    
    bool IndexOf(string this, char pattern, ref uint index)
    {
        bool result;
        uint i = 0;
        while (i < this.Length)
        {
            if (this[i] == pattern)
            {
                index = i;
                result = true;
                break;
            }
            i++;
        }
        return result;
    }
    bool IndexOf(string this, char pattern, uint searchIndex, ref uint index)
    {
        bool result;
        uint i;
        if ((searchIndex >= this.Length) || (this.Length == 0))
        {
            
        }
        else
        {
            i = searchIndex;
            while (i < this.Length)
            {
                if (this[i] == pattern)
                {
                    index = i;
                    result = true;
                    break;
                }
                i++;
            }
        }
        return result;
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
        bool result;
        if (this.Length != 0)
        {
            result = this[0] == pattern;
        }
        return result;
    }
    string Replace(string original, string pattern, string replace)
    {
        string result;
        uint index;
        uint patternIndex;
            
        // Handle the case where the pattern is empty
        if (pattern.Length == 0)
        {
            result = original; // Return a copy of the original string
        }
        else
        {
            loop
            {
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
        }
        return result;
    }
    string Replace(string original, char pattern, char replace)
    {
        string result;
        uint i;
        char c;
        for (i=0; i < original.Length; i++)
        {
            c = original[i];
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
        bool result;
        if (original.Length != 0)
        {
            result = original[original.Length - 1] == pattern;
        }
        return result;
    }
    bool EndsWith(string original, string pattern)
    {
        bool result;
        if (original.Length >= pattern.Length)
        {
            result = original.Substring(original.Length - pattern.Length) == pattern;
        }
        return result;
    }
    bool Contains(string this, string needle)
    {
        bool result;
        if (needle.Length == 0)
        {
            result = true; // An empty string is always contained in any string
        }
        else if (this.Length < needle.Length)
        {
            // never true
        }
        else
        {
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
                    result = true;
                    break;
                }
            }
        }
        return result;
    }
    bool StartsWith(string this, string pattern)
    {
        uint i;
        bool result = true;
        if (pattern.Length > this.Length)
        {
            result = false;
        }
        else
        {
            for (i = 0; i < pattern.Length; i++)
            {
                if (this[i] != pattern[i])
                {
                    result = false;
                    break;
                }
            }
        }
        return result;
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
        uint i;
        uint j;
        bool found;
        bool result;
        if ((pattern.Length == 0) || (startIndex >= this.Length))
        {
            
        }
        else
        {
            for (i = startIndex; i <= this.Length - pattern.Length; i++)
            {
                found = true;
                for (j = 0; j < pattern.Length; j++)
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
                    result = true;
                    break;
                }
            }
        }
        return result;
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
        string padded;
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
        string current;
        foreach (var c in this)
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
    <string> Split(string this, char delimiter)
    {
        <string> stringList;
        string current;
        foreach (var c in this)
        {
            if (delimiter == c)
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
        uint i;
        string result;
        for (i = start; i < this.Length; i++)
        {
            Build(ref result, this[i]);
        }
        return result;
    }
    string Substring(string this, uint start, uint length) 
    {
        uint i;
        string result;
        for (i = start; i < start + length && i < this.Length; i++)
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
    bool Equals(string this, string other)
    {
        uint i = 0;
        uint length = this.Length;
        if (length != other.Length)
        {
            return false;
        }
        loop
        {
            if (i == length) { break; }
            if (this[i] != other[i])
            {
                return false;
            }
            i++;
        }
        return true;
    }
    
    int Compare(string left, string right) 
    {
        uint i;
        int  result;
        uint minLength = UInt.Min(left.Length, right.Length);
        for (i = 0; i < minLength; i++)
        {
            if (left[i] < right[i])
            {
                result = -1;
            }
            else if (left[i] > right[i])
            {
                result = 1;
            }
        }
        if (result == 0)
        {
            if (left.Length < right.Length)
            {
                result = -1;
            }
            else if (left.Length > right.Length)
            {
                result = 1;
            }
        }
        return result;
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
        char ch;
        uint i;
        string result;
        for (i=0; i < this.Length; i++)
        {
            ch = this[i];
            String.Build(ref result, ch.ToUpper());
        }
        return result;
    }
    string ToLower(string this)
    {
        char ch;
        uint i;
        string result;
        for (i=0; i < this.Length; i++)
        {
            ch = this[i];
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
