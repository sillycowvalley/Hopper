unit String
{
    uses "/Source/System/Char"
    
    string InsertChar(string this, uint index, char append) system;
    string Append(string this, string append) system;
    string Append(string this, char append) system;
    uint Length { get system; }
    Build(ref string build, string append) system;
    Build(ref string build, char append) system;
    Build(ref string build) system;
    
#ifdef H6502
    string Replace(string original, string pattern, string replace)
    {
        uint patternLength;
        uint replaceLength;
        uint originalLength;
        uint i;
        uint iCurrent;
        uint j;
        patternLength = pattern.Length;
        replaceLength = replace.Length;
        originalLength = original.Length;
        if (patternLength == 0)
        {
            return original;
        }
        string replaced;
        string search;
        loop
        {
            if (i == originalLength)
            {
                break;
            }
            //string search = original.Substring(i, patternLength);
            String.Build(ref search);
            iCurrent = i;
            uint count = patternLength;
            loop
            {
                if (iCurrent == originalLength)
                {
                    break;
                }
                if (count == 0)
                {
                    break;
                }
                String.Build(ref search, original[iCurrent]);
                iCurrent++;
                count--;
            }
            if (search == pattern)
            {
                for (j=0; j < replaceLength; j++)
                {
                    String.Build(ref replaced, replace[j]);
                }
                i = i + patternLength;
            }
            else
            {
                String.Build(ref replaced, original[i]);
                i++;
            }    
        }
        return replaced;
        
    }
    string Replace(string original, char pattern, char replace)
    {
        uint originalLength;
        char c;
        uint i;
        originalLength = original.Length;
        string replaced;
        for ( ; i < originalLength; i++)
        {
            c = original[i];
            if (c == pattern)
            {
                String.Build(ref replaced, replace);
            }
            else
            {
                String.Build(ref replaced, c);
            }
        }
        return replaced;
    }
    bool EndsWith(string original, char pattern)
    {
        return (original.Length > 0) && (original[original.Length-1] == pattern);
    }
    bool EndsWith(string original, string pattern)
    {
        uint length;
        uint originalLength;
        string substring;
        length = pattern.Length;
        originalLength = original.Length;
        if (originalLength < length)
        {
            return false;
        }
        substring = original.Substring(originalLength-length, length);
        return substring == pattern;
    }
    string Substring(string original, uint start)
    {
      uint i ;
      string result;
      for (i = start; i < original.Length; i++)
      {
          String.Build(ref result, original[i]);
      }    
      return result;
    }
    string Substring(string original, uint start, uint length)
    {
        uint originalLength;
        uint i;
        string result;
        if (length > 0)
        {
          originalLength = original.Length;
          for (i = start; i < originalLength; i++)
          {
            String.Build(ref result, original[i]);
            if (result.Length == length)
            {
                break;
            }
          }    
        }
        return result;
    }
    int Compare(string left, string right) // returns -1, 0, +1
    {
        uint ll;
        uint rl;
        uint i;
        int result;
        ll = left.Length;
        rl = right.Length;
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
            if (left[i] != right[i])
            {
                break;
            }
            i++;
        }
        loop
        {
            if ((ll == 0) && (rl == 0))
            {
                break; // "" == ""
            }
            
            // at this point the strings are identical up to left[i-1] == right[i-1]
            if ((i < ll) && (i < rl))
            {
                // [i-1] is the first difference
                if (int(left[i]) > int(right[i]))
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
    bool Contains(string this, char needle) system;
    bool Contains(string this, string needle)
    {
        uint index;
        return IndexOf(this, needle, ref index);
    }
    bool StartsWith(string this, char pattern) system;
    bool StartsWith(string this, string pattern)
    {
        uint length;
        uint i;
        length = pattern.Length;
        if (length <= this.Length)
        {
            for ( ; i < length; i++)
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
    bool IndexOf(string this, char pattern, ref uint index) system;
    bool IndexOf(string this, char pattern, uint searchIndex, ref uint index) system;
#else
    string Replace(string this, string pattern, string replace) system;
    string Replace(string this, char pattern, char replace) system;
    bool EndsWith(string this, char pattern) system;
    bool EndsWith(string this, string pattern) system;
    string Substring(string this, uint start) system;
    string Substring(string this, uint start, uint length) system;
    int Compare(string left, string right) system; // returns -1, 0, +1
    
    bool Contains(string this, char needle)
    {
        string result;
        char ch;
        uint i;
        uint length;
        length = this.Length;
        for ( ; i < length; i++)
        {
            ch = this[i];
            if (ch == needle)
            {
                return true;
            }
        }
        return false;
    }
    bool Contains(string this, string needle)
    {
        uint index;
        return IndexOf(this, needle, ref index);
    }
    bool StartsWith(string this, char pattern)
    {
        return (this.Length > 0) && (this[0] == pattern);
    }
    bool StartsWith(string this, string pattern)
    {
        uint i;
        uint length;
        length = pattern.Length;
        if (length <= this.Length)
        {
            for ( ; i < length; i++)
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
    bool IndexOf(string this, char pattern, ref uint index)
    {
        uint i;
        uint length;
        length = this.Length;
        for ( ; i < length; i++)
        {
            if (this[i] == pattern)
            {
                index = i;
                return true;
            }
        }
        return false;
    }
    bool IndexOf(string this, char pattern, uint searchIndex, ref uint index)
    {
        uint length;
        length = this.Length;
        loop
        {
            if (searchIndex >= length)
            {
                break;
            }
            if (this[searchIndex] == pattern)
            {
                index = searchIndex;
                return true;
            }
            searchIndex++;
        }
        return false;
    }
#endif    
    BuildFront(ref string build, char insert) system;
    
    char GetChar(string this, uint index) system;
    //{
    //    char c = this[index];
    //    return c;
    //}
    bool IsEmpty { get { return this.Length == 0; } }
    
    
    bool IndexOf(string this, string pattern, ref uint index)
    {
        bool found;
        uint pLength;
        uint pIndex;
        uint length;
        uint i;
        length = this.Length;
        pLength = pattern.Length;
        loop
        {
            if (pIndex + pLength > length)
            {
                break;
            }
            found = true;
            for (i=0; i < pLength; i++)
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
            pIndex++;
        }
        return found;
    }

    bool IndexOf(string this, string pattern, uint startIndex, ref uint index)
    {
        uint thisLength;
        string src;
        thisLength = this.Length;
        
        loop
        {
            if (startIndex >= thisLength-1)
            {
                break;
            }
            src = this.Substring(startIndex);
            if (src.IndexOf(pattern, ref index))
            {
                index = index + startIndex;
                return true;
            }
            break;
        }
        return false;
    }
    
    bool LastIndexOf(string this, char pattern, ref uint index)
    {
        uint length;
        uint i;
        loop
        {
            length = this.Length;
            if (length == 0)
            {
                break;
            }
            i = length-1;
            loop
            {
                if (this[i] == pattern)
                {
                    index = i;
                    return true;
                }
                if (i == 0)
                {
                    break;
                }
                i--;
            }
            break;
        }
        return false;
    }
    
    bool LastIndexOf(string this, char pattern, uint startIndex, ref uint index)
    {
        uint i;
        uint length;
        length = this.Length;
        if (startIndex < length)
        {
            i = startIndex;
            loop
            {
                if (this[i] == pattern)
                {
                    index = i;
                    return true;
                }
                if (i == 0)
                {
                    break;
                }
                i--;
            }
        }
        return false;
    }
    
    string Pad(string this, char append, uint width)
    {
        uint length;
        uint padding;
        string result;
        result = this;
        length = this.Length;
        if (width > length)
        {
            padding = width - length;
            while (padding > 0)
            {
                String.Build(ref result, append);
                padding--;
            }
        }
        return result;
    }
    
    string LeftPad(string this, char append, uint width)
    {
        uint length;
        uint padding;
        length = this.Length;
        if (width > length)
        {
            padding = width - length;
            while (padding > 0)
            {
                //this = this.InsertChar(0, append);
                String.BuildFront(ref this, append);
                padding--;
            }
        }
        return this;
    }
    
    string ToUpper(string this)
    {
        uint i;
        char c;
        uint length;
        string result;
        uint length = this.Length;
        for (; i < length; i++)
        {
            c = this[i];
            Build(ref result, c.ToUpper());
        }
        return result;
    }
    string ToLower(string this)
    {
        uint i;
        char c;
        uint length;
        string result;
        length = this.Length;
        for (; i < length; i++)
        {
            c = this[i];
            Build(ref result, c.ToLower());
        }
        return result;
    }
    
    string Trim(string this)
    {
        uint thisLength;
        uint iFirstNonSpace;
        uint iLastNonSpace;
        uint i;
        uint count;
        bool firstFound;   
        string result;
        thisLength = this.Length;
        if (thisLength > 0)
        {
            for (; i < thisLength; i++)
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
                for ( i=thisLength-1; i > 0; i--)
                {
                    if (this[i] != ' ')
                    {
                        iLastNonSpace = i;
                        // if there was a firstFound, there will always be a lastFound
                        break;
                    }
                }
                count = iLastNonSpace-iFirstNonSpace+1;
                i = iFirstNonSpace;
                loop
                {
                    if (i == thisLength)
                    {
                        break;
                    }    
                    if (count == 0)
                    {
                        break;
                    }
                    String.Build(ref result, this[i]);
                    i++;
                    count--;
                }
            }
        }
        return result;
    }
    
    string TrimLeft(string this)
    {
        uint i;
        uint thisLength;
        uint iFirstNonSpace;
        uint iLastNonSpace;
        bool firstFound;
        string result;
         thisLength = this.Length;
        if (thisLength > 0)
        {
            firstFound = false;
            for ( ; i < thisLength; i++)
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
                i = iFirstNonSpace;
                loop
                {
                    if (i == thisLength)
                    {
                        break;
                    }
                    String.Build(ref result, this[i]);
                    i++;
                }
            }
        }
        return result;
    }
 
    <string> Split(string this, string delimiters)
    {
        char c;
        char d;
        bool delim;
        uint i;
        uint length;
        uint dlength;
        <string> stringList;
        string accumulator;

        length = this.Length;
        dlength = delimiters.Length;
        for (; i < length; i++)
        {
            c = this[i];
            delim = false;
            for (uint di = 0; di < dlength; di++)
            {
                d = delimiters[di];
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
                Build(ref accumulator, c);
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
        uint i;
        char ch;
        uint length;
        <string> stringList;
        string accumulator;
        length = this.Length;
        for (; i < length; i++)
        {
            ch = this[i];
            if (ch == delimiter)
            {
                if (accumulator.Length > 0)
                {
                    stringList.Append(accumulator);
                    accumulator = "";
                }
            }
            else
            {
                Build(ref accumulator, ch);
            }
        }
        if (accumulator.Length > 0)
        {
            stringList.Append(accumulator);
        }
        return stringList;
    }
}
