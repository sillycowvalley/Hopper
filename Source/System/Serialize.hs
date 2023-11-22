unit Serialize
{
    uses "/Source/System/System"
    
    bool valueToJSON(variant jsonValue, ref string output)
    {
        bool success = true;
        type vt = typeof(jsonValue);
        switch (vt)
        {
            case list:
            {
                <variant> object = jsonValue;
                success = arrayToJSON(object, ref output);
            }
            case dictionary:
            {
                <string, variant> object = jsonValue;
                success = objectToJSON(object, ref output);
            }
            case delegate:
            {
                // the only thing in Hopper that can stand-in for 'null'
                // is an uninitialized delegate
                if (uint(jsonValue) != 0)
                {
                    success = false;
                }
                output = output + "null";
            }
            case bool:
            {
                bool boolean = bool(jsonValue);
                output = output + (boolean ? "true" : "false");
            }
            case uint:
            case byte:
            {
                uint number = uint(jsonValue);
                output = output + number.ToString();
            }
            case int:
            {
                int number = int(jsonValue);
                output = output + number.ToString();
            }
            case long:
            {
                long number = long(jsonValue);
                output = output + number.ToString();
            }
            case float:
            {
                float number = float(jsonValue);
                output = output + number.ToString();
            }
            case string:
            {
                string str = jsonValue;
                output = output + '"';
                foreach (var ch in str)
                {
                    byte b = byte(ch);
                    switch (b)
                    {
                        case 0x08: { output = output + "\\b"; continue; }
                        case 0x09: { output = output + "\\t"; continue; }
                        case 0x0A: { output = output + "\\n"; continue; }
                        case 0x0C: { output = output + "\\f"; continue; }
                        case 0x0D: { output = output + "\\r"; continue; }
                        case 0x22: { output = output + "\\" + char(b); continue; } // \"
                        case 0x5C: { output = output + "\\" + char(b); continue; } // \\
                        default:   { if (b < 32) { success = false; break; } }
                    }
                    output = output + ch;
                }
                output = output + '"';
            }
            default:
            {
                success = false;
            }
        }
        return success;
    }
    bool arrayToJSON(<variant> jsonArray, ref string output)
    {
        bool success = true;
        output = output + "[";
        bool first = true;
        foreach (var value in jsonArray)
        {
            if (!first)                          { output = output + ","; }
            if (!valueToJSON(value, ref output)) { break; }    
            first = false;
        }
        output = output + "]";
        return success;
    }
    bool objectToJSON(<string, variant> jsonObject, ref string output)
    {
        bool success = true;
        output = output + "{";
        bool first = true;
        foreach (var kv in jsonObject)
        {
            if (typeof(kv.key) != string)           { success = false; break; }
            if (!first)                             { output = output + ","; }
            output = output + '"' + kv.key + '"' + ":";
            if (!valueToJSON(kv.value, ref output)) { break; }
            first = false;
        }
        output = output + "}";
        return success;
    }
    consumeLeadingWhitespace(ref string input)
    {
        uint i = 0;
        uint length = input.Length;
        loop
        {
            if (i == length)                { break; }
            if ((input[i]).IsWhitespace())  { i++; continue; }
            break; // non-whitespace
        }
        input = input.Substring(i);
    }
    bool consume(char expected, ref string input)
    {
        bool success = false;
        uint length = input.Length;
        if (length > 0)
        {
            if (input[0] == expected)
            {
                success = true;
                input = input.Substring(1);
            }
        }
        return success;
    }
    bool stringFromJSON(ref string input, ref string str)
    {
        bool success = true;
        loop
        {
            consumeLeadingWhitespace(ref input);
            if (!consume('"', ref input))
            {
                success = false;
                break;
            }
            str = "";
            success = false;
            while (input.Length > 0)
            {
                char ch = input[0];
                input = input.Substring(1);
                if (ch == '"')
                {
                    success = true;
                    break;
                }
                else if (ch == '`')
                {
                    str = str + '"';
                }
                else
                {
                    str = str + ch;
                }
            }
            break;
        } // loop
        return success;
    }
    bool numberFromJSON(ref string input, ref variant value)
    {
        bool success = true;
        loop
        {
            consumeLeadingWhitespace(ref input);
            string str = "";
            success = false;
            bool hasDigits = false;
            while (input.Length > 0)
            {
                char ch = input[0];
                if (ch.IsWhitespace())
                {
                    success = hasDigits;
                    if (success)
                    {
                        ch = str[str.Length-1];
                        success = ch.IsDigit();
                    }
                    break;
                }
                else if (ch.IsDigit())
                {
                    input = input.Substring(1);
                    str = str + ch;
                    hasDigits = true;
                }
                else if ((ch == '-') && (str.Length == 0))
                {
                    input = input.Substring(1);
                    str = str + ch;
                }
                else if ((ch == '.') && hasDigits && !str.Contains('.'))
                {
                    input = input.Substring(1);
                    str = str + ch;
                }
                else if ((ch == 'e') && hasDigits && !str.Contains('e') && !str.Contains('E'))
                {
                    input = input.Substring(1);
                    str = str + ch;
                }
                else if ((ch == 'E') && hasDigits && !str.Contains('e') && !str.Contains('E'))
                {
                    input = input.Substring(1);
                    str = str + ch;
                }
                else if ((ch == '-') && (str.EndsWith('e') || str.EndsWith('E')))
                {
                    input = input.Substring(1);
                    str = str + ch;
                }
                else
                {
                    success = hasDigits;
                    if (success)
                    {
                        ch = str[str.Length-1];
                        success = ch.IsDigit();
                    }
                    break;
                }
            } // while
            if (success)
            {
                if (str.Contains('.') || str.Contains('e') || str.Contains('E'))
                {
                    float f;
                    success = Float.TryParse(str, ref f);
                    if (success)
                    {
                        value = f;
                    }
                }
                else
                {
                    long l;
                    success = Long.TryParse(str, ref l);
                    if (success)
                    {
                        if (l >= 0)
                        {
                            if (l <= 0xFFFF)
                            {
                                value = uint(l);
                            }
                            else
                            {
                                value = l;
                            }
                        }
                        else if (l >= -32768)
                        {
                           value = int(l);
                        }
                        else
                        {
                            value = l;
                        }
                    }
                }
            }
            break;
        } // loop
        return success;
    }
    bool valueFromJSON(ref string input, ref variant value)
    {
        bool success = true;
        loop
        {
            consumeLeadingWhitespace(ref input);
            if (input.Length == 0) { success = false; break; }
            char ch = input[0];
            switch(ch)
            {
                case '[':
                {
                    <variant> jsonArray;
                    success = arrayFromJSON(ref input, ref jsonArray);
                    value = jsonArray;    
                }
                case '{':
                {
                    <string, variant> jsonObject;
                    success = objectFromJSON(ref input, ref jsonObject);
                    value = jsonObject;
                }
                case 't':
                {
                    if (!input.StartsWith("true"))            { success = false; break; }
                    input = input.Substring(4);
                    value = true;
                }
                case 'f':
                {
                    if (!input.StartsWith("false"))           { success = false; break; }
                    input = input.Substring(5);
                    value = false;
                }
                case 'n':
                {
                    if (!input.StartsWith("null"))            { success = false; break; }
                    input = input.Substring(4);
                    delegate null; // uninitialized delegate
                    value = null;
                }
                case '"':
                {
                    string str;
                    if (!stringFromJSON(ref input, ref str))  { success = false; break; }
                    value = str;
                }
                default:
                {
                    if (!numberFromJSON(ref input, ref value))  { success = false; break; }
                }
            }
            break;
        } // loop
        return success;
    }
    
    bool arrayFromJSON(ref string input, ref <variant> jsonArray)
    {
        bool success = true;
        loop
        {
            consumeLeadingWhitespace(ref input);
            if (!consume('[', ref input))             { success = false; break; }
            loop
            {
                consumeLeadingWhitespace(ref input);
                if (input.Length == 0)                      { success = false; break; }
                if (input[0] == ']')                        { input = input.Substring(1); break; }
            
                if (jsonArray.Length != 0)
                {
                    if (!consume(',', ref input))           { success = false; break; }        
                }
                
                variant value = 0;
                if (!valueFromJSON(ref input, ref value)) { success = false; break; }
                jsonArray.Append(value);
                
            } // loop
            break;
        } // loop
        return success;
    }
    
    bool objectFromJSON(ref string input, ref <string, variant> jsonObject)
    {
        bool success = true;
        loop
        {
            consumeLeadingWhitespace(ref input);
            if (!consume('{', ref input))               { success = false; break; }
            loop 
            {
                consumeLeadingWhitespace(ref input);
                if (input.Length == 0)                      { success = false; break; }
                if (input[0] == '}')                        { input = input.Substring(1); break; }
            
                if (jsonObject.Count != 0)
                {
                    if (!consume(',', ref input))           { success = false; break; }        
                }
            
                string key;
                if (!stringFromJSON(ref input, ref key))    { success = false; break; }
                consumeLeadingWhitespace(ref input);
                if (!consume(':', ref input))               { success = false; break; }
                variant value = 0; // must be initialized
                if (!valueFromJSON(ref input, ref value))   { success = false; break; }
                jsonObject[key] = value;
            } // loop
            break;
        } // loop
        return success;
    }
    
    bool TryFromJSON(string input, ref <string, variant> jsonObject)
    {
        bool success = true;
        loop
        {
            // unescape everything except the quotes (substitute rarely used alternate character ` for them)
            string unescaped;
            bool inQuotes;
            bool escape = false;
            foreach (var ch in input)
            {
                if (inQuotes)
                {
                    if (ch == '\\')   { escape = true; continue; }
                    if (escape)
                    {
                        escape = false;
                        switch (ch)
                        {
                            case '"': { ch = '`'; }
                            case 'b': { ch = char(0x08); }
                            case 't': { ch = char(0x09); }
                            case 'n': { ch = char(0x0A); }
                            case 'f': { ch = char(0x0C); }
                            case 'r': { ch = char(0x0D); }
                            default:  { success = false; break; }
                        }
                        unescaped = unescaped + ch;
                        continue;
                    }
                    if (ch == '"')
                    {
                        unescaped = unescaped + ch;
                        inQuotes = false;
                    }
                    else
                    {
                        unescaped = unescaped + ch;
                    }
                }
                else
                {
                    unescaped = unescaped + ch;
                    if (ch == '"')  { inQuotes = true; }    
                }
            }
            if (inQuotes) { success = false; break; }
            success = objectFromJSON(ref unescaped, ref jsonObject);
            break;
        } // loop
        return success;
    }
    bool TryToJSON(<string, variant> jsonObject, ref string output)
    {
        return objectToJSON(jsonObject, ref output); 
    }
    
    
}
