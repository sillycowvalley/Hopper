unit List
{
    uint Length { get system; }
    Append(<V> this, V value) system;
    Remove(<V> this, uint index) system;
    V GetItem(<V> this, uint index) system;
    SetItem(<V> this, uint index, V value) system;
    Insert(<V> this, uint index, V value) system;
    Clear(<V> this) system;
    
    bool Contains(<V> this, V value) system;
    
    string ToString(<variant> this)
    {
        string result;
        foreach (var vi in this)
        {
            if (result == "")
            {
                result = "<";
            }
            else
            {
                result += ", ";
            }
            result += vi.ToString();
        }
        result += ">";
        return result;
    }
}
