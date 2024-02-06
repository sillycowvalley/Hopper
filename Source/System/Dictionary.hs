unit Dictionary
{
    uint Count { get system; }
    Set(<K,V> this, K key, V value) system;
    bool Contains(<K,V> this, K key) system;
    V Get(<K,V> this, K key) system;
    Clear(<K,V> this) system;
    
#ifdef HOPPER_6502    
    long HashKey(string str) system;
#endif    

    string ToString(<variant,variant> this)
    {
        string result = "<";
        foreach (var di in this)
        {
            if (result != "<")
            {
                result += ", ";
            }
            result += "<" + (di.key).ToString() + ", " + (di.value).ToString() + ">";
        }
        result += ">";
        return result;
    }
}
