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
}
