unit List
{
    delegate int ListCompareDelegate(<V> this, uint ai, uint bi);
    
    uint Count   { get system; }
    Append(<V> this, V value) system;
    Remove(<V> this, uint index) system;
    V GetItem(<V> this, uint index) system;
    SetItem(<V> this, uint index, V value) system;
    Insert(<V> this, uint index, V value) system;
    Clear(<V> this) system;
    
    bool Contains(<V> this, V value) system;
    
    string ToString(<variant> this)
    {
        string result = "<";
        foreach (var vi in this)
        {
            if (result != "<")
            {
                result += ", ";
            }
            result += vi.ToString();
        }
        result += ">";
        return result;
    }
    
    swap(<variant> this, uint ia, uint ib)
    {
        variant tmp = this.GetItem(ia);
        this.SetItem(ia, this.GetItem(ib));
        this.SetItem(ib, tmp);
    }
       
    // Inspired by: https://www.geeksforgeeks.org/c-bubble-sort/
    // Yes, I know bubblesort is slow but it is super simple and not recursive.
    Sort(<V> this, ListCompareDelegate comparer)
    {
        uint count = this.Count;
        for (uint i = 0; i < count - 1; i++) 
        {
            bool swapped; 
            for (uint j = 0; j < count - i - 1; j++) 
            {
                if (comparer(this, j, j+1) > 0) 
                {
                    swap(this, j, j + 1);
                    swapped = true;
                }
            }
            if (!swapped) { break; }
        }
    }
}
