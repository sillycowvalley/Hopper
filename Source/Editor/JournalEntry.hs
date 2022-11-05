unit JournalEntry
{
    // JournalEntry 
    //   c : char
    //   x : uint
    //   y : uint
    //   t : uint   0=insertion, type 1=deletion
    
    // JournalRecord < JournalEntry >
    
    < <string, uint> > New()
    {
        < <string, uint> > instance;
        
        //instance["c"] = 0; // empty char, uninitialized
        //instance["x"] = 0;
        //instance["y"] = 0;
        //instance["t"] = 0;
        
        return instance;
    }
    
    < < <string, uint > > > NewJournalRecord()
    {
        < < <string, uint> > > instance;
        return instance;
    }
    
}