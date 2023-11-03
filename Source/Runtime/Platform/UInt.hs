unit HRUInt
{    
    uint ToLong(uint ui)
    {
        uint this = HRLong.New();
        WriteWord(this+2, ui);
        WriteWord(this+4, 0);
        return this;
    }
}
