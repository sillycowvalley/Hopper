unit Bool
{
    string ToString(bool this)
    {
        if (this)
        {
            return "true";
        }
        return "false";
    }
}