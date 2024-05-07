unit Bool
{
    // Converts a boolean to its string representation: "true" or "false"
    string ToString(bool this)
    {
        if (this)
        {
            return "true";
        }
        else
        {
            return "false";
        }
    }
    
    // Converts a string to a boolean value. Returns true if string is "true", false otherwise.
    bool TryParse(string content, ref bool returnValue)
    {
        if (content.ToLower() == "true")
        {
            returnValue = true;
            return true;
        }
        else if (content.ToLower() == "false")
        {
            returnValue = false;
            return true;
        }
        return false;
    }
}
