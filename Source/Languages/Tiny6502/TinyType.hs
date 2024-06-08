unit TinyType
{
    bool IsTypeCompatible(string expectedType, string actualType)
    {
        // Check for exact match
        if (expectedType == actualType)
        {
            return true;
        }
    
        // Implement additional logic for type compatibility
        // For example, handle implicit conversions, array size checks, etc.
    
        return false;
    }
    TypeError(string expected, string actual)
    {
        Token token = TinyScanner.Current();
        Error(token.SourcePath, token.Line, "type mismatch: expected '" + expected +"', got '" + actual + "'");
    }
                
}
