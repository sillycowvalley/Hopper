unit TinyType
{
    bool IsTypeCompatible(string expectedType, string actualType)
    {
        if (expectedType.StartsWith("const ") && !actualType.StartsWith("const "))
        {
            expectedType = expectedType.Substring(6); // non-const -> const
        }
        if (expectedType.StartsWith("const ") && actualType.StartsWith("const "))
        {
            expectedType = expectedType.Substring(6); // const -> const
            actualType   = actualType.Substring(6);
        }
        
        // Check for exact match
        if (expectedType == actualType)
        {
            return true;
        }
    
        // Implement additional logic for type compatibility
        // For example, handle implicit conversions, array size checks, etc.
        switch (expectedType)
        {
            case "word":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        return true; // byte as word
                    }
                    case "+int":
                    {
                        return true; // positive int as word
                    }
                    case "int":
                    {
                        return false; // int as word requires cast
                    }
                    default:
                    {
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented
                    }
                }
            }
            case "int":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        return true; // byte as int
                    }
                    case "+int":
                    {
                        return true; // internal int ranges are int
                    }
                    case "word":
                    {
                        return false; // word as int requires cast
                    }
                    default:
                    {
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented
                    }
                }
            }
            default:
            {
                TypeError(expectedType, actualType);
                PrintLn(actualType + " as " + expectedType);
                Die(0x0A); // not implemented
            }
        }
        return false;
    }
    TypeError(string expected, string actual)
    {
        Token token = TinyScanner.Current();
        Error(token.SourcePath, token.Line, "type mismatch: expected '" + expected +"', got '" + actual + "'");
    }
                
}
