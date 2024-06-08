unit TinyType
{
    bool IsIndexType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|char|byte|word|+int|").Contains(typeName);
    }
    bool IsNumericType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|char|byte|word|int|+int|").Contains(typeName);
    }
    bool IsBoolableType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|char|byte|word|int|+int|bool|").Contains(typeName);
    }
    string GetArrayMemberType(string arrayType)
    {
        string memberType = arrayType;
        uint index;
        if (memberType.IndexOf('[', ref index))
        {
            memberType = memberType.Substring(0, index);
            memberType = memberType.Replace("const ", "");
        }
        return memberType;
    }
    string WiderType(string leftType, string rightType)
    {
        if ((leftType == "word") || (rightType == "word"))
        {
            return "word";
        }
        if ((leftType == "int") || (rightType == "int"))
        {
            return "int";
        }
        if ((leftType == "+int") || (rightType == "+int"))
        {
            return "+int";
        }
        if ((leftType == "byte") || (rightType == "byte"))
        {
            return "byte";
        }
        if ((leftType == "char") || (rightType == "char"))
        {
            return "char";
        }
        return leftType; // bool?
    }
    
    bool IsAutomaticCast(string expectedType, string actualType)
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
            case "byte":
            {
                switch (actualType)
                {
                    case "char":
                    {
                        return true; // char as byte
                    }
                    case "int":
                    {
                        return false; // int as byte requires cast
                    }
                    case "word":
                    {
                        return false; // word as byte requires cast
                    }
                    default:
                    {
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented
                    }
                }
            }
            case "char":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        return true; // byte as char
                    }
                    case "int":
                    {
                        return false; // int as char requires cast
                    }
                    case "word":
                    {
                        return false; // word as char requires cast
                    }
                    default:
                    {
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented
                    }
                }
            }
            case "word":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        return true; // byte as word
                    }
                    case "char":
                    {
                        return true; // char as word
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
                    case "char":
                    {
                        return true; // char as int
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
