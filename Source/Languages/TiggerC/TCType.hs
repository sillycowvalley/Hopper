unit TCType
{
    bool IsIndexType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|byte|word|+int|").Contains(typeName);
    }
    bool IsNumericType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|byte|word|int|+int|").Contains(typeName);
    }
    bool IsByteType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|byte|char|bool|").Contains(typeName);
    }
    bool IsSignedType(string typeName)
    {
        typeName = "|" + typeName.Replace("const ", "") + "|";
        return ("|int|+int|").Contains(typeName);
    }
    bool IsPointerType(string typeName)
    {
        return typeName.EndsWith("[]");
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
    bool IsArrayType(string arrayType, ref string memberType)
    {
        if (arrayType.Contains('['))
        {
            memberType = GetArrayMemberType(arrayType);
            return true;
        }
        return false;
    }
    
    bool MatchNumericTypesForAddition(string otherType, ref string actualType)
    {
        if ( ((otherType == "const char") || (otherType == "char")) && ((actualType == "const char") || (actualType == "char")) )
        {
            return true;
        }
        
        bool leftPointer  = otherType.EndsWith("[]");
        bool rightPointer = actualType.EndsWith("[]");
        if (leftPointer || rightPointer)
        {
            string leftType  = otherType.Replace ("const ", "");
            string rightType = actualType.Replace("const ", "");
            if (leftPointer && rightPointer)
            {
                return leftType == rightType;
            }
            else if (leftPointer)
            {
                if (IsAutomaticCast("word", rightType, false, false))
                {
                    actualType = otherType;
                    return true;
                }
                return false;
            }
            else // rightPointer
            {
                return IsAutomaticCast("word", otherType, false, false);
            }
        }
        return MatchNumericTypes(otherType, ref actualType);
    }
    bool MatchNumericTypesForRelational(string otherType, ref string actualType)
    {
        if ( ((otherType == "const char") || (otherType == "char")) && ((actualType == "const char") || (actualType == "char")) )
        {
            return true;
        }
        return MatchTypes(otherType, ref actualType);
    }
    bool MatchNumericTypes(string otherType, ref string actualType)
    {
        if ((otherType == "char") || (otherType == "bool") || (actualType == "char") || (actualType == "bool"))
        {
            return false;
        }
        if ((otherType == "const char") || (otherType == "const bool") || (actualType == "const char") || (actualType == "const bool"))
        {
            return false;
        }
        return MatchTypes(otherType, ref actualType);
    }
    
    bool MatchBoolTypes(string otherType, string actualType)
    {
        return ((otherType == "bool") || (otherType == "const bool")) && ((actualType == "bool") || (actualType == "const bool"));
    }
    bool MatchNumericLiteral(string actualType, string literalType)
    {
        if (IsNumericType(actualType) && IsNumericType(literalType))
        {
            if (IsByteType(actualType))
            {
                return IsByteType(literalType); // if target is byte then literal must be byte
            }
            if (IsByteType(literalType))
            {
                return true; // byte is compatible with uint, int, int+ or byte
            }
            switch (literalType)
            {
                case "const byte":
                case "byte":
                {
                    return true; // byte is compatible with any numeric type
                }
                case "const +int":
                case "+int":
                {
                    return (actualType == "word") || (actualType == "+int") || (actualType == "int");
                }
                case "const int":
                case "int":
                {
                    return (actualType == "+int") || (actualType == "int");
                }
                case "const word":
                case "word":
                {
                    return (actualType == "+int") || (actualType == "word");
                }
                default:
                {
                    Print("literalType="+ literalType);
                    Die(0x0A); // ?
                }
            }
        }
        return false;
    }
    
    bool MatchTypes(string otherType, ref string actualType)
    {
        if (otherType != actualType)
        {
            if (IsAutomaticCast(otherType, actualType, true, false))
            {
                actualType = otherType; // PrintLn(" true:" + actualType);
            }
            else if (IsAutomaticCast(actualType, otherType, false, false))
            {
                otherType = actualType; // PrintLn(" false:" + actualType);
            }
        }
        return otherType == actualType;
    }
    
    bool IsAutomaticCast(string expectedType, string actualType, bool doUnder, bool asCast)
    {
        if (expectedType.Contains("[") && actualType.Contains("[") && asCast)
        {
            return true; // any pointer type can be deliberately cast to any other pointer type
        }
        
        if (expectedType.StartsWith("const ") && !actualType.StartsWith("const "))
        {
            expectedType = expectedType.Substring(6); // non-const -> const
        }
        if (actualType.StartsWith("const ") && !expectedType.StartsWith("const "))
        {
            actualType = actualType.Substring(6); // non-const -> const
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
        
        if (expectedType.Contains("[") && actualType.Contains("["))
        {
            if (expectedType.EndsWith("[]"))
            {
                string memberType;
                _ = IsArrayType(actualType, ref memberType);
                // type[nn] -> type[] is fine
                return (expectedType == memberType + "[]") || (expectedType == memberType + "[]");
            }
        }
        
        
        // Implement additional logic for type compatibility
        // For example, handle implicit conversions, array size checks, etc.
        switch (expectedType)
        {
            case "bool":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        if (asCast)
                        {
                            TCCode.ToBool(true, doUnder);
                        }
                        return asCast; // byte as bool requires cast
                    }
                    case "char":
                    {
                        if (asCast)
                        {
                            TCCode.ToBool(true, doUnder);
                        }
                        return asCast; // char as bool requires cast
                    }
                    case "word":
                    {
                        if (asCast)
                        {
                            TCCode.ToBool(false, doUnder);
                        }
                        return asCast; // word as bool requires cast
                    }
                    case "int":
                    {
                        if (asCast)
                        {
                            TCCode.ToBool(false, doUnder);
                        }
                        return asCast; // word as int requires cast
                    }
                    case "+int":
                    {
                        if (asCast)
                        {
                            TCCode.ToBool(false, doUnder);
                        }
                        return asCast; // word as +int requires cast
                    }
                    default:
                    {
                        string memberType;
                        if (IsArrayType(actualType, ref memberType))
                        {
                            return false;
                        }
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented?
                    }
                }
            }
            case "byte":
            {
                switch (actualType)
                {
                    case "char":
                    {
                        return asCast; // char as byte requires cast
                    }
                    case "int":
                    {
                        if (asCast)
                        {
                            TCCode.WordToByte(doUnder, "byte");
                        }
                        return asCast; // int as byte requires cast
                    }
                    case "+int":
                    {
                        if (asCast)
                        {
                            TCCode.WordToByte(doUnder, "byte");
                        }
                        return asCast; // +int as byte requires cast
                    }
                    case "word":
                    {
                        if (asCast)
                        {
                            TCCode.WordToByte(doUnder, "byte");
                        }
                        return asCast; // word as byte requires cast
                    }
                    case "bool":
                    {
                        return asCast; // bool as byte requires cast
                    }
                    default:
                    {
                        string memberType;
                        if (IsArrayType(actualType, ref memberType))
                        {
                            return false;
                        }
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented?
                    }
                }
            }
            case "char":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        return asCast; // byte as char requires cast
                    }
                    case "int":
                    {
                        return false; // int as char requires cast
                    }
                    case "+int":
                    {
                        return false; // +int as char requires cast
                    }
                    case "word":
                    {
                        if (asCast)
                        {
                            TCCode.WordToByte(doUnder, "char");
                        }
                        return asCast; // word as char requires cast
                    }
                    case "bool":
                    {
                        return asCast; // bool as char requires cast
                    }
                    default:
                    {
                        string memberType;
                        if (IsArrayType(actualType, ref memberType))
                        {
                            return false;
                        }
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented?
                    }
                }
            }
            case "word":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        TCCode.CastPad(doUnder);
                        return true; // byte as word
                    }
                    case "char":
                    {
                        if (asCast)
                        {
                            TCCode.CastPad(doUnder);
                        }
                        return asCast; // char as word requires cast
                    }
                    case "+int":
                    {
                        return true; // positive int as word
                    }
                    case "int":
                    {
                        return asCast; // int as word requires cast
                    }
                    case "bool":
                    {
                        if (asCast)
                        {
                            TCCode.CastPad(doUnder);
                        }
                        return asCast; // bool as word requires cast
                    }
                    default:
                    {
                        string memberType;
                        if (IsArrayType(actualType, ref memberType))
                        {
                            return asCast; // type[] as word requires cast
                        }
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented?
                    }
                }
            }
            case "int":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        TCCode.CastPad(doUnder);
                        return true; // byte as int
                    }
                    case "char":
                    {
                        if (asCast)
                        {
                            TCCode.CastPad(doUnder);
                        }
                        return asCast; // char as int requires cast
                    }
                    case "+int":
                    {
                        return true; // internal int ranges are int
                    }
                    case "word":
                    {
                        return asCast; // word as int requires cast
                    }
                    case "bool":
                    {
                        if (asCast)
                        {
                            TCCode.CastPad(doUnder);
                        }
                        return asCast; // bool as int requires cast
                    }
                    default:
                    {
                        string memberType;
                        if (IsArrayType(actualType, ref memberType))
                        {
                            return asCast; // type[] as int requires cast
                        }
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented
                    }
                }
            }
            case "+int":
            {
                switch (actualType)
                {
                    case "byte":
                    {
                        TCCode.CastPad(doUnder);
                        return true; // byte as +int
                    }
                    case "char":
                    {
                        if (asCast)
                        {
                            TCCode.CastPad(doUnder);
                        }
                        return asCast; // char as +int requires cast
                    }
                    case "int":
                    {
                        return false; // int as +int requires cast?!?!
                    }
                    case "word":
                    {
                        return false; // word as +int requires cast
                    }
                    case "bool":
                    {
                        if (asCast)
                        {
                            TCCode.CastPad(doUnder);
                        }
                        return asCast; // bool as +int requires cast
                    }
                    default:
                    {
                        string memberType;
                        if (IsArrayType(actualType, ref memberType))
                        {
                            return asCast; // type[] as +int requires cast
                        }
                        TypeError(expectedType, actualType);
                        PrintLn(actualType + " as " + expectedType);
                        Die(0x0A); // not implemented?
                    }
                }
            }
            default:
            {
                string memberType;
                if (IsArrayType(expectedType, ref memberType))
                {
                    return false;
                }
                TypeError(expectedType, actualType);
                PrintLn(actualType + " as " + expectedType + " not implemented");
                //Die(0x0A); // not implemented?
            }
        }
        return false;
    }
    TypeError(string expected, string actual)
    {
        Token token = TCScanner.Current();
        Error(token.SourcePath, token.Line, "type mismatch: expected '" + expected +"', was '" + actual + "'");
    }
                
}
