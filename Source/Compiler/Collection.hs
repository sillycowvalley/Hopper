unit Collection
{
    string ParseConstant(string typeExpected, ref string actualType)
    {
        string value;
        bool isArray;
        uint arrayDim;
        uint sz;
        loop
        {   
            if (typeExpected == "string")
            {
                // simple
            }
            else if (typeExpected == "byte[]")
            {
                // constant array
            }
            else if (Types.IsArray(typeExpected))
            {
                arrayDim = Types.GetArraySizeFromCollection(typeExpected);
                isArray = true;
            }
            else
            {
                Parser.ErrorAtCurrent("hex data constant expected");
                break;
            }
            Parser.Advance(); // {
            loop
            {
                <string,string> currentToken = Parser.CurrentToken;
                HopperToken ttype = Token.GetType(currentToken);
                if (ttype != HopperToken.RBrace)
                {
                    string actualElementType;
                    string byteStr = ParseConstantExpression("byte", ref actualElementType, true);
                    if (HadError)
                    {
                        break;
                    }
                    
                    uint v;
                    if (!UInt.TryParse(byteStr, ref v) || (v > 255))
                    {
                        Parser.ErrorAtCurrent("hex byte constant expected");
                        break;
                    }
                    String.Build(ref value, char(v));
                    sz++;
                    currentToken = Parser.CurrentToken;
                    ttype = Token.GetType(currentToken);
                    if (ttype == HopperToken.Comma)
                    {
                        Parser.Advance();
                        continue;
                    }
                }
                Parser.Consume(HopperToken.RBrace, "'}' expected");
                break;
            } // loop
            
            if (isArray && (arrayDim != sz))
            {
                Parser.ErrorAtCurrent("expected " + arrayDim.ToString() + " bytes of array data, was " + sz.ToString() + " bytes");
                break;
            }
            if (typeExpected == "string")
            {
                actualType = typeExpected;
            }
            else
            {
                actualType = "byte[" + sz.ToString() + "]";
            }
            break;
        } // loop
        return value;
    }
    
    
    
}
