unit Type
{
    string ToString(type this)
    {
        string result;
        switch (this)
        {
            case int:
            {
                result = "int";
            }
            case char:
            {
                result = "char";
            }
            case uint:
            {
                result = "uint";
            }
            case byte:
            {
                result = "byte";
            }
            case long:
            {
                result = "long";
            }
            case float:
            {
                result = "float";
            }
            case string:
            {
                result = "string";
            }
            case enum:
            {
                result = "enum";
            }
            case flags:
            {
                result = "flags";
            }
            case pair:
            {
                result = "pair";
            }
            case file:
            {
                result = "file";
            }
            case directory:
            {
                result = "directory";
            }
            case array:
            {
                result = "array";
            }
            case list:
            {
                result = "list";
            }
            case dictionary:
            {
                result = "dictionary";
            }
            case variant:
            {
                result = "variant";
            }
            default:
            {
                result = "unknown type in Type.ToString()";
            }
        }
        return result;
    }
}
