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
            case bool:
            {
                result = "bool";
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
            case list: // same value as 'record' for now : RECORD
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
            case delegate:
            {
                result = "delegate";
            }
            case type:
            {
                result = "type";
            }
            default:
            {
                byte b = byte(this);
                result = "unknown type 0x" + b.ToHexString(2) +"in Type.ToString()";
            }
        }
        return result;
    }
}
