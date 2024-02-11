unit Variant
{
    uses "/Source/System/Diagnostics"
    
    string ToString(variant vr)
    {
        string result;
        switch (typeof(vr))
        {
            case int:    { result = (int(vr)).ToString(); }
            case char:   { result = (char(vr)).ToString(); }
            case uint:   { result = (uint(vr)).ToString(); }
            case byte:   { result = (byte(vr)).ToString(); }
            case bool:   { result = (bool(vr)).ToString(); }
            case type:   { result = (type(vr)).ToString(); }
            case long:   { long ln = vr; result = ln.ToString(); }
            case float:  { long fl = vr; result = fl.ToString(); }
            case string: { string str = vr; result = str; }
            case list:
            {
                <variant> lst = vr;
                result = lst.ToString();
            }
            case dictionary:
            {
                <variant,variant> dict = vr;
                result = dict.ToString();
            }
            case array:
            case pair:
            case enum:
            case flags:
            case file:
            case directory:
            case variant:
            case delegate:
            {
                result = "Variant.ToString(..) not implemented for " + (typeof(vr)).ToString();
            }
        }
        return result;
    }
    long ToLong(variant vr)
    {
        long result;
        switch (typeof(vr))
        {
            case int:    
            { 
                int i = int(vr); 
                result = i;
            }
            case uint:   
            { 
                uint ui = uint(vr); 
                result = ui;
            }
            case byte:   
            { 
                byte b = byte(vr);
                result = b;
            }
            case long:   
            { 
                result = vr; 
            }
            default:
            {
                Diagnostics.Die(0x09); // invalid variant type
            }
        }
        return result;
    }
}
