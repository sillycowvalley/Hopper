unit System
{

    const string HexeExtension = ".hexe";
    const string HasmExtension = ".hasm";


    uses "Char"
    uses "Bool"
    uses "Byte"
    uses "Int"
    uses "UInt"
    uses "Long"
    uses "Float"
    uses "Time"
    uses "String"
    uses "List"
    uses "Dictionary"
    uses "Pair"
    uses "Array"
    uses "File"
    uses "Path"
    uses "Directory"
    uses "Type"
    uses "Variant"
    
    <string> Arguments { get system; }
    string CurrentDirectory { get system; set system; }
    Beep() system;
}
