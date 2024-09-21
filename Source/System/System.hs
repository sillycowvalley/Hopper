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
    uses "Path"
    uses "Type"
    uses "Variant"
    
#if !defined(BLOCKFILESYSTEM)
    uses "File"
    uses "Directory"
#endif    
    
    <string> Arguments { get system; }
#if !defined(BLOCKFILESYSTEM)    
    string CurrentDirectory { get system; set system; }
#else
    string CurrentDirectory 
    { 
        get { return FileSystem.getCwd(); }
        set { _ = FileSystem.chDir(value); } // error check?
    }
#endif
    Beep() system;
}
