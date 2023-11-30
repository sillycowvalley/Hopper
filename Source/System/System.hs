unit System
{

    const string hexeExtension = ".hexe";
   	const string hasmExtension = ".hasm";


    uses "/Source/System/Char"
    uses "/Source/System/Bool"
    uses "/Source/System/Byte"
    uses "/Source/System/Int"
    uses "/Source/System/UInt"
    uses "/Source/System/Long"
    uses "/Source/System/Float"
    uses "/Source/System/Time"
    uses "/Source/System/String"
    uses "/Source/System/List"
    uses "/Source/System/Dictionary"
    uses "/Source/System/Pair"
    uses "/Source/System/Array"
    uses "/Source/System/File"
    uses "/Source/System/Path"
    uses "/Source/System/Directory"
    uses "/Source/System/Type"
    
    uint     HexeVersion { get system ; }
    <string> Arguments { get system; }
    string CurrentDirectory { get system; set system; }
    Beep() system;
}
