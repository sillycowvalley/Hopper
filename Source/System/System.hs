unit System
{
    
    //const string hexeExtension = ".hexe2";
    //const string hasmExtension = ".hasm2";
    const string hexeExtension = ".hexe";
   	const string hasmExtension = ".hasm";
    
#ifndef H6502
    bool Trace { get { return false; }  set { } }
    bool Warp  { get { return false; }  set { } }
#endif
    
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
    
    <string> Arguments { get system; }
    string CurrentDirectory { get system; set system; }
    Beep() system;
    
    // launch another application (on exit, restore the currently running one)
    uint Execute(string programPath, <string> arguments) system;
    
}
