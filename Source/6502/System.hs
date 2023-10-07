unit System
{
    #define H6502
    
    bool Trace { get system; set system; }
    bool Warp { get system; set system; }
    
    uses "/Source/6502/Firmware/Hardware"
    uses "/Source/6502/Firmware/Memory"
    
    uses "/Source/System/Char"
    uses "/Source/System/Bool"
    uses "/Source/System/Byte"
    uses "/Source/System/Int"
    uses "/Source/System/UInt"
    
#ifndef TINYHOPPER    
    uses "/Source/System/Long"
    //uses "/Source/System/Float"
    uses "/Source/System/String"
    uses "/Source/System/Array"
    uses "/Source/System/List"
    uses "/Source/System/Dictionary"
    uses "/Source/System/Pair"
    uses "/Source/System/Type"
#endif
    
    uses "/Source/System/Screen"
    uses "/Source/System/Diagnostics" // for Diagnostics.Die and Diagnostics.Trace
    uses "/Source/System/Time"
    uses "/Source/System/Serial"
    uses "/Source/System/IO"
    
}
