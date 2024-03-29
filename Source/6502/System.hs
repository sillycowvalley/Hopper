unit System
{
    #define HOPPER_6502
    
    
    uses "/Source/6502/Firmware/Hardware"
    uses "/Source/6502/Firmware/Memory"
    
    uses "/Source/System/Char"
    uses "/Source/System/Bool"
    uses "/Source/System/Byte"
    uses "/Source/System/Int"
    uses "/Source/System/UInt"
    
    uses "/Source/System/Long"
    uses "/Source/System/String"
    uses "/Source/System/Array"
    uses "/Source/System/List"
    uses "/Source/System/Dictionary"
    uses "/Source/System/Pair"
    uses "/Source/System/Type"
    
    uses "/Source/System/Screen"
    uses "/Source/System/Diagnostics" // for Diagnostics.Die and Diagnostics.Trace
    uses "/Source/System/Time"
    uses "/Source/System/Serial"
    uses "/Source/System/IO"
    
    bool Trace { get system; set system; }
    bool Warp { get system; set system; }

    // execute an array of Hopper opCodes inline (starting at the opcode at 'index')
    //   (use & operator to determine offsets of locals and globals)
    uint Inline(byte[] code, uint index) system;
}
