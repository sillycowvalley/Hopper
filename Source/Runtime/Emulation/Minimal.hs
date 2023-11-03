unit Minimal
{
    uses  "/Source/System/Char"
    uses  "/Source/System/Byte"
    uses  "/Source/System/UInt"
    uses  "/Source/System/Int"
    uses  "/Source/System/IO"
    
    uses "/Source/Runtime/Emulation/System"
    uses "/Source/Runtime/Emulation/Diagnostics"
    uses "/Source/Runtime/Emulation/Time"
    uses "/Source/Runtime/Emulation/Long"
    
    byte error;
    byte Error 
    { set { error = value; SetError(error); } get { return error; }}
    
    
    
}
