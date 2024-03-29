unit Minimal
{
    uses  "/Source/System/Char"
    uses  "/Source/System/Byte"
    uses  "/Source/System/UInt"
    uses  "/Source/System/Int"
    uses  "/Source/System/IO"
    
    uses "System"
    uses "Diagnostics"
    uses "Time"
    
#ifdef INCLUDE_LONGS    
    uses "Long"
#endif
#ifdef INCLUDE_FLOATS    
    uses "Float"
#endif
    
    byte error;
    byte Error 
    { set { error = value; SetError(error); } get { return error; }}
}
