unit Minimal
{
#ifdef VALUE_TYPE_RUNTIME    
    uses "/Source/Minimal/Char"
    uses "/Source/Minimal/Byte"
    uses "/Source/Minimal/UInt"
    uses "/Source/Minimal/Int"
#else
    uses "/Source/System/Char"
    uses "/Source/System/Byte"
    uses "/Source/System/UInt"
    uses "/Source/System/Int"
    
#endif    
    uses "System"
    uses "Diagnostics"
    uses "Time"
    
    uses  "/Source/System/IO"
    
    
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
