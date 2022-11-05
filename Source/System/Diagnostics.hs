unit Diagnostics
{
    Assert(bool assertion, string message) system;
    Assert(bool assertion, string message, string src, uint ln) system;
    
    SetError(uint error) system;

    // lastError codes:
    //   0x00 - ok
    //   0x01 - list index out of range
    //   0x02 - array index out of range
    //   0x03 - no entry for key in dictionary
    //   0x04 - division by zero attempted
    //   0x05 - string index out of range
    //   0x06 - call stack overflow
    //   0x07 - argument stack overflow
    //   0x08 - failed dynamic cast
    //   0x09 - invalid variant type
    //   0x0A - feature not implemented
    //   0x0B - system failure (internal error)
    //   0x0C - memory allocation failure
    //   0x0D - invalid or uninitialized delegate
    //   0x0E - user error (like compilation failure)
    Die(uint error) system;

    OutputDebug(string output) system;
    OutputDebug(<string,string> output) system;
    OutputDebug(<string> output) system;
    OutputDebug(< <string> > output) system;
    OutputDebug(< <uint> > output) system;
    OutputDebug(< uint > output) system;
}
