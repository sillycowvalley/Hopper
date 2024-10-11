unit System
{
    #define MINIMAL_RUNTIME
#ifdef CPU_Z80
    #define NO_JIX_INSTRUCTIONS
#endif
    //#define NO_PACKED_INSTRUCTIONS
    
    uses "Char"
    uses "Bool"
    uses "Byte"
    uses "Int"
    uses "UInt"
    uses "Time"
    uses "String"
    uses "Array"
    uses "Type"
    
    uses "Long"
    uses "Float"
    uses "List"
    
    uses "Path"
    
    uses "Serial"
    uses "Diagnostics"
    
    uses "Runtime"

#if defined(BLOCKFILESYSTEM)
    string CurrentDirectory 
    { 
        get { return FileSystem.getCwd(); }
        set { _ = FileSystem.chDir(value); } // error check?
    }
#endif
    
    
}
