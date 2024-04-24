program rZ80Gen
{
    #define CPU_Z80
    #define ROM_16K
    
    uses "/Source/Minimal/Type"
    uses "/Source/Minimal/Diagnostics"
    
    uses "Z80/String"
    uses "Z80/Array"
    uses "Z80/Memory"
    uses "Z80/GC"
       
    Hopper()
    {
        uint str = String.New();
        str =  String.NewFromConstant0(0, 0);
        _ =  String.NewFromConstant1(0xAA55);
        _ =  GC.Clone(str);
        _ =  String.GetChar(str, 0);
        GC.Release(str);
        
        _ = Memory.Available();
        _ = Memory.Maximum();
        
        String.BuildClear(ref str);
        String.BuildChar(ref str, 'a');
        String.BuildString(ref str, str);
        String.BuildFront(ref str, 'a');
        
        _ = String.GetLength(str);
        uint arr = Array.New(10, Type.UInt);
        _ = Array.NewFromConstant(0, 10, Type.Byte);
        _ = Array.GetCount(arr);
        _ = Array.GetItem(arr, 0);
        Array.SetItem(arr, 0, 0);
    }
}
