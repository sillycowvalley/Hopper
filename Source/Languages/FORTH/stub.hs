

program Stub
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "/Source/Languages/FORTH/Precompiled"
    
     // size of array must be specified at compile time .. could improve this ..
    byte[0x2000] binary;
    
    {
        // The compiler isn't great with string constant literals.
        // Works better if it is a string variable like this:
        
        string precompiled = PRECOMPILED; // (only 'created' as a string once)
            
        uint n = byte(precompiled[0])  + byte(precompiled[1]) << 8;
        PrintLn("0x" +   n.ToHexString(4));
        
        // The problem with interacting directly with the constant literal
        // is that the entire thing is created as a string and put on the stack
        // every time you refer to it.  It works, but it would be expensive:
        PrintLn(("Constant Literal").Substring(9,7));
        
        n = byte((PRECOMPILED).GetChar(0)) + byte((PRECOMPILED).GetChar(1)) << 8;
        PrintLn("0x" +   n.ToHexString(4));
        
        PrintLn("String length: " +   precompiled.Length.ToString());
        
        // Export the string to a binary file:
        file binFile = File.Create("/Source/Languages/FORTH/Meta-Compiler/precompiled.bin");
        foreach (var ch in precompiled)
        {
            binFile.Append(byte(ch));
        }
        binFile.Flush();
        
        // Reading the binary file back into an array of byte
        binFile = File.Open("/Source/Languages/FORTH/Meta-Compiler/precompiled.bin");
        uint i; // initialized to zero by default
        loop
        {
            // The last Read will be 'invalid' which will append a 0 at the end of the data
            byte data = binFile.Read(); 
            if (!binFile.IsValid())  { break; } 
            binary[i] = data;
            i++;
        }
        
        PrintLn("Data length:   " +   i.ToString()); // extra i++ before IsValid() failed
    }
}
