program Command
{
//#define SERIAL_CONSOLE
    uses "Common"
    
    string Name                 { get { return "COPY";  } }
    string Description          { get { return "copy one or more files between directories (alias CP)"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return true;  } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return true;  } }
    bool   SupportsMask         { get { return true;  } } // *.*
    bool   SupportsRecursive    { get { return false; } } // -s
    bool   SupportsConfirmation { get { return true;  } } // -y
    bool   RequiresArguments    { get { return true;  } }
    
    ShowArguments() {}
    bool Argument(string arg) { return false; }
    bool OnDirectory(ShellObject shellObject) { return true; }
    
    uint copies = 0;
    bool OnFile(ShellObject shellObject, bool first, uint maxLength)
    {
        string sourcePath = shellObject.Path;
        string fileName = Path.GetFileName(sourcePath);
        string destinationPath = Path.Combine(Common.DestinationFolder, fileName);
        bool success = File.Copy(sourcePath, destinationPath, true);
        if (success)
        {
            copies++;
        }
        return success;
    } 
    
    {
        if (Common.Arguments()) 
        { 
            Common.Walk(); 
            if (copies > 0)
            {
                string plural = (copies != 1) ? "s" : "";
                Common.Write(copies.ToString() + " file" + plural + " copied", Colour.MatrixBlue);
                IO.WriteLn();
            }
        }
    }
}

