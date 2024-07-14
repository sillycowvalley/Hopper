unit SysCalls
{
    <string,byte> syscalls;

    addEntry(string sysCallName)
    {
        syscalls[sysCallName] = byte(syscalls.Count);
    }
    
    New()
    {
        syscalls.Clear(); // in case called a 2nd time
        
        addEntry("String.NewFromConstant");
        addEntry("String.Build");
        addEntry("String.New");
        addEntry("String.BuildFront");
        addEntry("Array.NewFromConstant");
        addEntry("Time.Seconds_Get");
        addEntry("String.Length_Get");
        addEntry("Time.Delay");
        addEntry("Diagnostics.Die");
        addEntry("Serial.Connect");
        addEntry("String.GetChar");
        
        addEntry("Array.New");
        addEntry("Array.Count_Get");
        addEntry("Array.GetItem");
        addEntry("Array.SetItem");
        
        addEntry("Serial.ReadChar");
        addEntry("Serial.WriteChar");
        addEntry("Serial.IsAvailable_Get");
        addEntry("Memory.ReadByte");
        addEntry("Memory.WriteByte");
        addEntry("Memory.Available");
        addEntry("Memory.Maximum");
        addEntry("Memory.Allocate");
        addEntry("Memory.Free");
        addEntry("Byte.ToHex");
        
        addEntry("Int.GetByte");
        addEntry("Int.FromBytes");
        
        addEntry("Array.Slice");
        addEntry("Array.ItemType_Get");
        
        addEntry("Long.New");
        addEntry("Long.NewFromConstant");
        addEntry("Long.FromBytes");
        addEntry("Long.GetByte");
        
        addEntry("Float.New");
        addEntry("Float.NewFromConstant");
        addEntry("Float.FromBytes");
        addEntry("Float.GetByte");
        
        addEntry("Time.Millis_Get");
        
        addEntry("_Variant.Type");
        
        addEntry("Variant.Box");
        addEntry("Variant.UnBox");
        
        addEntry("Screen.Print");
        addEntry("Screen.PrintLn");
        addEntry("Screen.Clear");
        addEntry("Screen.SetCursor");
        addEntry("Screen.Columns_Get");
        addEntry("Screen.Rows_Get");
        addEntry("Screen.CursorX_Get");
        addEntry("Screen.CursorY_Get");
        addEntry("Screen.Suspend");
        addEntry("Screen.Resume");
        addEntry("Screen.DrawChar");
        
        addEntry("Int.ToFloat");
        addEntry("Int.ToLong");
        
        addEntry("UInt.ToLong");
        addEntry("UInt.ToInt");
        
        addEntry("Long.ToString");
        addEntry("UInt.GetByte");
        addEntry("Long.ToFloat");
        addEntry("Long.ToInt");
        addEntry("Long.ToUInt");
        addEntry("UInt.FromBytes");
        addEntry("Time.SampleMicros_Get");
        addEntry("Long.Add");
        addEntry("Long.Sub");
        addEntry("Long.Div");
        addEntry("Long.Mul");
        addEntry("Long.Mod");
        addEntry("Long.EQ");
        addEntry("Long.LT");
        addEntry("Long.LE");
        addEntry("Long.GT");
        addEntry("Long.GE");
        addEntry("Long.Negate");
        
        addEntry("Float.ToString");
        addEntry("Time.SampleMicros_Set");
        addEntry("_Float.New");
        addEntry("_Float.NewFromConstant");
        addEntry("Float.Add");
        addEntry("Float.Sub");
        addEntry("Float.Div");
        addEntry("Float.Mul");
        addEntry("Float.EQ");
        addEntry("Float.LT");
        addEntry("Float.LE");
        addEntry("Float.GT");
        addEntry("Float.GE");
        
        addEntry("_Time.Millis_Get");
        addEntry("Screen.ShowCursor_Set");
        
        addEntry("System.Arguments_Get");
        addEntry("System.CurrentDirectory_Get");
        addEntry("System.CurrentDirectory_Set");
        addEntry("System.Beep");
        
        addEntry("Pair.New");
        addEntry("String.Append");
        
        addEntry("File.Exists");
        addEntry("File.New");
        addEntry("File.Open");
        addEntry("File.Create");
        addEntry("File.ReadLine");
        addEntry("File.Read");
        addEntry("File.IsValid");
        addEntry("File.Append");
        addEntry("File.Flush");
        addEntry("File.Delete");
        addEntry("File.GetSize");
        
        addEntry("Directory.Exists");
        addEntry("Directory.New");
        addEntry("Directory.IsValid");
        addEntry("Directory.Open");
        addEntry("Directory.GetDirectoryCount");
        addEntry("Directory.GetFileCount");
        addEntry("Directory.GetFile");
        addEntry("Directory.GetDirectory");
        
        addEntry("Keyboard.ReadKey");
        addEntry("Keyboard.IsAvailable_Get");
        addEntry("Keyboard.ToKey");
        
        addEntry("Keyboard.ClickX_Get");
        addEntry("Keyboard.ClickY_Get");
        addEntry("Keyboard.ClickUp_Get");
        addEntry("Keyboard.ClickDouble_Get");
        addEntry("Keyboard.ScrollDelta_Get");
        
        addEntry("Diagnostics.OutputDebug");
        addEntry("Diagnostics.Assert");
        addEntry("_Diagnostics.Die2");
        addEntry("Diagnostics.SetError");

        addEntry("Types.TypeOf");      // gives 'true' type (for instance, "variant" and not the boxed contents of a variant)
        addEntry("Types.ValueTypeOf"); // if the type is a list, dictionary, pair or array, then return its value type
        addEntry("Types.KeyTypeOf");   // if the type is a dictionary then return its key type
        addEntry("Types.BoxTypeOf");   // regular "TypeOf" except for boxed variants where the boxed value type is returned (used for "typeof")
        addEntry("Types.VerifyValueTypes");
        
        addEntry("_String.Build2");
        
        addEntry("WiFi.Connect");
        addEntry("WiFi.IP_Get");
        addEntry("WiFi.Status_Get");
        addEntry("WiFi.Disconnect");
        
        addEntry("_Array.NewFromConstant2");
        
        addEntry("Directory.Create");
        addEntry("Directory.Delete");
        
        addEntry("Runtime.PC_Get");
        addEntry("Runtime.SP_Get");
        addEntry("Runtime.BP_Get");
        addEntry("Runtime.CSP_Get");
        addEntry("Runtime.GetStackWord");
        addEntry("Runtime.GetStackType");
        addEntry("Runtime.GetCallStackWord");
        addEntry("Runtime.Execute");
        addEntry("Runtime.Inline");
        addEntry("Runtime.UserCode_Get");
        
        
        addEntry("Time.Time_Get");
        addEntry("Time.Date_Get");
        
        addEntry("Runtime.InDebugger_Get");
        addEntry("Runtime.DateTime_Get");
        
        addEntry("Memory.ReadProgramByte");
        addEntry("Memory.WriteProgramByte");
        addEntry("Memory.ReadProgramWord");
        addEntry("Memory.WriteProgramWord");
        
        addEntry("File.GetDate");
        addEntry("Directory.GetDate");
        
        addEntry("Memory.ProgramOffset_Set");
        
        addEntry("Serial.WriteString"); // A0
        
        // placeholder:
        addEntry("Pair.Key");  // A1
        
        addEntry("Dictionary.New");
        addEntry("Serial.Close");
        addEntry("Serial.IsValid");
        addEntry("String.InsertChar"); // A5
        addEntry("Pair.Value");        // A6
        addEntry("Char.ToString");     // A7
        
        addEntry("Hardware.LED_Set");
        
        addEntry("Dictionary.Count_Get");
        addEntry("Dictionary.Set");
        addEntry("Dictionary.Contains");
        addEntry("Dictionary.Get");
        addEntry("Dictionary.Next");
        addEntry("Dictionary.Clear");
        
        addEntry("System.Trace_Set");
        addEntry("System.Trace_Get");
        
        addEntry("Dictionary.HashKey");
        
        addEntry("Clipboard.HasText_Get");
        addEntry("Clipboard.GetText");
        addEntry("Clipboard.SetText");
        
        addEntry("_String.BuildFront2");
        
        addEntry("Memory.ReadBit");
        addEntry("Memory.WriteBit");
        
        addEntry("Char.ToUpper");
        addEntry("Char.IsUpper");
        addEntry("Char.IsDigit");
        addEntry("Char.IsLetterOrDigit");
        addEntry("Char.IsLower");
        addEntry("Byte.ToDigit");
        addEntry("_Byte.ToHex2");
        addEntry("Char.IsHexDigit");
        addEntry("Char.ToLower");
        
        addEntry("String.StartsWith");
        addEntry("String.Contains");
        addEntry("String.IndexOf");
        
        addEntry("System.Warp_Set");
        addEntry("System.Warp_Get");
        
        addEntry("_Time.Delay2");
        
        addEntry("_Long.Inc");
        addEntry("_Long.AddRef");
        addEntry("_Long.MulRef");
        
        addEntry("_Array.GetItemUInt");
        addEntry("_Array.SetItemUInt");
        
        addEntry("File.GetTimeStamp");
        addEntry("_Int.ToBytes");
        
        addEntry("File.GetTime");
        addEntry("Directory.GetTime");
        
        addEntry("String.Trim");
        addEntry("String.TrimLeft");
        addEntry("String.TrimRight");
        addEntry("String.PushImmediate");
        addEntry("String.ToUpper");
        addEntry("String.ToLower");
        
        addEntry("Clipboard.GetChar");
        
        addEntry("Memory.ReadWord");
        addEntry("Memory.WriteWord");
        
        addEntry("_Unused.PinMode");
        addEntry("_Unused.DigitalRead");
        addEntry("_Unused.DigitalWrite");
        
        addEntry("Memory.ReadCodeByte");
        addEntry("Memory.WriteCodeByte");
        addEntry("Memory.ReadCodeWord");
        addEntry("Memory.WriteCodeWord");
        
        addEntry("_Long.GetByte");
        addEntry("_Int.GetByte2");
        addEntry("_Float.GetByte");
        addEntry("_Long.FromBytes");
        addEntry("_Int.FromBytes2");
        addEntry("_Float.FromBytes");
        addEntry("UInt.ToFloat");
        addEntry("Serial.Ports_Get");
        
        addEntry("String.Compare");
        addEntry("String.EndsWith");
        addEntry("String.Substring");
        addEntry("String.Replace");
        
        addEntry("Float.ToUInt");
        addEntry("Float.ToLong");
        
        addEntry("Long.AddB");
        addEntry("Long.SubB");
        
        addEntry("Float.Sin");
        addEntry("Float.Cos");
        addEntry("Float.ATan2");
        addEntry("Float.Sqrt");
        
        addEntry("List.New");
        addEntry("List.Count_Get");
        addEntry("List.Append");
        addEntry("List.Insert");
        addEntry("List.GetItem");
        addEntry("List.GetItemAsVariant");
        addEntry("List.SetItem");
        addEntry("List.Clear");
        addEntry("List.Remove");
        addEntry("List.Contains");
        
    }
    
    bool TryParseSysCall(string name, ref byte index)
    {
        bool success = false;
        if (syscalls.Contains(name))
        {
            index = syscalls[name];
            success = true;
        }
        return success;
    }
    
    string GetSysCallName(byte iSysCall)
    {
        string name;
        foreach (var kv in syscalls)
        {
            if (kv.value == iSysCall)
            {
                name = kv.key;
                break;
            }
        }
        return name;
    }
    <string,byte> GetSysCalls()
    {
        return syscalls;
    }
}
