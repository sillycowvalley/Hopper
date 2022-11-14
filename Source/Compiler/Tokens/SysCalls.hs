unit SysCalls
{
    <string,byte> syscalls;

    addEntry(string unitName, string methodName)
    {
        syscalls[unitName + '.' + methodName] = byte(syscalls.Count);
    }
    New()
    {
        byte index = 0;
        //addEntry("RegisterObjectSlot");
        //addEntry("RegisterObjectSlotRET");
        //addEntry("DynamicCast");
        
        addEntry("String", "NewFromConstant");
        addEntry("String", "NewFromChar");
        addEntry("String", "New");
        addEntry("String", "Append");
        addEntry("String", "InsertChar");
        addEntry("String", "Compare");
        addEntry("String", "Length_Get");
        addEntry("String", "EndsWith");
        addEntry("String", "Substring");
        addEntry("String", "Replace");
        addEntry("String", "GetChar");
        
        addEntry("Array", "New");
        addEntry("Array", "Count_Get");
        addEntry("Array", "GetItem");
        addEntry("Array", "SetItem");
        
        addEntry("List", "New");
        addEntry("List", "Length_Get");
        addEntry("List", "Append");
        addEntry("List", "Insert");
        addEntry("List", "GetItem");
        addEntry("List", "GetItemAsVariant");
        addEntry("List", "SetItem");
        addEntry("List", "Clear");
        addEntry("List", "Remove");
        addEntry("List", "Contains");
        
        addEntry("Dictionary", "New");
        addEntry("Dictionary", "Count_Get");
        addEntry("Dictionary", "Set");
        addEntry("Dictionary", "Contains");
        addEntry("Dictionary", "Get");
        addEntry("Dictionary", "Next");
        addEntry("Dictionary", "Clear");
        
        addEntry("Pair", "New");
        addEntry("Pair", "Set");
        addEntry("Pair", "Key");
        addEntry("Pair", "KeyType");
        addEntry("Pair", "Value");
        addEntry("Pair", "ValueType");
        
        addEntry("Variant", "Type");
        addEntry("Variant", "Box");
        addEntry("Variant", "UnBox");
        
        addEntry("Screen", "Print");
        addEntry("Screen", "PrintLn");
        addEntry("Screen", "Clear");
        addEntry("Screen", "SetCursor");
        addEntry("Screen", "Columns_Get");
        addEntry("Screen", "Rows_Get");
        addEntry("Screen", "CursorX_Get");
        addEntry("Screen", "CursorY_Get");
        addEntry("Screen", "Suspend");
        addEntry("Screen", "Resume");
        addEntry("Screen", "DrawChar");
        
        addEntry("Int", "ToFloat");
        addEntry("Int", "ToLong");
        
        addEntry("UInt", "ToLong");
        addEntry("UInt", "ToInt");
        
        addEntry("Long", "ToString");
        addEntry("Long", "ToBytes");
        addEntry("Long", "ToFloat");
        addEntry("Long", "ToInt");
        addEntry("Long", "ToUInt");
        addEntry("Long", "New");
        addEntry("Long", "NewFromConstant");
        addEntry("Long", "Add");
        addEntry("Long", "Sub");
        addEntry("Long", "Div");
        addEntry("Long", "Mul");
        addEntry("Long", "Mod");
        addEntry("Long", "EQ");
        addEntry("Long", "LT");
        addEntry("Long", "LE");
        addEntry("Long", "GT");
        addEntry("Long", "GE");
        addEntry("Long", "Negate");
     
        addEntry("Float", "ToString");
        addEntry("Float", "ToBytes");
        addEntry("Float", "New");
        addEntry("Float", "NewFromConstant");
        addEntry("Float", "Add");
        addEntry("Float", "Sub");
        addEntry("Float", "Div");
        addEntry("Float", "Mul");
        addEntry("Float", "EQ");
        addEntry("Float", "LT");
        addEntry("Float", "LE");
        addEntry("Float", "GT");
        addEntry("Float", "GE");
        
        addEntry("Time", "Millis_Get");
        addEntry("Time", "Micros_Get");
        
        addEntry("System", "Arguments_Get");
        addEntry("System", "CurrentDirectory_Get");
        addEntry("System", "CurrentDirectory_Set");
        addEntry("System", "Beep");
        addEntry("System", "Execute");
        addEntry("System", "RegisterObject");
        
        addEntry("File", "Exists");
        addEntry("File", "New");
        addEntry("File", "Open");
        addEntry("File", "Create");
        addEntry("File", "ReadLine");
        addEntry("File", "Read");
        addEntry("File", "IsValid");
        addEntry("File", "Append");
        addEntry("File", "Flush");
        addEntry("File", "Delete");
        addEntry("File", "GetSize");
        
        addEntry("Directory", "Exists");
        addEntry("Directory", "New");
        addEntry("Directory", "IsValid");
        addEntry("Directory", "Open");
        addEntry("Directory", "GetDirectoryCount");
        addEntry("Directory", "GetFileCount");
        addEntry("Directory", "GetFile");
        addEntry("Directory", "GetDirectory");
        
        addEntry("Keyboard", "ReadKey");
        addEntry("Keyboard", "IsAvailable_Get");
        addEntry("Keyboard", "ToKey");
        
        addEntry("Keyboard", "ClickX_Get");
        addEntry("Keyboard", "ClickY_Get");
        addEntry("Keyboard", "ClickUp_Get");
        addEntry("Keyboard", "ClickDouble_Get");
        addEntry("Keyboard", "ScrollDelta_Get");
        
        addEntry("Diagnostics", "OutputDebug");
        addEntry("Diagnostics", "Assert");
        addEntry("Diagnostics", "Die");
        addEntry("Diagnostics", "SetError");

        addEntry("Types", "TypeOf");      // gives 'true' type (for instance, "variant" and not the boxed contents of a variant)
        addEntry("Types", "ValueTypeOf"); // if the type is a list, dictionary, pair or array, then return its value type
        addEntry("Types", "KeyTypeOf");   // if the type is a dictionary then return its key type
        addEntry("Types", "BoxTypeOf");   // regular "TypeOf" except for boxed variants where the boxed value type is returned (used for "typeof")
        addEntry("Types", "VerifyValueTypes");
        
        addEntry("String", "Build");
        
        addEntry("WebServer", "Method_Get");
        addEntry("WebServer", "URL_Get");
        addEntry("WebServer", "Arguments_Get");
        addEntry("WebServer", "On");
        addEntry("WebServer", "Send");
        addEntry("WebServer", "ClearHandlers");
        
        //addEntry("File", "ReadAllLines");
    }
    
    bool TryParse(string name, ref byte index)
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
