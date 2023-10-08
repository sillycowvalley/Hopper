program TestSuite
{

    //uses "/Source/6502/System"
    uses "/Source/System/System"
    
#ifndef H6502
//#define TESTFLOATS
#endif
    
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"

#ifdef TEXTBUFFER
    uses "/Source/Editor/TextBuffer"
#endif
    PrintFailed(string message)
    {
        Trace = false;
        PrintLn("  " + message, MatrixRed, 0);
#ifdef H6502
       Diagnostics.Die(0x0B); // system failure / internal error
#endif         
    }
    
    TestStringSystem()
    {
        PrintLn("system 'string'"); // string methods written in Hopper (some specific to H6502)
        
        // string Replace(string original, string pattern, string replace)
        string original = "a=10";
        string replaced = original.Replace("=", " = ");
        if (replaced != "a = 10")
        {
            PrintFailed("'string': Replace failed 1");
        }
        original = "a = 10";
        replaced = original.Replace("=", " = ");
        if (replaced != "a  =  10")
        {
            PrintFailed("'string': Replace failed 2");
        }
        original = "a <> 10";
        replaced = original.Replace("<>", "#");
        if (replaced != "a # 10")
        {
            PrintFailed("'string': Replace failed 3");
        }
        replaced = original.Replace("a", "a");
        if (replaced != "a <> 10")
        {
            PrintFailed("'string': Replace failed 4");
        }
        replaced = original.Replace("", "a");
        if (replaced != "a <> 10")
        {
            PrintFailed("'string': Replace failed 5");
        }
        replaced = original.Replace("a", "");
        if (replaced != " <> 10")
        {
            PrintFailed("'string': Replace failed 6");
        }
        replaced = original.Replace("", "");
        if (replaced != "a <> 10")
        {
            PrintFailed("'string': Replace failed 7");
        }
        
        
        // string Replace(string original, char pattern, char replace)
        replaced = original.Replace('a', 'b');
        if (replaced != "b <> 10")
        {
            PrintFailed("'string': Replace failed 8");
        }
        replaced = original.Replace('a', 'a');
        if (replaced != "a <> 10")
        {
            PrintFailed("'string': Replace failed 9");
        }
        
        // bool EndsWith(string original, string pattern)
        original = "The End";
        if (!original.EndsWith("End"))
        {
            PrintFailed("'string': EndsWith failed 1");
        }
        if (original.EndsWith("Bob"))
        {
            PrintFailed("'string': EndsWith failed 2");
        }
        // bool EndsWith(string original, char pattern)
        if (!original.EndsWith('d'))
        {
            PrintFailed("'string': EndsWith failed 3");
        }
        if (original.EndsWith('b'))
        {
            PrintFailed("'string': EndsWith failed 4");
        }
        // bool StartsWith(string this, char pattern)
        if (!original.StartsWith('T'))
        {
            PrintFailed("'string': StartsWith failed 1");
        }
        if (original.StartsWith('E'))
        {
            PrintFailed("'string': StartsWith failed 2");
        }
        
        // bool StartsWith(string this, string pattern)
        if (!original.StartsWith("The"))
        {
            PrintFailed("'string': StartsWith failed 3");
        }
        if (original.StartsWith("End"))
        {
            PrintFailed("'string': StartsWith failed 4");
        }
        
        
        // int Compare(string left, string right) // returns -1, 0, +1
        if (String.Compare("aaa", "bbb") != -1)
        {
            PrintFailed("'string': Compare failed 1");
        }
        if (String.Compare("aaa", "aaa") != 0)
        {
            PrintFailed("'string': Compare failed 2");
        }
        if (String.Compare("aaa", "aaaa") == 0)
        {
            PrintFailed("'string': Compare failed 3");
        }
        if (String.Compare("aaaa", "aaa") == 0)
        {
            PrintFailed("'string': Compare failed 4");
        }
        if (String.Compare("aaaa", "") == 0)
        {
            PrintFailed("'string': Compare failed 5");
        }
        if (String.Compare("", "aaaa") == 0)
        {
            PrintFailed("'string': Compare failed 6");
        }
        if (String.Compare("bbb", "aaa") != 1)
        {
            PrintFailed("'string': Compare failed 7");
        }
        
        string insertString = 'a' + "bcde";
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed("'string': Insert failed 1");
        }
        insertString = "abcd" + 'e';
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed("'string': Insert failed 2");
        }
        insertString = "bcde";
        insertString = insertString.InsertChar(0, 'a');
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed("'string': Insert failed 3");
        }
        insertString = "abcd";
        insertString = insertString.InsertChar(4, 'e');
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed("'string': Insert failed 4");
        }
        insertString = "abde";
        insertString = insertString.InsertChar(2, 'c');
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed("'string': Insert failed 5");
        }
        uint index;
        if (!insertString.IndexOf('a', ref index))
        {
            PrintFailed("'string': IndexOf failed 1");
        }
        if (index != 0)
        {
            PrintFailed("'string': IndexOf failed 2");
        }
        if (!insertString.IndexOf('c', ref index))
        {
            PrintFailed("'string': IndexOf failed 3");
        }
        if (index != 2)
        {
            PrintFailed("'string': IndexOf failed 4");
        }
        if (!insertString.IndexOf('e', ref index))
        {
            PrintFailed("'string': IndexOf failed 5");
        }
        if (index != 4)
        {
            PrintFailed("'string': IndexOf failed 6");
        }
        if (insertString.IndexOf('z', ref index))
        {
            PrintFailed("'string': IndexOf failed 7");
        }
        
        if (insertString.IndexOf('a', 1, ref index))
        {
            PrintFailed("'string': IndexOf failed 8");
        }
        if (!insertString.IndexOf('c', 2, ref index))
        {
            PrintFailed("'string': IndexOf failed 9");
        }
        if (index != 2)
        {
            PrintFailed("'string': IndexOf failed 9");
        }
        if (!insertString.IndexOf('e', 2, ref index))
        {
            PrintFailed("'string': IndexOf failed 10");
        }
        if (index != 4)
        {
            PrintFailed("'string': IndexOf failed 11");
        }
        if (insertString.IndexOf('z', 10, ref index))
        {
            PrintFailed("'string': IndexOf failed 12");
        }
        
        
        // string Substring(string original, uint start)
        // string Substring(string original, uint start, uint length)
        // Build(ref string build, string append)
        // Build(ref string build, char append)
        // bool IndexOf(string this, string pattern, ref uint index)
        // bool IndexOf(string this, char pattern, uint startIndex, ref uint index)
        // bool IndexOf(string this, string pattern, uint startIndex, ref uint index)
        // bool LastIndexOf(string this, char pattern, ref uint index)
        // bool LastIndexOf(string this, char pattern, uint startIndex, ref uint index)
        // string Pad(string this, char append, uint width)
        // string LeftPad(string this, char append, uint width)
        // string ToUpper(string this)
        // string ToLower(string this)
        // bool Contains(string this, char needle)
        // bool Contains(string this, string needle)
        // string Trim(string this)
        // <string> Split(string this, string delimiters)
        // <string> Split(string this, char delimiter)
    }
    
#ifdef TEXTBUFFER
    TestTextBuffer()
    {
        PrintLn("TextBuffer");
        
        TextBuffer.Initialize();
        
        string ln = "test content";
        
        TextBuffer.StartJournal();
        int x = 0;
        int y = 0;
        foreach (var c in ln)
        {
            TextBuffer.Insert(ref x, ref y, c);
        }
        TextBuffer.EndJournal();
        
        TextBuffer.StartJournal();
        TextBuffer.Insert(ref x, ref y, char(0x0A));
        TextBuffer.EndJournal();
        
        TextBuffer.StartJournal();
        foreach (var c in ln)
        {
            TextBuffer.Insert(ref x, ref y, c);
        }
        TextBuffer.EndJournal();
        
        int lineCount = TextBuffer.GetLineCount();
        if (lineCount != 2)
        {
            //PrintLn(lineCount.ToString());
            PrintFailed("TextBuffer: Insert failed");
        }
        
        if (!TextBuffer.CanUndo())
        {
            PrintFailed("TextBuffer: CanUndo failed");
        }
        int count = 0;
        < <string, int > > undoJournal = TextBuffer.GetUndo();
        foreach (var record in undoJournal)
        {  
            foreach (var kv in record)
            {
                count++;
            }
        }
        if (count != 48)
        {
            PrintFailed("TextBuffer: GetUndo failed");
        }
        
    }
#endif
    
    TestArray()
    {
        PrintLn("Array");
        int[5] intArray;
        intArray[0] = -3;
        intArray[1] = 20000;
        intArray[2] = 1;
        
        if (intArray.Count != 5)
        {
            PrintFailed("Array Int: Length failed");
        }
        
        if (intArray[2] != 1)
        {
            PrintFailed("Array Int: [2] = failed");
        }
        if (intArray[0] != -3)
        {
            PrintFailed("Array Int: [0] = failed");
        }
        if (intArray[1] != 20000)
        {
            PrintFailed("Array Int: [1] = failed");
        }

        if (intArray[3] != 0)
        {
            PrintFailed("Array Int: zero initialization failed");
        }
        int count = 0;
        
        foreach (var a in intArray)
        {
            count++;
        }
        if (count != 5)
        {
            PrintFailed("Array Int: foreach failed");
        }
        count = 0;
        
        uint[5] uintArray;
        uintArray[0] = 3;
        uintArray[1] = 40000;
        uintArray[2] = 1;
        if (uintArray.Count != 5)
        {
            PrintFailed("Array UInt: Length failed");
        }
        
        if (uintArray[2] != 1)
        {
            PrintFailed("Array UInt: [2] = failed");
        }
        
        if (uintArray[0] != 3)
        {
            PrintFailed("Array UInt: [0] = failed");
        }
        if (uintArray[1] != 40000)
        {
            PrintFailed("Array UInt: [1] = failed");
        }
        if (uintArray[3] != 0)
        {
            PrintFailed("Array UInt: zero initialization failed");
        }
        int count = 0;
        
        foreach (var a in uintArray)
        {
            count++;
        }
        if (count != 5)
        {
            PrintFailed("Array UInt: foreach failed");
        }
        count = 0;
        
        char[5] charArray;
        charArray[0] = char(0);
        charArray[1] = 'a';
        charArray[2] = 'b';
        if (charArray.Count != 5)
        {
            PrintFailed("Array Char: Length failed");
        }
        
        if (charArray[2] != 'b')
        {
            PrintFailed("Array Char: [2] = failed");
        }
        
        if (charArray[0] != char(0))
        {
            PrintFailed("Array Char: [0] = failed");
        }
        if (charArray[1] != 'a')
        {
            PrintFailed("Array Char: [1] = failed");
        }
        if (charArray[3] != char(0))
        {
            PrintFailed("Array Char: zero initialization failed");
        }
        int count = 0;
        
        foreach (var a in charArray)
        {
            count++;
        }
        if (count != 5)
        {
            PrintFailed("Array Char: foreach failed");
        }
        count = 0;
        
        bool[10] boolArray;
        boolArray[0] = true;
        boolArray[1] = false;
        boolArray[2] = true;
        boolArray[4] = false;
        boolArray[5] = true;
        boolArray[6] = true;
        boolArray[7] = false;
        boolArray[8] = true;
        boolArray[9] = true;
        if (boolArray.Count != 10)
        {
            PrintFailed("Array Bool: Length failed");
        }
        
        if (boolArray[2] != true)
        {
            PrintFailed("Array Bool: [2] = failed");
        }
        
        if (boolArray[0] != true)
        {
            PrintFailed("Array Bool: [0] = failed");
        }
        if (boolArray[1] != false)
        {
            PrintFailed("Array Bool: [1] = failed");
        }
        if (boolArray[3] != false)
        {
            PrintFailed("Array Bool: zero initialization failed");
        }
        if (boolArray[4] != false)
        {
            PrintFailed("Array Bool: [4] = failed");
        }
        if (boolArray[5] != true)
        {
            PrintFailed("Array Bool: [5] = failed");
        }
        if (boolArray[6] != true)
        {
            PrintFailed("Array Bool: [6] = failed");
        }
        if (boolArray[7] != false)
        {
            PrintFailed("Array Bool: [7] = failed");
        }
        if (boolArray[8] != true)
        {
            PrintFailed("Array Bool: [8] = failed");
        }
        if (boolArray[9] != true)
        {
            PrintFailed("Array Bool: [9] = failed");
        }
        int count = 0;
        
        foreach (var a in boolArray)
        {
            if (a)
            {
                count++;
            }
        }
        if (count != 6)
        {
            PrintFailed("Array Bool: foreach failed");
        }
        count = 0;
        
    }
    
    TestDictionary()
    {
        PrintLn("Dictionary");
        
        <char,string> charDictionary;
        charDictionary.Set('a', "a value");
        charDictionary.Set('b', "b value");
        charDictionary.Set('c', "c value");
        if (charDictionary.Count != 3)
        {
            PrintFailed("Dictionary Char: Set failed");
        }
        if (!charDictionary.Contains('a'))
        {
            PrintFailed("Dictionary Char: Contains failed 1");
        }
        if (charDictionary.Contains('d'))
        {
            PrintFailed("Dictionary Char: Contains failed 2");
        }
        string result = charDictionary.Get('b');
        if (result != "b value")
        {
            PrintFailed("Dictionary Char: Get failed");
        }
        charDictionary.Set('c', "new c value");
        result = charDictionary.Get('c');
        if (result != "new c value")
        {
            PrintFailed("Dictionary Char: Set replace failed");
        }
        
        <uint,string> uintDictionary;
        uintDictionary.Set(0, "a value");
        uintDictionary.Set(1, "b value");
        uintDictionary.Set(2, "c value");
        if (uintDictionary.Count != 3)
        {
            PrintFailed("Dictionary UInt: Set failed");
        }
        if (!uintDictionary.Contains(0))
        {
            PrintFailed("Dictionary UInt: Contains failed 1");
        }
        if (uintDictionary.Contains(3))
        {
            PrintFailed("Dictionary UInt: Contains failed 2");
        }
        string result = uintDictionary.Get(1);
        if (result != "b value")
        {
            PrintFailed("Dictionary UInt: Get failed");
        }
        uintDictionary.Set(2, "new c value");
        result = uintDictionary.Get(2);
        if (result != "new c value")
        {
            PrintFailed("Dictionary UInt: Set replace failed");
        }
        
        <string,string> stringDictionary;
        stringDictionary.Set("a", "a value");
        stringDictionary.Set("b", "b value");
        stringDictionary.Set("c", "c value");
        if (stringDictionary.Count != 3)
        {
            PrintFailed("Dictionary String: Set failed");
        }
        if (!stringDictionary.Contains("a"))
        {
            PrintFailed("Dictionary String: Contains failed 1");
        }
        if (stringDictionary.Contains("d"))
        {
            PrintFailed("Dictionary String: Contains failed 2");
        }
        string result = stringDictionary.Get("b");
        if (result != "b value")
        {
            PrintFailed("Dictionary String: Get failed");
        }
        stringDictionary.Set("c", "new c value");
        result = stringDictionary.Get("c");
        if (result != "new c value")
        {
            PrintFailed("Dictionary String: Set replace failed");
        }
        
        stringDictionary["d"] = "d value";
        stringDictionary["e"] = "e value";
        stringDictionary["f"] = "f value";
        if (stringDictionary.Count != 6)
        {
            PrintFailed("Dictionary String: [] failed");
        }
        result = stringDictionary.Get("e");
        if (result != "e value")
        {
            PrintFailed("Dictionary String: [] failed");
        }
        
        int count = 0;
        foreach (var kv in stringDictionary)
        {
            count++;
        }
        if (count != 6)
        {
            PrintFailed("Dictionary String: foreach failed");
        }
        
        stringDictionary.Clear();
        if (stringDictionary.Count != 0)
        {
            PrintFailed("Dictionary String: Clear failed");
        }
      }
      TestValueDictionary()
      {
        PrintLn("Value Dict");  
        <uint, string> names;
        names[0] = "Zero";
        names[1] = "One";
        names[2] = "Two";
        names[3] = "Three";
        names[4] = "Four";
        uint length = 0;
        foreach (var nv in names)
        {
            uint key = nv.key;
            string name = nv.value;
            length = length + name.Length;
        }
        if (length != 19)
        {
            PrintFailed("Dictionary : <uint, string> foreach failed");
        }
        if (!names.Contains(4))
        {
            PrintFailed("Dictionary : <uint, string> !Contains failed");
        }
        if (names.Contains(5))
        {
            PrintFailed("Dictionary : <uint, string> Contains failed");
        }
        names.Set(2, "Twee");
        if (names[2] != "Twee")
        {
            PrintFailed("Dictionary : <uint, string> Set failed");
        }
    }
    TestDictionaryExpandVV()
    {
        PrintLn("Dict Expand: VV");
        <uint,uint> expandValueValue;
        for (uint i = 0; i < 65; i++)
        {
            expandValueValue[i] = i;
        }
        uint totalk = 0;
        uint totalv = 0;
        uint count = 0;
        foreach (var kv in expandValueValue)
        {
            totalk = totalk + kv.key;
            totalv = totalv + kv.value;
            count++;    
        }
        if (count != 65)
        {
            PrintFailed("expandValueValue failed 1");
        }
        if (totalk != 2080)
        {
            PrintFailed("expandValueValue failed 2");
        }
        if (totalv != 2080)
        {
            PrintFailed("expandValueValue failed 3");
        }
   	}
    TestDictionaryExpandRV()
    {
        PrintLn("Dict Expand: RV");
        uint totalk = 0;
        uint totalv = 0;
        uint count = 0;
        <string,uint> expandReferenceValue;
        for (uint i = 0; i < 65; i++)
        {
            expandReferenceValue[i.ToString()] = i;
        }
        foreach (var kv in expandReferenceValue)
        {
            uint k;
            if (UInt.TryParse(kv.key, ref k))
            {
                totalk = totalk + k;
            }
            totalv = totalv + kv.value;
            count++;    
        }
        if (count != 65)
        {
            PrintFailed("expandReferenceValue failed 1");
        }
        if (totalk != 2080)
        {
            PrintFailed("expandReferenceValue failed 2");
        }
        if (totalv != 2080)
        {
            PrintFailed("expandReferenceValue failed 3");
        }
    }   
   	TestDictionaryExpandVR()
   	{
        PrintLn("Dict Expand: VR");
        uint totalk = 0;
        uint totalv = 0;
        uint count = 0;
        <uint,string> expandValueReference;
        for (uint i = 0; i < 65; i++)
        {
            expandValueReference[i] = i.ToString();
        }
        foreach (var kv in expandValueReference)
        {
            totalk = totalk + kv.key;
            uint v;
            if (UInt.TryParse(kv.value, ref v))
            {
                totalv = totalv + v;
            }
            count++;    
        }
        if (count != 65)
        {
            PrintFailed("expandValueReference failed 1");
        }
        if (totalk != 2080)
        {
            PrintFailed("expandValueReference failed 2");
        }
        if (totalv != 2080)
        {
            PrintFailed("expandValueReference failed 3");
        }
    }
    TestDictionaryExpandRR()
   	{    
        PrintLn("Dict Expand: RR");
        uint totalk = 0;
        uint totalv = 0;
        uint count = 0;
        <string,string> expandReferenceReference;
        for (uint i = 0; i < 65; i++)
        {
            expandReferenceReference[i.ToString()] = i.ToString();
        }
        foreach (var kv in expandReferenceReference)
        {
            uint k;
            uint v;
            if (UInt.TryParse(kv.key, ref k))
            {
                totalk = totalk + k;
            }
            if (UInt.TryParse(kv.value, ref v))
            {
                totalv = totalv + v;
            }
            count++;    
        }
        if (count != 65)
        {
            PrintFailed("expandReferenceReference failed 1");
        }
        if (totalk != 2080)
        {
            PrintFailed("expandReferenceReference failed 2");
        }
        if (totalv != 2080)
        {
            PrintFailed("expandReferenceReference failed 3");
        }
    }
   	TestDictionaryOfLists()
   	{
    	   PrintLn("Dict of Lists");
        
        <string> list0;
        list0.Append("one");
        list0.Append("two");
        list0.Append("three");
        
        <string> list2;
        list2.Append("four");
        list2.Append("five");
        list2.Append("six");
       
        <string,<string> > dictionaryOfLists;
        
        dictionaryOfLists["key"] = list0;
        dictionaryOfLists["key2"] = list2;
        
        int count = 0;
        foreach (var kv in dictionaryOfLists)
        {
            string key = kv.key;
            foreach (var s in kv.value)
            {
                count++;
            }
        }
        if (dictionaryOfLists.Contains("key"))
        {
            <string> value = dictionaryOfLists.Get("key");
            foreach (var s in value)
            {
                count++;
            }
        }
        if (count != 9)
        {
            PrintFailed("Dictionary : <string, <string>> failed");
        }
		
		      count = 0;
        < string, <string> > dictionaryOfLists2;
        dictionaryOfLists2["one"] = list0;
        dictionaryOfLists2["two"] = list2;
         
        foreach (var kv4 in dictionaryOfLists2)
        {
            string k = kv4.key;
            //Print(k + "->");
            foreach (var v2 in kv4.value)
            {
                count = count + 1;
            }
        }
        if (count != 6)
        {
            PrintFailed("Dictionary : <string, <string> > foreach failed");
        }
        if (!dictionaryOfLists2.Contains("two"))
        {
            PrintFailed("Dictionary : <string, <string> > !Contains failed");
        }
        if (dictionaryOfLists2.Contains("three"))
        {
            PrintFailed("Dictionary : <string, <string> > Contains failed");
        }
    }
    TestDictionaryOfDictionaries()
   	{
    	PrintLn("Dict of Dict");
        
        <string,int> dictionary0;
        dictionary0["One"] = 1;
        dictionary0["Two"] = 2;
        dictionary0["Three" ] = 3;
        
        <string,int> dictionary2;
        dictionary2["Four"] = 4;
        dictionary2["Five"] = 5;
        dictionary2["Six"] = 6;
		dictionary2["Seven"] = 7;
       
        < string, <string,int> > dictionaryOfDictionaries;
        dictionaryOfDictionaries["dict1"] = dictionary0;
        dictionaryOfDictionaries["dict2"] = dictionary2;
        
        int count = 0;
        <string,int> dictionary3 = dictionaryOfDictionaries.Get("dict1");
        foreach (var kv in dictionary3)
        {
            count = count +  kv.value;
        }
        if (count != 6)
        {
            PrintFailed("Dictionary : <string, <string, int> > failed 1");
        }
        
        count = 0;
        < string, <string,int> > dictionaryOfDictionaries2;
        dictionaryOfDictionaries2["one"] = dictionary0;
        dictionaryOfDictionaries2["two"] = dictionary2;
         
        foreach (var kv in dictionaryOfDictionaries2)
        {
            string k = kv.key;
            foreach (var kv2 in kv.value)
            {
                string k2 = kv2.key;
                int v2 = kv2.value;
                count = count + v2;
            }
        }
        if (count != 28)
        {
            PrintFailed("Dictionary : <string, <string, int> > failed 2");
        }
        
        
    }

    TestList()
    {
        PrintLn("List");
        <string> stringList;
        stringList.Append("item 1");
        stringList.Append("item 2");
        stringList.Append("item 3");
        
        if (stringList.Length != 3)
        {
            PrintFailed("List String: Length failed");
        }
        int i = 0;
        foreach (var item in stringList)
        {
            i++;
        }
        if (i != 3)
        {
            PrintFailed("List String: foreach failed");
        }
        string result = stringList.GetItem(1);
        if (result != "item 2")
        {
            PrintFailed("List String: GetItem failed");
        }
        stringList.SetItem(1, "new item");
        result = stringList.GetItem(1);
        if (result != "new item")
        {
            PrintFailed("List String: SetItem failed");
        }
        
        stringList.Clear();
        if (stringList.Length != 0)
        {
            PrintFailed("List String: Clear failed");
        }
        
        <bool> boolList;
        boolList.Append(true);
        boolList.Append(false);
        boolList.Append(false);
        
        if (boolList.Length != 3)
        {
            PrintFailed("List Boolean: Length failed");
        }
        i = 0;
        foreach (var item in boolList)
        {
            i++;
        }
        if (i != 3)
        {
            PrintFailed("List Boolean: foreach failed");
        }
        
        bool bresult = boolList.GetItem(0);
        if (bresult != true)
        {
            PrintFailed("List Boolean: GetItem failed");
        }
        bresult = boolList.GetItem(1);
        if (bresult != false)
        {
            PrintFailed("List Boolean: GetItem failed");
        }
        bresult = boolList.GetItem(2);
        if (bresult != false)
        {
            PrintFailed("List Boolean: GetItem failed");
        }
        boolList.SetItem(1, true);
        bresult = boolList.GetItem(1);
        if (bresult != true)
        {
            PrintFailed("List Boolean: SetItem failed");
        }
        
        boolList.Clear();
        if (boolList.Length != 0)
        {
            PrintFailed("List Boolean: Clear failed");
        }
        
        <string> plainlist;
        plainlist.Append("one");
        plainlist.Append("two");
        plainlist.Append("three");
        
        < <string> > listOfLists;
        < < <string> > > listOfListsOfLists;

        listOfLists.Append(plainlist);
        
        listOfListsOfLists.Append(listOfLists);
        
        <string> item = listOfLists.GetItem(0);
        string value = item.GetItem(1);
        if (value != "two")
        {
            PrintFailed("List : < < string > > GetItem failed");
        }
        uint count = listOfLists.Length;
        if (count != 1)
        {
            PrintFailed("List : < < string > > Length failed");
        }
        
        < <string> > lOL = listOfListsOfLists.GetItem(0);
        <string> lOS = lOL.GetItem(0);
        value = lOS.GetItem(0);
        if (value != "one")
        {
            PrintFailed("List : < < < string > > > GetItem failed");
        }
                
        listOfLists.Clear();
        count = listOfLists.Length;
        if (count != 0)
        {
            PrintFailed("List : < < string > > Clear failed");
        }


        
        plainlist.Remove(1);
        if (plainlist.Length != 2)
        {
            PrintFailed("List : Remove failed 1");
        }
        
        plainlist.Remove(0);
        if (plainlist.Length != 1)
        {
            PrintFailed("List : Remove failed 2");
        }

        plainlist.Remove(0);
        if (plainlist.Length != 0)
        {
            PrintFailed("List : Remove failed 3");
        }
        plainlist.Insert(0, "aaa");
        if (plainlist[0] != "aaa")
        {
            PrintFailed("List : Insert failed 1");
        }
        plainlist.Insert(1, "end");
        if (plainlist[1] != "end")
        {
            PrintFailed("List : Insert failed 2");
        }
        plainlist.Insert(1, "bbb");
        if (plainlist[1] != "bbb")
        {
            PrintFailed("List : Insert failed 3");
        }
        long key = 0xAABBCCDD;
        <byte> bytes = key.ToBytes();
        string bstr;
        foreach (var b in bytes)
        {
            string bs = b.ToHexString(2);
            bstr = bstr + bs;
        }
        if (bstr != "DDCCBBAA")
        {
            PrintFailed("List : long.ToBytes failed");
        }
    }
  	 TestListOfDictionaries()
	   {
	       PrintLn("List of Dict");

        <string> plainlist;
        plainlist.Append("one");
        plainlist.Append("two");
        plainlist.Append("three");
        
        <string,int> dictionary0;
        dictionary0["one"] = 1;
        dictionary0["two"] = 2;
        dictionary0["three" ] =3;
        
        <string,int> dictionary2;
        dictionary2["four"] = 4;
        dictionary2["five"] = 5;
        dictionary2["six"] = 6;
       
        < <string,int> > listOfDictionaries;
        listOfDictionaries.Append(dictionary0);
        listOfDictionaries.Append(dictionary2);
        
        int total = 0;
        foreach (var dict in listOfDictionaries)
        {
            foreach (var kv in dict)
            {
                total = total + kv.value;
            }
        }
        if (total != 21)
        {
            PrintFailed("List : < <string, int> > failed");
        }
        
        uint count = 0;
        < <string> > listOfLists2;
        listOfLists2.Append(plainlist);
        listOfLists2.Append(plainlist);
        foreach (var list2 in listOfLists2)
        {
            foreach (var v3 in list2)
            {
                string vs = v3;
                count = count + 1;
            }
        }
        if (count != 6)
        {
            PrintFailed("List : < <string> > failed");
        }
        
        total = 0;
        <string, int> dictionary4;
        dictionary4["one"] = 1;
        dictionary4["two"] = 2;
        dictionary4["three"] = 3;
        < <string,int> > listOfDictionaries2;
        listOfDictionaries2.Append(dictionary4);
        listOfDictionaries2.Append(dictionary4);
        foreach (var v5 in listOfDictionaries2)
        {
            foreach (var kv5 in v5)
            {
                string k = kv5.key;
                int v6 = kv5.value;
                total = total + v6;
            }
        }
        if (total != 12)
        {
            PrintFailed("List : < <string, int> > failed");
        }
   	}

    TestEquals()
    { 
        PrintLn("'==' constants");
        
        // contants
        if (1 == 0)
        {
            PrintFailed("1 == 0 failed");
        }
        if (!(1 == 1))
        {
            PrintFailed("!(1 == 1) failed");
        }
        
#ifdef TESTFLOATS
        if (1.0 == 0)
        {
            PrintFailed("1.0 == 0 failed 1");
        }
        if (1 == 0.0)
        {
            PrintFailed("1 == 0.0 failed");
        }
        if (!(1.0 == 1.0))
        {
            PrintFailed("!(1.0 == 1.0) failed");
        }
        if (!(1.0 == 1))
        {
            PrintFailed("!(1.0 == 1) failed 1");
        }
        if (!(1 == 1.0))
        {
            PrintFailed("!(1 == 1.0) failed 1");
        }
#endif
        if ('a' == 'b')
        {
            PrintFailed("'a' == 'b' failed");
        }
        if (!('a' == 'a'))
        {
            PrintFailed("!('a' == 'a') failed");
        }
        if ("a" == "b")
        {
            PrintFailed("\"a\" == \"b\" failed");
        }
        if (!("a" == "a"))
        {
            PrintFailed("!(\"a\" == \"a\") failed");
        }
        
        PrintLn("'==' byte");
        byte ba = 1;
        byte bb = 0;
        byte bc = 1;
        if (!(ba == 1))
        {
            PrintFailed("(!ba == 1) failed");
        }
        if (ba == 0)
        {
            PrintFailed("ba == 0 failed");
        }
        if (!(1 == ba))
        {
            PrintFailed("(!1 == ba) failed");
        }
        if (0 == ba)
        {
            PrintFailed("0 == ba failed");
        }
        if (!(ba == ba))
        {
            PrintFailed("(!ba == ba) failed");
        }
        if (ba == bb)
        {
            PrintFailed("ba == bb failed");
        }
        if (!(ba == bc))
        {
            PrintFailed("(!ba == bc) failed");
        }

        PrintLn("'==' int");
        int ia = 1;
        int ib = 0;
        int ic = 1;
        if (!(ia == 1))
        {
            PrintFailed("(!ia == 1) failed");
        }
        if (ia == 0)
        {
            PrintFailed("ia == 0 failed");
        }
        if (!(1 == ia))
        {
            PrintFailed("(!1 == ia) failed");
        }
        if (0 == ia)
        {
            PrintFailed("0 == ia failed");
        }
        if (!(ia == ia))
        {
            PrintFailed("(!ia == ia) failed");
        }
        if (ia == ib)
        {
            PrintFailed("ia == ib failed");
        }
        if (!(ia == ic))
        {
            PrintFailed("(!ia == ic) failed");
        }
        if (!(ia == ba))
        {
            PrintFailed("(!ia == bb) failed");
        }
        if (ia == bb)
        {
            PrintFailed("(!ia == bb) failed");
        }
        if (!(ba == ia))
        {
            PrintFailed("(!ba == ib) failed");
        }
        if (ba == ib)
        {
            PrintFailed("(!ba == ib) failed");
        }
      
        PrintLn("'==' long");
        long la = 1;
        long lb = 0;
        long lc = 1;
        if (!(la == 1))
        {
            PrintFailed("(!la == 1) failed");
        }
        if (la == 0)
        {
            PrintFailed("la == 0 failed");
        }
        if (!(1 == la))
        {
            PrintFailed("(!1 == la) failed");
        }
        if (0 == la)
        {
            PrintFailed("0 == la failed");
        }
        if (!(la == la))
        {
            PrintFailed("(!la == la) failed");
        }
        if (la == lb)
        {
            PrintFailed("la == lb failed");
        }
        if (!(la == lc))
        {
            PrintFailed("(!la == lc) failed");
        }
        if (!(la == ba))
        {
            PrintFailed("(!la == ba) failed");
        }
        if (la == bb)
        {
            PrintFailed("(!la == bb) failed");
        }
        if (!(ba == la))
        {
            PrintFailed("(!ba == la) failed");
        }
        if (ba == lb)
        {
            PrintFailed("(!ba == lb) failed");
        }
        if (!(la == ia))
        {
            PrintFailed("(!la == ia) failed");
        }
        if (la == ib)
        {
            PrintFailed("(!la == ib) failed");
        }
        if (!(ia == la))
        {
            PrintFailed("(!ia == la) failed");
        }
        if (ia == lb)
        {
            PrintFailed("(!ia == lb) failed");
        }

        
#ifdef TESTFLOATS        
        PrintLn("'==' float");
        float fa = 1;
        float fb = 0;
        float fc = 1;
        if (!(fa == 1))
        {
            PrintFailed("(!fa == 1) failed");
        }
        if (fa == 0)
        {
            PrintFailed("fa == 0 failed");
        }
        if (!(1 == fa))
        {
            PrintFailed("(!1 == fa) failed");
        }
        if (0 == fa)
        {
            PrintFailed("0 == fa failed");
        }
        if (!(fa == fa))
        {
            PrintFailed("(!fa == fa) failed");
        }
        if (fa == fb)
        {
            PrintFailed("fa == fb failed");
        }
        if (!(fa == fc))
        {
            PrintFailed("(!fa == fc) failed");
        }
        if (!(fa == ba))
        {
            PrintFailed("(!fa == ba) failed");
        }
        if (fa == bb)
        {
            PrintFailed("(!fa == bb) failed");
        }
        if (!(ba == fa))
        {
            PrintFailed("(!ba == fa) failed");
        }
        if (ba == fb)
        {
            PrintFailed("(!ba == fb) failed");
        }
        if (!(fa == ia))
        {
            PrintFailed("(!fa == ia) failed");
        }
        if (fa == ib)
        {
            PrintFailed("(!fa == ib) failed");
        }
        if (!(ia == fa))
        {
            PrintFailed("(!ia == fa) failed");
        }
        if (ia == fb)
        {
            PrintFailed("(!ia == fb) failed");
        }
        if (!(fa == la))
        {
            PrintFailed("(!fa == la) failed");
        }
        if (fa == lb)
        {
            PrintFailed("(!fa == lb) failed");
        }
        if (!(la == fa))
        {
            PrintFailed("(!la == fa) failed");
        }
        if (la == fb)
        {
            PrintFailed("(!la == fb) failed");
        }
#endif // TESTFLOATS        
    }
#ifndef TINYHOPPER    
    TestFiles()
    {
        PrintLn("File text IO");
        
        string testPath = "/temp/testfile.txt";
        if (File.Exists(testPath))
        {
            File.Delete(testPath);
        }
        
        file testFile = File.Create(testPath);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        File.Append(testFile, "Test Content");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Flush(..) failed");
        }
        
        file testFile2 = File.Open(testPath);
        if (!testFile2.IsValid())
        {
            PrintFailed("File.Open(..) failed");
        }
        string ln = testFile2.ReadLine();
        if (!testFile2.IsValid())
        {
            PrintFailed("File.ReadLine(..) failed");
        }
        if (ln != "Test Content")
        {
            PrintLn("'" + ln + "'");
            PrintFailed("File text IO failed");
        }
        
        long pos = 0;
        ln = "";
        loop
        {
            byte b = Read(testFile2, pos);
            if (!testFile2.IsValid())
            {
                break;
            }
            ln = ln + char(b);
            pos = pos + 1;
        }
        if (ln != "Test Content")
        {
            PrintFailed("File char IO failed");
        }
        
        PrintLn("File byte IO");
        
        if (File.Exists(testPath))
        {
            File.Delete(testPath);
        }
        
        testFile = File.Create(testPath);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        byte content = 42;
        File.Append(testFile, content);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Flush(..) failed");
        }
        
        testFile2 = File.Open(testPath);
        if (!testFile2.IsValid())
        {
            PrintFailed("File.Open(..) failed");
        }
        content = testFile2.Read();
        if (!testFile2.IsValid())
        {
            PrintFailed("File.Read(..) failed");
        }
        if (content != 42)
        {
            PrintFailed("File byte IO failed");
        }
        
        if (File.Exists(testPath))
        {
            File.Delete(testPath);
        }
    }
#endif

#ifdef TESTFLOATS  
    const float  globalFloat = 3.141;
    
    const float  globalFloat2 = 4.141;
#endif
    
    const uint    globalUInt   = 10000;
    const uint    globalUInt2  = 20000;
    
    const long   globalLong  = 100000;
    const long   globalLong2  = 100001;
    const string globalConstant = "aaaaa";
    const int    globalInt  = 10000;
    const string globalConstant2 = "zzzzz";
    const string globalConstant3 = "aaaaa1";
    const int    globalInt2  = 10001;

    
    TestConstants()
    {
        PrintLn("Global const");
        
        string localConstant = "aaaaa";
#ifdef TESTFLOATS          
        float  localFloat = 3.141;
#endif        
        long   localLong  = 100000;
        int    localInt   = 10000;
        if (globalConstant != localConstant)
        {
            PrintFailed("global const string failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat != localFloat)
        {
            PrintFailed("global const float 1 failed");
        }
#endif        
        if (globalLong != localLong)
        {
            PrintFailed("global const long failed");
        }
        if (globalInt != localInt)
        {
            PrintFailed("global const int failed");
        }
#ifdef TESTFLOATS        
        localFloat = localFloat + 1;
#endif
        localLong = localLong + 1;
        localConstant = localConstant + "1";
        localInt = localInt + 1;
        if (globalConstant3 != localConstant)
        {
            PrintFailed("global const string 2 failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat2 != localFloat)
        {
            PrintFailed("global const float 2 failed");
        }
#endif
        if (globalLong2 != localLong)
        {
            PrintFailed("global const long 2 failed");
        }
        if (globalInt2 != localInt)
        {
            PrintFailed("global const int 2 failed");
        }
    }
    
    TestLessThan()
    {
        PrintLn("'<'");
        string localConstant = "zzzzz";
        int    localInt   = 10001;
#ifdef TESTFLOATS          
        float  localFloat = 4.141;
#endif
        long   localLong  = 100001;
        
        uint localUInt0 =   12;
        uint localUInt1 =   23;
        uint localUInt2 = 1234;
        uint localUInt3 = 4567;
        uint localUInt4 = 256;
        uint localUInt5 = 257;
        if (!(localUInt0 < localUInt1))
        {
            PrintFailed("uint < failed 1");    
        }
        if (!(localUInt1 < localUInt2))
        {
            PrintFailed("uint < failed 2");    
        }
        if (!(localUInt2 < localUInt3))
        {
            PrintFailed("uint < failed 3");    
        }
        if (localUInt1 < localUInt0)
        {
            PrintFailed("uint < failed 4");    
        }
        if (localUInt2 < localUInt1)
        {
            PrintFailed("uint < failed 5");    
        }
        if (localUInt3 < localUInt2)
        {
            PrintFailed("uint < failed 6");    
        }
        if (localUInt0 < localUInt0)
        {
            PrintFailed("uint < failed 7");    
        }
        if (localUInt1 < localUInt1)
        {
            PrintFailed("uint < failed 8");    
        }
        if (localUInt2 < localUInt2)
        {
            PrintFailed("uint < failed 9");    
        }
        if (localUInt3 < localUInt3)
        {
            PrintFailed("uint < failed 10");    
        }
        if (!(localUInt4 < localUInt5))
        {
            PrintFailed("uint < failed 11");    
        }
        if (localUInt5 < localUInt4)
        {
            PrintFailed("uint < failed 12");    
        }
        if (localUInt4 < localUInt4)
        {
            PrintFailed("uint < failed 13");    
        }
        if (!(globalConstant < localConstant))
        {
            PrintFailed("string < failed");
        }
#ifdef TESTFLOATS          
        if (!(globalFloat < localFloat))
        {
            PrintFailed("float < failed");
        }
#endif        
        if (!(globalLong < localLong))
        {
            PrintFailed("long < failed");
        }
        if (!(globalInt < localInt))
        {
            PrintFailed("int < failed");
        }
        if (localConstant < globalConstant)
        {
            PrintFailed("string < failed");
        }
#ifdef TESTFLOATS          
        if (localFloat < globalFloat)
        {
            PrintFailed("float < failed");
        }
#endif
        if (localLong < globalLong)
        {
            PrintFailed("long < failed");
        }
        if (localInt < globalInt)
        {
            PrintFailed("int < failed");
        }
        if (!(localInt < localLong))
        {
            PrintFailed("int < long failed");
        }
        if (localLong < localInt)
        {
            PrintFailed("long < int failed");
        }
#ifdef TESTFLOATS          
        if (localInt < localFloat)
        {
            PrintFailed("int < float failed");
        }
        if (localLong < localFloat)
        {
            PrintFailed("long < float failed");
        }
        if (!(localFloat < localInt))
        {
            PrintFailed("float < int failed");
        }
        if (localLong < localFloat)
        {
            PrintFailed("float < long failed");
        }
#endif        
    }
    TestLessThanOrEqual()
    {
        PrintLn("'<='");
        string localConstant = "zzzzz";
#ifdef TESTFLOATS          
        float  localFloat = 4.141;
#endif        
        long   localLong  = 100001;
        int    localInt   = 10001;
        if (!(globalConstant <= localConstant))
        {
            PrintFailed("string <= failed");
        }
        if (!(globalConstant <= globalConstant))
        {
            PrintFailed("string <= failed");
        }
        if (!(localConstant <= localConstant))
        {
            PrintFailed("string <= failed");
        }
#ifdef TESTFLOATS          
        if (!(globalFloat <= localFloat))
        {
            PrintFailed("float <= failed");
        }
        if (!(globalFloat <= globalFloat))
        {
            PrintFailed("float <= failed");
        }
        if (!(localFloat <= localFloat))
        {
            PrintFailed("float <= failed");
        }
#endif        
        if (!(globalLong <= localLong))
        {
            PrintFailed("long <= failed");
        }
        if (!(globalLong <= globalLong))
        {
            PrintFailed("long <= failed");
        }
        if (!(localLong <= localLong))
        {
            PrintFailed("long <= failed");
        }
        if (!(globalInt <= localInt))
        {
            PrintFailed("int <= failed");
        }
        if (!(globalInt <= globalInt))
        {
            PrintFailed("int <= failed");
        }
        if (!(localInt <= localInt))
        {
            PrintFailed("int <= failed");
        }
        if (localConstant <= globalConstant)
        {
            PrintFailed("string <= failed");
        }
#ifdef TESTFLOATS          
        if (localFloat <= globalFloat)
        {
            PrintFailed("float <= failed");
        }
#endif
        if (localLong <= globalLong)
        {
            PrintFailed("long <= failed");
        }
        if (localInt <= globalInt)
        {
            PrintFailed("int <= failed");
        }
        
        if (!(localInt <= localLong))
        {
            PrintFailed("int <= long failed");
        }
        if (localLong <= localInt)
        {
            PrintFailed("long <= int failed");
        }
#ifdef TESTFLOATS  
        if (localInt <= localFloat)
        {
            PrintFailed("int <= float failed");
        }
        if (localLong <= localFloat)
        {
            PrintFailed("long <= float failed");
        }
        if (!(localFloat <= localInt))
        {
            PrintFailed("float <= int failed");
        }
        if (localLong <= localFloat)
        {
            PrintFailed("float <= long failed");
        }
#endif
    }
    
    TestGreaterThan()
    {
        PrintLn("'>'");
        string localConstant = "zzzzz";
#ifdef TESTFLOATS          
        float  localFloat = 4.141;
#endif
        long   localLong  = 100001;
        int    localInt   = 10001;

        if (!(localConstant > globalConstant))
        {
            PrintFailed("string > failed");
        }
        if (globalConstant > localConstant)
        {
            PrintFailed("string > failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat > localFloat)
        {
            PrintFailed("float > failed");
        }
#endif
        if (globalLong > localLong)
        {
            PrintFailed("long > failed");
        }
        if ((globalInt > localInt))
        {
            PrintFailed("int > failed");
        }
        if ((localInt > localInt))
        {
            PrintFailed("int > failed 2");
        }
#ifdef TESTFLOATS  
        if (!(localFloat > globalFloat))
        {
            PrintFailed("float > failed");
        }
#endif
        if (!(localLong > globalLong))
        {
            PrintFailed("long > failed");
        }
        if (!(localInt > globalInt))
        {
            PrintFailed("int > failed");
        }
        if (localInt > localLong)
        {
            PrintFailed("int > long failed");
        }
        if (!(localLong > localInt))
        {
            PrintFailed("long > int failed");
        }
#ifdef TESTFLOATS  
        if (!(localInt > localFloat))
        {
            PrintFailed("int > float failed");
        }
        if (!(localLong > localFloat))
        {
            PrintFailed("long > float failed");
        }
        if (localFloat > localInt)
        {
            PrintFailed("float > int failed");
        }
        if (!(localLong > localFloat))
        {
            PrintFailed("float > long failed");
        }
#endif
    }
    
    TestGreaterThanOrEqual()
    {
        PrintLn("'>='");
        string localConstant = "zzzzz";
#ifdef TESTFLOATS  
        float  localFloat = 4.141;
#endif
        long   localLong  = 100001;
        int    localInt   = 10001;
        if (globalConstant >= localConstant)
        {
            PrintFailed("string >= failed");
        }
        if (!(globalConstant >= globalConstant))
        {
            PrintFailed("string >= failed");
        }
        if (!(localConstant >= localConstant))
        {
            PrintFailed("string >= failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat >= localFloat)
        {
            PrintFailed("float >= failed");
        }
        if (!(globalFloat >= globalFloat))
        {
            PrintFailed("float >= failed");
        }
        if (!(localFloat >= localFloat))
        {
            PrintFailed("float >= failed");
        }
#endif
        if (globalLong >= localLong)
        {
            PrintFailed("long >= failed");
        }
        if (!(globalLong >= globalLong))
        {
            PrintFailed("long >= failed");
        }
        if (!(localLong >= localLong))
        {
            PrintFailed("long >= failed");
        }
        if (globalInt >= localInt)
        {
            PrintFailed("int >= failed");
        }
        if (!(globalInt >= globalInt))
        {
            PrintFailed("int >= failed");
        }
        if (!(localInt >= localInt))
        {
            PrintFailed("int >= failed");
        }
        if (!(localConstant >= globalConstant))
        {
            PrintFailed("string >= failed");
        }
#ifdef TESTFLOATS          
        if (!(localFloat >= globalFloat))
        {
            PrintFailed("float >= failed");
        }
#endif
        if (!(localLong >= globalLong))
        {
            PrintFailed("long >= failed");
        }
        if (!(localInt >= globalInt))
        {
            PrintFailed("int >= failed");
        }
        
        if (localInt >= localLong)
        {
            PrintFailed("int >= long failed");
        }
        if (!(localLong >= localInt))
        {
            PrintFailed("long >= int failed");
        }
#ifdef TESTFLOATS         
        if (!(localInt >= localFloat))
        {
            PrintFailed("int >= float failed");
        }
        if (!(localLong >= localFloat))
        {
            PrintFailed("long >= float failed");
        }
        if (localFloat >= localInt)
        {
            PrintFailed("float >= int failed");
        }
        if (!(localLong >= localFloat))
        {
            PrintFailed("float >= long failed");
        }
#endif
    }

    TestString()    
    {
        PrintLn("'string'");
        string abcde  = "abcde";
        
        if (abcde + 'f' != "abcdef")
        {
            PrintFailed("append char failed");
        }
        string sub = abcde.Substring(3);
        if (sub != "de")
        {
            PrintFailed("Substring 1 failed");
        }
        sub = abcde.Substring(2,2);
        if (sub != "cd")
        {
            PrintFailed("Substring 2 failed");
        }
        if (!abcde.EndsWith("de"))
        {
            PrintFailed("EndsWith 1 failed");
        }
        if (!abcde.EndsWith('e'))
        {
            PrintFailed("EndsWith 2 failed");
        }
        string replaced = abcde.Replace("bc", "xyz");
        if (replaced != "axyzde")
        {
            PrintFailed("Replace 1 failed");
        }
        replaced = abcde.Replace("de", "y");
        if (replaced != "abcy")
        {
            PrintFailed("Replace 2 failed");
        }
        replaced = abcde.Replace('c', 'z');
        if (replaced != "abzde")
        {
            PrintFailed("Replace 3 failed");
        }
        replaced = abcde.Replace("ab", "ab");
        if (replaced != "abcde")
        {
            PrintFailed("Replace 4 failed");
        }
        replaced = abcde.Replace('c', 'c');
        if (replaced != "abcde")
        {
            PrintFailed("Replace 5 failed");
        }
    }
    
    TestStringCompare()
    {
        PrintLn("String compare");
        
        string aaaaa  = "aaaaa";
        string zzzzz  = "zzzzz";
        string aaaaa2 = "aaaaa";
        
        if (aaaaa > aaaaa2)
        {
            PrintFailed("string > failed 1");
        }
        if (aaaaa >= zzzzz)
        {
            PrintFailed("string >= failed");
        }
        if (aaaaa > zzzzz)
        {
            PrintFailed("string > failed");
        }
        if (!(aaaaa <= zzzzz))
        {
            PrintFailed("string <= failed");
        }
        if (!(aaaaa < zzzzz))
        {
            PrintFailed("string < failed");
        }
        if (aaaaa == zzzzz)
        {
            PrintFailed("string == failed");
        }
        if (!(aaaaa == aaaaa2))
        {
            PrintFailed("string == failed");
        }
        if (aaaaa < aaaaa2)
        {
            PrintFailed("string < failed");
        }
        
    }
    
    bool TrueCounter(ref int count)
    {
        count = count + 1;
        return true;
    }
    bool FalseCounter(ref int count)
    {
        count = count + 1;
        return false;
    }
    
    TestBooleanShortCircuit()
    {
        PrintLn("Short circuit");
        
        int trueCount = 0;
        int falseCount = 0;
        if (!(FalseCounter(ref falseCount) || TrueCounter(ref trueCount)))
        {
            PrintFailed("'false || true' failed 1");
        }
        if (trueCount != 1)
        {
            PrintFailed("'false || true' failed 2");
        }
        if (falseCount != 1)
        {
            PrintFailed("'false || true' failed 3");
        }
                
        trueCount = 0;
        falseCount = 0;
        if (!(TrueCounter(ref trueCount) || FalseCounter(ref falseCount)))
        {
            PrintFailed("'true || false' failed");
        }
        if (trueCount != 1)
        {
            PrintFailed("'true || false' failed");
        }
        if (falseCount != 0)
        {
            PrintFailed("'true || false' failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        if (!(TrueCounter(ref trueCount) || TrueCounter(ref trueCount) || TrueCounter(ref trueCount)))
        {
            PrintFailed("'true || true || true' failed");
        }
        if (trueCount != 1)
        {
            PrintFailed("'true || true || true' failed");
        }
        
        falseCount = 0;
        if (FalseCounter(ref falseCount) || FalseCounter(ref falseCount))
        {
            PrintFailed("'false || false' failed");
        }
        if (falseCount != 2)
        {
            PrintFailed("'false || false' failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        if (!(FalseCounter(ref falseCount) || FalseCounter(ref falseCount) || TrueCounter(ref trueCount)))
        {
            PrintFailed("'false || false || true' failed");
        }
        if (trueCount != 1)
        {
            PrintFailed("'false || false || true' failed");
        }
        if (falseCount != 2)
        {
            PrintFailed("'false || false || true' failed");
        }
        
        
        trueCount = 0;
        falseCount = 0;
        if (FalseCounter(ref falseCount) && TrueCounter(ref trueCount))
        {
            PrintFailed("'false && true' failed");
        }
        if (trueCount != 0)
        {
            PrintFailed("'false && true' failed");
        }
        if (falseCount != 1)
        {
            PrintFailed("'false && true' failed");
        }
                
        trueCount = 0;
        falseCount = 0;
        if (TrueCounter(ref trueCount) && FalseCounter(ref falseCount))
        {
            PrintFailed("'true && false' failed");
        }
        if (trueCount != 1)
        {
            PrintFailed("'true && false' failed");
        }
        if (falseCount != 1)
        {
            PrintFailed("'true && false' failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        if (!(TrueCounter(ref trueCount) && TrueCounter(ref trueCount) && TrueCounter(ref trueCount)))
        {
            PrintFailed("'true && true && true' failed");
        }
        if (trueCount != 3)
        {
            PrintFailed("'true && true && true' failed");
        }
        
        falseCount = 0;
        if (FalseCounter(ref falseCount) && FalseCounter(ref falseCount))
        {
            PrintFailed("'false && false' failed");
        }
        if (falseCount != 1)
        {
            PrintFailed("'false && false' failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        if (FalseCounter(ref falseCount) && FalseCounter(ref falseCount) && TrueCounter(ref trueCount))
        {
            PrintFailed("'false && false && true' failed");
        }
        if (trueCount != 0)
        {
            PrintFailed("'false && false && true' failed");
        }
        if (falseCount != 1)
        {
            PrintFailed("'false && false && true' failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        if (TrueCounter(ref trueCount) && FalseCounter(ref falseCount) && FalseCounter(ref falseCount))
        {
            PrintFailed("'true && false && false' failed");
        }
        if (trueCount != 1)
        {
            PrintFailed("'true && false && false' failed");
        }
        if (falseCount != 1)
        {
            PrintFailed("'true && false && false' failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        if (TrueCounter(ref trueCount) && TrueCounter(ref trueCount) && FalseCounter(ref falseCount))
        {
            PrintFailed("'true && true && false' failed");
        }
        if (trueCount != 2)
        {
            PrintFailed("'true && true && false' failed");
        }
        if (falseCount != 1)
        {
            PrintFailed("'true && true && false' failed");
        }
    }

    uint gindex = 0;
    TestRef()
    {
        PrintLn("'ref' arguments");
        string test = "Test String";
        
        if (!test.IndexOf("String", 4, ref gindex))
        {
            PrintFailed("IndexOf ref of ref of global failed A");
        }
        if (gindex != 5)
        {
            PrintFailed("IndexOf ref of ref of global failed B");
        }
        
        uint hex = 0;
        string word = "0x002A";
        if (UInt.TryParse(word, ref hex))
        {
        }
        if (hex != 42)
        {
            PrintFailed("TryParse ref failed (hex value type)");
        }
        long l = 0;
        word = "100000";
        if (Long.TryParse(word, ref l))
        {
        }
        if (l != 100000)
        {
            PrintFailed("Long.TryParse ref failed (reference type)");
        }
        
        
        uint index = 0;
        if (!test.IndexOf("String", 4, ref index))
        {
            PrintFailed("IndexOf ref of ref of local failed");
        }
        if (index != 5)
        {
            PrintFailed("IndexOf ref of ref of local failed");
        }
        
        
    }
    
    TestSwitch()
    {
        PrintLn("'switch'");
        
        int count = 0;
        char check = 'A';
        switch (check)
        {
            case 'A':
            {
                int inc = 1;
                count = count + inc;
            }
            case 'B':
            {
                int inc = 2;
                count = count + inc;
            }
            default:
            {
                int inc = 4;
                count = count + inc;
            }
        }
        if (count != 1)
        {
            PrintFailed("'switch' 1 failed");
        }
        count = 0;
        check = 'B';
        switch (check)
        {
            case 'A':
            {
                int inc = 1;
                count = count + inc;
            }
            case 'B':
            {
                int inc = 2;
                count = count + inc;
            }
            default:
            {
                int inc = 4;
                count = count + inc;
            }
        }
        if (count != 2)
        {
            PrintFailed("'switch' 2 failed");
        }
        count = 0;
        check = 'C';
        switch (check)
        {
            case 'A':
            {
                int inc = 1;
                count = count + inc;
            }
            case 'B':
            {
                int inc = 2;
                count = count + inc;
            }
            default:
            {
                int inc = 4;
                count = count + inc;
            }
        }
        if (count != 4)
        {
            PrintFailed("'switch' 3 failed");
        }
    }
    TestWhile()
    {
        PrintLn("'while'");
        
        int trueCount = 0;
        int falseCount = 0;
        int count = 0;
        while ((falseCount < 10) && !(FalseCounter(ref falseCount)))
        {
            count++;
        }
        if ((falseCount != 11) && (count != 10))
        {
            PrintFailed("'while' 1 failed");
        }
        count = 0;
        while ((trueCount < 10) && TrueCounter(ref trueCount))
        {
            count++;
        }
        if ((trueCount != 11) && (count != 10))
        {
            PrintFailed("'while' 2 failed");
        }
        
        trueCount = 0;
        falseCount = 0;
        uint total = 0;
        while (TrueCounter(ref trueCount) && (trueCount <= 10))
        {
            falseCount = 0;
            while (!(FalseCounter(ref falseCount)) && (falseCount <= 10))
            {
                total++;
            }
        }
        if ((trueCount != 11) || (falseCount != 11)  || (total != 100))
        {
            PrintFailed("'while' 3 failed");
        }
    } // TestWhile
    
    TestVariantDictionary()
    {
        PrintLn("<string,variant>");    
        
        <string, variant> dict;
        
        string ss = "string";
        dict["string"] = "string";
        dict["true"] = true;
        dict["false"] = false;
        dict["uint"] = uint(10);
        long ll = 100;
        dict["long"] = ll;
        
        <string> slist;
        slist.Append("string");
        dict["slist"] = slist;
        
        uint count = 0;
        foreach (var kv in dict)
        {
            switch (kv.key)
            {
                case "string":
                {
                    string s = kv.value;
                    if (s == "string")
                    {
                        count++;
                    }
                    else
                    {
                        PrintFailed("<string,variant> 1");
                    }
                }
                case "slist":
                {
                    <string> sl = kv.value;
                    string v = sl[0];
                    if (v == "string")
                    {
                        count++;
                    }
                    else
                    {
                        PrintFailed("<string,variant> 2");
                    }
                }
                case "long":
                {
                    long l = long(kv.value);
                    if (l == 100)
                    {
                        count++;
                    }
                    else
                    {
                        PrintFailed("<string,variant> 3");
                    }
                }
                case "uint":
                {
                    uint u = uint(kv.value);
                    if (u == 10)
                    {
                        count++;
                    }
                    else
                    {
                        PrintFailed("<string,variant> 4");
                    }
                }
                case "true":
                {
                    bool b = bool(kv.value);
                    if (b)
                    {
                        count++;
                    }
                    else
                    {
                        PrintFailed("<string,variant> 5");
                    }
                }
                case "false":
                {
                    bool b = bool(kv.value);
                    if (!b)
                    {
                        count++;
                    }
                    else
                    {
                        PrintFailed("<string,variant> 6");
                    }
                }
                default:
                {
                    PrintFailed("<string,variant> 7");
                }
            }
        }
        if (count != 6)
        {
            PrintFailed("<string,variant> 8");
        }
        
    } // TestVariantDictionary
    
    TestForEach()
    {
        PrintLn("'foreach'");    
        
        <string> lst;
        lst.Append("one");
        lst.Append("two");
        lst.Append("three");
        lst.Append("four");
        lst.Append("five");
        
        uint count = 0;
        foreach (var s in lst)
        {
            if (s == "two")
            {
                continue;
            }
            count++;
            if (s == "four")
            {
                break;
            }
        }
        if (count != 3)
        {
            PrintFailed("'foreach' 1");
        }
        
        count = 0;
        string ss = "abcde";
        foreach (var s in ss)
        {
            if (s == 'b')
            {
                continue;
            }
            count++;
            if (s == 'd')
            {
                break;
            }
        }
        if (count != 3)
        {
            PrintFailed("'foreach' 2");
        }
        
        count = 0;
        char[5] arr;
        arr[0] = 'a';
        arr[1] = 'b';
        arr[2] = 'c';
        arr[3] = 'd';
        arr[4] = 'e';
        
        foreach (var s in arr)
        {
            if (s == 'b')
            {
                continue;
            }
            count++;
            if (s == 'd')
            {
                break;
            }
        }
        if (count != 3)
        {
            PrintFailed("'foreach' 3");
        }

        count = 0;
        <uint,char> dict;
        dict[0] = 'a';
        dict[1] = 'b';
        dict[2] = 'c';
        dict[3] = 'd';
        dict[4] = 'e';

        uint breaks = 0;        
        uint continues = 0;
        foreach (var s in dict)
        {
            if (s.value == 'b')
            {
                continues++;
                continue;
            }
            count++;
            if (s.value == 'd')
            {
                breaks++;
                break;
            }
        }
        if ((count == 0) || (count == 5) || (continues > 1) || (breaks != 1))
        {
            PrintFailed("'foreach' 4");
        }
        
        breaks = 0;        
        continues = 0;
        count = 0;
        foreach (var s in dict)
        {
            if (s.key == 0)
            {
                continues++;
                continue;
            }
            count++;
            if (s.key == 3)
            {
                breaks++;
                break;
            }
        }
        if ((count == 0) || (count == 5) || (continues > 1) || (breaks != 1))
        {
            PrintFailed("'foreach' 5");
        }
        
        <string,char> dict2;
        dict2["0"] = 'a';
        dict2["1"] = 'b';
        dict2["2"] = 'c';
        dict2["3"] = 'd';
        dict2["4"] = 'e';
        
        count = 0;
        breaks = 0;        
        continues = 0;
        foreach (var s in dict2)
        {
            if (s.value == 'b')
            {
                continues++;
                continue;
            }
            count++;
            if (s.value == 'd')
            {
                breaks++;
                break;
            }
        }
        if ((count == 0) || (count == 5) || (continues > 1) || (breaks != 1))
        {
            PrintFailed("'foreach' 6");
        }
        
        breaks = 0;        
        continues = 0;
        count = 0;
        foreach (var s in dict2)
        {
            if (s.key == "0")
            {
                continues++;
                continue;
            }
            count++;
            if (s.key == "3")
            {
                breaks++;
                break;
            }
        }
        if ((count == 0) || (count == 5) || (continues > 1) || (breaks != 1))
        {
            PrintFailed("'foreach' 7");
        }
    } //TestForEach()
    
    TestFor()
    {
        PrintLn("'for'"); 
        
        <string> lst;
        lst.Append("one");
        lst.Append("two");
        lst.Append("three");
        lst.Append("four");
        lst.Append("five");
        
        uint count = 0;
        for (uint i = 0; i < 5; i++)
        {
            if (lst[i] == "two")
            {
                continue;
            }
            count++;
            if (lst[i] == "four")
            {
                break;
            }
        }
        if (count != 3)
        {
            PrintFailed("'for' 1");
        }
        
        uint i2 = 0;
        count = 0;
        bool bob; // old compiler cannot deal with empty statement
        for ( bob = true ; i2 < 5; i2++)
        {
            if (lst[i2] == "two")
            {
                continue;
            }
            count++;
            if (lst[i2] == "four")
            {
                break;
            }
        }
        if (count != 3)
        {
            PrintFailed("'for' 2");
        }
        
    } // TestFor
    
    TestUIntMath()
    {
        PrintLn("'uint'");
        
        // globalUInt   = 10000;
        // globalUInt2  = 20000;
        uint localUInt  = 10000;
        uint localUInt2 = 20000;
        
        // +
        if (localUInt + localUInt != 20000)
        {
            PrintFailed("'uint' 1");        
        }
        if (localUInt2 + localUInt2 != 40000)
        {
            PrintFailed("'uint' 2");        
        }
        if (globalUInt + localUInt != 20000)
        {
            PrintFailed("'uint' 3");        
        }
        if (globalUInt2 + localUInt2 != 40000)
        {
            PrintFailed("'uint' 4");        
        }
        if (localUInt2 + localUInt2 + globalUInt2 != 60000)
        {
            PrintFailed("'uint' 5");
        }
        
        // -
        if (localUInt2 - localUInt != 10000)
        {
            PrintFailed("'uint' 6");
        }
        if (localUInt2 - localUInt - localUInt != 0)
        {
            PrintFailed("'uint' 7");
        }
        
        // *
        if (localUInt2 != 2 * localUInt)
        {
            PrintFailed("'uint' 8");
        }
        if (localUInt * 5 != 50000)
        {
            PrintFailed("'uint' 12");
        }
        
        // /
        if (localUInt2 / 2 != localUInt)
        {
            PrintFailed("'uint' 9");
        }
        if (localUInt / 250 != 40)
        {
            PrintFailed("'uint' 10");
        }
        
        // %
        if (localUInt % 3 != 1)
        {
            PrintFailed("'uint' 11");
        }
    } // TestUIntMath
    
    TestIntMath()
    {
        PrintLn("'int'");
        
        // globalInt   = 10000;
        // globalInt2  = 10001;
        int localInt  = 10000;
        int localInt2 = 10001;
        int localIntNeg = 0 - localInt;
        
        // +
        if (localInt + localInt != 20000)
        {
            PrintFailed("'int' 1");        
        }
        if (localInt2 + localInt2 != 20002)
        {
            PrintFailed("'int' 2");        
        }
        if (globalInt + localInt != 20000)
        {
            PrintFailed("'int' 3");        
        }
        if (globalInt2 + localInt2 != 20002)
        {
            PrintFailed("'int' 4");        
        }
        if (localInt2 + localInt2 + globalInt2 != 30003)
        {
            PrintFailed("'int' 5");
        }
        
        // -
        if (localInt2 - localInt != 1)
        {
            PrintFailed("'int' 6");
        }
        if (localInt - localInt2 - localInt2 != -10002)
        {
            PrintFailed("'int' 7");
        }
        if (-localInt2 != -10001)
        {
            PrintFailed("'int' 12");
        }
        
        // *
        if (20000 != 2 * localInt)
        {
            PrintFailed("'int' 8");
        }
        if (localInt * 3 != 30000)
        {
            PrintFailed("'int' 9");
        }
        if (-20000 != -2 * localInt)
        {
            PrintFailed("'int' 10");
        }
        
        if (20000 != -2 * localIntNeg)
        {
            PrintFailed("'int' 11");
        }
        
        // /
        if (localInt / 2 != 5000)
        {
            PrintFailed("'int' 12");
        }
        if (localInt / 250 != 40)
        {
            PrintFailed("'int' 13");
        }
        if (localInt / -2 != -5000)
        {
            PrintFailed("'int' 14");
        }
        if (localIntNeg / -2 != 5000)
        {
            PrintFailed("'int' 15");
        }
        
        // %
        if (localInt % 3 != 1)
        {
            PrintFailed("'int' 16");
        }
        //if (localIntNeg % 3 != 1)
        //{
        //    PrintFailed("'int' 17");
        //}
    } // TestIntMath
	
	
    TestLongMath()
    {
        PrintLn("'long'");
        
        // globalLong   = 100000;
        // globalLong2  = 100001;
        long localLong  = 100000;
        long localLong2 = 100001;
        long localLongNeg = 0 - localLong;
        
        // +
        if (localLong + localLong != 200000)
        {
            PrintFailed("'long' 1");        
        }
        if (localLong2 + localLong2 != 200002)
        {
            PrintFailed("'long' 2");        
        }
        if (globalLong + localLong != 200000)
        {
            PrintFailed("'long' 3");        
        }
        if (globalLong2 + localLong2 != 200002)
        {
            PrintFailed("'long' 4");        
        }
        if (localLong2 + localLong2 + globalLong2 != 300003)
        {
            PrintFailed("'long' 5");
        }
        
        // -
        if (localLong2 - localLong != 1)
        {
            PrintFailed("'long' 6");
        }
        if (localLong - localLong2 - localLong2 != -100002)
        {
            PrintFailed("'long' 7");
        }
        if (-localLong2 != -100001)
        {
            PrintFailed("'long' 8");
        }
        
        // *
        if (200000 != 2 * localLong)
        {
            PrintFailed("'long' 9");
        }
        if (localLong * 3 != 300000)
        {
            PrintFailed("'long' 10");
        }
        if (-200000 != -2 * localLong)
        {
            PrintFailed("'long' 11");
        }
        
        if (200000 != -2 * localLongNeg)
        {
            PrintFailed("'long' 12");
        }
        
        // /
        if (localLong / 2 != 50000)
        {
            PrintFailed("'long' 13");
        }
        
        if (localLong / 2500 != 40)
        {
            PrintFailed("'long' 14");
        }
        if (localLong / -2 != -50000)
        {
            PrintFailed("'long' 15");
        }
        if (localLongNeg / -2 != 50000)
        {
            PrintFailed("'long' 16");
        }
        
        // %
        if (localLong % 3 != 1)
        {
            PrintFailed("'long' 17");
        }
        //if (localLongNeg % 3 != 1)
        //{
        //    PrintFailed("'long' 18");
        //}
    } // TestLongMath
    
    {
        Screen.Clear();
        
        TestString();
        TestStringSystem();
        TestStringCompare();
        
        TestEquals();
        TestGreaterThan();
        TestGreaterThanOrEqual();
        TestLessThanOrEqual();
        TestLessThan();
        
        TestConstants();
        
        TestLongMath();
        TestUIntMath();
        TestIntMath();
        
        

#ifndef H6502
        TestVariantDictionary(); // Variant.Box
#endif   
		
        TestForEach();
        
        TestDictionary();       
        TestDictionaryOfLists();
        TestListOfDictionaries();
        TestDictionaryOfDictionaries();
        TestValueDictionary();
        
        
        TestList();
        TestArray();
        
        
        TestSwitch();
        
        TestRef();
        TestBooleanShortCircuit();
        TestWhile();
        
        TestFor();
        
        TestDictionaryExpandVV();
        TestDictionaryExpandRV();
      		TestDictionaryExpandVR();
      		TestDictionaryExpandRR();
        
        
                      
#ifndef H6502   
        TestFiles();
#ifdef TEXTBUFFER
        TestTextBuffer();
#endif
        
        
        
#else
        PrintLn("Done");
#endif

#ifndef H6502
        Key key = ReadKey();
#endif
    }
}

