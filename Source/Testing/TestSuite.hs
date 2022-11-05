program TestSuite
{

//#define TINYHOPPER
    
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"
#ifdef TEXTBUFFER
    uses "/Source/Editor/TextBuffer"
#endif
    PrintFailed(string message)
    {
        PrintLn("  " + message, Red, Black);
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
        intArray[0] = 3;
        intArray[1] = 2;
        intArray[2] = 1;
        if (intArray.Count != 5)
        {
            PrintFailed("Array Int: Length failed");
        }
        if (intArray[2] != 1)
        {
            PrintFailed("Array Int: [] = failed");
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
    }
    
    TestDictionary()
    {
        PrintLn("Dictionary");
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
            PrintFailed("Dictionary String: Contains failed");
        }
        if (stringDictionary.Contains("d"))
        {
            PrintFailed("Dictionary String: Contains failed");
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
        
        count = 0;
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
        
        <string,int> dictionary0;
        dictionary0["one"] = 1;
        dictionary0["two"] = 2;
        dictionary0["three" ] = 3;
        
        <string,int> dictionary2;
        dictionary2["four"] = 4;
        dictionary2["five"] = 5;
        dictionary2["six"] = 6;
       
        < string, <string,int> > dictionaryOfDictionaries;
        dictionaryOfDictionaries["dict1"] = dictionary0;
        dictionaryOfDictionaries["dict2"] = dictionary2;
        
        count = 0;
        <string,int> dictionary3 = dictionaryOfDictionaries.Get("dict1");
        foreach (var kv in dictionary3)
        {
            count = count +  kv.value;
        }
        if (count != 6)
        {
            PrintFailed("Dictionary : <string, <string, int> > failed");
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
        if (count != 21)
        {
            PrintFailed("Dictionary : <string, <string, int> > failed");
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
        
        count = 0;
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
        PrintLn("Equals constants");
        
        // contants
        if (1 == 0)
        {
            PrintFailed("1 == 0 failed");
        }
        if (!(1 == 1))
        {
            PrintFailed("!(1 == 1) failed");
        }
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
        
        PrintLn("Equals byte");
        int ba = 1;
        int bb = 0;
        int bc = 1;
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
        
        PrintLn("Equals int");
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
        PrintLn("Equals long");
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
        
        
        PrintLn("Equals float");
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

    const float  globalFloat = 3.141;
    
    const float  globalFloat2 = 4.141;
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
        float  localFloat = 3.141;
        long   localLong  = 100000;
        int    localInt   = 10000;
        if (globalConstant != localConstant)
        {
            PrintFailed("global const string failed");
        }
        if (globalFloat != localFloat)
        {
            PrintFailed("global const float 1 failed");
        }
        if (globalLong != localLong)
        {
            PrintFailed("global const long failed");
        }
        if (globalInt != localInt)
        {
            PrintFailed("global const int failed");
        }
        localFloat = localFloat + 1;
        localLong = localLong + 1;
        localConstant = localConstant + "1";
        localInt = localInt + 1;
        
        if (globalConstant3 != localConstant)
        {
            PrintFailed("global const string 2 failed");
        }
        if (globalFloat2 != localFloat)
        {
            PrintFailed("global const float 2 failed");
        }
        if (globalLong2 != localLong)
        {
            PrintFailed("global const long 2 failed");
        }
        if (globalInt2 != localInt)
        {
            PrintFailed("global const long 2 failed");
        }
    }
    
    TestLessThan()
    {
        PrintLn("Less than");
        
        string localConstant = "zzzzz";
        int    localInt   = 10001;
        float  localFloat = 4.141;
        long   localLong  = 100001;
        if (!(globalConstant < localConstant))
        {
            PrintFailed("string < failed");
        }
        if (!(globalFloat < localFloat))
        {
            PrintFailed("float < failed");
        }
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
        if (localFloat < globalFloat)
        {
            PrintFailed("float < failed");
        }
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
    }
    TestLessThanOrEqual()
    {
        PrintLn("Less than or equal");
        
        string localConstant = "zzzzz";
        float  localFloat = 4.141;
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
        if (localFloat <= globalFloat)
        {
            PrintFailed("float <= failed");
        }
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
    }
    
    TestGreaterThan()
    {
        PrintLn("Greater than");
        
        string localConstant = "zzzzz";
        float  localFloat = 4.141;
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
        if (globalFloat > localFloat)
        {
            PrintFailed("float > failed");
        }
        if (globalLong > localLong)
        {
            PrintFailed("long > failed");
        }
        if ((globalInt > localInt))
        {
            PrintFailed("int > failed");
        }
        if (!(localFloat > globalFloat))
        {
            PrintFailed("float > failed");
        }
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
    }
    
    TestGreaterThanOrEqual()
    {
        PrintLn("Greater than or equal");
        
        string localConstant = "zzzzz";
        float  localFloat = 4.141;
        long   localLong  = 100001;
        int    localInt   = 10001;
    
        if (globalConstant >= localConstant)
        {
            PrintFailed("string >= failed A");
        }
        if (!(globalConstant >= globalConstant))
        {
            PrintFailed("string >= failed B");
        }
        if (!(localConstant >= localConstant))
        {
            PrintFailed("string >= failed C");
        }
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
            PrintFailed("string >= failed D");
        }
        if (!(localFloat >= globalFloat))
        {
            PrintFailed("float >= failed");
        }
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
            PrintFailed("string >= failed E");
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
        PrintLn("Boolean short circuit");
        
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
        
        uint hex = 0;
        string word = "0x002A";
        if (Token.TryParseHex(word, ref hex))
        {
        }
        if (hex != 42)
        {
            PrintFailed("TryParseHex ref failed (value type)");
        }
        long l = 0;
        word = "100000";
        if (Token.TryParseLong(word, ref l))
        {
        }
        if (l != 100000)
        {
            PrintFailed("TryParseLong ref failed (reference type)");
        }
        
        string test = "Test String";
        uint index = 0;
        if (!test.IndexOf("String", 4, ref index))
        {
            PrintFailed("IndexOf ref of ref of local failed");
        }
        if (index != 5)
        {
            PrintFailed("IndexOf ref of ref of local failed");
        }
        if (!test.IndexOf("String", 4, ref gindex))
        {
            PrintFailed("IndexOf ref of ref of global failed");
        }
        if (gindex != 5)
        {
            PrintFailed("IndexOf ref of ref of global failed");
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
    
    {
        Screen.Clear();
        
        TestFor();
        TestForEach();
        TestVariantDictionary();
 
        TestRef();
               
        TestFiles();
#ifdef TEXTBUFFER
        TestTextBuffer();
#endif
        TestBooleanShortCircuit();
        
        TestStringCompare();
        TestEquals();
 
        TestConstants();
        
        TestGreaterThan();
        TestGreaterThanOrEqual();
        TestLessThanOrEqual();
        TestLessThan();
        
        TestList();
        TestSwitch();
        
        TestDictionary();
        TestArray();
        
        TestWhile();
        
        Key key = ReadKey();
    }
}
