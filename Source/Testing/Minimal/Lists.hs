program Lists
{
    uses "/Source/Library/Boards/Hopper6502"
    
    PrintFailed(string message)
    {
        WriteLn("  " + message);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
    
    TestList32()
    {
        WriteLn("List 32");
        <float> floatList;
        floatList.Append(0);
        floatList.Append(20);     // byte
        floatList.Append(-10);    // int
        floatList.Append(40000);  // uint
        floatList.Append(100000); // long
        floatList.Append(1.5);
        floatList.Append(2.5);
        floatList.Append(3.5);
        floatList.Append(4.5);
        
        float total;
        foreach (var f in floatList)
        {
            total += f;
        }
        
        if (total != 140022.0)
        {
            PrintFailed("List 32: float failed");
        }
        else
        {
            WriteLn("List 32: float passed");
        }
        
        <long> longList;
        longList.Append(0);
        longList.Append(20);     // byte
        longList.Append(-10);    // int
        longList.Append(40000); // uint
        longList.Append(100000);
        longList.Append(-40000);
        longList.Append(80000);
        longList.Append(-80000);
        
        long ltotal;
        foreach (var l in longList)
        {
            ltotal += l;
        }
        if (ltotal != 100010)
        {
            PrintFailed("List 32: long failed");
        }
        else
        {
            WriteLn("List 32: long passed");
        }
    }
    
    TestListUInt()
    {
        WriteLn("List UInt");
        <uint> uintList;
        uintList.Append(1);
        uintList.Append(2);
        uintList.Append(3);
        uintList.Append(40000);
        uintList.Append(5);
        
        if (uintList.Count != 5)
        {
            PrintFailed("List UInt: Length failed");
        }
        else
        {
            WriteLn("List UInt: Length passed");
        }
        
        int i = 0;
        foreach (var item in uintList)
        {
            i++;
        }
        if (i != 5)
        {
            PrintFailed("List UInt: foreach failed");
        }
        else
        {
            WriteLn("List UInt: foreach passed");
        }
        
        uint result = uintList.GetItem(2);
        if (result != 3)
        {
            PrintFailed("List UInt: GetItem failed");
        }
        else
        {
            WriteLn("List UInt: GetItem passed");
        }
        
        uintList.SetItem(2, 10);
        result = uintList.GetItem(2);
        if (result != 10)
        {
            PrintFailed("List UInt: SetItem failed");
        }
        else
        {
            WriteLn("List UInt: SetItem passed");
        }
        
        uintList.Insert(2, 15);
        result = uintList.GetItem(2);
        if (result != 15)
        {
            PrintFailed("List UInt: Insert failed");
        }
        else
        {
            WriteLn("List UInt: Insert passed");
        }
        
        if (!uintList.Contains(15))
        {
            PrintFailed("List UInt: Contains failed");
        }
        else
        {
            WriteLn("List UInt: Contains passed");
        }
        
        if (uintList.Contains(999))
        {
            PrintFailed("List UInt: Contains failed (non-existent item)");
        }
        else
        {
            WriteLn("List UInt: Contains passed (non-existent item)");
        }
        
        uintList.Remove(2);
        result = uintList.GetItem(2);
        if (result != 10)
        {
            PrintFailed("List UInt: Remove failed");
        }
        else
        {
            WriteLn("List UInt: Remove passed");
        }
        
        uintList.Clear();
        if (uintList.Count != 0)
        {
            PrintFailed("List UInt: Clear failed");
        }
        else
        {
            WriteLn("List UInt: Clear passed");
        }
    }
    
    
    TestList()
    {
        WriteLn("List");
        <string> stringList;
        stringList.Append("item 1");
        stringList.Append("item 2");
        stringList.Append("item 3");
        
        if (stringList.Count != 3)
        {
            PrintFailed("List String: Length failed");
        }
        else
        {
            WriteLn("List String: Length passed");
        }
        
        int i = 0;
        string appended;
        foreach (var item in stringList)
        {
            i++;
            appended += item;
            appended += ',';
        }
        if (i != 3)
        {
            PrintFailed("List String: foreach failed");
        }
        else
        {
            WriteLn("List String: foreach passed");
        }
        if (appended != "item 1,item 2,item 3,")
        {
            PrintFailed("List String: foreach failed");
        }
        else
        {
            WriteLn("List String: foreach passed");
        }
        
        string result = stringList.GetItem(1);
        if (result != "item 2")
        {
            PrintFailed("List String: GetItem failed");
        }
        else
        {
            WriteLn("List String: GetItem passed");
        }
        
        stringList.SetItem(1, "new item");
        result = stringList.GetItem(1);
        if (result != "new item")
        {
            PrintFailed("List String: SetItem failed");
        }
        else
        {
            WriteLn("List String: SetItem passed");
        }
        
        stringList.Insert(1, "inserted item");
        result = stringList.GetItem(1);
        if (result != "inserted item")
        {
            PrintFailed("List String: Insert failed");
        }
        else
        {
            WriteLn("List String: Insert passed");
        }
        
        if (!stringList.Contains("inserted item"))
        {
            PrintFailed("List String: Contains failed");
        }
        else
        {
            WriteLn("List String: Contains passed");
        }
        
        if (stringList.Contains("non-existent item"))
        {
            PrintFailed("List String: Contains failed (non-existent item)");
        }
        else
        {
            WriteLn("List String: Contains passed (non-existent item)");
        }
        
        stringList.Remove(1);
        result = stringList.GetItem(1);
        if (result != "new item")
        {
            PrintFailed("List String: Remove failed");
        }
        else
        {
            WriteLn("List String: Remove passed");
        }
        
        stringList.Clear();
        if (stringList.Count != 0)
        {
            PrintFailed("List String: Clear failed");
        }
        else
        {
            WriteLn("List String: Clear passed");
        }
        
        <bool> boolList;
        boolList.Append(true);
        boolList.Append(false);
        boolList.Append(false);
        
        if (boolList.Count != 3)
        {
            PrintFailed("List Boolean: Length failed");
        }
        else
        {
            WriteLn("List Boolean: Length passed");
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
        else
        {
            WriteLn("List Boolean: foreach passed");
        }
        
        bool bresult = boolList.GetItem(0);
        if (bresult != true)
        {
            PrintFailed("List Boolean: GetItem failed");
        }
        else
        {
            WriteLn("List Boolean: GetItem passed (index 0)");
        }
        
        bresult = boolList.GetItem(1);
        if (bresult != false)
        {
            PrintFailed("List Boolean: GetItem failed");
        }
        else
        {
            WriteLn("List Boolean: GetItem passed (index 1)");
        }
        
        bresult = boolList.GetItem(2);
        if (bresult != false)
        {
            PrintFailed("List Boolean: GetItem failed");
        }
        else
        {
            WriteLn("List Boolean: GetItem passed (index 2)");
        }
        
        boolList.SetItem(1, true);
        bresult = boolList.GetItem(1);
        if (bresult != true)
        {
            PrintFailed("List Boolean: SetItem failed");
        }
        else
        {
            WriteLn("List Boolean: SetItem passed");
        }
        
        if (!boolList.Contains(true))
        {
            PrintFailed("List Boolean: Contains failed (true)");
        }
        else
        {
            WriteLn("List Boolean: Contains passed (true)");
        }
        
        if (!boolList.Contains(false))
        {
            PrintFailed("List Boolean: Contains failed (false)");
        }
        else
        {
            WriteLn("List Boolean: Contains passed (false)");
        }
        
        boolList.Clear();
        if (boolList.Count != 0)
        {
            PrintFailed("List Boolean: Clear failed");
        }
        else
        {
            WriteLn("List Boolean: Clear passed");
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
        else
        {
            WriteLn("List : < < string > > GetItem passed");
        }
        
        uint count = listOfLists.Count;
        if (count != 1)
        {
            PrintFailed("List : < < string > > Length failed");
        }
        else
        {
            WriteLn("List : < < string > > Length passed");
        }
        
        < <string> > lOL = listOfListsOfLists.GetItem(0);
        <string> lOS = lOL.GetItem(0);
        value = lOS.GetItem(0);
        if (value != "one")
        {
            PrintFailed("List : < < < string > > > GetItem(0) failed");
        }
        else
        {
            WriteLn("List : < < < string > > > GetItem(0) passed");
        }
        value = lOS.GetItem(1);
        if (value != "two")
        {
            PrintFailed("List : < < < string > > > GetItem(1) failed");
        }
        else
        {
            WriteLn("List : < < < string > > > GetItem(1) passed");
        }
        value = lOS.GetItem(2);
        if (value != "three")
        {
            PrintFailed("List : < < < string > > > GetItem(2) failed");
        }
        else
        {
            WriteLn("List : < < < string > > > GetItem(2) passed");
        }
                
        listOfLists.Clear();
        count = listOfLists.Count;
        if (count != 0)
        {
            PrintFailed("List : < < string > > Clear failed");
        }
        else
        {
            WriteLn("List : < < string > > Clear passed");
        }
        
        plainlist.Remove(1);
        if (plainlist.Count != 2)
        {
            PrintFailed("List : Remove failed 1");
        }
        else
        {
            WriteLn("List : Remove passed 1");
        }
        
        plainlist.Remove(0);
        if (plainlist.Count != 1)
        {
            PrintFailed("List : Remove failed 2");
        }
        else
        {
            WriteLn("List : Remove passed 2");
        }
        
        plainlist.Remove(0);
        if (plainlist.Count != 0)
        {
            PrintFailed("List : Remove failed 3");
        }
        else
        {
            WriteLn("List : Remove passed 3");
        }
        
        plainlist.Insert(0, "aaa");
        if (plainlist[0] != "aaa")
        {
            PrintFailed("List : Insert failed 1");
        }
        else
        {
            WriteLn("List : Insert passed 1");
        }
        
        plainlist.Insert(1, "end");
        if (plainlist[1] != "end")
        {
            PrintFailed("List : Insert failed 2");
        }
        else
        {
            WriteLn("List : Insert passed 2");
        }
        
        plainlist.Insert(1, "bbb");
        if (plainlist[1] != "bbb")
        {
            PrintFailed("List : Insert failed 3");
        }
        else
        {
            WriteLn("List : Insert passed 3");
        }
        
        long key = 0xAABBCCDD;
        <byte> bytes;
        bytes.Append(key.GetByte(0));
        bytes.Append(key.GetByte(1));
        bytes.Append(key.GetByte(2));
        bytes.Append(key.GetByte(3));
        string bstr;
        foreach (var b in bytes)
        {
            string bs = b.ToHexString(2);
            bstr = bstr + bs;
        }
        if (bstr != "DDCCBBAA")
        {
            PrintFailed("List : long.GetByte failed");
        }
        else
        {
            WriteLn("List : long.GetByte passed");
        }
    }
    
    Hopper()
    {
        TestListUInt();
        WriteLn("TestListUInt completed");
        TestList();
        WriteLn("TestList completed");
        TestList32();
        WriteLn("TestList32 completed");
        WriteLn();
        WriteLn("ListTests Ok");
    }
}

