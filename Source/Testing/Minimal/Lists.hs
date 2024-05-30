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
        foreach (var item in stringList)
        {
            i++;
        }
        if (i != 3)
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
        TestList();
        WriteLn("TestList completed");
        TestList32();
        WriteLn("TestList32 completed");
        WriteLn();
        WriteLn("ListTests Ok");
    }
}

