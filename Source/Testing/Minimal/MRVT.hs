program MRVT
{
    uses "/Source/Minimal/IO"
    
    PrintFailed(string message)
    {
        WriteLn("  " + message);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
    
    TestStringTrim()
    {
        WriteLn("'string' Trim");
        string str = "   untrimmed string   ";
        string trimmed = str;
        String.TrimLeft(ref trimmed);
        if (trimmed != "untrimmed string   ")
        {
            PrintFailed("'string': TrimLeft failed 1");
        }
        trimmed = "";
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimLeft failed 2");
        }
        trimmed = " ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimLeft failed 3");
        }
        trimmed = " a ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "a ")
        {
            PrintFailed("'string': TrimLeft failed 4");
        }
        trimmed = "a ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "a ")
        {
            PrintFailed("'string': TrimLeft failed 5");
        }
        
        trimmed = str;
        trimmed = trimmed.TrimLeft();
        if (trimmed != "untrimmed string   ")
        {
            PrintFailed("'string': TrimLeft failed 6");
        }
        
        trimmed = "";
        trimmed = trimmed.TrimLeft();
        if (trimmed != "")
        {
            PrintFailed("'string': TrimLeft failed 7");
        }
                
        trimmed = str;
        String.TrimRight(ref trimmed);
        if (trimmed != "   untrimmed string")
        {
            PrintFailed("'string': TrimRight failed 8");
        }
        trimmed = "";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimRight failed 9");
        }
        trimmed = "  ";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimRight failed 10");
        }
        trimmed = " ";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimRight failed 11");
        }
        trimmed = " a";
        String.TrimRight(ref trimmed);
        if (trimmed != " a")
        {
            PrintFailed("'string': TrimRight failed 12");
        }
        
        trimmed = str;
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "untrimmed string")
        {
            PrintFailed("'string': Trim failed 13");
        }
        trimmed = "";
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': Trim failed 14");
        }
        trimmed = "    ";
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': Trim failed 15");
        }
        
        trimmed = str;
        trimmed = trimmed.Trim();
        if (trimmed != "untrimmed string")
        {
            PrintFailed("'string': Trim failed 16");
        }
        trimmed = str;
        String.Trim(ref trimmed);
        if (trimmed != "untrimmed string")
        {
            PrintFailed("'string': Trim failed 17");
        }
        
        str = "abcde";
        trimmed = str.Substring(2);
        if (trimmed != "cde")
        {
            PrintFailed("'string': Substring failed 18");
        }
        str = "abcde";
        trimmed = str.Substring(2, 2);
        if (trimmed != "cd")
        {
            PrintFailed("'string': Substring failed 19");
        }
        str = "abcde";
        Substring(ref str, 1);
        if (str != "bcde")
        {
            PrintFailed("'string': Substring failed 20");
        }
        str = "abcde";
        trimmed = str.Substring(3);
        if (trimmed != "de")
        {
            PrintFailed("'string': Substring failed 21");
        }
        str = "abcde";
        trimmed = str.Substring(10);
        if (trimmed != "")
        {
            PrintFailed("'string': Substring failed 22");
        }
        str = "abcde";
        Substring(ref str, 10);
        if (str != "")
        {
            PrintFailed("'string': Substring failed 23");
        }
        str = "abcde";
        trimmed = str.Substring(3,10);
        if (trimmed != "de")
        {
            PrintFailed("'string': Substring failed 24");
        }
        str = "abcde";
        trimmed = str.Substring(10,10);
        if (trimmed != "")
        {
            PrintFailed("'string': Substring failed 25");
        }
        str = "abcde";
        Substring(ref str, 0);
        if (str != "abcde")
        {
            PrintFailed("'string': Substring failed 26");
        }
        
    }
    
    TestCharSystem()
    {
        WriteLn("system 'char'");
        
        char ch = 'a';
        string str = ch.ToString();
        if (str != "a")
        {
            PrintFailed("'char': ToString failed 1");
        }
        
        ch = 'a';
        str = "bcde";
        str = ch + str;
        if (str != "abcde")
        {
            PrintFailed("'char': ToString failed 2");
        }
        
        ch = 'e';
        str = "abcd";
        str = str + ch;
        if (str != "abcde")
        {
            PrintFailed("'char': ToString failed 3");
        }
        
        char ch3 = 'x';
        str = ch3.ToString();
        if (str != "x")
        {
            PrintFailed("'char': ToString failed 5");
        }
        
        char ch2 = '4';
        ch = '2';
        str = ch2 + ch;
        if (str != "42")
        {
            PrintFailed("'char': ToString failed 4");
        }
        
        str = 'a' + 'b' + 'c' + 'd' + 'e';
        if (str != "abcde")
        {
            PrintFailed("'char': ToString failed 9");
        }
        
        str = "abcd";
        str = str + 'e';
        if (str != "abcde")
        {
            PrintFailed("'char': ToString failed 6");
        }
        
        str = "bcde";
        str = 'a' + str;
        if (str != "abcde")
        {
            PrintFailed("'char': ToString failed 7");
        }
        
        str = "bang";
        str = '"' + str + '"';
        if (str.Length != 6)
        {
            PrintFailed("'char': ToString failed 8");
        }
    }
    
    TestStringSystem()
    {
        WriteLn("system 'string'"); // string methods written in Hopper (some specific to HOPPER_6502)
        
        if (String.Compare("bbb", "aaa") != 1)
        {
            int result = String.Compare("bbb", "aaa");
            WriteLn(result.ToString());
            PrintFailed("'string': Compare failed 7");
        }
        
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
        
        string build;
        Build (ref build, "");
        if (build != "")
        {
            PrintFailed("'string': Build failed 1");
        }
        Build (ref build, "abc");
        if (build != "abc")
        {
            PrintFailed("'string': Build failed 2");
        }
        Build (ref build, "def");
        if (build != "abcdef")
        {
            PrintFailed("'string': Build failed 3");
        }
        
        Build (ref build, "012345678901234567890123456789");
        if (build != "abcdef012345678901234567890123456789")
        {
            PrintFailed("'string': Build failed 4");
        }
        Build (ref build);
        if (build != "")
        {
            PrintFailed("'string': Build failed 5");
        }
        
        Build (ref build);
        if (build != "")
        {
            PrintFailed("'string': Build failed 6");
        }
        Build (ref build, 'a');
        if (build != "a")
        {
            PrintFailed("'string': Build failed 7");
        }
        Build (ref build, 'b');
        if (build != "ab")
        {
            PrintFailed("'string': Build failed 8");
        }
        build = "012345678901234567890123456789";
        Build (ref build, 'a');
        if (build != "012345678901234567890123456789a")
        {
            PrintFailed("'string': Build failed 9");
        }
        
        Build (ref build);
        if (build != "")
        {
            PrintFailed("'string': Build failed 10");
        }
        BuildFront (ref build, 'a');
        if (build != "a")
        {
            PrintFailed("'string': Build failed 11");
        }
        BuildFront (ref build, 'b');
        if (build != "ba")
        {
            PrintFailed("'string': Build failed 12");
        }
        build = "012345678901234567890123456789";
        BuildFront (ref build, 'a');
        if (build != "a012345678901234567890123456789")
        {
            PrintFailed("'string': Build failed 13");
        }
    }
    
    TestStringCompare()
    {
        WriteLn("String compare");
        
        string aaaaa  = "aaaaa";
        string zzzzz  = "zzzzz";
        string aaaaa2 = "aaaaa";
        string empty  = "";
        
        if (aaaaa > aaaaa2)
        {
            PrintFailed("string > failed 1");
        }
        if (aaaaa >= zzzzz)
        {
            PrintFailed("string >= failed 1");
        }
        if (aaaaa > zzzzz)
        {
            PrintFailed("string > failed 1");
        }
        if (!(aaaaa <= zzzzz))
        {
            PrintFailed("string <= failed 1");
        }
        if (!(aaaaa < zzzzz))
        {
            PrintFailed("string < failed 1");
        }
        if (aaaaa == zzzzz)
        {
            PrintFailed("string == failed 1");
        }
        if (!(aaaaa == aaaaa2))
        {
            PrintFailed("string == failed 2");
        }
        if (!(aaaaa != zzzzz))
        {
            PrintFailed("string != failed 1");
        }
        if (aaaaa != aaaaa2)
        {
            PrintFailed("string != failed 2");
        }
        if (aaaaa < aaaaa2)
        {
            PrintFailed("string < failed 2");
        }
        if (aaaaa == "")
        {
            PrintFailed("string == failed 3");
        }
        if (!(aaaaa != ""))
        {
            PrintFailed("string != failed 3");
        }
        if ("" == aaaaa)
        {
            PrintFailed("string == failed 4");
        }
        if (!("" != aaaaa))
        {
            PrintFailed("string != failed 4");
        }
        if (!(empty == ""))
        {
            PrintFailed("string == failed 5");
        }
        if (empty != "")
        {
            PrintFailed("string != failed 5");
        }
        if (!("" == empty))
        {
            PrintFailed("string == failed 6");
        }
        if ("" != empty)
        {
            PrintFailed("string != failed 6");
        }
        
        if (aaaaa < "")
        {
            PrintFailed("string < failed 3");
        }
        if (!("" < aaaaa))
        {
            PrintFailed("string < failed 4");
        }
        if (empty < "")
        {
            PrintFailed("string < failed 5");
        }
        
        if (!(aaaaa > ""))
        {
            PrintFailed("string > failed 3");
        }
        if ("" > aaaaa)
        {
            PrintFailed("string > failed 4");
        }
        if (empty > "")
        {
            PrintFailed("string > failed 5");
        }
        
        if (aaaaa <= "")
        {
            PrintFailed("string <= failed 3");
        }
        if (!("" <= aaaaa))
        {
            PrintFailed("string <= failed 4");
        }
        if (!(empty <= ""))
        {
            PrintFailed("string <= failed 5");
        }
        
        if (!(aaaaa >= ""))
        {
            PrintFailed("string >= failed 3");
        }
        if ("" >= aaaaa)
        {
            PrintFailed("string >= failed 4");
        }
        if (!(empty >= ""))
        {
            PrintFailed("string >= failed 5");
        }
    }
    
    TestString()    
    {
        WriteLn("'string'");
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
    
    TestStringCase()
    {
        WriteLn("string 'case'");
        string test = "Test String";
        string result = test;
        result = result.ToUpper();
        if (result != "TEST STRING")
        {
            PrintFailed("ToUpper 1 failed");
        }
        result = test;
        String.ToUpper(ref result);
        if (result != "TEST STRING")
        {
            PrintFailed("ToUpper 2 failed");
        }
        result = test;
        result = result.ToLower();
        if (result != "test string")
        {
            PrintFailed("ToLower 1 failed");
        }
        result = test;
        String.ToLower(ref result);
        if (result != "test string")
        {
            PrintFailed("ToLower 2 failed");
        }
    }
    
    const string constRegular  = "0123456789";
    const string constHex      = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
    const byte[] constHexBytes = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
    
    string globalRegular       = "0123456789";
    string globalHex           = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
    byte[10] globalHexBytes    = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
    
    TestHexStrings()
    {
        WriteLn("'hex' strings");
        
        string regular;
        string hex;
        foreach (var ch in constRegular)
        {
            regular += ch;
        }
        foreach (var  ch in constHex)
        {
            hex += ch;
        }
        if (hex != regular)
        {
            PrintFailed("'hex' strings failed 1");
        }
        if (hex != "0123456789")
        {
            PrintFailed("'hex' strings failed 2");
        }
        regular = "";
        hex = "";
        foreach (var ch in constRegular)
        {
            regular += ch;
        }
        foreach (var  b in constHexBytes)
        {
            hex += char(b);
        }
        if (hex != regular)
        {
            PrintFailed("'hex' bytes failed 1");
        }
        
        regular = "";
        hex = "";
        for (uint i = 0; i < 10; i++)
        {
            regular += constRegular[i];
        }
        for (uint i = 0; i < 10; i++)
        {
            hex += constHex[i];
        }
        if (hex != regular)
        {
            PrintFailed("'hex' strings failed 3");
        }
        if (hex != "0123456789")
        {
            PrintFailed("'hex' strings failed 4");
        }
        regular = "";
        hex = "";
        for (uint i = 0; i < 10; i++)
        {
            regular += constRegular[i];
        }
        for (uint i = 0; i < 10; i++)
        {
            hex += char(constHexBytes[i]);
        }
        if (hex != regular)
        {
            PrintFailed("'hex' bytes failed 2");
        }
        
        regular = "";
        hex = "";
        foreach (var ch in globalRegular)
        {
            regular += ch;
        }
        foreach (var  ch in globalHex)
        {
            hex += ch;
        }
        if (hex != regular)
        {
            PrintFailed("'hex' strings failed 5");
        }
        if (hex != "0123456789")
        {
            PrintFailed("'hex' strings failed 6");
        }
        regular = "";
        hex = "";
        foreach (var ch in globalRegular)
        {
            regular += ch;
        }
        foreach (var  b in globalHexBytes)
        {
            hex += char(b);
        }
        if (hex != regular)
        {
            PrintFailed("'hex' bytes failed 3");
        }
        
        regular = "";
        hex = "";
        for (uint i = 0; i < 10; i++)
        {
            regular += globalRegular[i];
        }
        for (uint i = 0; i < 10; i++)
        {
            hex += globalHex[i];
        }
        if (hex != regular)
        {
            PrintFailed("'hex' strings failed 7");
        }
        if (hex != "0123456789")
        {
            PrintFailed("'hex' strings failed 8");
        }
        regular = "";
        hex = "";
        for (uint i = 0; i < 10; i++)
        {
            regular += globalRegular[i];
        }
        for (uint i = 0; i < 10; i++)
        {
            hex += char(globalHexBytes[i]);
        }
        if (hex != regular)
        {
            PrintFailed("'hex' bytes failed 4");
        }
        
        
        string   localRegular  = "0123456789";
        string   localHex      = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
        byte[10] localHexBytes = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
    
        regular = "";
        hex = "";
        foreach (var ch in localRegular)
        {
            regular += ch;
        }
        foreach (var  ch in localHex)
        {
            hex += ch;
        }
        if (hex != regular)
        {
            PrintFailed("'hex' strings failed 9");
        }
        if (hex != "0123456789")
        {
            PrintFailed("'hex' strings failed 10");
        }
        regular = "";
        hex = "";
        foreach (var ch in localRegular)
        {
            regular += ch;
        }
        foreach (var  b in localHexBytes)
        {
            hex += char(b);
        }
        if (hex != regular)
        {
            PrintFailed("'hex' bytes failed 5");
        }
        
        
        regular = "";
        hex = "";
        for (uint i = 0; i < 10; i++)
        {
            regular += localRegular[i];
        }
        for (uint i = 0; i < 10; i++)
        {
            hex += localHex[i];
        }
        if (hex != regular)
        {
            PrintFailed("'hex' strings failed 11");
        }
        if (hex != "0123456789")
        {
            PrintFailed("'hex' strings failed 12");
        }
        regular = "";
        hex = "";
        for (uint i = 0; i < 10; i++)
        {
            regular += localRegular[i];
        }
        for (uint i = 0; i < 10; i++)
        {
            hex += char(localHexBytes[i]);
        }
        if (hex != regular)
        {
            PrintFailed("'hex' bytes failed 6");
        }
    }
    
    uint gindex = 0;
    TestRef()
    {
        WriteLn("'ref' arguments");
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
        WriteLn("'switch'");
        
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
        loop
        {
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
                    break;
                    count = count + inc;
                }
                default:
                {
                    int inc = 4;
                    count = count + inc;
                }
            }
        }
        if (count != 0)
        {
            PrintFailed("'switch' 4 failed");
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
        WriteLn("Short circuit");
        
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
    
    TestWhile()
    {
        WriteLn("'while'");
        
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
    
    TestForEach()
    {
        WriteLn("'foreach'");    
        
        uint count = 0;
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

    } //TestForEach()
    
    
    TestEquals()
    { 
        WriteLn("'=='");
        // contants
        if (1 == 0)
        {
            PrintFailed("== 1");
        }
        if (!(1 == 1))
        {
            PrintFailed("== 2");
        }
        
        if ('a' == 'b')
        {
            PrintFailed("== 3");
        }
        if (!('a' == 'a'))
        {
            PrintFailed("== 4");
        }
        
        byte ba = 1;
        byte bb = 0;
        byte bc = 1;
        if (!(ba == 1))
        {
            PrintFailed("== 7");
        }
        if (ba == 0)
        {
            PrintFailed("== 8");
        }
        if (!(1 == ba))
        {
            PrintFailed("== 9");
        }
        if (0 == ba)
        {
            PrintFailed("== 10");
        }
        if (!(ba == ba))
        {
            PrintFailed("== 11");
        }
        if (ba == bb)
        {
            PrintFailed("== 12");
        }
        if (!(ba == bc))
        {
            PrintFailed("== 13");
        }

        int ia = 1;
        int ib = 0;
        int ic = 1;
        if (!(ia == 1))
        {
            PrintFailed("== 14");
        }
        if (ia == 0)
        {
            PrintFailed("== 15");
        }
        if (!(1 == ia))
        {
            PrintFailed("== 16");
        }
        if (0 == ia)
        {
            PrintFailed("== 17");
        }
        if (!(ia == ia))
        {
            PrintFailed("== 18");
        }
        if (ia == ib)
        {
            PrintFailed("== 19");
        }
        if (!(ia == ic))
        {
            PrintFailed("== 20");
        }
        if (!(ia == ba))
        {
            PrintFailed("== 21");
        }
        if (ia == bb)
        {
            PrintFailed("== 22");
        }
        if (!(ba == ia))
        {
            PrintFailed("== 23");
        }
        if (ba == ib)
        {
            PrintFailed("== 24");
        }
      
        
        
    }
    
    const uint    globalUInt   = 10000;
    const uint    globalUInt2  = 20000;
    
    const int    globalInt  = 10000;
    const int    globalInt2  = 10001;

    
    TestConstants()
    {
        WriteLn("'const'");
        int    localInt   = 10000;
        if (globalInt != localInt)
        {
            PrintFailed("const 1");
        }
        localInt = localInt + 1;
        if (globalInt2 != localInt)
        {
            PrintFailed("const 2");
        }
    }
    
    TestLessThan()
    {
        WriteLn("'<'");
        int    localInt   = 10001;
        
        uint localUInt0 =   12;
        uint localUInt1 =   23;
        uint localUInt2 = 1234;
        uint localUInt3 = 4567;
        uint localUInt4 = 256;
        uint localUInt5 = 257;
        if (!(localUInt0 < localUInt1))
        {
            PrintFailed("< 27");    
        }
        if (!(localUInt1 < localUInt2))
        {
            PrintFailed("< 28");    
        }
        if (!(localUInt2 < localUInt3))
        {
            PrintFailed("< 29");    
        }
        if (localUInt1 < localUInt0)
        {
            PrintFailed("< 30");    
        }
        if (localUInt2 < localUInt1)
        {
            PrintFailed("< 31");    
        }
        if (localUInt3 < localUInt2)
        {
            PrintFailed("< 32");    
        }
        if (localUInt0 < localUInt0)
        {
            PrintFailed("< 33");    
        }
        if (localUInt1 < localUInt1)
        {
            PrintFailed("< 34");    
        }
        if (localUInt2 < localUInt2)
        {
            PrintFailed("< 35");    
        }
        if (localUInt3 < localUInt3)
        {
            PrintFailed("< 36");    
        }
        if (!(localUInt4 < localUInt5))
        {
            PrintFailed("< 37");    
        }
        if (localUInt5 < localUInt4)
        {
            PrintFailed("< 38");    
        }
        if (localUInt4 < localUInt4)
        {
            PrintFailed("< 39");    
        }
        if (!(globalInt < localInt))
        {
            PrintFailed("< 40");
        }
        if (localInt < globalInt)
        {
            PrintFailed("< 41");
        }
    }
    TestLessThanOrEqual()
    {
        WriteLn("'<='");
        int    localInt   = 10001;
        int localNegInt1  = -10000;
        int localNegInt2  = -10001;
        if (!(globalInt <= localInt))
        {
            PrintFailed("<= 1");
        }
        if (!(globalInt <= globalInt))
        {
            PrintFailed("<= 2");
        }
        if (!(localNegInt2 <= localNegInt1))
        {
            PrintFailed("<= 3");
        }
        if (!(localNegInt1 <= globalInt))
        {
            PrintFailed("<= 4");
        }
        if (!(localNegInt1 <= localInt))
        {
            PrintFailed("<= 5");
        }
        
        if (!(localInt <= localInt))
        {
            PrintFailed("<= 6");
        }
        if (!(localInt <= 20001))
        {
            PrintFailed("<= 7");
        }
        if (localInt <= 5001)
        {
            PrintFailed("<= 8");
        }
        if (!(localInt <= 10001))
        {
            PrintFailed("<= 9");
        }
        
        if (localInt <= globalInt)
        {
            PrintFailed("<= 10");
        }
        
    }
    
    TestGreaterThan()
    {
        int    localInt   = 10001;
        WriteLn("'>'");

        if ((globalInt > localInt))
        {
            PrintFailed("> 1");
        }
        if ((localInt > localInt))
        {
            PrintFailed("> 2");
        }
        if (!(localInt > globalInt))
        {
            PrintFailed("> 3");
        }
    }
    
    TestGreaterThanOrEqual()
    {
        WriteLn("'>='");
        int    localInt   = 10001;
        if (globalInt >= localInt)
        {
            PrintFailed(">= 55");
        }
        if (!(globalInt >= globalInt))
        {
            PrintFailed(">= 56");
        }
        if (!(localInt >= localInt))
        {
            PrintFailed(">= 57");
        }
        if (!(localInt >= globalInt))
        {
            PrintFailed(">= 58");
        }
    }
    flags PFlags
    {
        None = 0,
        One = 0x01,
        Two = 0x02,
        Four = 0x04
    }
    
    uint gProp;
    uint Prop { get { return gProp; } set { gProp = value; } }
    
    PFlags fProp;
    PFlags FProp { get { return fProp; } set { fProp = value; } }
    
    TestPropertyMath()
    {
        WriteLn("Properties");
        Prop = 42;
        Prop += 1;
        if (Prop != 43)
        {
            PrintFailed("Props 59");        
        }
        Prop -= 1;
        if (Prop != 42)
        {
            PrintFailed("Props 60");        
        }
        Prop = 3;
        Prop *= 6;
        if (Prop != 18)
        {
            PrintFailed("Props 61");        
        }
        Prop /= 6;
        if (Prop != 3)
        {
            PrintFailed("Props 62");        
        }
        Prop++;
        if (Prop != 4)
        {
            PrintFailed("Props 63");
        }
        Prop--;
        if (Prop != 3)
        {
            PrintFailed("Props 64");
        }
        Prop = Prop + Prop;
        if (Prop != 6)
        {
            PrintFailed("Props 64");
        }
        
        
        fProp |= PFlags.Four;
        if (fProp != PFlags.Four)
        {
            PrintFailed("Props 65");
        }
        fProp &= PFlags.Two;
        if (fProp != PFlags.None)
        {
            PrintFailed("Props 66");
        }
        
        FProp |= PFlags.Four;
        if (FProp != PFlags.Four)
        {
            PrintFailed("Props 67");
        }
        FProp &= PFlags.Two;
        if (FProp != PFlags.None)
        {
            PrintFailed("Props 68");
        }
    }

    TestUIntMath()
    {
        WriteLn("'uint'");
    
        // globalUInt   = 10000;
        // globalUInt2  = 20000;
        uint localUInt  = 10000;
        uint localUInt2 = 20000;
        
        // +
        if (localUInt + localUInt != 20000)
        {
            PrintFailed("UInt 69");        
        }
        if (localUInt2 + localUInt2 != 40000)
        {
            PrintFailed("UInt 70");        
        }
        if (globalUInt + localUInt != 20000)
        {
            PrintFailed("UInt 71");        
        }
        if (globalUInt2 + localUInt2 != 40000)
        {
            PrintFailed("UInt 72");        
        }
        if (localUInt2 + localUInt2 + globalUInt2 != 60000)
        {
            PrintFailed("UInt 73");
        }
        
        // -
        if (localUInt2 - localUInt != 10000)
        {
            PrintFailed("UInt 74");
        }
        if (localUInt2 - localUInt - localUInt != 0)
        {
            PrintFailed("UInt 75");
        }
        
        // *
        if (localUInt2 != 2 * localUInt)
        {
            PrintFailed("UInt 76");
        }
        if (localUInt * 5 != 50000)
        {
            PrintFailed("UInt 77");
        }
        
        // /
        if (localUInt2 / 2 != localUInt)
        {
            PrintFailed("UInt 78");
        }
        if (localUInt / 250 != 40)
        {
            PrintFailed("UInt 79");
        }
        
        // %
        if (localUInt % 3 != 1)
        {
            PrintFailed("UInt 80");
        }
        
        
        uint test = 0xAA55;
        
        uint testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed("UInt 81");
        }
        
        testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed("UInt 82");
        }
    } // TestUIntMath
    
    TestIntMath()
    {
        WriteLn("'int'");
        
        // globalInt   = 10000;
        // globalInt2  = 10001;
        int localInt  = 10000;
        int localInt2 = 10001;
        int localIntNeg = 0 - localInt;
        
        if (localInt.GetByte(0) != 0x10)
        {
            PrintFailed("Int 83");        
        }
        if (localInt.GetByte(1) != 0x27)
        {
            PrintFailed("Int 84");        
        }
        // +
        if (localInt + localInt != 20000)
        {
            PrintFailed("Int 85");        
        }
        if (localInt2 + localInt2 != 20002)
        {
            PrintFailed("Int 86");        
        }
        if (globalInt + localInt != 20000)
        {
            PrintFailed("Int 87");        
        }
        if (globalInt2 + localInt2 != 20002)
        {
            PrintFailed("Int 88");        
        }
        if (localInt2 + localInt2 + globalInt2 != 30003)
        {
            PrintFailed("Int 89");
        }
        
        // -
        if (localInt2 - localInt != 1)
        {
            PrintFailed("Int 90");
        }
        if (localInt - localInt2 - localInt2 != -10002)
        {
            PrintFailed("Int 91");
        }
        if (-localInt2 != -10001)
        {
            PrintFailed("Int 92");
        }
        
        // *
        if (20000 != 2 * localInt)
        {
            PrintFailed("Int 93");
        }
        if (localInt * 3 != 30000)
        {
            PrintFailed("Int 94");
        }
        if (-20000 != -2 * localInt)
        {
            PrintFailed("Int 95");
        }
        
        if (20000 != -2 * localIntNeg)
        {
            PrintFailed("Int 96");
        }
        
        // /
        if (localInt / 2 != 5000)
        {
            PrintFailed("Int 97");
        }
        if (localInt / 250 != 40)
        {
            PrintFailed("Int 98");
        }
        if (localInt / -2 != -5000)
        {
            PrintFailed("Int 99");
        }
        if (localIntNeg / -2 != 5000)
        {
            PrintFailed("Int 100");
        }
        if (2000 / 100 != 20)
        {
            PrintFailed("Int 101");
        }
        if (2000 / 50 != 40)
        {
            PrintFailed("Int 102");
        }
        if (2000 / 10 != 200)
        {
            PrintFailed("Int 103");
        }
        if (-2000 / 100 != -20)
        {
            PrintFailed("Int 104");
        }
        if (-2000 / 50 != -40)
        {
            PrintFailed("Int 105");
        }
        if (int(2000) / -100 != -20)
        {
            PrintFailed("Int 106");
        }
        if (int(2000) / -50 != -40)
        {
            PrintFailed("Int 107");
        }
        if (int(2000) / -10 != -200)
        {
            PrintFailed("Int 108");
        }
        if (-2000 / -100 != 20)
        {
            PrintFailed("Int 109");
        }
        if (-2000 / -50 != 40)
        {
            PrintFailed("Int 110");
        }
        if (-2000 / -10 != 200)
        {
            PrintFailed("Int 111");
        }
        if (2000 / 1 != 2000)
        {
            PrintFailed("Int 112");
        }
        if (2000 / 2 != 1000)
        {
            PrintFailed("Int 113");
        }
        if (2000 / 4 != 500)
        {
            PrintFailed("Int 114");
        }
        if (2000 / 8 != 250)
        {
            PrintFailed("Int 115");
        }
        if (int(2000) / -1 != -2000)
        {
            PrintFailed("Int 116");
        }
        if (int(2000) / -2 != -1000)
        {
            PrintFailed("Int 117");
        }
        if (int(2000) / -4 != -500)
        {
            PrintFailed("Int 118");
        }
        if (int(2000) / -8 != -250)
        {
            PrintFailed("Int 119");
        }
        
        
        if (1*0 != 0)
        {
            PrintFailed("Int 120");
        }
        if (0*1 != 0)
        {
            PrintFailed("Int 121");
        }
        if (0*0 != 0)
        {
            PrintFailed("Int 122");
        }
        if (1*2 != 2)
        {
            PrintFailed("Int 123");
        }
        if (2*1 != 2)
        {
            PrintFailed("Int 124");
        }
        if (7*11 != 77)
        {
            PrintFailed("Int 125");
        }
        if (11*7 != 77)
        {
            PrintFailed("Int 126");
        }
        if (303*2 != 606)
        {
            PrintFailed("Int 127");
        }
        if (2*303 != 606)
        {
            PrintFailed("Int 128");
        }
        if (303*3 != 909)
        {
            PrintFailed("Int 129");
        }
        if (3*303 != 909)
        {
            PrintFailed("Int 130");
        }
        if (303*4 != 1212)
        {
            PrintFailed("Int 131");
        }
        if (4*303 != 1212)
        {
            PrintFailed("Int 132");
        }
        if (303*8 != 2424)
        {
            PrintFailed("Int 133");
        }
        if (8*303 != 2424)
        {
            PrintFailed("Int 134");
        }
        
        int a = -49;
        int b = 229;
        if (a * b != -11221)
        {
            PrintFailed("Int 135");
        }
        if (b * a != -11221)
        {
            PrintFailed("Int 136");
        }
        
        // %
        if (localInt % 3 != 1)
        {
            PrintFailed("Int 137");
        }
        
        int test = 0x1234;
        
        int testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed("Int 138");
        }
        
        testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed("Int 139");
        }
        
    } // TestIntMath
    
    TestArray()
    {
        WriteLn("Array");
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
        count = 0;
        
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
        count = 0;
        
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
        count = 0;
        
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
    
    
    Hopper()
    {
        WriteLn();
        WriteLn("Minimal Runtime Validation Tests:");
        
        TestStringSystem();
        TestStringCompare();
        TestCharSystem();
        TestStringTrim();
        TestString();
        TestStringCase();
        
        TestHexStrings();
        TestArray();
        TestSwitch();
        TestRef();
        TestBooleanShortCircuit();
        TestWhile();
        TestForEach();
        
        TestEquals();
        TestLessThan();
        TestGreaterThan();
        TestGreaterThanOrEqual();
        TestLessThanOrEqual();
        
        TestConstants();
        TestUIntMath();
        TestIntMath();
        TestPropertyMath();
        
        WriteLn("  Passed");
    }
}
