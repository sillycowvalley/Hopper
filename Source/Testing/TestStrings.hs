program TestStrings
{
#define MCU

    uses "/Source/System/System"
    
    uses "/Source/System/String"
    
    uses "/Source/System/IO"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"

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
            PrintFailed("'string': TrimLeft failed 3");
        }
        trimmed = " ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimLeft failed 4");
        }
        trimmed = " a ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "a ")
        {
            PrintFailed("'string': TrimLeft failed 5");
        }
        trimmed = "a ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "a ")
        {
            PrintFailed("'string': TrimLeft failed 6");
        }
        
        trimmed = str;
        trimmed = trimmed.TrimLeft();
        if (trimmed != "untrimmed string   ")
        {
            PrintFailed("'string': TrimLeft failed 2");
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
            PrintFailed("'string': TrimRight failed 1");
        }
        trimmed = "";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimRight failed 2");
        }
        trimmed = "  ";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimRight failed 3");
        }
        trimmed = " ";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': TrimRight failed 4");
        }
        trimmed = " a";
        String.TrimRight(ref trimmed);
        if (trimmed != " a")
        {
            PrintFailed("'string': TrimRight failed 5");
        }
        
        trimmed = str;
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "untrimmed string")
        {
            PrintFailed("'string': Trim failed 3");
        }
        trimmed = "";
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': Trim failed 4");
        }
        trimmed = "    ";
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed("'string': Trim failed 5");
        }
        
        trimmed = str;
        trimmed = trimmed.Trim();
        if (trimmed != "untrimmed string")
        {
            PrintFailed("'string': Trim failed 1");
        }
        trimmed = str;
        String.Trim(ref trimmed);
        if (trimmed != "untrimmed string")
        {
            PrintFailed("'string': Trim failed 2");
        }
        
        str = "abcde";
        trimmed = str.Substring(2);
        if (trimmed != "cde")
        {
            PrintFailed("'string': Substring failed 1");
        }
        str = "abcde";
        trimmed = str.Substring(2, 2);
        if (trimmed != "cd")
        {
            PrintFailed("'string': Substring failed 2");
        }
        str = "abcde";
        Substring(ref str, 1);
        if (str != "bcde")
        {
            PrintFailed("'string': Substring failed 3");
        }
        str = "abcde";
        trimmed = str.Substring(3);
        if (trimmed != "de")
        {
            PrintFailed("'string': Substring failed 4");
        }
        str = "abcde";
        trimmed = str.Substring(10);
        if (trimmed != "")
        {
            PrintFailed("'string': Substring failed 5");
        }
        str = "abcde";
        Substring(ref str, 10);
        if (str != "")
        {
            PrintFailed("'string': Substring failed 6");
        }
        str = "abcde";
        trimmed = str.Substring(3,10);
        if (trimmed != "de")
        {
            PrintFailed("'string': Substring failed 7");
        }
        str = "abcde";
        trimmed = str.Substring(10,10);
        if (trimmed != "")
        {
            PrintFailed("'string': Substring failed 8");
        }
        str = "abcde";
        Substring(ref str, 0);
        if (str != "abcde")
        {
            PrintFailed("'string': Substring failed 9");
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
    
    {
        
        TestStringSystem();
        TestStringCompare();
        TestCharSystem();
        TestStringTrim();
        TestString();
        TestStringCase();
        TestHexStrings();
        TestRef();
        
        // bool IndexOf(string this, string pattern, ref uint index)
        // bool IndexOf(string this, char pattern, uint startIndex, ref uint index)
        // bool IndexOf(string this, string pattern, uint startIndex, ref uint index)
        // bool LastIndexOf(string this, char pattern, ref uint index)
        // bool LastIndexOf(string this, char pattern, uint startIndex, ref uint index)
        // string Pad(string this, char append, uint width)
        // string LeftPad(string this, char append, uint width)
        // bool Contains(string this, char needle)
        // bool Contains(string this, string needle)
        // <string> Split(string this, string delimiters)
        // <string> Split(string this, char delimiter)                      
        WriteLn();
        WriteLn("TestStrings Ok");

#ifndef MCU
        Key key = ReadKey();
#endif
    }
}

