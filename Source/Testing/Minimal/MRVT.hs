program MRVT
{
    
    #define CPU_Z80
    
    uses "/Source/Minimal/IO"
    
    PrintFailed(string message, uint instance)
    {
        WriteLn("  Failed: " + message + " " + instance.ToString());
        Diagnostics.Die(0x0B); // system failure / internal error
    }
    
    TestStringTrim()
    {
        string prompt = "'string' Trim";
        WriteLn(prompt);
        
        string str = "   untrimmed string   ";
        string trimmed = str;
        String.TrimLeft(ref trimmed);
        if (trimmed != "untrimmed string   ")
        {
            PrintFailed(prompt, 1);
        }
        trimmed = "";
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 2);
        }
        trimmed = " ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 3);
        }
        trimmed = " a ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "a ")
        {
            PrintFailed(prompt, 4);
        }
        trimmed = "a ";
        String.TrimLeft(ref trimmed);
        if (trimmed != "a ")
        {
            PrintFailed(prompt, 5);
        }
        
        trimmed = str;
        trimmed = trimmed.TrimLeft();
        if (trimmed != "untrimmed string   ")
        {
            PrintFailed(prompt, 6);
        }
        
        trimmed = "";
        trimmed = trimmed.TrimLeft();
        if (trimmed != "")
        {
            PrintFailed(prompt, 7);
        }
                
        trimmed = str;
        String.TrimRight(ref trimmed);
        if (trimmed != "   untrimmed string")
        {
            PrintFailed(prompt, 8);
        }
        trimmed = "";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 9);
        }
        trimmed = "  ";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 10);
        }
        trimmed = " ";
        String.TrimRight(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 11);
        }
        trimmed = " a";
        String.TrimRight(ref trimmed);
        if (trimmed != " a")
        {
            PrintFailed(prompt, 12);
        }
        
        trimmed = str;
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "untrimmed string")
        {
            PrintFailed(prompt, 13);
        }
        trimmed = "";
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 14);
        }
        trimmed = "    ";
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        String.TrimRight(ref trimmed);
        String.TrimLeft(ref trimmed);
        if (trimmed != "")
        {
            PrintFailed(prompt, 15);
        }
        
        trimmed = str;
        trimmed = trimmed.Trim();
        if (trimmed != "untrimmed string")
        {
            PrintFailed(prompt, 16);
        }
        trimmed = str;
        String.Trim(ref trimmed);
        if (trimmed != "untrimmed string")
        {
            PrintFailed(prompt, 17);
        }
        
        str = "abcde";
        trimmed = str.Substring(2);
        if (trimmed != "cde")
        {
            PrintFailed(prompt, 18);
        }
        str = "abcde";
        trimmed = str.Substring(2, 2);
        if (trimmed != "cd")
        {
            PrintFailed(prompt, 19);
        }
        str = "abcde";
        Substring(ref str, 1);
        if (str != "bcde")
        {
            PrintFailed(prompt, 20);
        }
        str = "abcde";
        trimmed = str.Substring(3);
        if (trimmed != "de")
        {
            PrintFailed(prompt, 21);
        }
        str = "abcde";
        trimmed = str.Substring(10);
        if (trimmed != "")
        {
            PrintFailed(prompt, 22);
        }
        str = "abcde";
        Substring(ref str, 10);
        if (str != "")
        {
            PrintFailed(prompt, 23);
        }
        str = "abcde";
        trimmed = str.Substring(3,10);
        if (trimmed != "de")
        {
            PrintFailed(prompt, 24);
        }
        str = "abcde";
        trimmed = str.Substring(10,10);
        if (trimmed != "")
        {
            PrintFailed(prompt, 25);
        }
        str = "abcde";
        Substring(ref str, 0);
        if (str != "abcde")
        {
            PrintFailed(prompt, 26);
        }
        
    }
    
    TestCharSystem()
    {
        string prompt = "system 'char'";
        WriteLn(prompt);
        
        
        char ch = 'a';
        string str = ch.ToString();
        if (str != "a")
        {
            PrintFailed(prompt, 1);
        }
        
        ch = 'a';
        str = "bcde";
        str = ch + str;
        if (str != "abcde")
        {
            PrintFailed(prompt, 2);
        }
        
        ch = 'e';
        str = "abcd";
        str = str + ch;
        if (str != "abcde")
        {
            PrintFailed(prompt, 3);
        }
        
        char ch3 = 'x';
        str = ch3.ToString();
        if (str != "x")
        {
            PrintFailed(prompt, 5);
        }
        
        char ch2 = '4';
        ch = '2';
        str = ch2 + ch;
        if (str != "42")
        {
            PrintFailed(prompt, 4);
        }
        
        str = 'a' + 'b' + 'c' + 'd' + 'e';
        if (str != "abcde")
        {
            PrintFailed(prompt, 9);
        }
        
        str = "abcd";
        str = str + 'e';
        if (str != "abcde")
        {
            PrintFailed(prompt, 6);
        }
        
        str = "bcde";
        str = 'a' + str;
        if (str != "abcde")
        {
            PrintFailed(prompt, 7);
        }
        
        str = "bang";
        str = '"' + str + '"';
        if (str.Length != 6)
        {
            PrintFailed(prompt, 8);
        }
    }
    
    TestStringSystem()
    {
        string prompt = "system 'string'";
        WriteLn(prompt); // string methods written in Hopper (some specific to HOPPER_6502)
        string build;
        
        int result;
        if (String.Compare("bbb", "aaa") != 1)
        {
            result = String.Compare("bbb", "aaa");
            WriteLn(result.ToString());
            PrintFailed(prompt, 7);
        }
        
        // string Replace(string original, string pattern, string replace)
        string original = "a=10";
        string replaced = original.Replace("=", " = ");
        if (replaced != "a = 10")
        {
            PrintFailed(prompt, 1);
        }
        original = "a = 10";
        replaced = original.Replace("=", " = ");
        if (replaced != "a  =  10")
        {
            PrintFailed(prompt, 2);
        }
        original = "a <> 10";
        replaced = original.Replace("<>", "#");
        if (replaced != "a # 10")
        {
            PrintFailed(prompt, 3);
        }
        replaced = original.Replace("a", "a");
        if (replaced != "a <> 10")
        {
            PrintFailed(prompt, 4);
        }
        replaced = original.Replace("", "a");
        if (replaced != "a <> 10")
        {
            PrintFailed(prompt, 5);
        }
        replaced = original.Replace("a", "");
        if (replaced != " <> 10")
        {
            PrintFailed(prompt, 6);
        }
        replaced = original.Replace("", "");
        if (replaced != "a <> 10")
        {
            PrintFailed(prompt, 7);
        }
        
        // string Replace(string original, char pattern, char replace)
        replaced = original.Replace('a', 'b');
        if (replaced != "b <> 10")
        {
            PrintFailed(prompt, 8);
        }
        
        replaced = original.Replace('a', 'a');
        if (replaced != "a <> 10")
        {
            PrintFailed(prompt, 9);
        }
        
        // bool EndsWith(string original, string pattern)
        original = "The End";
        if (!original.EndsWith("End"))
        {
            PrintFailed(prompt, 1);
        }
        if (original.EndsWith("Bob"))
        {
            PrintFailed(prompt, 2);
        }
        // bool EndsWith(string original, char pattern)
        if (!original.EndsWith('d'))
        {
            PrintFailed(prompt, 3);
        }
        if (original.EndsWith('b'))
        {
            PrintFailed(prompt, 4);
        }
        // bool StartsWith(string this, char pattern)
        if (!original.StartsWith('T'))
        {
            PrintFailed(prompt, 1);
        }
        if (original.StartsWith('E'))
        {
            PrintFailed(prompt, 2);
        }
        
        
        // bool StartsWith(string this, string pattern)
        if (!original.StartsWith("The"))
        {
            PrintFailed(prompt, 3);
        }
        if (original.StartsWith("End"))
        {
            PrintFailed(prompt, 4);
        }
        
        
        // int Compare(string left, string right) // returns -1, 0, +1
        if (String.Compare("aaa", "bbb") != -1)
        {
            PrintFailed(prompt, 1);
        }
        if (String.Compare("aaa", "aaa") != 0)
        {
            PrintFailed(prompt, 2);
        }
        if (String.Compare("aaa", "aaaa") == 0)
        {
            PrintFailed(prompt, 3);
        }
        if (String.Compare("aaaa", "aaa") == 0)
        {
            PrintFailed(prompt, 4);
        }
        if (String.Compare("aaaa", "") == 0)
        {
            PrintFailed(prompt, 5);
        }
        if (String.Compare("", "aaaa") == 0)
        {
            PrintFailed(prompt, 6);
        }
        
        string insertString = 'a' + "bcde";
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed(prompt, 1);
        }
        insertString = "abcd" + 'e';
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed(prompt, 2);
        }
        insertString = "bcde";
        insertString = insertString.InsertChar(0, 'a');
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed(prompt, 3);
        }
        insertString = "abcd";
        insertString = insertString.InsertChar(4, 'e');
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed(prompt, 4);
        }
        insertString = "abde";
        insertString = insertString.InsertChar(2, 'c');
        if (String.Compare(insertString, "abcde") != 0)
        {
            PrintFailed(prompt, 5);
        }
        uint index;
        if (!insertString.IndexOf('a', ref index))
        {
            PrintFailed(prompt, 1);
        }
        if (index != 0)
        {
            PrintFailed(prompt, 2);
        }
        if (!insertString.IndexOf('c', ref index))
        {
            PrintFailed(prompt, 3);
        }
        if (index != 2)
        {
            PrintFailed(prompt, 4);
        }
        if (!insertString.IndexOf('e', ref index))
        {
            PrintFailed(prompt, 5);
        }
        if (index != 4)
        {
            PrintFailed(prompt, 6);
        }
        if (insertString.IndexOf('z', ref index))
        {
            PrintFailed(prompt, 7);
        }
        if (insertString.IndexOf('a', 1, ref index))
        {
            PrintFailed(prompt, 8);
        }
        if (!insertString.IndexOf('c', 2, ref index))
        {
            PrintFailed(prompt, 9);
        }
        if (index != 2)
        {
            PrintFailed(prompt, 9);
        }
        if (!insertString.IndexOf('e', 2, ref index))
        {
            PrintFailed(prompt, 10);
        }
        if (index != 4)
        {
            PrintFailed(prompt, 11);
        }
        if (insertString.IndexOf('z', 10, ref index))
        {
            PrintFailed(prompt, 12);
        }
        
        String.Build (ref build, "");
        if (build != "")
        {
            PrintFailed(prompt, 1);
        }
        String.Build (ref build, "abc");
        if (build != "abc")
        {
            PrintFailed(prompt, 2);
        }
        String.Build (ref build, "def");
        if (build != "abcdef")
        {
            PrintFailed(prompt, 3);
        }
        
        String.Build (ref build, "012345678901234567890123456789");
        if (build != "abcdef012345678901234567890123456789")
        {
            PrintFailed(prompt, 4);
        }
        String.Build (ref build);
        if (build != "")
        {
            PrintFailed(prompt, 5);
        }
        
        String.Build (ref build);
        if (build != "")
        {
            PrintFailed(prompt, 6);
        }
        String.Build (ref build, 'a');
        if (build != "a")
        {
            PrintFailed(prompt, 7);
        }
        String.Build (ref build, 'b');
        if (build != "ab")
        {
            PrintFailed(prompt, 8);
        }
        build = "012345678901234567890123456789";
        String.Build (ref build, 'a');
        if (build != "012345678901234567890123456789a")
        {
            PrintFailed(prompt, 9);
        }
        
        String.Build (ref build);
        if (build != "")
        {
            PrintFailed(prompt, 10);
        }
        String.BuildFront (ref build, 'a');
        if (build != "a")
        {
            PrintFailed(prompt, 11);
        }
        String.BuildFront (ref build, 'b');
        if (build != "ba")
        {
            PrintFailed(prompt, 12);
        }
        
        build = "012345678901234567890123456789";
        String.BuildFront (ref build, 'a');
        if (build != "a012345678901234567890123456789")
        {
            PrintFailed(prompt, 13);
        }
    }
    
    TestStringCompare()
    {
        string prompt = "String compare";
        WriteLn(prompt);
        
        string aaaaa  = "aaaaa";
        string zzzzz  = "zzzzz";
        string aaaaa2 = "aaaaa";
        string empty  = "";
        
        if (aaaaa > aaaaa2)
        {
            PrintFailed(prompt, 1);
        }
        if (aaaaa >= zzzzz)
        {
            PrintFailed(prompt, 2);
        }
        if (aaaaa > zzzzz)
        {
            PrintFailed(prompt, 3);
        }
        if (!(aaaaa <= zzzzz))
        {
            PrintFailed(prompt, 4);
        }
        if (!(aaaaa < zzzzz))
        {
            PrintFailed(prompt, 5);
        }
        if (aaaaa == zzzzz)
        {
            PrintFailed(prompt, 6);
        }
        if (!(aaaaa == aaaaa2))
        {
            PrintFailed(prompt, 7);
        }
        if (!(aaaaa != zzzzz))
        {
            PrintFailed(prompt, 8);
        }
        if (aaaaa != aaaaa2)
        {
            PrintFailed(prompt, 9);
        }
        if (aaaaa < aaaaa2)
        {
            PrintFailed(prompt, 10);
        }
        if (aaaaa == "")
        {
            PrintFailed(prompt, 11);
        }
        if (!(aaaaa != ""))
        {
            PrintFailed(prompt, 12);
        }
        if ("" == aaaaa)
        {
            PrintFailed(prompt, 13);
        }
        if (!("" != aaaaa))
        {
            PrintFailed(prompt, 14);
        }
        if (!(empty == ""))
        {
            PrintFailed(prompt, 15);
        }
        if (empty != "")
        {
            PrintFailed(prompt, 16);
        }
        if (!("" == empty))
        {
            PrintFailed(prompt, 17);
        }
        if ("" != empty)
        {
            PrintFailed(prompt, 18);
        }
        
        if (aaaaa < "")
        {
            PrintFailed(prompt, 19);
        }
        if (!("" < aaaaa))
        {
            PrintFailed(prompt, 20);
        }
        if (empty < "")
        {
            PrintFailed(prompt, 21);
        }
        
        if (!(aaaaa > ""))
        {
            PrintFailed(prompt, 22);
        }
        if ("" > aaaaa)
        {
            PrintFailed(prompt, 23);
        }
        if (empty > "")
        {
            PrintFailed(prompt, 24);
        }
        
        if (aaaaa <= "")
        {
            PrintFailed(prompt, 25);
        }
        if (!("" <= aaaaa))
        {
            PrintFailed(prompt, 26);
        }
        if (!(empty <= ""))
        {
            PrintFailed(prompt, 27);
        }
        
        if (!(aaaaa >= ""))
        {
            PrintFailed(prompt, 28);
        }
        if ("" >= aaaaa)
        {
            PrintFailed(prompt, 29);
        }
        if (!(empty >= ""))
        {
            PrintFailed(prompt, 30);
        }
    }
    
    TestString()    
    {
        string prompt = "'string'";
        WriteLn(prompt);
        string abcde  = "abcde";
        
        if (abcde + 'f' != "abcdef")
        {
            PrintFailed(prompt, 1);
        }
        string sub = abcde.Substring(3);
        if (sub != "de")
        {
            PrintFailed(prompt, 2);
        }
        sub = abcde.Substring(2,2);
        if (sub != "cd")
        {
            PrintFailed(prompt, 3);
        }
        if (!abcde.EndsWith("de"))
        {
            PrintFailed(prompt, 4);
        }
        if (!abcde.EndsWith('e'))
        {
            PrintFailed(prompt, 5);
        }
        string replaced = abcde.Replace("bc", "xyz");
        if (replaced != "axyzde")
        {
            PrintFailed(prompt, 6);
        }
        replaced = abcde.Replace("de", "y");
        if (replaced != "abcy")
        {
            PrintFailed(prompt, 7);
        }
        replaced = abcde.Replace('c', 'z');
        if (replaced != "abzde")
        {
            PrintFailed(prompt, 8);
        }
        replaced = abcde.Replace("ab", "ab");
        if (replaced != "abcde")
        {
            PrintFailed(prompt, 9);
        }
        replaced = abcde.Replace('c', 'c');
        if (replaced != "abcde")
        {
            PrintFailed(prompt, 10);
        }
    }
    
    TestStringCase()
    {
        string prompt = "string 'case'";
        WriteLn(prompt);
        
        string test = "Test String";
        string result = test;
        result = result.ToUpper();
        if (result != "TEST STRING")
        {
            PrintFailed(prompt, 1);
        }
        result = test;
        String.ToUpper(ref result);
        if (result != "TEST STRING")
        {
            PrintFailed(prompt, 2);
        }
        result = test;
        result = result.ToLower();
        if (result != "test string")
        {
            PrintFailed(prompt, 3);
        }
        result = test;
        String.ToLower(ref result);
        if (result != "test string")
        {
            PrintFailed(prompt, 4);
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
        string prompt = "'hex' strings";
        WriteLn(prompt);
        
        string regular;
        string hex;
#ifndef CPU_Z80        
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
            PrintFailed(prompt, 1);
        }
        if (hex != "0123456789")
        {
            PrintFailed(prompt, 2);
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
            PrintFailed(prompt, 3);
        }
#endif        
        regular = "";
        hex = "";
        uint i;
        for (i = 0; i < 10; i++)
        {
            regular += constRegular[i];
        }
        for (i = 0; i < 10; i++)
        {
            hex += constHex[i];
        }
        if (hex != regular)
        {
            PrintFailed(prompt, 4);
        }
        if (hex != "0123456789")
        {
            PrintFailed(prompt, 5);
        }
        regular = "";
        hex = "";
        for (i = 0; i < 10; i++)
        {
            regular += constRegular[i];
        }
        for (i = 0; i < 10; i++)
        {
            hex += char(constHexBytes[i]);
        }
        if (hex != regular)
        {
            PrintFailed(prompt, 6);
        }

#ifndef CPU_Z80        
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
            PrintFailed(prompt, 7);
        }
        if (hex != "0123456789")
        {
            PrintFailed(prompt, 8);
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
            PrintFailed(prompt, 9);
        }
#endif
        regular = "";
        hex = "";
        for (i = 0; i < 10; i++)
        {
            regular += globalRegular[i];
        }
        for (i = 0; i < 10; i++)
        {
            hex += globalHex[i];
        }
        if (hex != regular)
        {
            PrintFailed(prompt, 10);
        }
        if (hex != "0123456789")
        {
            PrintFailed(prompt, 11);
        }
        regular = "";
        hex = "";
        for (i = 0; i < 10; i++)
        {
            regular += globalRegular[i];
        }
        for (i = 0; i < 10; i++)
        {
            hex += char(globalHexBytes[i]);
        }
        if (hex != regular)
        {
            PrintFailed(prompt, 12);
        }
        
        
        string   localRegular  = "0123456789";
        string   localHex      = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
        byte[10] localHexBytes = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
    
#ifndef CPU_Z80    
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
            PrintFailed(prompt, 13);
        }
        if (hex != "0123456789")
        {
            PrintFailed(prompt, 14);
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
            PrintFailed(prompt, 15);
        }
#endif        
        
        regular = "";
        hex = "";
        for (i = 0; i < 10; i++)
        {
            regular += localRegular[i];
        }
        for (i = 0; i < 10; i++)
        {
            hex += localHex[i];
        }
        if (hex != regular)
        {
            PrintFailed(prompt, 16);
        }
        if (hex != "0123456789")
        {
            PrintFailed(prompt, 17);
        }
        regular = "";
        hex = "";
        for (i = 0; i < 10; i++)
        {
            regular += localRegular[i];
        }
        for (i = 0; i < 10; i++)
        {
            hex += char(localHexBytes[i]);
        }
        if (hex != regular)
        {
            PrintFailed(prompt, 18);
        }
    }
    
    uint gindex = 0;
    TestRef()
    {
        string prompt = "'ref'";
        WriteLn(prompt);
        
        string test = "Test String";
        
        if (!test.IndexOf("String", 4, ref gindex))
        {
            PrintFailed(prompt, 1);
        }
        if (gindex != 5)
        {
            PrintFailed(prompt, 2);
        }
        
        uint hex = 0;
        string word = "0x002A";
        if (UInt.TryParse(word, ref hex))
        {
        }
        if (hex != 42)
        {
            PrintFailed(prompt, 3);
        }
        
        uint index = 0;
        if (!test.IndexOf("String", 4, ref index))
        {
            PrintFailed(prompt, 4);
        }
        if (index != 5)
        {
            PrintFailed(prompt, 5);
        }
    }
    
    TestSwitch()
    {
        string prompt = "'switch'";
        WriteLn(prompt);
        
        int count = 0;
        char check = 'A';
        int inc;
        switch (check)
        {
            case 'A':
            {
                inc = 1;
                count = count + inc;
            }
            case 'B':
            {
                inc = 2;
                count = count + inc;
            }
            default:
            {
                inc = 4;
                count = count + inc;
            }
        }
        if (count != 1)
        {
            PrintFailed(prompt, 1);
        }
        count = 0;
        check = 'B';
        switch (check)
        {
            case 'A':
            {
                inc = 1;
                count = count + inc;
            }
            case 'B':
            {
                inc = 2;
                count = count + inc;
            }
            default:
            {
                inc = 4;
                count = count + inc;
            }
        }
        if (count != 2)
        {
            PrintFailed(prompt, 2);
        }
        count = 0;
        check = 'C';
        switch (check)
        {
            case 'A':
            {
                inc = 1;
                count = count + inc;
            }
            case 'B':
            {
                inc = 2;
                count = count + inc;
            }
            default:
            {
                inc = 4;
                count = count + inc;
            }
        }
        if (count != 4)
        {
            PrintFailed(prompt, 3);
        }
        loop
        {
            count = 0;
            check = 'B';
            switch (check)
            {
                case 'A':
                {
                    inc = 1;
                    count = count + inc;
                }
                case 'B':
                {
                    inc = 2;
#ifndef CPU_Z80                    
                    break;
                    count = count + inc;
#endif                    
                }
                default:
                {
                    inc = 4;
                    count = count + inc;
                }
            }
            break;
        }
        if (count != 0)
        {
            PrintFailed(prompt, 4);
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
        string prompt = "'short circuit'";
        WriteLn(prompt);
        
        
        int trueCount = 0;
        int falseCount = 0;
        if (!(FalseCounter(ref falseCount) || TrueCounter(ref trueCount)))
        {
            PrintFailed(prompt, 1);
        }
        if (trueCount != 1)
        {
            PrintFailed(prompt, 2);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 3);
        }
                
        trueCount = 0;
        falseCount = 0;
        if (!(TrueCounter(ref trueCount) || FalseCounter(ref falseCount)))
        {
            PrintFailed(prompt, 4);
        }
        if (trueCount != 1)
        {
            PrintFailed(prompt, 5);
        }
        if (falseCount != 0)
        {
            PrintFailed(prompt, 6);
        }
        
        trueCount = 0;
        falseCount = 0;
        if (!(TrueCounter(ref trueCount) || TrueCounter(ref trueCount) || TrueCounter(ref trueCount)))
        {
            PrintFailed(prompt, 7);
        }
        if (trueCount != 1)
        {
            PrintFailed(prompt, 8);
        }
        
        falseCount = 0;
        if (FalseCounter(ref falseCount) || FalseCounter(ref falseCount))
        {
            PrintFailed(prompt, 9);
        }
        if (falseCount != 2)
        {
            PrintFailed(prompt, 10);
        }
        
        trueCount = 0;
        falseCount = 0;
        if (!(FalseCounter(ref falseCount) || FalseCounter(ref falseCount) || TrueCounter(ref trueCount)))
        {
            PrintFailed(prompt, 11);
        }
        if (trueCount != 1)
        {
            PrintFailed(prompt, 12);
        }
        if (falseCount != 2)
        {
            PrintFailed(prompt, 13);
        }
        
        
        trueCount = 0;
        falseCount = 0;
        if (FalseCounter(ref falseCount) && TrueCounter(ref trueCount))
        {
            PrintFailed(prompt, 14);
        }
        if (trueCount != 0)
        {
            PrintFailed(prompt, 15);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 16);
        }
                
        trueCount = 0;
        falseCount = 0;
        if (TrueCounter(ref trueCount) && FalseCounter(ref falseCount))
        {
            PrintFailed(prompt, 17);
        }
        if (trueCount != 1)
        {
            PrintFailed(prompt, 18);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 19);
        }
        
        trueCount = 0;
        falseCount = 0;
        if (!(TrueCounter(ref trueCount) && TrueCounter(ref trueCount) && TrueCounter(ref trueCount)))
        {
            PrintFailed(prompt, 20);
        }
        if (trueCount != 3)
        {
            PrintFailed(prompt, 21);
        }
        
        falseCount = 0;
        if (FalseCounter(ref falseCount) && FalseCounter(ref falseCount))
        {
            PrintFailed(prompt, 22);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 23);
        }
        
        trueCount = 0;
        falseCount = 0;
        if (FalseCounter(ref falseCount) && FalseCounter(ref falseCount) && TrueCounter(ref trueCount))
        {
            PrintFailed(prompt, 24);
        }
        if (trueCount != 0)
        {
            PrintFailed(prompt, 25);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 26);
        }
        
        trueCount = 0;
        falseCount = 0;
        if (TrueCounter(ref trueCount) && FalseCounter(ref falseCount) && FalseCounter(ref falseCount))
        {
            PrintFailed(prompt, 27);
        }
        if (trueCount != 1)
        {
            PrintFailed(prompt, 28);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 29);
        }
        
        trueCount = 0;
        falseCount = 0;
        if (TrueCounter(ref trueCount) && TrueCounter(ref trueCount) && FalseCounter(ref falseCount))
        {
            PrintFailed(prompt, 30);
        }
        if (trueCount != 2)
        {
            PrintFailed(prompt, 31);
        }
        if (falseCount != 1)
        {
            PrintFailed(prompt, 32);
        }
    }
    
    TestWhile()
    {
        string prompt = "'while'";
        WriteLn(prompt);
        
        int trueCount = 0;
        int falseCount = 0;
        int count = 0;
        while ((falseCount < 10) && !(FalseCounter(ref falseCount)))
        {
            count++;
        }
        
        if ((falseCount != 11) && (count != 10))
        {
            PrintFailed(prompt, 1);
        }
        count = 0;
        while ((trueCount < 10) && TrueCounter(ref trueCount))
        {
            count++;
        }
        if ((trueCount != 11) && (count != 10))
        {
            PrintFailed(prompt, 2);
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
            PrintFailed(prompt, 3);
        }
    } // TestWhile
    
    TestFor()
    {
        string prompt = "'for'";
        WriteLn(prompt);

        uint count = 0;
        uint i = 0;
        for (i = 0; i < 10; i++)
        {
            count++;
        }
        if (count != 10)
        {
            PrintFailed(prompt, 1);
        }
        
        count = 0;
        for (i = 0; i < 10; i++)
        {
            if (i == 2)
            {
                continue;
            }
            count++;
            if (i == 6)
            {
                break;
            }
        }
        if (count != 6)
        {
            PrintFailed(prompt, 2);
        }
        
        count = 0;
        i = 0;
        loop
        {
            if (i == 2)
            {
                i++;
                continue;
            }
            count++;
            if (i == 6)
            {
                break;
            }
            i++;
            if (i >= 10) { break; }
        }
        if (count != 6)
        {
            PrintFailed(prompt, 3);
        }
    }
    TestForEach()
    {
        string prompt = "'foreach'";
        WriteLn(prompt);
        
        uint count = 0;
        string ss = "abcde";
        foreach (var s in ss)
        {
            if (s == 'b')
            {
                continue;
            }
            count++;
#ifndef CPU_Z80            
            if (s == 'd')
            {
                break;
            }
#endif
        }
#ifndef CPU_Z80
        if (count != 3)
        {
            PrintFailed(prompt, 1);
        }
#else
        if (count != 4)
        {
            PrintFailed(prompt, 1);
        }
#endif
        
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
#ifndef CPU_Z80            
            if (s == 'd')
            {
                break;
            }
#endif
        }
#ifndef CPU_Z80
        if (count != 3)
        {
            PrintFailed(prompt, 1);
        }
#else
        if (count != 4)
        {
            PrintFailed(prompt, 1);
        }
#endif
    } //TestForEach()
    
    
    TestEquals()
    { 
        string prompt = "'=='";
        WriteLn(prompt);

        // contants
        if (1 == 0)
        {
            PrintFailed(prompt, 1);
        }
        if (!(1 == 1))
        {
            PrintFailed(prompt, 2);
        }
        
        if ('a' == 'b')
        {
            PrintFailed(prompt, 3);
        }
        if (!('a' == 'a'))
        {
            PrintFailed(prompt, 4);
        }
        
        byte ba = 1;
        byte bb = 0;
        byte bc = 1;
        if (!(ba == 1))
        {
            PrintFailed(prompt, 7);
        }
        if (ba == 0)
        {
            PrintFailed(prompt, 8);
        }
        if (!(1 == ba))
        {
            PrintFailed(prompt, 9);
        }
        if (0 == ba)
        {
            PrintFailed(prompt, 10);
        }
        if (!(ba == ba))
        {
            PrintFailed(prompt, 11);
        }
        if (ba == bb)
        {
            PrintFailed(prompt, 12);
        }
        if (!(ba == bc))
        {
            PrintFailed(prompt, 13);
        }

        int ia = 1;
        int ib = 0;
        int ic = 1;
        if (!(ia == 1))
        {
            PrintFailed(prompt, 14);
        }
        if (ia == 0)
        {
            PrintFailed(prompt, 15);
        }
        if (!(1 == ia))
        {
            PrintFailed(prompt, 16);
        }
        if (0 == ia)
        {
            PrintFailed(prompt, 17);
        }
        if (!(ia == ia))
        {
            PrintFailed(prompt, 18);
        }
        if (ia == ib)
        {
            PrintFailed(prompt, 19);
        }
        if (!(ia == ic))
        {
            PrintFailed(prompt, 20);
        }
        if (!(ia == ba))
        {
            PrintFailed(prompt, 21);
        }
        if (ia == bb)
        {
            PrintFailed(prompt, 22);
        }
        if (!(ba == ia))
        {
            PrintFailed(prompt, 23);
        }
        if (ba == ib)
        {
            PrintFailed(prompt, 24);
        }
      
        
        
    }
    
    const uint    globalUInt   = 10000;
    const uint    globalUInt2  = 20000;
    
    const int    globalInt  = 10000;
    const int    globalInt2  = 10001;

    
    TestConstants()
    {
        string prompt = "'c'";
        WriteLn(prompt);
    
        int    localInt   = 10000;
        if (globalInt != localInt)
        {
            PrintFailed(prompt, 1);
        }
        localInt = localInt + 1;
        if (globalInt2 != localInt)
        {
            PrintFailed(prompt, 2);
        }
    }
    
    TestLessThan()
    {
        string prompt = "'<'";
        WriteLn(prompt);

        int    localInt   = 10001;
        
        uint localUInt0 =   12;
        uint localUInt1 =   23;
        uint localUInt2 = 1234;
        uint localUInt3 = 4567;
        uint localUInt4 = 256;
        uint localUInt5 = 257;
        if (!(localUInt0 < localUInt1))
        {
            PrintFailed(prompt, 27);    
        }
        if (!(localUInt1 < localUInt2))
        {
            PrintFailed(prompt, 28);    
        }
        if (!(localUInt2 < localUInt3))
        {
            PrintFailed(prompt, 29);    
        }
        if (localUInt1 < localUInt0)
        {
            PrintFailed(prompt, 30);    
        }
        if (localUInt2 < localUInt1)
        {
            PrintFailed(prompt, 31);    
        }
        if (localUInt3 < localUInt2)
        {
            PrintFailed(prompt, 32);    
        }
        if (localUInt0 < localUInt0)
        {
            PrintFailed(prompt, 33);    
        }
        if (localUInt1 < localUInt1)
        {
            PrintFailed(prompt, 34);    
        }
        if (localUInt2 < localUInt2)
        {
            PrintFailed(prompt, 35);    
        }
        if (localUInt3 < localUInt3)
        {
            PrintFailed(prompt, 36);    
        }
        if (!(localUInt4 < localUInt5))
        {
            PrintFailed(prompt, 37);    
        }
        if (localUInt5 < localUInt4)
        {
            PrintFailed(prompt, 38);    
        }
        if (localUInt4 < localUInt4)
        {
            PrintFailed(prompt, 39);    
        }
        if (!(globalInt < localInt))
        {
            PrintFailed(prompt, 40);
        }
        if (localInt < globalInt)
        {
            PrintFailed(prompt, 41);
        }
    }
    TestLessThanOrEqual()
    {
        string prompt = "'<='";
        WriteLn(prompt);

        int    localInt   = 10001;
        int localNegInt1  = -10000;
        int localNegInt2  = -10001;
        if (!(globalInt <= localInt))
        {
            PrintFailed(prompt, 1);
        }
        if (!(globalInt <= globalInt))
        {
            PrintFailed(prompt, 2);
        }
        if (!(localNegInt2 <= localNegInt1))
        {
            PrintFailed(prompt, 3);
        }
        if (!(localNegInt1 <= globalInt))
        {
            PrintFailed(prompt, 4);
        }
        if (!(localNegInt1 <= localInt))
        {
            PrintFailed(prompt, 5);
        }
        
        if (!(localInt <= localInt))
        {
            PrintFailed(prompt, 6);
        }
        if (!(localInt <= 20001))
        {
            PrintFailed(prompt, 7);
        }
        if (localInt <= 5001)
        {
            PrintFailed(prompt, 8);
        }
        if (!(localInt <= 10001))
        {
            PrintFailed(prompt, 9);
        }
        
        if (localInt <= globalInt)
        {
            PrintFailed(prompt, 10);
        }
        
    }
    
    TestGreaterThan()
    {
        string prompt = "'>'";
        WriteLn(prompt);
    
        int    localInt   = 10001;
        
        if ((globalInt > localInt))
        {
            PrintFailed(prompt, 1);
        }
        if ((localInt > localInt))
        {
            PrintFailed(prompt, 2);
        }
        if (!(localInt > globalInt))
        {
            PrintFailed(prompt, 3);
        }
    }
    
    TestGreaterThanOrEqual()
    {
        string prompt = "'>='";
        WriteLn(prompt);
    
        int    localInt   = 10001;
        if (globalInt >= localInt)
        {
            PrintFailed(prompt, 55);
        }
        if (!(globalInt >= globalInt))
        {
            PrintFailed(prompt, 56);
        }
        if (!(localInt >= localInt))
        {
            PrintFailed(prompt, 57);
        }
        if (!(localInt >= globalInt))
        {
            PrintFailed(prompt, 58);
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
        string prompt = "'properties'";
        WriteLn(prompt);
        
        Prop = 42;
        Prop += 1;
        if (Prop != 43)
        {
            PrintFailed(prompt, 59);        
        }
        Prop -= 1;
        if (Prop != 42)
        {
            PrintFailed(prompt, 60);        
        }
        Prop = 3;
        Prop *= 6;
        if (Prop != 18)
        {
            PrintFailed(prompt, 61);        
        }
        Prop /= 6;
        if (Prop != 3)
        {
            PrintFailed(prompt, 62);        
        }
        Prop++;
        if (Prop != 4)
        {
            PrintFailed(prompt, 63);
        }
        Prop--;
        if (Prop != 3)
        {
            PrintFailed(prompt, 64);
        }
        Prop = Prop + Prop;
        if (Prop != 6)
        {
            PrintFailed(prompt, 64);
        }
        
        
        fProp |= PFlags.Four;
        if (fProp != PFlags.Four)
        {
            PrintFailed(prompt, 65);
        }
        fProp &= PFlags.Two;
        if (fProp != PFlags.None)
        {
            PrintFailed(prompt, 66);
        }
        
        FProp |= PFlags.Four;
        if (FProp != PFlags.Four)
        {
            PrintFailed(prompt, 67);
        }
        FProp &= PFlags.Two;
        if (FProp != PFlags.None)
        {
            PrintFailed(prompt, 68);
        }
    }

    TestUIntMath()
    {
        string prompt = "'uint'";
        WriteLn(prompt);
    
        // globalUInt   = 10000;
        // globalUInt2  = 20000;
        uint localUInt  = 10000;
        uint localUInt2 = 20000;
        
        // +
        if (localUInt + localUInt != 20000)
        {
            PrintFailed(prompt, 69);        
        }
        if (localUInt2 + localUInt2 != 40000)
        {
            PrintFailed(prompt, 70);        
        }
        if (globalUInt + localUInt != 20000)
        {
            PrintFailed(prompt, 71);        
        }
        if (globalUInt2 + localUInt2 != 40000)
        {
            PrintFailed(prompt, 72);        
        }
        if (localUInt2 + localUInt2 + globalUInt2 != 60000)
        {
            PrintFailed(prompt, 73);
        }
        
        // -
        if (localUInt2 - localUInt != 10000)
        {
            PrintFailed(prompt, 74);
        }
        if (localUInt2 - localUInt - localUInt != 0)
        {
            PrintFailed(prompt, 75);
        }
        
        // *
        if (localUInt2 != 2 * localUInt)
        {
            PrintFailed(prompt, 76);
        }
        if (localUInt * 5 != 50000)
        {
            PrintFailed(prompt, 77);
        }
        
        // /
        if (localUInt2 / 2 != localUInt)
        {
            PrintFailed(prompt, 78);
        }
        if (localUInt / 250 != 40)
        {
            PrintFailed(prompt, 79);
        }
        
        // %
        if (localUInt % 3 != 1)
        {
            PrintFailed(prompt, 80);
        }
        
        
        uint test = 0xAA55;
        
        uint testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(prompt, 81);
        }
        
        testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(prompt, 82);
        }
    } // TestUIntMath
    
    TestIntMath()
    {
        string prompt = "'int'";
        WriteLn(prompt);
        
        // globalInt   = 10000;
        // globalInt2  = 10001;
        int localInt  = 10000;
        int localInt2 = 10001;
        int localIntNeg = 0 - localInt;
        
        if (localInt.GetByte(0) != 0x10)
        {
            PrintFailed(prompt, 83);        
        }
        if (localInt.GetByte(1) != 0x27)
        {
            PrintFailed(prompt, 84);        
        }
        // +
        if (localInt + localInt != 20000)
        {
            PrintFailed(prompt, 85);        
        }
        if (localInt2 + localInt2 != 20002)
        {
            PrintFailed(prompt, 86);        
        }
        if (globalInt + localInt != 20000)
        {
            PrintFailed(prompt, 87);        
        }
        if (globalInt2 + localInt2 != 20002)
        {
            PrintFailed(prompt, 88);        
        }
        if (localInt2 + localInt2 + globalInt2 != 30003)
        {
            PrintFailed(prompt, 89);
        }
        
        // -
        if (localInt2 - localInt != 1)
        {
            PrintFailed(prompt, 90);
        }
        if (localInt - localInt2 - localInt2 != -10002)
        {
            PrintFailed(prompt, 91);
        }
        if (-localInt2 != -10001)
        {
            PrintFailed(prompt, 92);
        }
        
        // *
        if (20000 != 2 * localInt)
        {
            PrintFailed(prompt, 93);
        }
        if (localInt * 3 != 30000)
        {
            PrintFailed(prompt, 94);
        }
        if (-20000 != -2 * localInt)
        {
            PrintFailed(prompt, 95);
        }
        
        if (20000 != -2 * localIntNeg)
        {
            PrintFailed(prompt, 96);
        }
        
        // /
        if (localInt / 2 != 5000)
        {
            PrintFailed(prompt, 97);
        }
        if (localInt / 250 != 40)
        {
            PrintFailed(prompt, 98);
        }
        if (localInt / -2 != -5000)
        {
            PrintFailed(prompt, 99);
        }
        if (localIntNeg / -2 != 5000)
        {
            PrintFailed(prompt, 100);
        }
        if (2000 / 100 != 20)
        {
            PrintFailed(prompt, 101);
        }
        if (2000 / 50 != 40)
        {
            PrintFailed(prompt, 102);
        }
        if (2000 / 10 != 200)
        {
            PrintFailed(prompt, 103);
        }
        if (-2000 / 100 != -20)
        {
            PrintFailed(prompt, 104);
        }
        if (-2000 / 50 != -40)
        {
            PrintFailed(prompt, 105);
        }
        if (int(2000) / -100 != -20)
        {
            PrintFailed(prompt, 106);
        }
        if (int(2000) / -50 != -40)
        {
            PrintFailed(prompt, 107);
        }
        if (int(2000) / -10 != -200)
        {
            PrintFailed(prompt, 108);
        }
        if (-2000 / -100 != 20)
        {
            PrintFailed(prompt, 109);
        }
        if (-2000 / -50 != 40)
        {
            PrintFailed(prompt, 110);
        }
        if (-2000 / -10 != 200)
        {
            PrintFailed(prompt, 111);
        }
        if (2000 / 1 != 2000)
        {
            PrintFailed(prompt, 112);
        }
        if (2000 / 2 != 1000)
        {
            PrintFailed(prompt, 113);
        }
        if (2000 / 4 != 500)
        {
            PrintFailed(prompt, 114);
        }
        if (2000 / 8 != 250)
        {
            PrintFailed(prompt, 115);
        }
        if (int(2000) / -1 != -2000)
        {
            PrintFailed(prompt, 116);
        }
        if (int(2000) / -2 != -1000)
        {
            PrintFailed(prompt, 117);
        }
        if (int(2000) / -4 != -500)
        {
            PrintFailed(prompt, 118);
        }
        if (int(2000) / -8 != -250)
        {
            PrintFailed(prompt, 119);
        }
        
        
        if (1*0 != 0)
        {
            PrintFailed(prompt, 120);
        }
        if (0*1 != 0)
        {
            PrintFailed(prompt, 121);
        }
        if (0*0 != 0)
        {
            PrintFailed(prompt, 122);
        }
        if (1*2 != 2)
        {
            PrintFailed(prompt, 123);
        }
        if (2*1 != 2)
        {
            PrintFailed(prompt, 124);
        }
        if (7*11 != 77)
        {
            PrintFailed(prompt, 125);
        }
        if (11*7 != 77)
        {
            PrintFailed(prompt, 126);
        }
        if (303*2 != 606)
        {
            PrintFailed(prompt, 127);
        }
        if (2*303 != 606)
        {
            PrintFailed(prompt, 128);
        }
        if (303*3 != 909)
        {
            PrintFailed(prompt, 129);
        }
        if (3*303 != 909)
        {
            PrintFailed(prompt, 130);
        }
        if (303*4 != 1212)
        {
            PrintFailed(prompt, 131);
        }
        if (4*303 != 1212)
        {
            PrintFailed(prompt, 132);
        }
        if (303*8 != 2424)
        {
            PrintFailed(prompt, 133);
        }
        if (8*303 != 2424)
        {
            PrintFailed(prompt, 134);
        }
        
        int a = -49;
        int b = 229;
        if (a * b != -11221)
        {
            PrintFailed(prompt, 135);
        }
        if (b * a != -11221)
        {
            PrintFailed(prompt, 136);
        }
        
        // %
        if (localInt % 3 != 1)
        {
            PrintFailed(prompt, 137);
        }
        
        int test = 0x1234;
        
        int testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(prompt, 138);
        }
        
        testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(prompt, 139);
        }
        localIntNeg = -20;
        localInt = 50;
        if (localIntNeg / localInt != 0)
        {
            PrintFailed(prompt, 140);
        }
        
    } // TestIntMath
    
    TestArray()
    {
        string prompt = "Array";
        WriteLn(prompt);
        int[5] intArray;
        intArray[0] = -3;
        intArray[1] = 20000;
        intArray[2] = 1;
        
        if (intArray.Count != 5)
        {
            PrintFailed(prompt, 1);
        }
        
        if (intArray[2] != 1)
        {
            PrintFailed(prompt, 2);
        }
        if (intArray[0] != -3)
        {
            PrintFailed(prompt, 3);
        }
        if (intArray[1] != 20000)
        {
            PrintFailed(prompt, 4);
        }

        if (intArray[3] != 0)
        {
            PrintFailed(prompt, 5);
        }
        int count = 0;
#ifndef CPU_Z80        
        foreach (var a in intArray)
        {
            count++;
        }
        if (count != 5)
        {
            PrintFailed(prompt, 6);
        }
        count = 0;
#endif
        
        uint[5] uintArray;
        uintArray[0] = 3;
        uintArray[1] = 40000;
        uintArray[2] = 1;
        if (uintArray.Count != 5)
        {
            PrintFailed(prompt, 7);
        }
        
        if (uintArray[2] != 1)
        {
            PrintFailed(prompt, 8);
        }
        
        if (uintArray[0] != 3)
        {
            PrintFailed(prompt, 9);
        }
        if (uintArray[1] != 40000)
        {
            PrintFailed(prompt, 10);
        }
        if (uintArray[3] != 0)
        {
            PrintFailed(prompt, 11);
        }
        count = 0;
#ifndef CPU_Z80        
        foreach (var a in uintArray)
        {
            count++;
        }
        if (count != 5)
        {
            PrintFailed(prompt, 12);
        }
        count = 0;
#endif        
        char[5] charArray;
        charArray[0] = char(0);
        charArray[1] = 'a';
        charArray[2] = 'b';
        if (charArray.Count != 5)
        {
            PrintFailed(prompt, 13);
        }
        
        if (charArray[2] != 'b')
        {
            PrintFailed(prompt, 14);
        }
        
        if (charArray[0] != char(0))
        {
            PrintFailed(prompt, 15);
        }
        if (charArray[1] != 'a')
        {
            PrintFailed(prompt, 16);
        }
        if (charArray[3] != char(0))
        {
            PrintFailed(prompt, 17);
        }
        count = 0;
#ifndef CPU_Z80        
        foreach (var a in charArray)
        {
            count++;
        }
        if (count != 5)
        {
            PrintFailed(prompt, 18);
        }
        count = 0;
#endif
        
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
            PrintFailed(prompt, 19);
        }
        if (boolArray[2] != true)
        {
            PrintFailed(prompt, 20);
        }
        if (boolArray[0] != true)
        {
            PrintFailed(prompt, 21);
        }
        if (boolArray[1] != false)
        {
            PrintFailed(prompt, 22);
        }
        if (boolArray[3] != false)
        {
            PrintFailed(prompt, 23);
        }
        if (boolArray[4] != false)
        {
            PrintFailed(prompt, 24);
        }
        if (boolArray[5] != true)
        {
            PrintFailed(prompt, 25);
        }
        if (boolArray[6] != true)
        {
            PrintFailed(prompt, 26);
        }
        if (boolArray[7] != false)
        {
            PrintFailed(prompt, 27);
        }
        if (boolArray[8] != true)
        {
            PrintFailed(prompt, 28);
        }
        if (boolArray[9] != true)
        {
            PrintFailed(prompt, 29);
        }
        
        count = 0;
#ifndef CPU_Z80        
        foreach (var a in boolArray)
        {
            if (a)
            {
                count++;
            }
        }
        if (count != 6)
        {
            PrintFailed(prompt, 30);
        }
        count = 0;
#endif
    }
    
    
    Hopper()
    {
        
        WriteLn();
        WriteLn("Minimal Runtime Validation Tests:");

        TestArray();        
/*
        TestCharSystem();
        TestStringCompare();
        TestString();
        TestStringCase();
        TestHexStrings();
        TestRef();
        TestStringTrim();
        TestStringSystem();

        TestForEach();
        TestFor();
        TestWhile();
        TestBooleanShortCircuit();
        TestEquals();
        TestLessThan();
        TestGreaterThan();
        TestGreaterThanOrEqual();
        TestLessThanOrEqual();
        TestConstants();
        TestPropertyMath();
        TestSwitch();
        TestUIntMath();
        TestIntMath();
  */      
        WriteLn("  Passed");
    }
}
