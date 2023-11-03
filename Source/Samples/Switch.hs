program Switch
{
    
    uses "Screen"
    
    TestFunction(char switchKey)
    {
        switch (switchKey)
        {
            case 'A':
            {
                PrintLn("A");
            }
            case 'B':
            {
                PrintLn("B");
            }
            case 'C':
            {
                PrintLn("C");
            }
            case 'D':
            {
                PrintLn("D");
            }
            default:
            {
                PrintLn("E");
            }
        }
    }
    
    TestStringFunction(string switchKey)
    {
        switch (switchKey)
        {
            case "A":
            {
                PrintLn("A");
            }
            case "B":
            {
                PrintLn("B");
            }
            case "C":
            {
                PrintLn("C");
            }
            case "D":
            {
                PrintLn("D");
            }
            default:
            {
                PrintLn("E");
            }
        }
    }
    
    {
        TestFunction('A');
        TestFunction('B');
        TestFunction('C');
        TestFunction('D');
        TestFunction('Z');
        
        TestStringFunction("A");
        TestStringFunction("B");
        TestStringFunction("C");
        TestStringFunction("D");
        TestStringFunction("Z");
    }
}