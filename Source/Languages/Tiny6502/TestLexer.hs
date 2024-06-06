program TestLexer
{
    #define EXPERIMENTAL
    #define MCU
     
    uses "TinyToken"
    uses "Lexer"
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    string Clean(string value)
    {
        string clean;
        clean = value.Replace("\\", "/");
        return clean;
    }

    Hopper()
    {
        Initialize();
        //<string> arguments = Arguments;
        //string sourcePath = "/data/test.tc"; // Path.GetFullPath(arguments[0]);
        string sourcePath = "/data/testsuite.tc"; // Path.GetFullPath(arguments[0]);
        WriteLn(sourcePath);
        string source;
        if (!File.TryReadAllText(sourcePath, ref source))
        {
            // failed to read source
        }
        
        Lexer lexer = Lexer(source);

        loop
        {
            Token token = Lexer.ScanToken(ref lexer);
            IO.WriteLn(">> " + TinyToken.ToString(token.Type) + " '" + Clean(token.Lexeme) + "'");

            if (token.Type == TokenType.EOF)
            {
                break;
            }
        }
    }
}
