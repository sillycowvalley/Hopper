program TestLexer
{
    uses "Lexer"
    uses "TinyToken"
    uses "/Source/System/System"
    uses "/Source/System/IO"

    Hopper()
    {
        string sourcePath = Arguments[0];
        string source = File.ReadAllText(sourcePath);
        Lexer lexer = Lexer(source);

        loop
        {
            Token token = scanToken(ref lexer);
            IO.WriteLn(token.Type.ToString() + " " + token.Lexeme);

            if (token.Type == TokenType.EOF)
            {
                break;
            }
        }
    }
}
