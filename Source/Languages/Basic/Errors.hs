unit Errors
{
    
#ifndef TERSE
    string getMessage(uint number)
    {
        string message;
        switch (number)
        {
            case 1:  { message = "Unknown command"; }
            case 2:  { message = "Bad line number"; }
            case 3:  { message = "Unexpected character"; }
            case 4:  { message = "Integer expected"; }
            case 5:  { message = "Out of memory for code"; }
            case 6:  { message = "Out of memory for expression stack"; }
            case 7:  { message = "Out of memory for call stack"; }
            case 8:  { message = "RETURN without GOSUB"; }
            case 9:  { message = "Syntax error"; }
            case 10: { message = '"' + " expected"; }
            case 11: { message = "EOL expected"; }
            case 12: { message = "TO expected"; }
            case 13: { message = "FOR without NEXT"; }
            case 14: { message = "NEXT without FOR"; }
            case 15: { message = "Integer expression expected"; }
            case 16: { message = "Division by zero"; }
            case 17: { message = ')' + " expected"; }
            case 18: { message = "Unexpected base time"; }
            case 19: { message = "DO without UNTIL"; }
            case 20: { message = "UNTIL without DO"; }
            case 21: { message = "Internal system error"; }
            case 22: { message = "Feature not implemented"; }
            case 23: { message = "Boolean expression expected"; }
            case 24: { message = "Invalid hexadecimal literal"; }
            case 25: { message = "Variable name expected"; }
            case 26: { message = ',' + " expected"; }
            default: { message = "Undefined error message"; }
        }
        return message;
    }
#endif

    Error(uint number, char token, uint lineNumber)
    {
        bool wasContent;
        Write("Error " + number.ToString());
#ifndef TERSE
        string message = getMessage(number);
        if (message.Length != 0)
        {
            Write(": " + message);
            wasContent = true;
        }
#endif  
        if (token != char(0))
        {
            Write(" at '" + token + "' ");
            wasContent = true;
        }
        if ((lineNumber != 0) && (lineNumber <= Source.LineLimit))
        {
            Write(" on line " + lineNumber.ToString());
            wasContent = true;
        }
        if (wasContent)
        {
            Write('.');
        }
        WriteLn();
        Condition = Conditions.Error; 
    }
    Error(uint number, char token)
    {
        Error(number, token, LineNumber);
    }
    Error(uint number)
    {
       Error(number, char(0), LineNumber);
    }
}
