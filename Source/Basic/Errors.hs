unit Errors
{
#ifndef COMPACT
    
    string GetMessage(uint number)
    {
        string message;
        switch (number)
        {
            case 1:  { message = "Unknown command."; }
            case 2:  { message = "Unknown argument."; }
            case 3:  { message = "Invalid or illegal line number. Must be 1..9999."; }
            case 4:  { message = "Expression expected."; }
            case 5:  { message = "Unknown instruction."; }
            case 6:  { message = "'=' expected."; }
            case 7:  { message = "Variable identifier expected."; }
            case 8:  { message = "Variable identifier cannot be reserved word."; }
            case 9:  { message = "END expected."; }
            case 10: { message = "Unexpected trailing content."; }
            case 11: { message = "Destination line does not exist."; }
            case 12: { message = "Closing '\"' expected."; }
      		    case 13: { message = "Target line does not exist."; }
       	    case 14: { message = "Unexpected character in expression:"; }
      	     case 15: { message = "Integer expected."; }
            case 16: { message = "Integers expected for operation."; }
            case 17: { message = "Divide by zero."; }
            case 18: { message = "Type mismatch."; }
            case 19: { message = "THEN expected."; }
            case 20: { message = "Boolean expression expected."; }
            case 21: { message = "Unmatched WEND."; }
            case 22: { message = "'(' expected."; }
            case 23: { message = "WEND expected."; }
            case 24: { message = "Unmatched NEXT."; }
            case 25: { message = "NEXT expected."; }
            case 26: { message = "')' expected."; }
            case 27: { message = "Out of memory."; }
            case 28: { message = "Array index out of range."; }
            case 29: { message = "Positive integer expression expected."; }
            case 30: { message = "TO expected."; }
            case 31: { message = "Undefined variable in expression."; }
            case 32: { message = "Constant integer expression expected."; }
            case 33: { message = "Array variable identifier expected."; }
            case 34: { message = "Out of memory for code."; }
            case 35: { message = "Out of memory for variables."; }
			         case 36: { message = "Internal error."; }
			         case 37: { message = "Line number expected."; }
        }
        return message;
    }
#endif
    
    Error(uint number, string content, uint ln, char token)
    {
        Write("Error " + number.ToString() + ":");
#ifndef COMPACT
        string message = GetMessage(number);
        if (message.Length > 0)
        {
            Write(" " + message);
			if (token != char(0))
			{
         	    Write(" '" + token + "'.");
            }
        }
#endif  
        WriteLn();     
        
        if (content.Length > 0)
        {
            Write(" '" + content + "'");
        }
        if (ln != 0)
        {
            Write(" on line " + ln.ToString());
        }
        if ((content.Length > 0) || (ln != 0))
        {
            WriteLn();
        }
        WasError = true;
    }
   	Error(uint number, string content, uint ln)
    {
	    Error(number, content, ln, char(0));
    }
    
    Error(uint number)
    {
        Error(number, "", 0);
    }
    Error(uint number, uint ln)
    {
        Error(number, "", ln);
    }
    Error(uint number, string content)
    {
        Error(number, content, 0);
    }
}
