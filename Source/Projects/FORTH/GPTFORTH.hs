program GTPFORTH
{
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    const uint stackLimit = 1024;
    int[stackLimit] stack;
    uint sp = 0; // Initialize stack pointer
    bool running = true; // Control the main loop
    
    const uint memorySize = 1024;
    int[memorySize] memory;
    
    record Word
    {
        string Name;
        <string> Definition;
    }
    
    <Word> wordList;
    bool definingWord = false;
    string currentWordName;
    <string> currentWordDefinition;
    
    // Push a value onto the stack
    push(int value)
    {
        if (sp < stackLimit)
        {
            stack[sp] = value;
            sp++;
        }
        else
        {
            WriteLn("Stack Overflow");
        }
    }
    
    // Pop a value from the stack
    int pop()
    {
        if (sp > 0)
        {
            sp--;
            return stack[sp];
        }
        else
        {
            WriteLn("Stack Underflow");
            return 0; // Default error value
        }
    }
    
    // Execute a single token
    executeToken(string token)
    {
        int number;
        token = token.ToLower(); // Convert token to lowercase for case-insensitive comparison
        if (definingWord)
        {
            if (token == ";")
            {
                definingWord = false;
                Word newWord;
                newWord.Name = currentWordName;
                newWord.Definition = currentWordDefinition;
                wordList.Append(newWord);
                WriteLn("Defined word: " + currentWordName);
            }
            else if (currentWordName.Length == 0)
            {
                currentWordName = token; // Set the word name
            }
            else
            {
                currentWordDefinition.Append(token);
            }
        }
        else if (Int.TryParse(token, ref number))
        {
            push(number);
        }
        else
        {
            switch (token)
            {
                case ":":
                {
                    definingWord = true;
                    currentWordDefinition.Clear();
                    currentWordName = ""; // Reset the word name
                }
                case ".":
                {
                    int top = pop();
                    Write(top.ToString());
                }
                case "+":
                {
                    int top = pop();
                    int next = pop();
                    push(next + top);
                }
                case "-":
                {
                    int top = pop();
                    int next = pop();
                    push(next - top);
                }
                case "*":
                {
                    int top = pop();
                    int next = pop();
                    push(next * top);
                }
                case "/":
                {
                    int top = pop();
                    int next = pop();
                    if (top == 0)
                    {
                        WriteLn("Division by zero");
                        push(0); // Default error value
                    }
                    else
                    {
                        push(next / top);
                    }
                }
                case "mod":
                {
                    int top = pop();
                    int next = pop();
                    if (top == 0)
                    {
                        WriteLn("Division by zero");
                        push(0); // Default error value
                    }
                    else
                    {
                        push(next % top);
                    }
                }
                case "negate":
                {
                    int top = pop();
                    push(-top);
                }
                case "abs":
                {
                    int top = pop();
                    push(top < 0 ? -top : top);
                }
                // logical operations
                case "and":
                {
                    int top = pop();
                    int next = pop();
                    
                    byte b0 = top.GetByte(0) & next.GetByte(0);
                    byte b1 = top.GetByte(1) & next.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                case "or":
                {
                    int top = pop();
                    int next = pop();
                    
                    byte b0 = top.GetByte(0) | next.GetByte(0);
                    byte b1 = top.GetByte(1) | next.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                case "xor":
                {
                    int top = pop();
                    int next = pop();
                    
                    byte b0 = top.GetByte(0) ^ next.GetByte(0);
                    byte b1 = top.GetByte(1) ^ next.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                case "invert":
                {
                    int top = pop();
                    
                    byte b0 = ~top.GetByte(0);
                    byte b1 = ~top.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                
                // comparison operations
                case "=":
                {
                    int top = pop();
                    int next = pop();
                    push(next == top ? -1 : int(0));
                }
                case "<":
                {
                    int top = pop();
                    int next = pop();
                    push(next < top ? -1 : int(0));
                }
                case ">":
                {
                    int top = pop();
                    int next = pop();
                    push(next > top ? -1 : int(0));
                }
                
                // stack operations
                case "dup":
                {
                    if (sp > 0)
                    {
                        int top = stack[sp - 1];
                        push(top);
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                case "drop":
                {
                    if (sp > 0)
                    {
                        _ = pop(); // Just pop the top value
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                case "swap":
                {
                    if (sp > 1)
                    {
                        int top = pop();
                        int next = pop();
                        push(top);
                        push(next);
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                case "over":
                {
                    if (sp > 1)
                    {
                        int second = stack[sp - 2];
                        push(second);
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                case "rot":
                {
                    if (sp > 2)
                    {
                        int third = stack[sp - 3];
                        int second = stack[sp - 2];
                        int first = stack[sp - 1];
                        stack[sp - 3] = second;
                        stack[sp - 2] = first;
                        stack[sp - 1] = third;
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                case "-rot":
                {
                    if (sp > 2)
                    {
                        int third = stack[sp - 3];
                        int second = stack[sp - 2];
                        int first = stack[sp - 1];
                        stack[sp - 3] = first;
                        stack[sp - 2] = third;
                        stack[sp - 1] = second;
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                
                
                
                // memory operations
                case "!":
                {
                    int address = pop();
                    int value = pop();
                    if (address >= 0 && address < memorySize)
                    {
                        memory[address] = value;
                    }
                    else
                    {
                        WriteLn("Invalid memory address");
                    }
                }
                case "@":
                {
                    int address = pop();
                    if (address >= 0 && address < memorySize)
                    {
                        push(memory[address]);
                    }
                    else
                    {
                        WriteLn("Invalid memory address");
                        push(0); // Default error value
                    }
                }
                case "c!":
                {
                    int address = pop();
                    int value = pop() & 0xFF;
                    if (address >= 0 && address < memorySize)
                    {
                        memory[address] = value;
                    }
                    else
                    {
                        WriteLn("Invalid memory address");
                    }
                }
                case "c@":
                {
                    int address = pop();
                    if (address >= 0 && address < memorySize)
                    {
                        push(memory[address] & 0xFF);
                    }
                    else
                    {
                        WriteLn("Invalid memory address");
                        push(0); // Default error value
                    }
                }
                
                // input / output operations
                case "emit":
                {
                    int value = pop();
                    char ch = char(value.GetByte(0) & 0xFF);
                    Write(ch);
                }
                case "cr":
                {
                    WriteLn("");
                }
                case "key":
                {
                    char ch = Serial.ReadChar();
                    push(int(ch));
                }
                
                case "bye":
                {
                    running = false; // Set the flag to false to exit the loop
                }
                default:
                {
                    bool found = false;
                    foreach (var word in wordList)
                    {
                        if (word.Name == token)
                        {
                            found = true;
                            <string> definition = word.Definition;
                            foreach (var wordToken in definition)
                            {
                                executeToken(wordToken);
                            }
                            break;
                        }
                    }
                    if (!found)
                    {
                        WriteLn("Unknown token: " + token);
                    }
                }
            }
        }
    }
    
    // Process input and split into tokens
    processInput(string input)
    {
        uint start = 0;
        bool isToken = false;
    
        for (uint i = 0; i <= input.Length; i++)
        {
            if ((i == input.Length) || Char.IsWhitespace(input[i])) // Check for end of input or space
            {
                if (isToken) // End of a token
                {
                    string token = input.Substring(start, i - start);
                    executeToken(token);
                    isToken = false; // Reset token flag
                }
            }
            else if (!isToken) // Start of a new token
            {
                isToken = true;
                start = i;
            }
        }
    }
    
    // Main entry point
    Hopper()
    {
        string inputLine;
        while (running) // Continue running while the flag is true
        {
            Write(">>> ");
            if (ReadLn(ref inputLine))
            {
                processInput(inputLine);
            }
        }
        WriteLn("Exiting GTPFORTH interpreter."); // Message on exit
    }    
}

