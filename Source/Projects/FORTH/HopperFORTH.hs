program HopperFORTH
{
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    const uint stackLimit = 1024; // Define the maximum stack size
    int[stackLimit] stack; // Stack array
    int sp = 0; // Initialize stack pointer
    bool running = true; // Control the main loop
    
    const uint memorySize = 1024; // Define the memory size
    int[memorySize] memory; // Memory array
    
    // Define a record to represent a FORTH word
    record Word
    {
        string Name; // Word name
        <string> Definition; // Word definition
    }
    
    <Word> wordList; // List to store user-defined words
    bool definingWord = false; // Flag to indicate if a word is being defined
    string currentWordName; // Current word being defined
    <string> currentWordDefinition; // Current word definition being built
    
    // Push a value onto the stack ( n -- )
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
    
    // Pop a value from the stack ( -- n )
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
                // Start defining a new word ( -- )
                case ":":
                {
                    definingWord = true;
                    currentWordDefinition.Clear();
                    currentWordName = ""; // Reset the word name
                }
                // Print the top value on the stack ( n -- )
                case ".":
                {
                    int top = pop();
                    Write(top.ToString() + " ");
                }
                // Addition ( n1 n2 -- n1+n2 )
                case "+":
                {
                    int top = pop();
                    int next = pop();
                    push(next + top);
                }
                // Subtraction ( n1 n2 -- n1-n2 )
                case "-":
                {
                    int top = pop();
                    int next = pop();
                    push(next - top);
                }
                // Multiplication ( n1 n2 -- n1*n2 )
                case "*":
                {
                    int top = pop();
                    int next = pop();
                    push(next * top);
                }
                // Division ( n1 n2 -- n1/n2 )
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
                // Modulo ( n1 n2 -- n1%n2 )
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
                // Absolute value of the top value on the stack ( n -- |n| )
                case "abs":
                {
                    int top = pop();
                    push(top < 0 ? -top : top);
                }
                // Logical AND ( n1 n2 -- n1&n2 )
                case "and":
                {
                    int top = pop();
                    int next = pop();
                    
                    byte b0 = top.GetByte(0) & next.GetByte(0);
                    byte b1 = top.GetByte(1) & next.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                // Logical OR ( n1 n2 -- n1|n2 )
                case "or":
                {
                    int top = pop();
                    int next = pop();
                    
                    byte b0 = top.GetByte(0) | next.GetByte(0);
                    byte b1 = top.GetByte(1) | next.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                // Logical XOR ( n1 n2 -- n1^n2 )
                case "xor":
                {
                    int top = pop();
                    int next = pop();
                    
                    byte b0 = top.GetByte(0) ^ next.GetByte(0);
                    byte b1 = top.GetByte(1) ^ next.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                // Bitwise NOT ( n -- ~n )
                case "invert":
                {
                    int top = pop();
                    
                    byte b0 = ~top.GetByte(0);
                    byte b1 = ~top.GetByte(1);
                    push(Int.FromBytes(b0, b1));
                }
                // Comparison operations ( n1 n2 -- flag )
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
                // Duplicate the top value on the stack ( n -- n n )
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
                // Drop the top value on the stack ( n -- )
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
                // Swap the top two values on the stack ( n1 n2 -- n2 n1 )
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
                // Copy the second value on the stack to the top ( n1 n2 -- n1 n2 n1 )
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
                // Rotate the top three values on the stack ( n1 n2 n3 -- n2 n3 n1 )
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
                // Rotate the top three values on the stack in the opposite direction ( n1 n2 n3 -- n3 n1 n2 )
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
                // Fetch the nth item from the stack ( n -- n' )
                case "pick":
                {
                    int n = pop();
                    if (sp > n)
                    {
                        int value = stack[sp - n - 1];
                        push(value);
                    }
                    else
                    {
                        WriteLn("Stack Underflow");
                    }
                }
                // Store a value in memory ( n addr -- )
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
                // Fetch a value from memory ( addr -- n )
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
                // Store a byte in memory ( byte addr -- )
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
                // Fetch a byte from memory ( addr -- byte )
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
                // Output a character ( n -- )
                case "emit":
                {
                    int value = pop();
                    char ch = char(value.GetByte(0) & 0xFF);
                    Write(ch);
                }
                // Output a carriage return and line feed ( -- )
                case "cr":
                {
                    WriteLn("");
                }
                // Read a single character ( -- n )
                case "key":
                {
                    char ch = Serial.ReadChar();
                    push(int(ch));
                }
                // Has a key been pressed? ( -- n )
                case "key?":
                {
                    push(Serial.IsAvailable ? -1 : int(0));
                }
                // Exit the interpreter ( -- )
                case "bye":
                {
                    running = false; // Set the flag to false to exit the loop
                }
                // Execute user-defined words or handle unknown tokens
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
    
    // Process input and split into tokens ( input -- )
    processInput(string input)
    {
        uint start = 0;
        bool isToken = false;
        bool inComment = false;

        for (uint i = 0; i <= input.Length; i++)
        {
            if (inComment)
            {
                if ((i < input.Length) && (input[i] == ')'))
                {
                    inComment = false; // End of comment
                }
            }
            else
            {
                if ((i < input.Length) && (input[i] == '('))
                {
                    inComment = true; // Start of comment
                    isToken = false; // Reset token flag
                }
                else if ((i == input.Length) || Char.IsWhitespace(input[i])) // Check for end of input or space
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
    }
    
    // Define a word with a given name and definition
    defineWord(string name, <string> definition)
    {
        Word newWord;
        newWord.Name = name;
        newWord.Definition = definition;
        wordList.Append(newWord);
    }

    // Initialization method to define common FORTH words
    initialize()
    {
        <string> definition;
        
        // Define `constant` ( n -- )
        definition.Clear();
        definition.Append("create");
        definition.Append("does>");
        definition.Append("@");
        defineWord("constant", definition);
    
        // Define `nip` ( n1 n2 -- n2 )
        definition.Clear();
        definition.Append("swap");
        definition.Append("drop");
        defineWord("nip", definition);
        
        // Define `tuck` ( n1 n2 -- n2 n1 n2 )
        definition.Clear();
        definition.Append("dup");
        definition.Append("-rot");
        defineWord("tuck", definition);
        
        // Define `2dup` ( n1 n2 -- n1 n2 n1 n2 )
        definition.Clear();
        definition.Append("over");
        definition.Append("over");
        defineWord("2dup", definition);
        
        // Define `2drop` ( n1 n2 -- )
        definition.Clear();
        definition.Append("drop");
        definition.Append("drop");
        defineWord("2drop", definition);
        
        // Define `2swap` ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
        definition.Clear();
        definition.Append("2");
        definition.Append("pick");
        definition.Append("2");
        definition.Append("pick");
        definition.Append("rot");
        definition.Append("rot");
        defineWord("2swap", definition);
        
        // Define `2over` ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
        definition.Clear();
        definition.Append("3");
        definition.Append("pick");
        definition.Append("3");
        definition.Append("pick");
        defineWord("2over", definition);
    
        // Define `0=` ( n -- flag )
        definition.Clear();
        definition.Append("0");
        definition.Append("=");
        defineWord("0=", definition);
    
        // Define `0<` ( n -- flag )
        definition.Clear();
        definition.Append("0");
        definition.Append("<");
        defineWord("0<", definition);
    
        // Define `0>` ( n -- flag )
        definition.Clear();
        definition.Append("0");
        definition.Append(">");
        defineWord("0>", definition);
    
        // Define `max` ( n1 n2 -- max )
        definition.Clear();
        definition.Append("2dup");
        definition.Append("<");
        definition.Append("if");
        definition.Append("drop");
        definition.Append("else");
        definition.Append("nip");
        definition.Append("then");
        defineWord("max", definition);
    
        // Define `min` ( n1 n2 -- min )
        definition.Clear();
        definition.Append("2dup");
        definition.Append(">");
        definition.Append("if");
        definition.Append("drop");
        definition.Append("else");
        definition.Append("nip");
        definition.Append("then");
        defineWord("min", definition);
    
        // Define `depth` ( -- n )
        definition.Clear();
        definition.Append("sp");
        defineWord("depth", definition);
        
        // Define `negate` ( n -- -n )
        definition.Clear();
        definition.Append("0");
        definition.Append("swap");
        definition.Append("-");
        defineWord("negate", definition);
    }
    
    
        
    // Main entry point ( -- )
    Hopper()
    {
        // Initialize common FORTH words
        initialize();
        
        string inputLine;
        while (running) // Continue running while the flag is true
        {
            Write(">>> ");
            if (ReadLn(ref inputLine))
            {
                processInput(inputLine);
            }
        }
        WriteLn("Exiting HopperFORTH interpreter."); // Message on exit
    }    
}

