program HopperFORTH
{
    //uses "/Source/Library/Boards/PiPico"
    uses "/Source/Library/Boards/Hopper6502"
    
    const uint stackLimit = 1024; // Define the maximum stack size
    const uint memorySize = 1024; // Define the memory size
    
    int[stackLimit] stack; // Stack array
    int[memorySize] memory; // Memory array
    
    int sp = 0; // Initialize stack pointer
    bool running = true; // Control the main loop
    
    <uint> ifStack;
    <uint> elseStack;
    <uint> beginStack;  // Stack to track BEGIN positions
    <uint> doStack;     // Stack to track DO positions
    <int> doParameters; // Stack to hold DO loop parameters (start, end, current)
    
    // Define a record to represent a FORTH word
    record Word
    {
        string   Name;           // Word name
        <string> Definition;     // Word definition
    }
    string builtInWords = " : . .\" .s words + - * / mod abs and or xor invert = < > dup drop swap over rot -rot pick ! @ c! c@ emit cr key key? bye if else then begin until again 0branch branch do loop i exit seconds delay pin in out ";
    <Word> wordList; // List to store user-defined words
    bool definingWord; // Flag to indicate if a word is being defined
    string currentWordName; // Current word being defined
    <string> currentWordDefinition; // Current word definition being built
    
    int intListPop(<int> stackList)
    {
        uint index = stackList.Count-1;
        int result = stackList[index];
        stackList.Remove(index);
        return result;   
    }
    uint uintListPop(<uint> stackList)
    {
        uint index = stackList.Count-1;
        uint result = stackList[index];
        stackList.Remove(index);
        return result;   
    }
    
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
    executeToken(<string> currentDefinition, ref uint currentTokenIndex)
    {
        string token = currentDefinition[currentTokenIndex];
        string lowerToken = token.ToLower(); // Convert token to lowercase for case-insensitive comparison    
        
        int address;
        int value;
        bool currentTokenIndexModified;
        
        if (definingWord)
        {
            if (token == ";")
            {
                definingWord = false;
                Word newWord;
                newWord.Name = currentWordName;
                newWord.Definition = currentWordDefinition;
                wordList.Append(newWord);
                
                /*
                IO.Write("Defined word: " + currentWordName + " '");
                bool first = true;
                foreach (var token in currentWordDefinition)
                {
                    if (!first)
                    {
                        IO.Write(" ");
                    }
                    IO.Write(token);
                    first = false;
                }
                IO.WriteLn("'");
                */
            }
            else if (currentWordName.Length == 0)
            {
                currentWordName = token; // Set the word name
            }
            else
            {
                switch (lowerToken)
                {
                    case "if":
                    {
                        currentWordDefinition.Append(token);           // ELSE
                        currentWordDefinition.Append("0");             // Placeholder
                        ifStack.Append(currentWordDefinition.Count-1); // Placeholder index
                    }
                    case "else":
                    {
                        if (ifStack.Count > 0)
                        {
                            uint ifPos = uintListPop(ifStack);
                            currentWordDefinition[ifPos] = (currentWordDefinition.Count + 2).ToString(); // Address of ELSE
                        }
                        currentWordDefinition.Append(token);             // ELSE
                        currentWordDefinition.Append("0");               // Placeholder
                        elseStack.Append(currentWordDefinition.Count-1); // Placeholder index
                    }
                    case "then":
                    {
                        if (elseStack.Count > 0)
                        {
                            uint elsePos = uintListPop(elseStack);
                            currentWordDefinition[elsePos] = (currentWordDefinition.Count).ToString(); // Address of ELSE
                        }
                        else if (ifStack.Count > 0)
                        {
                            uint ifPos = uintListPop(ifStack);
                            currentWordDefinition[ifPos] = (currentWordDefinition.Count).ToString(); // Address of THEN
                        }
                        else
                        {
                            WriteLn("Error: unmatched THEN");
                        }
                        currentWordDefinition.Append(token); // THEN
                    }
                    case "begin":
                    {
                        beginStack.Append(currentWordDefinition.Count); // Record the position of BEGIN
                        currentWordDefinition.Append(token);           // Append BEGIN token
                    }
                    case "until":
                    {
                        if (beginStack.Count > 0)
                        {
                            uint beginPos = uintListPop(beginStack);
                            currentWordDefinition.Append("0branch");           // Append conditional branch
                            currentWordDefinition.Append(beginPos.ToString()); // Append position to jump to (BEGIN)
                        }
                        else
                        {
                            WriteLn("Error: unmatched UNTIL");
                        }
                    }
                    case "again":
                    {
                        if (beginStack.Count > 0)
                        {
                            uint beginPos = uintListPop(beginStack);
                            currentWordDefinition.Append("branch");            // Append unconditional branch
                            currentWordDefinition.Append(beginPos.ToString()); // Append position to jump to (BEGIN)
                        }
                        else
                        {
                            WriteLn("Error: unmatched AGAIN");
                        }
                    }
                    case "do":
                    {
                        currentWordDefinition.Append(token);         // Append DO token
                        doStack.Append(currentWordDefinition.Count); // Record the position after DO
                    }
                    case "loop":
                    {
                        if (doStack.Count > 0)
                        {
                            uint doPos  = uintListPop(doStack);
                            currentWordDefinition.Append(token);            // Append LOOP token
                            currentWordDefinition.Append(doPos.ToString()); // Append position to jump to (DO)
                        }
                        else
                        {
                            WriteLn("Error: unmatched LOOP");
                        }
                    }
                    case "i":
                    {
                        currentWordDefinition.Append(token); // Append I token
                    }
                    default:
                    {
                        currentWordDefinition.Append(token);
                    }
                }
            }
            currentTokenIndex++;
        }
        else if (Int.TryParse(token, ref value))
        {
            push(value);
            currentTokenIndex++;
        }
        else
        {
            bool found = false;
            bool builtIn = false;
            

            // most recently defined word first (so we can redefine existing words)            
            uint wordIndex = wordList.Count;
            loop
            {
                if (wordIndex == 0) { break; }
                wordIndex--;
                
                Word word = wordList[wordIndex];
                string lowerName = (word.Name).ToLower();
                if (lowerName == lowerToken)
                {
                    found = true;
                    <string> wordDefinition = word.Definition;
                    executeDefinition(wordDefinition);
                    break;
                }
            }
        
            if (!found)
            {
                // check for built-ins after user defined words so they can be redefined
                if (builtInWords.Contains(" " + lowerToken + " "))
                {
                    found = true;
                    builtIn = true;
                }
                else
                {
                    WriteLn("Unknown token: '" + token + "'");
                }
            }
            if (builtIn)
            {
                switch (lowerToken)
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
                        IO.Write(top.ToString() + " ");
                    }
                    // Print string ( ." <string> " -- )
                    case ".\"":
                    {
                        currentTokenIndex++;
                        string str = currentDefinition[currentTokenIndex];
                        IO.Write(str);
                    }
                    case ".s":
                    {
                        IO.Write("Stack: ");
                        for (int i = 0; i < sp; i++)
                        {
                            IO.Write((stack[i]).ToString() + " ");
                        }
                        IO.WriteLn("");
                    }
                    // List all defined words ( -- )
                    case "words":
                    {
                        IO.Write(builtInWords.Trim());
                        foreach (var word in wordList)
                        {
                            IO.Write(" " + word.Name);
                        }
                        IO.WriteLn(""); // Newline after listing words
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
                            value = stack[sp - n - 1];
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
                        address = pop();
                        value = pop();
                        if ((address >= 0) && (address < memorySize))
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
                        address = pop();
                        if ((address >= 0) && (address < memorySize))
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
                        address = pop();
                        value = pop() & 0xFF;
                        if ((address >= 0) && (address < memorySize))
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
                        address = pop();
                        if ((address >= 0) && (address < memorySize))
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
                        value = pop();
                        char ch = char(value.GetByte(0) & 0xFF);
                        IO.Write(ch);
                    }
                    // Output a carriage return and line feed ( -- )
                    case "cr":
                    {
                        IO.WriteLn("");
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
                    
                    case "if":
                    {
                        int condition = pop();
                        currentTokenIndex++; // consume the address token
                        if (condition == 0)
                        {
                            // Jump to the address after `if`
                            string addressString = currentDefinition[currentTokenIndex];
                            _ = UInt.TryParse(addressString, ref currentTokenIndex);
                            currentTokenIndexModified = true;
                        }
                    }
                    case "else":
                    {
                        // Jump to the address after `else`
                        currentTokenIndex++; // consume the address token
                        string addressString = currentDefinition[currentTokenIndex];
                        _ = UInt.TryParse(addressString, ref currentTokenIndex);
                        currentTokenIndexModified = true;
                    }
                    case "then":
                    {
                        // `THEN` is a no-op during execution
                    }
                    case "begin":
                    {
                        // `BEGIN` is a no-op during execution
                    }
                    case "0branch":
                    {
                        int condition = pop();
                        currentTokenIndex++; // consume the address token
                        if (condition == 0)
                        {
                            // Jump to the branch address
                            string addressString = currentDefinition[currentTokenIndex];
                            _ = UInt.TryParse(addressString, ref currentTokenIndex);
                            currentTokenIndexModified = true;
                        }
                    }
                    case "branch":
                    {
                        currentTokenIndex++; // consume the address token
                        
                        // Jump to the branch address
                        string addressString = currentDefinition[currentTokenIndex];
                        _ = UInt.TryParse(addressString, ref currentTokenIndex);
                        currentTokenIndexModified = true;
                    }
                    case "do":
                    {
                        int start = pop();
                        int end   = pop();
                        doParameters.Append(end);
                        doParameters.Append(start);
                        doParameters.Append(start); // Initialize the current index
                    }
                    case "loop":
                    {
                        int currentIndex = intListPop(doParameters);
                        int start        = intListPop(doParameters);
                        int end          = intListPop(doParameters);
                        currentIndex++;
                        currentTokenIndex++; // consume the address token
                        if (currentIndex < end)
                        {
                            doParameters.Append(end);
                            doParameters.Append(start);
                            doParameters.Append(currentIndex);
                            
                            // Jump back to the start of the loop
                            string addressString = currentDefinition[currentTokenIndex];
                            _ = UInt.TryParse(addressString, ref currentTokenIndex);
                            currentTokenIndexModified = true;
                        }
                    }
                    case "i":
                    {
                        int currentIndex = doParameters[doParameters.Count-1];
                        push(currentIndex);
                    }
                    case "exit":
                    {
                        currentTokenIndex = currentDefinition.Count; // Set the token index to the end to exit the current word
                        currentTokenIndexModified = true;
                    }
                    
                    case "seconds":
                    {
                        uint s = Time.Seconds;
                        int seconds = Int.FromBytes(s.GetByte(0), s.GetByte(1));
                        push(seconds);
                    }
                    case "delay":
                    {
                        int duration = pop();
                        Time.Delay(uint(duration));
                    }
                    case "pin":
                    {
                        int mode = pop();
                        int pin = pop();
                        MCU.PinMode(byte(pin), PinModeOption(mode));
                    }
                    case "in":
                    {
                        int pin = pop();
                        push(MCU.DigitalRead(byte(pin)) ? -1 : int(0));
                    }
                    case "out":
                    {
                        value = pop();
                        int pin = pop();
                        MCU.DigitalWrite(byte(pin), value == -1);
                    }
                    
                    
                    default:
                    {
                        WriteLn("Unknown built-in token: " + token);
                    }
                } // built-in switch
            }            
            if (!currentTokenIndexModified)
            {
                currentTokenIndex++;
            }
        }
    }
    
    // Process input and split into tokens ( input -- )
    processInput(string input)
    {
        uint start = 0;
        bool isToken = false;
        bool inComment = false;
        bool inEOLComment = false;
        <string> tokenBuffer;
        
        for (uint i = 0; i <= input.Length; i++)
        {
            if (inComment)
            {
                if ((i < input.Length) && (input[i] == ')'))
                {
                    inComment = false; // End of comment
                }
            }
            else if (inEOLComment)
            {
                if ((i == input.Length) || (input[i] == Char.EOL))
                {
                    inEOLComment = false; // End of end-of-line comment
                }
            }
            else
            {
                if ((i < input.Length) && (input[i] == '('))
                {
                    inComment = true; // Start of comment
                    isToken = false; // Reset token flag
                }
                else if ((i < input.Length) && (input[i] == '#'))
                {
                    inEOLComment = true; // Start of end-of-line comment
                    isToken = false; // Reset token flag
                }
                else if ((i + 1 < input.Length) && (input[i] == '.') && (input[i + 1] == '"'))
                {
                    i += 2; // Skip past the ."
                    uint startString = i;
                    while ((i < input.Length) && (input[i] != '"'))
                    {
                        i++;
                    }
                    if (i < input.Length)
                    {
                        string output = input.Substring(startString, i - startString);
                        if (output.StartsWith(' '))
                        {
                            output = output.Substring(1); // trim the ' ' delimiter
                        }
                        
                        tokenBuffer.Append(".\"");
                        tokenBuffer.Append(output);
                        
                        isToken = false; // Reset token flag
                    }
                }
                else if ((i == input.Length) || Char.IsWhitespace(input[i])) // Check for end of input or space
                {
                    if (isToken) // End of a token
                    {
                        string token = input.Substring(start, i - start);
                        tokenBuffer.Append(token);
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
        if (tokenBuffer.Count != 0)
        {
            bool useREPLWord = false;
            if (!definingWord) 
            {
                // we're not within a multiline definition:
                
                bool inDefinition;
                foreach (var word in tokenBuffer)
                {
                    switch (word)
                    {
                        case ":":
                        {
                            inDefinition = true;
                        }
                        case ";":
                        {
                            inDefinition = false;
                        }
                        case "if":
                        case "begin":
                        {
                            if (!inDefinition)
                            {        
                                useREPLWord = true;
                                break;
                            }
                        }
                    }
                }
            }
            
            if (useREPLWord)
            {
                // append temporary '<repl>' word
                tokenBuffer.Insert(0, "<repl>");
                tokenBuffer.Insert(0, ":");
                tokenBuffer.Append(";");
                tokenBuffer.Append("<repl>");
            }
            
            executeDefinition(tokenBuffer);
            
            if (useREPLWord)
            {
                // remove temporary '<repl>' word
                wordList.Remove(wordList.Count-1); 
            }
        }
    }
    
    executeDefinition(<string> definition)
    {
        for (uint currentTokenIndex=0; currentTokenIndex < definition.Count; )
        {
            executeToken(definition, ref currentTokenIndex);
        }
    }
    
    // Initialization method to define common FORTH words
    initialize()
    {
        <string> definition;
        
        // Define `nip` ( n1 n2 -- n2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("nip");
        definition.Append("swap");
        definition.Append("drop");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `tuck` ( n1 n2 -- n2 n1 n2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("tuck");
        definition.Append("dup");
        definition.Append("-rot");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `2dup` ( n1 n2 -- n1 n2 n1 n2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2dup");
        definition.Append("over");
        definition.Append("over");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `2drop` ( n1 n2 -- )
        definition.Clear();
        definition.Append(":");
        definition.Append("2drop");
        definition.Append("drop");
        definition.Append("drop");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `2swap` ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2swap");
        definition.Append("2");
        definition.Append("pick");
        definition.Append("2");
        definition.Append("pick");
        definition.Append("rot");
        definition.Append("rot");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `2over` ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2over");
        definition.Append("3");
        definition.Append("pick");
        definition.Append("3");
        definition.Append("pick");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `0=` ( n -- flag )
        definition.Clear();
        definition.Append(":");
        definition.Append("0=");
        definition.Append("0");
        definition.Append("=");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `0<` ( n -- flag )
        definition.Clear();
        definition.Append(":");
        definition.Append("0<");
        definition.Append("0");
        definition.Append("<");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `0>` ( n -- flag )
        definition.Clear();
        definition.Append(":");
        definition.Append("0>");
        definition.Append("0");
        definition.Append(">");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `max` ( n1 n2 -- max )
        definition.Clear();
        definition.Append(":");
        definition.Append("max");
        definition.Append("2dup");
        definition.Append(">");
        definition.Append("if");
        definition.Append("drop");
        definition.Append("else");
        definition.Append("nip");
        definition.Append("then");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `min` ( n1 n2 -- min )
        definition.Clear();
        definition.Append(":");
        definition.Append("min");
        definition.Append("2dup");
        definition.Append("<");
        definition.Append("if");
        definition.Append("drop");
        definition.Append("else");
        definition.Append("nip");
        definition.Append("then");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `depth` ( -- n )
        definition.Clear();
        definition.Append(":");
        definition.Append("depth");
        definition.Append("sp");
        definition.Append(";");
        executeDefinition(definition);
    
        // Define `negate` ( n -- -n )
        definition.Clear();
        definition.Append(":");
        definition.Append("negate");
        definition.Append("0");
        definition.Append("swap");
        definition.Append("-");
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `1+` ( n -- n+1 )
        definition.Clear();
        definition.Append(":");
        definition.Append("1+");
        definition.Append("1");
        definition.Append("+");
        definition.Append(";");
        executeDefinition(definition);
                
        // Define `1-` ( n -- n-1 )
        definition.Clear();
        definition.Append(":");
        definition.Append("1-");
        definition.Append("1");
        definition.Append("-");
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `2+` ( n -- n+2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2+");
        definition.Append("2");
        definition.Append("+");
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `2-` ( n -- n-2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2-");
        definition.Append("2");
        definition.Append("-");
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `2*` ( n -- n*2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2*");
        definition.Append("2");
        definition.Append("*");
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `2/` ( n -- n/2 )
        definition.Clear();
        definition.Append(":");
        definition.Append("2/");
        definition.Append("2");
        definition.Append("/");
        definition.Append(";");
        executeDefinition(definition);
                
        // Define `<>` ( n1 n2 -- flag )
        definition.Clear();
        definition.Append(":");
        definition.Append("<>");
        definition.Append("=");    // Compare n1 and n2
        definition.Append("0=");   // If they are equal, return false (0), otherwise true (-1)
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `led` ( n -- )
        definition.Clear();
        definition.Append(":");
        definition.Append("led");
        definition.Append((Board.BuiltInLED).ToString());
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `output` ( pin -- )
        definition.Clear();
        definition.Append(":");
        definition.Append("output");
        definition.Append("1");
        definition.Append("pin");
        definition.Append(";");
        executeDefinition(definition);
        
        // Define `input` ( pin -- )
        definition.Clear();
        definition.Append(":");
        definition.Append("input");
        definition.Append("0");
        definition.Append("pin");
        definition.Append(";");
        executeDefinition(definition);
    }
        
    // Main entry point ( -- )
    Hopper()
    {     
        IO.WriteLn();
        IO.WriteLn("Welcome to HopperFORTH v1.0");
    
        //Initialize common FORTH words
        initialize();
        
        IO.WriteLn("Type 'WORDS' to see a list of available words.");
        
        string inputLine;
        while (running) // Continue running while the flag is true
        {
            IO.Write(">>> ");
            if (ReadLn(ref inputLine))
            {
                processInput(inputLine);
            }
        }
        IO.WriteLn("Exited.");
    }    
}