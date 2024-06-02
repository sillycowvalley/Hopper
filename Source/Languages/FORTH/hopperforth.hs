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
    
    <int> ifStack;
    <int> elseStack;
    <int> beginStack;   // Stack to track BEGIN positions
    <int> doStack;      // Stack to track DO positions
    <int> doParameters; // Stack to hold DO loop parameters (start, end, current)
    
    // Define a record to represent a FORTH word
    record Word
    {
        string   Name;           // Word name
        <variant> Definition;     // Word definition
    }
    
    <Word>    wordList;              // List to store user-defined words
    bool      definingWord;          // Flag to indicate if a word is being defined
    string    currentWordName;       // Current word being defined
    <variant> currentWordDefinition; // Current word definition being built
    
    // optimization of string comparisons:
    string colonWord = ":";
    string semiColonWord = ";";
    string spaceToken = " ";
    
    int intListPop(<int> stackList)
    {
        uint index = stackList.Count-1;
        int result = stackList[index];
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
    defineWord(<variant> currentDefinition, ref int currentTokenIndex)
    {
        string token;
        uint   index;
        int    value;
        bool   builtIn;
        switch (typeof(currentDefinition[uint(currentTokenIndex)]))
        {
            case uint:   
            { 
                index = uint(currentDefinition[uint(currentTokenIndex)]);
                Word word = wordList[index];
                token = word.Name;
            }
            case byte:   
            { 
                index = uint(currentDefinition[uint(currentTokenIndex)]);
                Word word = wordList[index];
                token = word.Name;
            }
            case int:    
            { 
                value = int(currentDefinition[uint(currentTokenIndex)]);
                token = value.ToString();
            }
            case string: 
            {
                 token = currentDefinition[uint(currentTokenIndex)];
            }
        }
        
        if (token.Equals(semiColonWord))
        {
            definingWord = false;
            Word newWord;
            newWord.Name = currentWordName;
            newWord.Definition = currentWordDefinition;
            wordList.Append(newWord);
            
            /*
            IO.Write("Defined word: " + currentWordName + " '");
            bool first = true;
            
            foreach (var vtoken in currentWordDefinition)
            {
                if (!first)
                {
                    IO.Write(" ");
                }
                switch (typeof(vtoken))
                {
                    case string:
                    {
                        string t = vtoken;
                        IO.Write("S:" + t);
                    }
                    case uint:
                    {
                        Word word = wordList[uint(vtoken)];
                        string name = word.Name;
                        IO.Write("U:" + name);
                    }
                    case byte:
                    {
                        Word word = wordList[uint(vtoken)];
                        string name = word.Name;
                        IO.Write("B:" + name);
                    }
                    case int:
                    {
                        IO.Write("I:" + (int(vtoken)).ToString());
                    }
                    default:
                    {
                        byte t = byte(typeof(vtoken));
                        IO.Write("?:" + t.ToString());
                    }
                }
                first = false;
            }
            IO.WriteLn("'");
            */
            
        }
        else if (currentWordName.Length == 0)
        {
            currentWordName = token.ToLower(); // Set the word name (always lowercase)
        }
        else if (Int.TryParse(token, ref value))
        {
            currentWordDefinition.Append(value);
        }
        else
        {
            string lowerToken = token.ToLower(); // Convert token to lowercase for case-insensitive comparison  
            if (!wordToUInt(lowerToken, ref index, ref builtIn))
            {
                currentWordDefinition.Append(token); // case sensitive (could be ." argument)
            }
            else if (!builtIn)
            {
                currentWordDefinition.Append(uint(index));
            }
            else
            {
                switch (index)
                {
                    case 33: // "if"
                    {
                        currentWordDefinition.Append(byte(33)/*token*/);           // IF
                        currentWordDefinition.Append(0);               // Placeholder
                        ifStack.Append(int(currentWordDefinition.Count-1)); // Placeholder index
                    }
                    case 34: // "else"
                    {
                        if (ifStack.Count > 0)
                        {
                            int ifPos = intListPop(ifStack);
                            currentWordDefinition[uint(ifPos)] = int(currentWordDefinition.Count + 2); // Address of ELSE
                        }
                        currentWordDefinition.Append(byte(34)/*token*/);             // ELSE
                        currentWordDefinition.Append(0);                 // Placeholder
                        elseStack.Append(int(currentWordDefinition.Count-1)); // Placeholder index
                    }
                    case 35: // "then"
                    {
                        if (elseStack.Count > 0)
                        {
                            int elsePos = intListPop(elseStack);
                            currentWordDefinition[uint(elsePos)] = int(currentWordDefinition.Count); // Address of ELSE
                        }
                        else if (ifStack.Count > 0)
                        {
                            int ifPos = intListPop(ifStack);
                            currentWordDefinition[uint(ifPos)] = int(currentWordDefinition.Count); // Address of THEN
                        }
                        else
                        {
                            WriteLn("Error: unmatched THEN");
                        }
                        currentWordDefinition.Append(byte(35)/*token*/); // THEN
                    }
                    case 36: // "begin"
                    {
                        beginStack.Append(int(currentWordDefinition.Count));     // Record the position of BEGIN
                        currentWordDefinition.Append(byte(36)/*token*/);         // Append BEGIN token
                    }
                    case 37: // "until":
                    {
                        if (beginStack.Count > 0)
                        {
                            int beginPos = intListPop(beginStack);
                            currentWordDefinition.Append(byte(39)/*"0branch"*/);        // Append conditional branch
                            currentWordDefinition.Append(beginPos);                     // Append position to jump to (BEGIN)
                        }
                        else
                        {
                            WriteLn("Error: unmatched UNTIL");
                        }
                    }
                    case 38: // "again"
                    {
                        if (beginStack.Count > 0)
                        {
                            int beginPos = intListPop(beginStack);
                            currentWordDefinition.Append(byte(40)/*"branch"*/);       // Append unconditional branch
                            currentWordDefinition.Append(beginPos);                   // Append position to jump to (BEGIN)
                        }
                        else
                        {
                            WriteLn("Error: unmatched AGAIN");
                        }
                    }
                    case 41: // "do"
                    {
                        currentWordDefinition.Append(byte(41)/*token*/);     // Append DO token
                        doStack.Append(int(currentWordDefinition.Count));    // Record the position after DO
                    }
                    case 42: // "loop"
                    {
                        if (doStack.Count > 0)
                        {
                            int doPos  = intListPop(doStack);
                            currentWordDefinition.Append(byte(42)/*token*/);            // Append LOOP token
                            currentWordDefinition.Append(doPos.ToString());             // Append position to jump to (DO)
                        }
                        else
                        {
                            WriteLn("Error: unmatched LOOP");
                        }
                    }
                    case 43: // "i"
                    {
                        currentWordDefinition.Append(byte(43)/*token*/); // Append I token
                    }
                    
                    default:
                    {
                        currentWordDefinition.Append(byte(index));
                    }
                }
            }
        }
        currentTokenIndex++;
    }
    bool wordToUInt(string lowerToken, ref uint index, ref bool builtIn)
    {
        builtIn = false;
        index = wordList.Count;
        loop
        {
            if (index == 0) { break; }
            index--;
            Word word = wordList[index];
            if (lowerToken.Equals(colonWord))
            {
                // ignore ':'
                return false;
            }
            else if (lowerToken.Equals(word.Name))
            {
                <variant> definition = word.Definition;
                if (definition.Count == 1) // special token to imply 'built-in'
                {
                    if (typeof(definition[0]) == string)
                    {
                        string first = definition[0];
                        if (first.Equals(spaceToken))
                        {
                            builtIn = true;
                            return true;
                        }
                    }
                }
                return true;
            }
        }
        return false;
    }
    
    executeWord(<variant> currentDefinition, ref int currentTokenIndex)
    {
        bool currentTokenIndexModified;
        int value;
        string token;
        
        switch (typeof(currentDefinition[uint(currentTokenIndex)]))
        {
            case uint:   
            { 
                uint index = uint(currentDefinition[uint(currentTokenIndex)]);
                Word word = wordList[index];
                <variant> wordDefinition = word.Definition;
                
                executeDefinition(wordDefinition);
            }
            case byte:
            {
                byte builtIn = byte(currentDefinition[uint(currentTokenIndex)]);
                switch (builtIn)
                {
                    // Print the top value on the stack ( n -- )
                    case 1: // "."
                    {
                        int top = pop();
                        IO.Write(top.ToString() + " ");
                    }
                    // Print string ( ." <string> " -- )
                    case 2: // ".\""
                    {
                        currentTokenIndex++;
                        string str = currentDefinition[uint(currentTokenIndex)];
                        IO.Write(str);
                    }
                    case 3: // ".s"
                    {
                        IO.Write("Stack: ");
                        for (int i = 0; i < sp; i++)
                        {
                            IO.Write((stack[i]).ToString() + " ");
                        }
                        IO.WriteLn("");
                    }
                    // List all defined words ( -- )
                    case 4: // "words"
                    {
                        foreach (var word in wordList)
                        {
                            IO.Write(" " + word.Name);
                        }
                        IO.WriteLn(""); // Newline after listing words
                    }
                    
                    // Addition ( n1 n2 -- n1+n2 )
                    case 5: // "+"
                    {
                        int top = pop();
                        int next = pop();
                        push(next + top);
                    }
                    // Subtraction ( n1 n2 -- n1-n2 )
                    case 6: // "-"
                    {
                        int top = pop();
                        int next = pop();
                        push(next - top);
                    }
                    // Multiplication ( n1 n2 -- n1*n2 )
                    case 7: // "*"
                    {
                        int top = pop();
                        int next = pop();
                        push(next * top);
                    }
                    // Division ( n1 n2 -- n1/n2 )
                    case 8: // "/"
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
                    case 9: // "mod"
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
                    case 10: // "abs"
                    {
                        int top = pop();
                        push(top < 0 ? -top : top);
                    }
                    // Logical AND ( n1 n2 -- n1&n2 )
                    case 11: // "and"
                    {
                        int top = pop();
                        int next = pop();
                        
                        byte b0 = top.GetByte(0) & next.GetByte(0);
                        byte b1 = top.GetByte(1) & next.GetByte(1);
                        push(Int.FromBytes(b0, b1));
                    }
                    // Logical OR ( n1 n2 -- n1|n2 )
                    case 12: // "or"
                    {
                        int top = pop();
                        int next = pop();
                        
                        byte b0 = top.GetByte(0) | next.GetByte(0);
                        byte b1 = top.GetByte(1) | next.GetByte(1);
                        push(Int.FromBytes(b0, b1));
                    }
                    // Logical XOR ( n1 n2 -- n1^n2 )
                    case 13: // "xor"
                    {
                        int top = pop();
                        int next = pop();
                        
                        byte b0 = top.GetByte(0) ^ next.GetByte(0);
                        byte b1 = top.GetByte(1) ^ next.GetByte(1);
                        push(Int.FromBytes(b0, b1));
                    }
                    // Bitwise NOT ( n -- ~n )
                    case 14: // "invert"
                    {
                        int top = pop();
                        
                        byte b0 = ~top.GetByte(0);
                        byte b1 = ~top.GetByte(1);
                        push(Int.FromBytes(b0, b1));
                    }
                    // Comparison operations ( n1 n2 -- flag )
                    case 15: // "="
                    {
                        int top = pop();
                        int next = pop();
                        push(next == top ? -1 : int(0));
                    }
                    case 16: // "<"
                    {
                        int top = pop();
                        int next = pop();
                        push(next < top ? -1 : int(0));
                    }
                    
                    // Duplicate the top value on the stack ( n -- n n )
                    case 17: // "dup"
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
                    case 18: // "drop"
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
                    case 19: // "swap"
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
                    case 20: // "over"
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
                    case 21: // "rot"
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
                    case 22: // "-rot"
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
                    case 23: // "pick"
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
                    case 24://"!"
                    {
                        int address = pop();
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
                    case 25: // "@"
                    {
                        int address = pop();
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
                    case 26: // "c!"
                    {
                        int address = pop();
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
                    case 27: // "c@"
                    {
                        int address = pop();
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
                    case 28: // "emit"
                    {
                        value = pop();
                        char ch = char(value.GetByte(0) & 0xFF);
                        IO.Write(ch);
                    }
                    // Output a carriage return and line feed ( -- )
                    case 29: // "cr"
                    {
                        IO.WriteLn("");
                    }
                    // Read a single character ( -- n )
                    case 30: // "key"
                    {
                        char ch = Serial.ReadChar();
                        push(int(ch));
                    }
                    // Has a key been pressed? ( -- n )
                    case 31: // "key?"
                    {
                        push(Serial.IsAvailable ? -1 : int(0));
                    }
                    // Exit the interpreter ( -- )
                    case 32: // "bye"
                    {
                        running = false; // Set the flag to false to exit the loop
                    }
                    
                    case 33: // "if"
                    {
                        int condition = pop();
                        currentTokenIndex++; // consume the address token
                        if (condition == 0)
                        {
                            // Jump to the address after `if`
                            currentTokenIndex = int(currentDefinition[uint(currentTokenIndex)]);
                            currentTokenIndexModified = true;
                        }
                    }
                    case 34: // "else"
                    {
                        // Jump to the address after `else`
                        currentTokenIndex++; // consume the address token
                        currentTokenIndex = int(currentDefinition[uint(currentTokenIndex)]);
                        currentTokenIndexModified = true;
                    }
                    case 35: // "then"
                    {
                        // `THEN` is a no-op during execution
                    }
                    case 36: // "begin"
                    {
                        // `BEGIN` is a no-op during execution
                    }
                    
                    case 39: // "0branch"
                    {
                        int condition = pop();
                        currentTokenIndex++; // consume the address token
                        if (condition == 0)
                        {
                            // Jump to the branch address
                            currentTokenIndex = int(currentDefinition[uint(currentTokenIndex)]);
                            currentTokenIndexModified = true;
                        }
                    }
                    
                    case 40: // "branch"
                    {
                        currentTokenIndex++; // consume the address token
                        
                        // Jump to the branch address
                        currentTokenIndex = int(currentDefinition[uint(currentTokenIndex)]);
                        currentTokenIndexModified = true;
                    }
                    case 41://"do"
                    {
                        int start = pop();
                        int end   = pop();
                        doParameters.Append(end);
                        doParameters.Append(start);
                        doParameters.Append(start); // Initialize the current index
                    }
                    case 42: // "loop"
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
                            currentTokenIndex = int(currentDefinition[uint(currentTokenIndex)]);
                            currentTokenIndexModified = true;
                        }
                    }
                    case 43: // "i"
                    {
                        int currentIndex = doParameters[doParameters.Count-1];
                        push(currentIndex);
                    }
                    case 44: // "exit"
                    {
                        currentTokenIndex = int(currentDefinition.Count); // Set the token index to the end to exit the current word
                        currentTokenIndexModified = true;
                    }
                    
                    case 45: // "seconds"
                    {
                        uint s = Time.Seconds;
                        int seconds = Int.FromBytes(s.GetByte(0), s.GetByte(1));
                        push(seconds);
                    }
                    case 46: // "delay"
                    {
                        int duration = pop();
                        Time.Delay(uint(duration));
                    }
                    case 47: // "pin"
                    {
                        int mode = pop();
                        int pin = pop();
                        MCU.PinMode(byte(pin), PinModeOption(mode));
                    }
                    case 48: // "in"
                    {
                        int pin = pop();
                        push(MCU.DigitalRead(byte(pin)) ? -1 : int(0));
                    }
                    case 49: // "out"
                    {
                        value = pop();
                        int pin = pop();
                        MCU.DigitalWrite(byte(pin), value == -1);
                    }
                    case 50: // "sp"
                    {
                        push(sp);    
                    }
                    
                    default:
                    {
                        Die(0x0A);
                    }
                }
            }
            case int:
            { 
                value = int(currentDefinition[uint(currentTokenIndex)]);
                push(value);
            }
            case string: 
            {
                 token = currentDefinition[uint(currentTokenIndex)];
                 switch (token)
                 {
                     case ":":
                     {
                         definingWord = true;
                         currentWordDefinition.Clear();
                         currentWordName = ""; // Reset the word name
                     }
                     case "<repl>":
                     {
                         Word word = wordList[wordList.Count-1];
                         executeDefinition(word.Definition);
                     }
                     default:
                     {
                         IO.WriteLn("Unknown token: '" + token + "'");
                     }
                 }
            }
        }
        if (!currentTokenIndexModified)
        {
            currentTokenIndex++;
        }
    }
    
    executeToken(<variant> currentDefinition, ref int currentTokenIndex)
    {
        if (definingWord)
        {
            defineWord(currentDefinition, ref currentTokenIndex);
        }
        else
        {
            executeWord(currentDefinition, ref currentTokenIndex);
        }
    }
    
    // Process input and split into tokens ( input -- )
    processInput(string input)
    {
        uint start = 0;
        bool isToken = false;
        bool inComment = false;
        bool inEOLComment = false;
        <variant> tokenBuffer;
        
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
                        int value;
                        if (Int.TryParse(token, ref value))
                        {
                            tokenBuffer.Append(value);
                        }
                        else
                        {
                            uint index;
                            bool builtIn;
                            string lowerToken = token.ToLower();
                            if (wordToUInt(lowerToken, ref index, ref builtIn))
                            {
                                if (builtIn)
                                {
                                    tokenBuffer.Append(byte(index));
                                }
                                else
                                {
                                    tokenBuffer.Append(uint(index));
                                }
                            }
                            else
                            {
                                tokenBuffer.Append(token); // case sensitive token (undefined word or ." argument for example)
                            }
                        }
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
                foreach (var vword in tokenBuffer)
                {
                    if (typeof(vword) == string)
                    {
                        string word = vword;
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
                        }
                    }
                    else if (typeof(vword) == byte)
                    {
                        byte builtIn = byte(vword);
                        switch (builtIn)
                        {
                            case 33: // "if"
                            case 36: // "begin"
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
    
    executeDefinition(<variant> exDefinition)
    {
        int count = int(exDefinition.Count);
        for (int currentTokenIndex=0; currentTokenIndex < count; )
        {
            executeToken(exDefinition, ref currentTokenIndex);
        }
    }
    
    initializeBuiltIns()
    {
        <variant> definition;
        definition.Append(spaceToken); // special token to imply 'built-in'
        Word word;
        word.Definition = definition;
        
        <string> builtIns = (": . .\" .s words + - * / mod abs and or xor invert = < dup drop swap over rot -rot pick ! @ c! c@ emit cr key key? bye if else then begin until again 0branch branch do loop i exit seconds delay pin in out sp ").Split(' ');
        foreach (var name in builtIns)
        {
            word.Name = name;
            wordList.Append(word);
        }
    }
    initializeWord(string wordDefinition)
    {
        <variant> definition;
        <string> words = wordDefinition.Split(' ');
        foreach (var word in words)
        {
            definition.Append(word);
        }
        executeDefinition(definition);
        IO.Write(".");
    }
    // Initialization method to define common FORTH words
    initialize()
    {
        IO.Write("Initializing Dictionary: ");
        
        initializeBuiltIns();
        
        initializeWord(": > swap < ;");                         // `>`   ( n1 n2 -- flag )
        initializeWord(": nip swap drop ;");                    // `nip` ( n1 n2 -- n2 )
        initializeWord(": tuck dup -rot ;");                    // `tuck` ( n1 n2 -- n2 n1 n2 )
        initializeWord(": 2dup over over ;");                   // `2dup` ( n1 n2 -- n1 n2 n1 n2 )
        initializeWord(": 2drop drop drop ;");                  // `2drop` ( n1 n2 -- )
        initializeWord(": 2swap 2 pick 2 pick rot rot ;");      // `2swap` ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
        initializeWord(": 2over 3 pick 3 pick ;");              // `2over` ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
        initializeWord(": 0= 0 = ;");                           // `0=` ( n -- flag )
        initializeWord(": 0< 0 < ;");                           // `0<` ( n -- flag )
        initializeWord(": 0> 0 > ;");                           // `0>` ( n -- flag )
        initializeWord(": <> = 0= ;");                          //  `<>` ( n1 n2 -- flag )
        initializeWord(": <= > 0= ;");                          // `<=` ( n1 n2 -- flag )
        initializeWord(": >= < 0= ;");                          // `>=` ( n1 n2 -- flag )
        initializeWord(": max 2dup > if drop else nip then ;"); // `max` ( n1 n2 -- max )
        initializeWord(": min 2dup < if drop else nip then ;"); // `min` ( n1 n2 -- min )
        initializeWord(": depth sp ; ");                        // `depth` ( -- n )
        initializeWord(": negate 0 swap - ;");                  // `negate` ( n -- -n )
        initializeWord(": 1+ 1 + ;");                           // `1+` ( n -- n+1 )
        initializeWord(": 1- 1 - ;");                           // `1-` ( n -- n-1 )
        initializeWord(": 2+ 2 + ;");                           // `2+` ( n -- n+2 )
        initializeWord(": 2- 2 - ;");                           // `2-` ( n -- n-2 )
        initializeWord(": 2* 2 * ;");                           // `2*` ( n -- n*2 )
        initializeWord(": 2/ 2 / ;");                           // `2/` ( n -- n/2 )
        initializeWord(": led " + (Board.BuiltInLED).ToString() + " ;"); // `led` ( n -- )
        initializeWord(": output 1 pin ;");                     // `output` ( pin -- )
        initializeWord(": input 0 pin ;");                      // `input` ( pin -- )
        
        IO.WriteLn();
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
