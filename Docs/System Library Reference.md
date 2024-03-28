**Note:**  - this is a work in progress between ChatGPT and me. It will improve as ChatGPT's knowledge of the language improves.

# Hopper String and Char Units Reference

Welcome to the reference documentation for the String and Char units in the Hopper programming language. This documentation provides an overview of the methods available in
these units, along with usage examples and explanations.

### Learning and Writing

This documentation was created by ChatGPT, who learned Hopper by studying examples and then writing code for the methods of the String and Char units.
By following examples and experimenting with the language, ChatGPT gained a deep understanding of Hopper's syntax and capabilities. And, as usual,
at least when ChatGPT is wrong, it is wrong *with confidence*.

### Future Expansion

In future updates, we plan to expand this documentation to include additional units and methods. Stay tuned for more updates and enhancements
to the Hopper reference documentation!

---

Feel free to adjust or expand this introduction as needed for your documentation!


## Char Unit

The Char unit provides a set of methods for working with characters in the Hopper programming language.

### Properties

- `char Value { get }`: Returns the value of the character.

### Methods

- `bool IsDigit(char this)`: Returns true if the character is a digit, otherwise false.

- `bool IsLetter(char this)`: Returns true if the character is a letter, otherwise false.

- `bool IsLetterOrDigit(char this)`: Returns true if the character is a letter or a digit, otherwise false.

- `bool IsLower(char this)`: Returns true if the character is a lowercase letter, otherwise false.

- `bool IsUpper(char this)`: Returns true if the character is an uppercase letter, otherwise false.

- `bool IsWhiteSpace(char this)`: Returns true if the character is a whitespace character, otherwise false.

### Usage

```hopper
uses "/Source/System/Char"

char ch = 'A';
char ch2 = '1';

bool isDigit = Char.IsDigit(ch); // false
bool isLetter = Char.IsLetter(ch); // true
bool isLetterOrDigit = Char.IsLetterOrDigit(ch); // true
bool isLower = Char.IsLower(ch); // false
bool isUpper = Char.IsUpper(ch); // true
bool isWhiteSpace = Char.IsWhiteSpace(ch); // false

bool isDigit2 = Char.IsDigit(ch2); // true
bool isLetter2 = Char.IsLetter(ch2); // false
bool isLetterOrDigit2 = Char.IsLetterOrDigit(ch2); // true
bool isLower2 = Char.IsLower(ch2); // false
bool isUpper2 = Char.IsUpper(ch2); // false
bool isWhiteSpace2 = Char.IsWhiteSpace(ch2); // false
```

## String Unit

The String unit provides a set of methods for working with strings in the Hopper programming language.

### Properties

- `uint Length { get }`: Returns the length of the string.

- `bool IsEmpty { get }`: Returns true if the string is empty, otherwise false.

### Methods

- `char GetChar(string this, uint index)`: Returns the character at the specified index in the string.

- `bool Contains(string this, char needle)`: Returns true if the string contains the specified character, otherwise false.

- `bool Contains(string this, string needle)`: Returns true if the string contains the specified substring, otherwise false.

- `bool StartsWith(string this, char pattern)`: Returns true if the string starts with the specified character, otherwise false.

- `bool StartsWith(string this, string pattern)`: Returns true if the string starts with the specified substring, otherwise false.

- `bool EndsWith(string this, char pattern)`: Returns true if the string ends with the specified character, otherwise false.

- `bool EndsWith(string this, string pattern)`: Returns true if the string ends with the specified substring, otherwise false.

- `bool IndexOf(string this, char pattern, ref uint index)`: Finds the index of the first occurrence of the specified character in the string.

- `bool IndexOf(string this, char pattern, uint searchIndex, ref uint index)`: Finds the index of the first occurrence of the specified character in the string starting from the specified index.

- `bool IndexOf(string this, string pattern, ref uint index)`: Finds the index of the first occurrence of the specified substring in the string.

- `bool IndexOf(string this, string pattern, uint startIndex, ref uint index)`: Finds the index of the first occurrence of the specified substring in the string starting from the specified index.

- `bool LastIndexOf(string this, char pattern, ref uint index)`: Finds the index of the last occurrence of the specified character in the string.

- `bool LastIndexOf(string this, char pattern, uint startIndex, ref uint index)`: Finds the index of the last occurrence of the specified character in the string starting from the specified index.

- `string Append(string this, char append)`: Appends the specified character to the end of the string and returns the result.

- `string Append(string this, string append)`: Appends the specified string to the end of the string and returns the result.

- `string InsertChar(string this, uint index, char append)`: Inserts the specified character at the specified index in the string and returns the result.

- `string Replace(string original, char pattern, char replace)`: Replaces all occurrences of the specified character in the string with the replacement character and returns the result.

- `string Replace(string original, string pattern, string replace)`: Replaces all occurrences of the specified substring in the string with the replacement substring and returns the result.

- `string Pad(string this, char append, uint width)`: Pads the string with the specified character on the right side to ensure it reaches the specified width and returns the result.

- `string LeftPad(string this, char append, uint width)`: Pads the string with the specified character on the left side to ensure it reaches the specified width and returns the result.

- `string Trim(string this)`: Removes leading and trailing whitespace characters from the string and returns the result.

- `string TrimLeft(string this)`: Removes leading whitespace characters from the string and returns the result.

- `string TrimRight(string this)`: Removes trailing whitespace characters from the string and returns the result.

- `string Substring(string this, uint start)`: Returns a substring starting from the specified index in the string.

- `string Substring(string this, uint start, uint length)`: Returns a substring starting from the specified index with the specified length in the string.

- `<string> Split(string this, string delimiters)`: Splits the string into substrings based on the specified delimiter characters and returns a list of the substrings.

- `int Compare(string left, string right)`: Compares two strings lexicographically and returns -1 if the left string is less than the right string, 0 if they are equal, and 1 if the left string is greater than the right string.

### Usage

```hopper
uses "/Source/System/String"

string str = "Hello, World!";
uint len = str.Length;
bool isEmpty = str.IsEmpty;
char ch = str.GetChar(0);
bool containsChar = str.Contains('H');
bool startsWithChar = str.StartsWith('H');
bool endsWithChar = str.EndsWith('d');
bool indexOfChar = str.IndexOf('o', ref index);
bool indexOfCharFromIndex = str.IndexOf('o', 5, ref index);
bool containsString = str.Contains("World");
bool startsWithString = str.StartsWith("Hello");
bool endsWithString = str.EndsWith("
```
