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

---

## Byte Unit

This unit provides methods for working with 8-bit unsigned integers (`byte`), which are value types in Hopper.

### Methods

#### `string ToString(byte this)`

Converts the `byte` value to its equivalent string representation.

#### `string ToHexString(byte this, byte digits)`

Converts the `byte` value to its equivalent hexadecimal string representation, with the specified number of `digits`.

#### `string ToBinaryString(byte this)`

Converts the `byte` value to its equivalent binary string representation.

#### `char ToHex(byte this)`

Converts the `byte` value to its equivalent hexadecimal character.

#### `char ToDigit(byte this)`

Converts the `byte` value to its equivalent ASCII digit character.

### Examples

```hopper
byte value = 0x2A;
string hexString = Byte.ToHexString(value, 2);
Screen.PrintLn("Hexadecimal representation: " + hexString);

string binaryString = Byte.ToBinaryString(value);
Screen.PrintLn("Binary representation: " + binaryString);

char hexChar = Byte.ToHex(value);
Screen.PrintLn("Hexadecimal character: " + hexChar);

char digitChar = Byte.ToDigit(value);
Screen.PrintLn("ASCII digit character: " + digitChar);
```

---

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

---

## UInt Unit

This unit provides methods for working with 16-bit unsigned integers ('uint'), which are value types in Hopper.

### `bool TryParse(string content, ref uint returnValue)`
Attempts to parse the specified string content as an unsigned integer and stores the result in the `returnValue` parameter. Returns true if the parsing was successful, otherwise false.

### `string ToString(uint this)`
Converts the unsigned integer to its string representation.

### `ToString(uint this, ref string result)`
Converts the unsigned integer to its string representation and stores the result in the specified `result` string.

### `long ToLong(uint this) system`
Converts the unsigned integer to a long value.

### `float ToFloat(uint this) system`
Converts the unsigned integer to a float value.

### `string ToHexString(uint this, byte digits)`
Converts the unsigned integer to a hexadecimal string representation with the specified number of digits.

### `string ToBinaryString(uint this)`
Converts the unsigned integer to a binary string representation.

### `<byte> ToBytes(uint this)`
Converts the unsigned integer to a list of bytes.

### `byte GetByte(uint this, byte index)`
Returns the byte at the specified index in the unsigned integer.

### `uint FromBytes(byte b0, byte b1)`
Constructs an unsigned integer from two bytes.

### `uint Min(uint a, uint b)`
Returns the minimum of two unsigned integers.

### `uint Max(uint a, uint b)`
Returns the maximum of two unsigned integers.

### `Swap(ref uint a, ref uint b)`
Swaps the values of two unsigned integers.

### Examples

#### Using `TryParse`
```hopper
string input = "123";
uint value;
bool success = UInt.TryParse(input, ref value);
if (success)
{
    PrintLn("Parsed value: " + value);
}
else
{
    PrintLn("Failed to parse");
}
```

#### Converting to Hexadecimal
```hopper
uint number = 255;
string hex = UInt.ToHexString(number, 2); // hex will be "FF"
PrintLn(hex);
```

#### Swapping Values
```hopper
uint a = 10;
uint b = 20;
PrintLn("Before swap: a=" + a + ", b=" + b);
UInt.Swap(ref a, ref b);
PrintLn("After swap: a=" + a + ", b=" + b);
```

---

## Int Unit

This unit provides methods for working with 16-bit signed integers (`int`), which are value types in Hopper.

### Methods

#### `bool TryParse(string content, ref int returnValue)`

Attempts to parse the specified string `content` as an `int` and stores the result in `returnValue`. Returns `true` if the parse operation was successful, otherwise `false`.

#### `string ToString(int this)`

Converts the `int` value to its equivalent string representation.

#### `float ToFloat(int this)`

Converts the `int` value to its equivalent `float` representation.

#### `long ToLong(int this)`

Converts the `int` value to its equivalent `long` representation.

#### `string ToHexString(int this, byte digits)`

Converts the `int` value to its equivalent hexadecimal string representation, with the specified number of `digits`.

#### `byte[] ToBytes(int this)`

Converts the `int` value to an array of bytes.

#### `byte GetByte(int this, byte index)`

Returns the byte at the specified `index` in the `int` value.

#### `int FromBytes(byte b0, byte b1)`

Combines two bytes into an `int` value.

#### `int Abs(int value)`

Returns the absolute value of the specified `int` value.

#### `int Min(int a, int b)`

Returns the smaller of two `int` values.

#### `int Max(int a, int b)`

Returns the larger of two `int` values.

#### `void Swap(ref int a, ref int b)`

Swaps the values of two `int` variables.

### Examples

```hopper
int number = -123;
string numberString = Int.ToString(number);
Screen.PrintLn("Number: " + numberString);

int absValue = Int.Abs(number);
Screen.PrintLn("Absolute value: " + absValue.ToString());

int min = Int.Min(10, 5);
Screen.PrintLn("Min: " + min.ToString());

int max = Int.Max(10, 5);
Screen.PrintLn("Max: " + max.ToString());
```

---

## Long Unit

This unit provides methods for working with signed 32-bit integers (`long`), which are reference types in Hopper.

### Methods

#### `bool TryParse(string content, ref long returnValue)`

Attempts to parse the specified string `content` as a `long` and stores the result in `returnValue`. Returns `true` if the parse operation was successful, otherwise `false`.

#### `string ToString(long this)`

Converts the `long` value to its equivalent string representation.

#### `string ToHexString(long this, byte digits)`

Converts the `long` value to its equivalent hexadecimal string representation with the specified number of `digits`.

#### `string ToBinaryString(long this, byte digits)`

Converts the `long` value to its equivalent binary string representation with the specified number of `digits`.

#### `long Abs(long value)`

Returns the absolute value of the specified `long` value.

#### `long Min(long a, long b)`

Returns the smaller of two `long` values.

#### `long Max(long a, long b)`

Returns the larger of two `long` values.

### Examples

```hopper
long number = -123;
string numberString = number.ToString();
Screen.PrintLn("Number: " + numberString);

string hexString = number.ToHexString(4);
Screen.PrintLn("Hexadecimal: " + hexString);

string binaryString = number.ToBinaryString(8);
Screen.PrintLn("Binary: " + binaryString);

long absoluteValue = Long.Abs(number);
Screen.PrintLn("Absolute value: " + absoluteValue.ToString());
```

---

## Float Unit

This unit provides methods for working with 32-bit floating-point numbers (`float`), which are reference types in Hopper.

### Methods

#### `bool TryParse(string content, ref float returnValue)`

Attempts to parse the specified string `content` as a `float` and stores the result in `returnValue`. Returns `true` if the parse operation was successful, otherwise `false`.

#### `string ToString(float this)`

Converts the `float` value to its equivalent string representation.

#### `float Sin(float angle)`

Returns the sine of the specified angle in radians.

#### `float Cos(float angle)`

Returns the cosine of the specified angle in radians.

#### `float ATan2(float y, float x)`

Returns the angle whose tangent is the quotient of two specified numbers.

#### `float Sqrt(float value)`

Returns the square root of a specified number.

#### `float Radians(float angle)`

Converts the specified angle from degrees to radians.

#### `float Degrees(float angle)`

Converts the specified angle from radians to degrees.

#### `float Abs(float value)`

Returns the absolute value of the specified `float` value.

#### `float Min(float a, float b)`

Returns the smaller of two `float` values.

#### `float Max(float a, float b)`

Returns the larger of two `float` values.

### Constants

#### `float Pi`

A constant representing the mathematical value of pi (π).

### Examples

```hopper
float angle = 45.0;
float radians = Float.Radians(angle);
Screen.PrintLn("Radians: " + radians.ToString());

float sinValue = Float.Sin(radians);
Screen.PrintLn("Sine: " + sinValue.ToString());

float sqrtValue = Float.Sqrt(25.0);
Screen.PrintLn("Square root: " + sqrtValue.ToString());
```

---

## Directory Unit

The Directory unit in Hopper provides functionality for working with directories, similar to the .NET framework. Directories in Hopper are represented as reference types.

### Methods

#### `bool Exists(string path)`

Checks if the directory at the specified `path` exists.

#### `Delete(string path)`

Deletes the directory at the specified `path`.

#### `bool IsValid(directory this)`

Checks if the given `directory` object is valid.

#### `directory Open(string fullpath)`

Opens the directory at the specified `fullpath`.

#### `Create(string path)`

Creates a new directory at the specified `path`.

#### `uint GetDirectoryCount(directory this)`

Gets the count of directories inside the specified `directory`.

#### `uint GetDirectoryCount(directory this, ref uint skipped)`

Gets the count of directories inside the specified `directory`, skipping directories based on the `skipped` parameter.

#### `uint GetFileCount(directory this)`

Gets the count of files inside the specified `directory`.

#### `uint GetFileCount(directory this, ref uint skipped)`

Gets the count of files inside the specified `directory`, skipping files based on the `skipped` parameter.

#### `string GetDirectory(directory this, uint index)`

Gets the name of the directory at the specified `index` inside the given `directory`.

#### `string GetFile(directory this, uint index)`

Gets the name of the file at the specified `index` inside the given `directory`.

#### `string GetTime(string path)`

Gets the time of the directory or file at the specified `path`.

#### `string GetDate(string path)`

Gets the date of the directory or file at the specified `path`.

### Example Usage

```hopper
// Check if a directory exists
if (Directory.Exists("/path/to/directory"))
{
    Screen.PrintLn("Directory exists!");
}

// Create a new directory
Directory.Create("/path/to/new/directory");

// Open a directory and get its content count
directory dir = Directory.Open("/path/to/directory");
uint fileCount = dir.GetFileCount();
uint dirCount = dir.GetDirectoryCount();

Screen.PrintLn("Files count: " + fileCount);
Screen.PrintLn("Directories count: " + dirCount);

// Get the name of a file at index 0
string fileName = dir.GetFile(0);
Screen.PrintLn("First file: " + fileName);
```
---

## File Unit

This unit provides methods for working with files, similar to the behavior in .NET. File is a reference type in Hopper.

### Methods

#### `bool Exists(string path)`

Checks if a file exists at the specified `path`.

#### `Delete(string path)`

Deletes the file at the specified `path`.

#### `long GetSize(string path)`

Returns the size of the file at the specified `path`.

#### `string GetTime(string path)`

Returns the time when the file at the specified `path` was last modified.

#### `string GetDate(string path)`

Returns the date when the file at the specified `path` was last modified.

#### `long GetTimeStamp(string path)`

Returns the timestamp of the file at the specified `path`.

#### `bool IsValid(file this)`

Checks if the file object is valid.

#### `file Open(string fullpath)`

Opens the file at the specified `fullpath` for reading and returns a file object.

#### `string ReadLine(file this)`

Reads a line from the file object.

#### `byte Read(file this)`

Reads a byte from the file object.

#### `byte Read(file this, long seekPosition)`

Reads a byte from the file object at the specified `seekPosition`.

#### `uint Read(file this, byte[] data, uint bufferSize)`

Reads up to `bufferSize` bytes from the file object into the `data` array and returns the number of bytes read.

#### `file Create(string fullpath)`

Creates a new file at the specified `fullpath` for writing and returns a file object.

#### `Append(file this, byte content)`

Appends a byte to the file object.

#### `Append(file this, string content)`

Appends a string to the file object.

#### `Flush(file this)`

Flushes the contents of the file object to disk.

#### `bool Exists(ref string filePath, ref string extension, string searchFolder)`

Checks if a file exists at the specified `filePath` and searches for it in the `searchFolder` if necessary. Updates `filePath` with the correct path if found.

#### `bool TryReadAllText(string path, ref string content)`

Attempts to read all text from the file at the specified `path` into the `content` variable.

#### `uint CRC16(string filepath)`

Calculates and returns the CRC16 checksum of the file at the specified `filepath`.

#### `bool Copy(string sourcePath, string destinationPath)`

Copies the file from `sourcePath` to `destinationPath`, overwriting the destination file if it already exists.

#### `bool Copy(string sourcePath, string destinationPath, bool overwrite)`

Copies the file from `sourcePath` to `destinationPath`, optionally overwriting the destination file if it already exists, based on the value of `overwrite`.

### Examples

```hopper
string filePath = "example.txt";
if (File.Exists(filePath)) {
    File.Delete(filePath);
}

file fileObj = File.Create(filePath);
if (fileObj.IsValid()) {
    fileObj.Append("Hello, world!");
    fileObj.Flush();
}

string content;
if (File.TryReadAllText(filePath, ref content)) {
    Screen.PrintLn("File content: " + content);
}

string copyPath = "copy.txt";
if (File.Copy(filePath, copyPath, true)) {
    Screen.PrintLn("File copied successfully.");
}
```

---

## Path Unit

This unit provides methods for working with file paths, with behavior similar to .NET. 
Please note that:
- spaces are illegal in paths in Hopper (only letters and digits are allowed, along with a single `.` for the extension delimiter)
- forward slashes are used as path separators
- paths are case insensitive

### Methods

#### `bool IsValidPathCharacter(char c)`

Checks if the character `c` is a valid character for a path in Hopper (letters, digits, `/`, `.`).

#### `string GetFileName(string path)`

Returns the file name from the specified `path`.

#### `string GetDirectoryName(string fullPath)`

Returns the directory name from the specified `fullPath`.

#### `string Combine(string partOne, string partTwo)`

Combines two path parts (`partOne` and `partTwo`) into a single path, adding a `/` separator between them if necessary.

#### `string GetExtension(string path)`

Returns the extension of the file from the specified `path`.

#### `string GetFullPath(string path)`

Returns the full path for a valid file, including the correct case.

#### `string GetCorrectCase(string path)`

Returns the correct case (including path) of the full path.

#### `string MakeOptions(string filePath)`

Creates a path for an options file based on the given `filePath`.

### Examples

```hopper
string path = "/Documents/Example.txt";
string fileName = Path.GetFileName(path); // fileName = "Example.txt"

string fullPath = Path.GetFullPath(path); // fullPath = "/Documents/Example.txt"

string extension = Path.GetExtension(path); // extension = ".txt"

string combinedPath = Path.Combine("/Documents", "Example.txt"); // combinedPath = "/Documents/Example.txt"
```

---