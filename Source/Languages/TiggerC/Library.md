# Tigger C System/Library Functions

This document describes the available system/library functions in Tigger C, including their arguments and return values.

### Serial Communication

#### `writeChar`

**Description**: Writes a single character to the serial output.

**Prototype**: 
```c
func writeChar(char c);
```

**Arguments**:
- `c`: Character to be written.

**Return Value**: None.

#### `writeString`

**Description**: Writes a null-terminated string to the serial output.

**Prototype**: 
```c
func writeString(const char[] str);
```

**Arguments**:
- `str`: Null-terminated string to be written.

**Return Value**: None.

#### `writeWord`

**Description**: Writes a 16-bit word as a string to the serial output.

**Prototype**: 
```c
func writeWord(word num);
```

**Arguments**:
- `num`: 16-bit word to be written.

**Return Value**: None.

#### `writeInt`

**Description**: Writes a 16-bit signed integer as a string to the serial output.

**Prototype**: 
```c
func writeInt(int num);
```

**Arguments**:
- `num`: 16-bit signed integer to be written.

**Return Value**: None.

#### `writeHex`

**Description**: Writes a byte as two hexadecimal characters to the serial output.

**Prototype**: 
```c
func writeHex(byte value);
```

**Arguments**:
- `value`: Byte to be written as hexadecimal.

**Return Value**: None.

### EEPROM

#### `writePage`

**Description**: Writes a page of data to serial EEPROM.

**Prototype**: 
```c
func writePage(word address, const byte[] data);
```

**Arguments**:
- `address`: EEPROM address to write to.
- `data`: Data to be written.

**Return Value**: None.

#### `readPage`

**Description**: Reads a page of data from serial EEPROM.

**Prototype**: 
```c
func readPage(word address, byte[] buffer);
```

**Arguments**:
- `address`: EEPROM address to read from.
- `buffer`: Buffer to store the read data.

**Return Value**: None.

### GPIO (Pin) Control

#### `pinSet`

**Description**: Sets the value of a pin.

**Prototype**: 
```c
func pinSet(byte pin, bool value);
```

**Arguments**:
- `pin`: Pin number to set.
- `value`: Value to set (true for high, false for low).

**Return Value**: None.

#### `pinRead`

**Description**: Reads the value of a pin.

**Prototype**: 
```c
func bool pinRead(byte pin);
```

**Arguments**:
- `pin`: Pin number to read.

**Return Value**: Boolean value of the pin (true for high, false for low).

#### `pinMode`

**Description**: Sets the mode of a pin.

**Prototype**: 
```c
func pinMode(byte pin, byte mode);
```

**Arguments**:
- `pin`: Pin number to set the mode for.
- `mode`: Mode to set (INPUT, OUTPUT, INPUT_PULLUP).

**Return Value**: None.

### I2C

#### `I2CWrite`

**Description**: Writes data to an I2C device.

**Prototype**: 
```c
func I2CWrite(byte address, byte data);
```

**Arguments**:
- `address`: I2C address of the device.
- `data`: Data to be written.

**Return Value**: None.

#### `I2CRead`

**Description**: Reads data from an I2C device.

**Prototype**: 
```c
func byte I2CRead(byte address);
```

**Arguments**:
- `address`: I2C address of the device.

**Return Value**: Data read from the device.

### Timing

#### `millis`

**Description**: Gets the current system tick count.

**Prototype**: 
```c
func byte[] millis();
```

**Arguments**: None.

**Return Value**: Byte array containing the current tick count.

#### `elapsedMillis`

**Description**: Calculates the elapsed milliseconds since the start.

**Prototype**: 
```c
func word elapsedMillis(byte[] start);
```

**Arguments**:
- `start`: Byte array containing the start tick count.

**Return Value**: Elapsed milliseconds since the start.

#### `elapsedSeconds`

**Description**: Calculates the elapsed seconds since the start.

**Prototype**: 
```c
func word elapsedSeconds(byte[] start);
```

**Arguments**:
- `start`: Byte array containing the start tick count.

**Return Value**: Elapsed seconds since the start.

#### `delay`

**Description**: Delays execution for a specified number of milliseconds.

**Prototype**: 
```c
func delay(word milliseconds);
```

**Arguments**:
- `milliseconds`: Number of milliseconds to delay.

**Return Value**: None.

### Memory Management

#### `malloc`

**Description**: Allocates memory and returns a pointer to the allocated memory.

**Prototype**: 
```c
func byte[] malloc(word size);
```

**Arguments**:
- `size`: Number of bytes to allocate.

**Return Value**: Pointer to the allocated memory.

#### `free`

**Description**: Frees previously allocated memory.

**Prototype**: 
```c
func free(byte[] ptr);
```

**Arguments**:
- `ptr`: Pointer to the memory to be freed.

**Return Value**: None.

### String Functions

#### `strlen`

**Description**: Calculates the length of a null-terminated string.

**Prototype**:
```c
func word strlen(const char[] str);
```

**Arguments**:
- `str`: Null-terminated string.

**Return Value**: Length of the string.

#### `strcpy`

**Description**: Copies a null-terminated string from source to destination.

**Prototype**:
```c
func char[] strcpy(char[] dest, const char[] src);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.

**Return Value**: Destination array.

#### `strncpy`

**Description**: Copies up to `n` characters from source to destination.

**Prototype**:
```c
func char[] strncpy(char[] dest, const char[] src, word n);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.
- `n`: Maximum number of characters to copy.

**Return Value**: Destination array.

#### `strcat`

**Description**: Concatenates source string to the destination string.

**Prototype**:
```c
func char[] strcat(char[] dest, const char[] src);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.

**Return Value**: Destination array.

#### `strncat`

**Description**: Concatenates up to `n` characters from source string to the destination string.

**Prototype**:
```c
func char[] strncat(char[] dest, const char[] src, word n);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.
- `n`: Maximum number of characters to concatenate.

**Return Value**: Destination array.

#### `strcmp`

**Description**: Compares two strings lexicographically.

**Prototype**:
```c
func int strcmp(const char[] str1, const char[] str2);
```

**Arguments**:
- `str1`: First string.
- `str2`: Second string.

**Return Value**: Integer indicating the result of the comparison.

#### `strncmp`

**Description**: Compares up to `n` characters of two strings lexicographically.

**Prototype**:
```c
func int strncmp(const char[] str1, const char[] str2, word n);
```

#### `stricmp`

**Description**: Compares two strings lexicographically in a case-insensitive manner.

**Prototype**:
```c
func int stricmp(const char[] str1, const char[] str2);
```

**Arguments**:
- `str1`: First string.
- `str2`: Second string.

**Return Value**: Integer indicating the result of the comparison.

#### `strnicmp`

**Description**: Compares up to `n` characters of two strings lexicographically in a case-insensitive manner.

**Prototype**:
```c
func int strnicmp(const char[] str1, const char[] str2, word n);
```

**Arguments**:
- `str1`: First string.
- `str2`: Second string.
- `n`: Maximum number of characters to compare.

**Return Value**: Integer indicating the result of the comparison.

**Arguments**:
- `str1`: First string.
- `str2`: Second string.
- `n`: Maximum number of characters to compare.

**Return Value**: Integer indicating the result of the comparison.

#### `strchr`

**Description**: Finds the first occurrence of a character in a string.

**Prototype**:
```c
func char[] strchr(const char[] str, char c);
```

**Arguments**:
- `str`: String to search.
- `c`: Character to find.

**Return Value**: Pointer to the first occurrence of the character or null if not found.

#### `strrchr`

**Description**: Finds the last occurrence of a character in a string.

**Prototype**:
```c
func char[] strrchr(const char[] str, char c);
```

**Arguments**:
- `str`: String to search.
- `c`: Character to find.

**Return Value**: Pointer to the last occurrence of the character or null if not found.

#### `strstr`

**Description**: Finds the first occurrence of a substring in a string.

**Prototype**:
```c
func char[] strstr(const char[] haystack, const char[] needle);
```

**Arguments**:
- `haystack`: String to search in.
- `needle`: Substring to find.

**Return Value**: Pointer to the first occurrence of the substring or null if not found.

#### `strdup`

**Description**: Duplicates a string.

**Prototype**:
```c
func char[] strdup(const char[] str);
```

**Arguments**:
- `str`: String to duplicate.

**Return Value**: Pointer to the duplicated string.

#### `strtok`

**Description**: Tokenizes a string.

**Prototype**:
```c
func char[] strtok(char[] str, const char[] delimiters);
```

**Arguments**:
- `str`: String to tokenize.
- `delimiters`: Delimiters to use for tokenizing.

**Return Value**: Pointer to the next token or null if no more tokens.

### Character Functions

#### `isdigit`

**Description**: Checks if a character is a digit.

**Prototype**:
```c
func bool isdigit(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is a digit.

#### `isalpha`

**Description**: Checks if a character is alphabetic.

**Prototype**:
```c
func bool isalpha(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is alphabet

ic.

#### `isalnum`

**Description**: Checks if a character is alphanumeric.

**Prototype**:
```c
func bool isalnum(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is alphanumeric.

#### `isspace`

**Description**: Checks if a character is whitespace.

**Prototype**:
```c
func bool isspace(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is whitespace.

#### `toupper`

**Description**: Converts a character to uppercase.

**Prototype**:
```c
func char toupper(char c);
```

**Arguments**:
- `c`: Character to convert.

**Return Value**: Uppercase character.

#### `tolower`

**Description**: Converts a character to lowercase.

**Prototype**:
```c
func char tolower(char c);
```

**Arguments**:
- `c`: Character to convert.

**Return Value**: Lowercase character.


### File System Functions

#### fopen

**Description**: Opens a file or directory.

**Prototype**:
```c
func byte[] fopen(const char[] filename, const char[] mode);
```

**Arguments**:
- `filename`: Name of the file or directory to open.
- `mode`: Mode in which to open the file (e.g., "r" for read, "w" for write, etc.).

**Return Value**: A file handle if successful, or null if an error occurs.

#### fclose

**Description**: Closes an open file or directory.

**Prototype**:
```c
func int fclose(byte[] fileHandle);
```

**Arguments**:
- `fileHandle`: The handle of the file or directory to close.

**Return Value**: 0 on success, or -1 on error.

#### fread

**Description**: Reads data from an open file.

**Prototype**:
```c
func word fread(byte[] buffer, word size, word count, byte[] fileHandle);
```

**Arguments**:
- `buffer`: Buffer to store the read data.
- `size`: Size of each element to read.
- `count`: Number of elements to read.
- `fileHandle`: The handle of the file to read from.

**Return Value**: Number of elements successfully read.

#### fwrite

**Description**: Writes data to an open file.

**Prototype**:
```c
func word fwrite(const byte[] buffer, word size, word count, byte[] fileHandle);
```

**Arguments**:
- `buffer`: Buffer containing the data to write.
- `size`: Size of each element to write.
- `count`: Number of elements to write.
- `fileHandle`: The handle of the file to write to.

**Return Value**: Number of elements successfully written.

#### fseek

**Description**: Sets the file position indicator for the file.

**Prototype**:
```c
func int fseek(byte[] fileHandle, int offset, byte whence);
```

**Arguments**:
- `fileHandle`: The handle of the file.
- `offset`: Number of bytes to offset from `whence`.
- `whence`: Position from where the offset is applied (0: beginning, 1: current position, 2: end of file).

**Return Value**: 0 on success, or -1 on error.

#### ftell

**Description**: Returns the current file position indicator for the file.

**Prototype**:
```c
func word ftell(byte[] fileHandle);
```

**Arguments**:
- `fileHandle`: The handle of the file.

**Return Value**: Current file position as a word, or 0 on error.

#### remove

**Description**: Removes a file.

**Prototype**:
```c
func int remove(const char[] filename);
```

**Arguments**:
- `filename`: Name of the file to remove.

**Return Value**: 0 on success, or -1 on error.


#### mkdir

**Description**: Creates a new directory.

**Prototype**:
```c
func int mkdir(const char[] dirname);
```

**Arguments**:
- `dirname`: Name of the directory to create.

**Return Value**: 0 on success, or -1 on error.

#### rmdir

**Description**: Removes a directory.

**Prototype**:
```c
func int rmdir(const char[] dirname);
```

**Arguments**:
- `dirname`: Name of the directory to remove.

**Return Value**: 0 on success, or -1 on error.

#### opendir

**Description**: Opens a directory for reading.

**Prototype**:
```c
func byte[] opendir(const char[] dirname);
```

**Arguments**:
- `dirname`: Name of the directory to open.

**Return Value**: Directory handle if successful, or null if an error occurs.

#### readdir

**Description**: Reads the next entry in an open directory.

**Prototype**:
```c
func byte[] readdir(byte[] dirHandle);
```

**Arguments**:
- `dirHandle`: The handle of the directory to read from.

**Return Value**: The next directory entry, or null if no more entries are available.

#### closedir

**Description**: Closes an open directory.

**Prototype**:
```c
func int closedir(byte[] dirHandle);
```

**Arguments**:
- `dirHandle`: The handle of the directory to close.

**Return Value**: 0 on success, or -1 on error.

#### chdir

**Description**: Changes the current working directory.

**Prototype**:
```c
func int chdir(const char[] path);
```

**Arguments**:
- `path`: Path to the new working directory.

**Return Value**: 0 on success, or -1 on error.
