## Minimal Python for Hopper - Reference Document

### Introduction
This document outlines the subset of Python supported by the minimal Python interpreter implemented in Hopper. This interpreter is designed to run efficiently on resource-constrained devices like the 6502 microprocessor.

### Data Types
- **Integers**: Support for basic integer operations (addition, subtraction, multiplication, division).
- **Strings**: Support for string creation, concatenation, and basic manipulation.
- **Lists**: Dynamic arrays that can hold mixed data types. Support for list creation, indexing, and basic operations like append.
- **Booleans**: Boolean values `True` and `False`.

### Control Structures
- **Conditionals**:
  ```python
  if condition:
      # code block
  elif another_condition:
      # another code block
  else:
      # another code block
  ```
- **Loops**:
  - **For Loop**:
    ```python
    for item in iterable:
        # code block
    ```
  - **While Loop**:
    ```python
    while condition:
        # code block
    ```

### Functions
- **Function Definitions**:
  ```python
  def function_name(parameters):
      # code block
      return value
  ```
- **Lambda Functions**: Limited support for anonymous functions.
  ```python
  lambda parameters: expression
  ```

### Standard Library Subset
- **Math Module**: Basic mathematical functions and constants.
  ```python
  import math
  math.sqrt(x)
  math.pi
  ```
- **Sys Module**: Basic system functions.
  ```python
  import sys
  sys.exit()
  ```

### Basic I/O
- **Print**: Output to the console.
  ```python
  print("Hello, World!")
  ```

### Error Handling
- **Exceptions**: Basic support for try-except blocks.
  ```python
  try:
      # code block
  except Exception as e:
      # error handling code
  ```

### Object-Oriented Features
- **Classes and Objects**: Support for defining and using classes.
  ```python
  class MyClass:
      def __init__(self, value):
          self.value = value

      def method(self):
          return self.value

  obj = MyClass(10)
  print(obj.method())
  ```
- **Single Inheritance**: Basic support for single inheritance.

### Memory Management
- **Garbage Collection**: Automatic memory management to free up unused memory.

### Limitations
- **No Multi-threading**: The interpreter does not support multi-threading or asynchronous operations.
- **Limited Libraries**: Only a minimal subset of the standard library is available.
- **No Advanced Data Types**: Advanced data types like sets and dictionaries are not supported.

### Example Code
Here are some examples to illustrate the supported features:

#### Example 1: Basic Arithmetic
```python
a = 10
b = 20
c = a + b
print(c)  # Output: 30
```

#### Example 2: Conditional Statements
```python
x = 5
if x > 0:
    print("Positive")
elif x == 0:
    print("Zero")
else:
    print("Negative")
```

#### Example 3: Loops
```python
for i in range(5):
    print(i)

count = 0
while count < 5:
    print(count)
    count += 1
```

#### Example 4: Functions
```python
def add(a, b):
    return a + b

result = add(3, 4)
print(result)  # Output: 7
```

#### Example 5: Classes
```python
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def greet(self):
        return f"Hello, my name is {self.name} and I am {self.age} years old."

person = Person("Alice", 30)
print(person.greet())  # Output: Hello, my name is Alice and I am 30 years old.
```

---

This reference document provides an overview of the supported features in the minimal Python interpreter for Hopper. It aims to provide essential functionality while maintaining a small footprint suitable for resource-constrained environments.