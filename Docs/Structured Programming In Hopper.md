## **Book Title:**  
*Structured Programming with Hopper on Arduino-Style Microcontrollers*

---

## **Chapter 1: Introduction to Embedded Systems and Hopper**

### **Section 1.1: What is a Microcontroller?**
- Overview of embedded systems and microcontrollers.
- Introduction to Arduino-style microcontrollers and their applications.

### **Section 1.2: Overview of the Hopper Programming Language**
- Design philosophy and key features of Hopper.
- Setting up the development environment and writing your first Hopper program.

### **Section 1.3: Writing Your First Program – Blinking an LED**
- Structure of a Hopper program.
- Compiling and uploading code to a microcontroller.

---

## **Chapter 2: Simple Data Types and Control Structures**

### **Section 2.1: Variables, Data Types, and Operators**
- Hopper’s strict type system: `int`, `uint`, `bool`, `char`, etc.
- Declaring and initializing variables.
- Arithmetic, logical, and comparison operators.

### **Section 2.2: Control Structures – If, Switch, and Loops**
- Conditionals: `if`, `switch` statements.
- Loop structures: `loop`, `for`, `while`, and `foreach`.
- Using `break` and `continue` effectively.

### **Section 2.3: Functions and Procedures**
- Defining and calling functions in Hopper.
- Passing parameters and returning values.
- Modularizing code through functions.

---

## **Chapter 3: Value Types vs Reference Types**

### **Section 3.1: Understanding Value Types**
- Characteristics of value types: Stack allocation and value-copy behavior.
- Examples with `int`, `uint`, `bool`, and `char`.

### **Section 3.2: Understanding Reference Types**
- Characteristics of reference types: Heap allocation and reference behavior.
- Examples with arrays, lists, dictionaries, strings, floats, and longs.

### **Section 3.3: Using `ref` Arguments**
- Explanation of `ref` arguments and when to use them.
- Practical examples with both value and reference types.

---

## **Chapter 4: Breaking Down Complex Programs – Encapsulation and Units**

### **Section 4.1: Creating Units for Code Modularity**
- The importance of breaking down large programs into manageable units.
- Defining reusable units for encapsulating functionality.

### **Section 4.2: Public and Private Members**
- Declaring public and private members, methods, and properties.
- Controlling access to variables and methods within units.

### **Section 4.3: Encapsulation and Information Hiding**
- Best practices for encapsulation to protect data and control access.
- Structuring code to separate interface from implementation.

### **Section 4.4: Named Types – Records, Enums, and Flags**
- Defining and using named types like records, enums, and flags.
- Example project: Creating encapsulated units for sensor management, using private and public members to control access.

---

## **Chapter 5: Working with Arrays**

### **Section 5.1: Introduction to Arrays**
- Defining single-dimensional arrays in Hopper.
- Fixed-size arrays vs runtime-inferred size arrays.

### **Section 5.2: Accessing and Modifying Arrays**
- Accessing array elements and bounds checking.
- Iterating over arrays using loops.

### **Section 5.3: Practical Applications of Arrays**
- Example projects: Processing sensor data and storing values in arrays.

---

## **Chapter 6: Strings in Hopper**

### **Section 6.1: Declaring and Initializing Strings**
- Defining strings using `""` constants and hex array constants.
- Understanding the reference nature of strings.

### **Section 6.2: String Methods and `this` Argument**
- Common string helper methods like `Length()`, `Substring()`, `Concat()`, etc.
- Understanding the `this` argument and method chaining.

### **Section 6.3: Practical String Operations**
- String comparison, manipulation, and conversion to arrays.
- Example project: Building a simple text parser using strings.

---

## **Chapter 7: Lists and Dictionaries**

### **Section 7.1: Working with Lists**
- Defining and initializing lists in Hopper.
- Adding, removing, and accessing elements in a list.

### **Section 7.2: Iterating Over Lists**
- Iterating through lists using loops.
- Example project: Managing a dynamic list of connected devices.

### **Section 7.3: Working with Dictionaries**
- Defining key-value pairs in dictionaries.
- Adding, removing, and looking up values in dictionaries.

### **Section 7.4: Practical Use Cases for Lists and Dictionaries**
- Example project: Tracking sensor data using dictionaries for device IDs and values.

---

## **Chapter 8: Record Types, Enums, and Flags**

### **Section 8.1: Defining and Using Record Types**
- Creating record types to group related data.
- Accessing and modifying record fields.

### **Section 8.2: Using Enums**
- Declaring and using enumerations for defining named constants.
- Example project: Using enums to represent sensor states (e.g., `Active`, `Inactive`, `Error`).

### **Section 8.3: Using Flags**
- Declaring and using flags for representing bitwise options.
- Example project: Using flags to configure GPIO pin modes.

### **Section 8.4: Combining Record Types with Enums and Flags**
- Integrating enums and flags into record types for better structure and control.
- Example project: Organizing sensor configurations using records, enums, and flags.

---

## **Chapter 9: Floats and Longs as Reference Types**

### **Section 9.1: Introduction to Floats and Longs**
- Declaring and initializing floats and longs.
- Understanding the reference type nature of floats and longs.

### **Section 9.2: Arithmetic and Conversion with Floats and Longs**
- Performing calculations and converting between types.
- Practical considerations for precision and memory usage.

### **Section 9.3: Passing Floats and Longs as `ref` Arguments**
- Modifying float and long values using `ref`.
- Example project: Implementing a real-time system that adjusts calculations dynamically.

### **Section 9.4: Performance and Memory Considerations**
- Discussing memory overhead and performance optimization for floats and longs.
- Example project: Building an efficient data processing system using floating-point calculations.

---

## **Chapter 10: Working with Timers, Delays, and Real-Time Operations**

### **Section 10.1: Introduction to Timers and Delays**
- Using Hopper’s `Time` and `Delay` functions for real-time operations.
- Avoiding blocking code with non-blocking timing mechanisms.

### **Section 10.2: Creating Real-Time Systems**
- Writing code that responds to real-time events.
- Example project: Building a periodic sensor reading system using timers.

---

## **Chapter 11: Input and Output Operations**

### **Section 11.1: Digital Input and Output**
- Interfacing with GPIO for digital I/O operations.
- Example project: Controlling an LED using digital input.

### **Section 11.2: Analog Input and Output**
- Working with analog sensors and actuators.
- Example project: Reading temperature sensors and controlling analog devices.

### **Section 11.3: Serial Communication**
- Using Hopper’s `Serial` unit to communicate with external devices.
- Example project: Logging sensor data to a computer over serial communication.

---

## **Chapter 12: Error Handling and Diagnostics**

### **Section 12.1: Using Diagnostics in Hopper**
- Handling errors with `Diagnostics.Die` and error codes.
- Example project: Writing fault-tolerant code for critical systems.

### **Section 12.2: Debugging Techniques**
- Using Hopper’s debugging tools to identify and fix issues.
- Hands-on debugging exercises with microcontroller projects.

---

## **Chapter 13: Building Complete Systems**

### **Section 13.1: Project Planning and Design**
- Identifying requirements and designing modular components.
- Creating flowcharts and design documents.

### **Section 13.2: Combining Code Modules into a Complete Project**
- Integrating various system components (input, output, processing) into one cohesive program.
- Example project: Developing a complete home automation system.

### **Section 13.3: Testing and Refining Your System**
- Testing code for reliability and ensuring system stability.
- Optimizing code for memory and performance.

---

## **Chapter 14: Capstone Project**

### **Section 14.1: Defining Your Capstone Project**
- Guidelines for designing an advanced project using everything learned in the book.

### **Section 14.2: Building and Testing Your Capstone Project**
- Implementing the project, testing functionality, and ensuring robustness.

### **Section 14.3: Final Presentation and Future Directions**
- Presenting the completed project and discussing potential improvements or extensions.

---

This updated outline restores the focus on structured programming through encapsulation and unit-based design while maintaining the flow of progressing from simpler types to more complex systems. The new chapter on enums and flags helps to round out the treatment of named types.