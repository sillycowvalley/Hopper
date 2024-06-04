### Hopper 6502 Assembly Library Reference

#### Introduction
- **Overview**: This document provides detailed descriptions of the general libraries used in the r6502 project. These libraries are can be reusable across various Hopper 6502 Assembly programs.
- **Purpose of the Document**: To serve as a reference guide for developers using the r6502 libraries in their projects.
- **How to Use This Document**: Each library unit is described with its key functions, syntax, and examples.

---

#### Library Units

### 1. Serial Library

#### Description
The Serial Library provides functionalities for serial communication, including initialization, transmission, and reception of data.

#### Key Functions

1. **Initialize()**
   - **Syntax**: `Initialize()`
   - **Description**: Initializes the serial communication interface.
   - **Example**:
     ```assembly
     Serial.Initialize();
     ```

2. **ISR()**
   - **Syntax**: `ISR()`
   - **Description**: Interrupt service routine for serial communication.
   - **Example**:
     ```assembly
     Serial.ISR();
     ```

3. **EmptyTheBuffer()**
   - **Syntax**: `EmptyTheBuffer()`
   - **Description**: Empties the serial input buffer.
   - **Example**:
     ```assembly
     Serial.EmptyTheBuffer();
     ```

4. **IsAvailable()**
   - **Syntax**: `IsAvailable()`
   - **Description**: Checks if a character is available in the buffer.
   - **Example**:
     ```assembly
     if (Serial.IsAvailable())
     {
         // Character is available
     }
     ```

5. **WaitForChar()**
   - **Syntax**: `WaitForChar()`
   - **Description**: Waits for a character to be available in the buffer.
   - **Example**:
     ```assembly
     Serial.WaitForChar();
     ```

6. **WriteChar()**
   - **Syntax**: `WriteChar()`
   - **Description**: Transmits a character.
   - **Example**:
     ```assembly
     Serial.WriteChar();
     ```

7. **HexOut()**
   - **Syntax**: `HexOut()`
   - **Description**: Transmits a byte as two hex characters.
   - **Example**:
     ```assembly
     Serial.HexOut();
     ```

8. **HexIn()**
   - **Syntax**: `HexIn()`
   - **Description**: Reads two hex characters from the serial input buffer.
   - **Example**:
     ```assembly
     Serial.HexIn();
     ```

---

### 2. Time Library

#### Description
The Time Library provides timing and delay functionalities.

#### Key Functions

1. **Delay()**
   - **Syntax**: `Delay()`
   - **Description**: Delays execution for a specified number of milliseconds.
   - **Example**:
     ```assembly
     Time.Delay();
     ```

2. **DelayTOP()**
   - **Syntax**: `DelayTOP()`
   - **Description**: Delays execution using the value in TOP.
   - **Example**:
     ```assembly
     Time.DelayTOP();
     ```

3. **Millis()**
   - **Syntax**: `Millis()`
   - **Description**: Returns the number of milliseconds since the program started.
   - **Example**:
     ```assembly
     milliseconds = Time.Millis();
     ```

4. **Seconds()**
   - **Syntax**: `Seconds()`
   - **Description**: Returns the number of seconds since the program started.
   - **Example**:
     ```assembly
     seconds = Time.Seconds();
     ```

---

### 3. I2C Library

#### Description
The I2C Library provides functionalities for I2C communication.

#### Key Functions

1. **Scan()**
   - **Syntax**: `Scan()`
   - **Description**: Scans for I2C devices.
   - **Example**:
     ```assembly
     I2C.Scan();
     ```

2. **BeginTx()**
   - **Syntax**: `BeginTx()`
   - **Description**: Begins an I2C transmission.
   - **Example**:
     ```assembly
     I2C.BeginTx();
     ```

3. **BeginRx()**
   - **Syntax**: `BeginRx()`
   - **Description**: Begins an I2C reception.
   - **Example**:
     ```assembly
     I2C.BeginRx();
     ```

4. **EndTx()**
   - **Syntax**: `EndTx()`
   - **Description**: Ends an I2C transmission.
   - **Example**:
     ```assembly
     I2C.EndTx();
     ```

5. **Start()**
   - **Syntax**: `Start()`
   - **Description**: Sends an I2C start condition.
   - **Example**:
     ```assembly
     I2C.Start();
     ```

6. **Stop()**
   - **Syntax**: `Stop()`
   - **Description**: Sends an I2C stop condition.
   - **Example**:
     ```assembly
     I2C.Stop();
     ```

7. **Write()**
   - **Syntax**: `Write()`
   - **Description**: Writes a byte to the I2C bus.
   - **Example**:
     ```assembly
     I2C.Write();
     ```

8. **RequestFrom()**
   - **Syntax**: `RequestFrom()`
   - **Description**: Requests data from an I2C device.
   - **Example**:
     ```assembly
     I2C.RequestFrom();
     ```

9. **RequestFromTOPNEXT()**
   - **Syntax**: `RequestFromTOPNEXT()`
   - **Description**: Requests data from an I2C device using addresses in TOP and NEXT.
   - **Example**:
     ```assembly
     I2C.RequestFromTOPNEXT();
     ```

10. **Read()**
    - **Syntax**: `Read()`
    - **Description**: Reads a byte from the I2C buffer.
    - **Example**:
      ```assembly
      data = I2C.Read();
      ```

---

### 4. ACIA 6850 Library

#### Description
The ACIA 6850 Library provides functionalities for the Motorola 6850 ACIA serial communication.

#### Key Functions

1. **initialize()**
   - **Syntax**: `initialize()`
   - **Description**: Initializes the ACIA 6850.
   - **Example**:
     ```assembly
     ACIA.initialize();
     ```

2. **writeChar()**
   - **Syntax**: `writeChar()`
   - **Description**: Writes a character to the ACIA 6850.
   - **Example**:
     ```assembly
     ACIA.writeChar();
     ```

3. **isr()**
   - **Syntax**: `isr()`
   - **Description**: Interrupt service routine for the ACIA 6850.
   - **Example**:
     ```assembly
     ACIA.isr();
     ```

4. **pollRead()**
   - **Syntax**: `pollRead()`
   - **Description**: Polls the ACIA 6850 for incoming data.
   - **Example**:
     ```assembly
     ACIA.pollRead();
     ```

---

### 5. Serial EEPROM Library

#### Description
The Serial EEPROM Library provides functionalities for interacting with serial EEPROM devices.

#### Key Functions

1. **copyProgramPage()**
   - **Syntax**: `copyProgramPage()`
   - **Description**: Copies a page of data from the EEPROM.
   - **Example**:
     ```assembly
     SerialEEPROM.copyProgramPage();
     ```

2. **LoadFromEEPROM()**
   - **Syntax**: `LoadFromEEPROM()`
   - **Description**: Loads data from the EEPROM.
   - **Example**:
     ```assembly
     SerialEEPROM.LoadFromEEPROM();
     ```

3. **copyPageToEEPROM()**
   - **Syntax**: `copyPageToEEPROM()`
   - **Description**: Copies a page of data to the EEPROM.
   - **Example**:
     ```assembly
     SerialEEPROM.copyPageToEEPROM();
     ```

4. **SaveToEEPROM()**
   - **Syntax**: `SaveToEEPROM()`
   - **Description**: Saves data to the EEPROM.
   - **Example**:
     ```assembly
     SerialEEPROM.SaveToEEPROM();
     ```

---

### 6. W65C22 Library

#### Description
The W65C22 Library provides functionalities for the W65C22 VIA (Versatile Interface Adapter).

#### Key Functions

1. **Initialize()**
   - **Syntax**: `Initialize()`
   - **Description**: Initializes the W65C22 VIA.
   - **Example**:
     ```assembly
     W65C22.Initialize();
     ```

2. **ISR()**
   - **Syntax**: `ISR()`
   - **Description**: Interrupt service routine for the W65C22 VIA.
   - **Example**:
     ```assembly
     W65C22.ISR();
     ```

---

#### Dependencies on Stacks and ZeroPage

1. **Stacks Library**
   - **Description**: Manages the call stack and value stack.
   - **Key Functions**:
     - `Initialize()`: Initializes the stack.
     - `PushBP()`, `PopBP()`: Pushes and pops

 the base pointer.
     - `PushPC()`, `PopPC()`: Pushes and pops the program counter.
     - `PushTop()`, `PopTop()`: Pushes and pops the top of the stack.
     - `PushIDX()`, `PopIDX()`: Pushes and pops the index register.
     - `PushACC()`, `PopACC()`: Pushes and pops the accumulator.
   - **Example**:
     ```assembly
     Stacks.Initialize();
     ```

2. **ZeroPage Library**
   - **Description**: Defines commonly used zero-page memory addresses and constants.
   - **Key Variables**:
     - `SP`, `BP`, `CSP`: Stack pointers.
     - `PCL`, `PCH`: Program counter low and high bytes.
     - `ACCL`, `ACCH`: Accumulator low and high bytes.
     - `TOPL`, `TOPH`: Top of the stack low and high bytes.
     - `NEXTL`, `NEXTH`: Next value on the stack low and high bytes.
   - **Example**:
     ```assembly
     const byte SP = ZP.SP;
     ```

This document provides descriptions of the libraries used in the r6502 project, ensuring developers can effectively utilize these libraries in their own Hopper 6502 Assembly programs.