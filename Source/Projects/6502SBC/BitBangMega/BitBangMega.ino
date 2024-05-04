typedef unsigned char byte;
typedef uint16_t      uint;

const byte ssd1306InitTable[] = {
                                0xAE,   // Turn off display
                                0xD5,   // set display clock divider
                                0xF0,   // 0x80 default - $f0 is faster for less tearing
                                0xA8,   // set multiplex
                                0x3F,   // for 128x64
                                0x40,   // Startline 0
                                0x8D,   // Charge pump
                                0x14,   // VCCstate 14
                                0xA1,   // Segment remap
                                0xC8,   // Com output scan direction
                                0x20,   // Memory mode
                                0x00,   //
                                0xDA,   // Com-pins
                                0x12,
                                0xFE,   // Set contrast - full power!
                                0x7F,   // About half
                                0xD9,   // Set precharge
                                0x11,   // VCC state 11
                                0xA4,   // Display all on resume
                                0xAF,   // Display on
                                //0xA5,   // Entire display ON  A5 Enable / A4 Disable 
                                0xB0, 0x10, 0x00, // Page 0, column 0.
                                0xFF // Stop byte
                              };

const byte I2C_SCL = A0;
const byte I2C_SDA = A1;
    

byte I2C_Address;

void dly()
{
    //uint i = 1; // 'NOP' that optimizer doesn't remove
}
void sdaLow()
{
    pinMode(I2C_SDA, OUTPUT);
    digitalWrite(I2C_SDA, LOW);
}
void sdaHigh()
{
    pinMode(I2C_SDA, INPUT);
}
void sclLow()
{
    pinMode(I2C_SCL, OUTPUT);
    digitalWrite(I2C_SCL, LOW);
}
void sclHigh()
{
    pinMode(I2C_SCL, INPUT);
}

bool sdaRead()
{
    pinMode(I2C_SDA, INPUT);
    return digitalRead(I2C_SDA) != 0;
}
bool checkAck()
{
    sclHigh();
    dly();
    bool ack = sdaRead();
    sclLow();
    //Write(ack ? '+' : '-');
    return ack;
}

bool I2C_ByteOut(byte value);

void I2C_Initialize()
{
    sdaHigh();
    sclHigh();
}

void I2C_Start(bool isWrite)
{
    //Serial.print("[");
    
    sdaHigh();
    dly();
    sclHigh();
    dly();
    sdaLow();
    dly();
    sclLow();
    dly();
    
    byte value = (I2C_Address << 1) + (isWrite ? 0 : 1);
    I2C_ByteOut(value);
}
bool I2C_ByteOut(byte value)
{   
    //Serial.print(" ");
    //Serial.print(value, HEX);
    
    for (byte bit = 0; bit < 8; bit++)
    {
        if ((value & 0b10000000) == 0)
        {
            sdaLow();
        }
        else
        {
            sdaHigh();
        }
        dly();
        sclHigh();
        dly();
        sclLow();
        dly();
        value = value << 1;
    }
    
    // clock in the ack bit
    return checkAck();
}
void I2C_Stop()
{
    sdaLow();
    dly();
    sclHigh();
    dly();
    sdaHigh();
    dly();
    
    //Serial.println("]");
}

byte cursor;
byte tflags;
byte scroll;

void cmd(byte command)
{
    I2C_Address = 0x3C; // address of the device (78 on the back of the module is 3C << 1)
    I2C_Start(true);
    I2C_ByteOut(0x00);    // 0x00 for commands or 0x40 for data
    I2C_ByteOut(command);
}

void SSD1306_Initialize()
{
    I2C_Start(true);
    uint i;
    while (true)
    {
        byte value = ssd1306InitTable[i];
        if (value == 0xFF) { break; }
        I2C_ByteOut(value);
        i++;
    }
    I2C_Stop();
}

void SSD1306_SetLine(byte line)
{
    cmd(0x22); // Set page cmd
    I2C_ByteOut(line);
    I2C_ByteOut(0x07); // Ensure range
    I2C_Stop(); 
}
void SSD1306_SetColumn(byte column)
{
    cmd(0x21); // Set column command (0-127)
    I2C_ByteOut(column << 3); // 15 << 3 = 120
    I2C_ByteOut(0x7F); // 127
    I2C_Stop();
}

void SSD1306_Clear(byte colour)
{
    cursor = 0;
    tflags = 0;
    SSD1306_SetLine(0);
    SSD1306_SetColumn(0);
    I2C_Start(true);
    I2C_ByteOut(0x40); // 0x00 for commands or 0x40 for data
    for (uint i=0; i < 256; i++)
    {
        I2C_ByteOut(colour);
        I2C_ByteOut(colour);
        I2C_ByteOut(colour);
        I2C_ByteOut(colour);
    }
    I2C_Stop();
    
    cmd(0xD3); // Clear scroll
    scroll = 0;
    I2C_ByteOut(0x00);
    I2C_Stop();
}
void setup() 
{
    Serial.begin(115200);
    while (!Serial);
    pinMode(A2, OUTPUT);
}

void loop() 
{
  Serial.println();
  Serial.println("Started");
  Serial.println();

  digitalWrite(A2, HIGH);

  // good defaults
  I2C_Address = 0x3C; // address of the device (78 on the back of the module is 3C << 1)
  
  I2C_Initialize();
  
  delay(250);
  SSD1306_Initialize();
  
  delay(250);
  SSD1306_Clear(0xFF);
  delay(250);
  SSD1306_Clear(0xAA);
  delay(250);
  SSD1306_Clear(0x00);
  
  digitalWrite(A2, LOW); 
  for (;;)
  {
    delay(1); // otherwise the programmer cannot find the Mega again
  }
}
