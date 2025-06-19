#include <pins_arduino.h>
#include "portmap.h"

////////////////////////////////////////////////////////////
// These functions return real-time state (no debouncing)
////////////////////////////////////////////////////////////

#define btn_P_state() (analogRead(A16) < 10 ? true : false)
#define btn_C_state() (analogRead(A17) < 10 ? true : false)

////////////////////////////////////////////////////////////
// These functions are debounced.
// Note: They block if button state changes.
////////////////////////////////////////////////////////////
long debounceDelay = 16;

bool btn_P_debounced() 
{
  static bool btn_P_prev_state = false;
  bool btn_P = btn_P_state();

  if ( btn_P != btn_P_prev_state )    // Did button state change?
  {
    delay(debounceDelay);             // If so, wait and recheck
    return (btn_P_prev_state = btn_P_state());
  }
  return btn_P;
}

bool btn_C_debounced() 
{
  static bool btn_C_prev_state = false;
  bool btn_C = btn_C_state();
  
  if ( btn_C != btn_C_prev_state )    // Did button state change?
  {
    delay(debounceDelay);             // If so, wait and recheck
    return (btn_C_prev_state = btn_C_state());
  }
  return btn_C;
}

bool waitForPRelease()
{
    bool pressed = false;
    for (;;)
    {
        if (btn_P_debounced())
        {
            pressed = true;
        }
        else
        {
            break;  
        }
    }
    return pressed;
}
bool waitForCRelease()
{
    bool pressed = false;
    for (;;)
    {
        if (btn_C_debounced())
        {
            pressed = true;
        }
        else
        {
            break;  
        }
    }
    return pressed;
}

#define PHI2 MEGA_PB0 // 65C02 clock
#define SYNC MEGA_PB1
#define RW   MEGA_PB2

volatile bool capturing;
volatile bool doDump;
volatile uint ticks;
volatile uint dindex;
volatile byte data;
volatile word address;

void setup() {
    pinMode(LED_BUILTIN, OUTPUT);
    pinMode(PHI2, INPUT);
    pinMode(SYNC, INPUT);
    pinMode(RW, INPUT);

    configure_PINMODE_ADDR();
    configure_PINMODE_DATA();

    attachInterrupt(digitalPinToInterrupt(PHI2), OnClock, FALLING); // Data and Address are stable when PHI2 is falling
    Serial.begin(115200);

    capturing = false;
    doDump = false;
    ticks = 0;
    dindex = 0;
}

void Dump();


volatile byte dlog[10240];
volatile word alog[10240];
volatile byte flog[10240];

void OnClock() 
{
    word addr = ADDR();

    if (!capturing && (addr >= 0xFFFA) && (addr <= 0xFFFF))
    {
        // reset
        capturing = true;
        dindex = 0;
    }
    
    if (capturing)
    {
        dlog[dindex] = xDATA_IN();
        alog[dindex] = addr;
        flog[dindex] = (digitalRead(SYNC) ? 1 : 0) | (digitalRead(RW) ? 2 : 0);
        dindex++;
        if (dindex >= 10240)
        {
            capturing = false;
            doDump = true;
            dindex = 0;
        }
    }
    ticks++;
}

// the loop function runs over and over again forever
void loop() {
  
    if (btn_P_state())
    {
        Serial.print("Capturing");
        if (waitForPRelease())
        {
            dindex = 0;
            capturing = true;
            Serial.println();
        }
    }
    if (btn_C_state())
    {
        if (waitForCRelease())
        {
            Dump();
        }
    }
    if (doDump)
    {
        Dump();
        doDump = false;
    }
  
    ticks = 0;
    digitalWrite(LED_BUILTIN, HIGH);  // turn the LED on (HIGH is the voltage level)
    delay(500);                      // wait for a second
    digitalWrite(LED_BUILTIN, LOW);   // turn the LED off by making the voltage LOW
    delay(500);                      // wait for a second
    //Serial.print("Ticks: "); Serial.println(ticks);
}



enum AddressingMode {Undefined, Immediate, Accumulator, Relative, Absolute, AbsoluteIndirect, AbsoluteXIndexedIndirect, XIndexedAbsolute, YIndexedAbsolute, ZeroPage, 
                     XIndexedZeroPage, YIndexedZeroPage, ZeroPageIndirect, XIndexedZeroPageIndirect, ZeroPageIndirectYIndexed}; 

void Dump()
{
    char output[64];
    uint currentAddress;
    byte currentData;
    bool currentSync;
    bool currentRead;
    unsigned char  currentOpCode = 0;
    unsigned int   operands = 0;
    unsigned int   previousOperand = 0;
    unsigned int   absoluteAddress = 0;
    AddressingMode addressingMode = Undefined;

    uint i = 0;
    /*
    for (;;)  
    {
        currentData    = dlog[i];
        currentAddress = alog[i];
        currentSync = ((flog[i] & 1) != 0);
        currentRead = ((flog[i] & 2) != 0);

        sprintf(output, "\t\t\t\t\t0x%04X\t0x%02X\t\t%c\t%c ?",  currentAddress, currentData, (char)(currentRead? 'R' : 'W'), (char)(currentSync? 'S' : ' '));
        Serial.println(output);
        i++;
        if (i > 20) { break; }
    }
    i = 0;
    */
    for (;;)  
    {
        currentData    = dlog[i];
        currentAddress = alog[i];
        currentSync = ((flog[i] & 1) != 0);
        currentRead = ((flog[i] & 2) != 0);

        if ((currentAddress >= 0x0100) && (currentAddress <= 0x01FF)) 
        {
            // stack (needs to be before operands below in case operation accesses stack first like JSR does)
            sprintf(output, "\t\t\t\t\t0x%04X\t0x%02X\t\t%c STACK",  currentAddress, currentData, (char)(currentRead? 'R' : 'W'));
            Serial.println(output);
        }
        
        else if (operands > 0)
        {
          if (operands == 1)
          {
            switch (addressingMode)
            {
              case XIndexedZeroPageIndirect: // (zeropage,X)
                sprintf(output, "\t(0x%02X, X)",    currentData);
                break;
              case ZeroPage: // zeropage
                sprintf(output, "\t0x%02X",         currentData);
                break;
              case ZeroPageIndirect: // (zeropage)
                sprintf(output, "\t(0x%02X)",       currentData);
                break;
              case Immediate: // #immediate
                sprintf(output, "\t# 0x%02X",       currentData);
                break;
              case Absolute: // absolute
                absoluteAddress = (currentData << 8) + previousOperand;
                if (currentOpCode == 0x20) // JSR is special
                {
                  sprintf(output, "\t\t\t0x%04X",     absoluteAddress);
                }
                else
                {
                  sprintf(output, "\t0x%04X",     absoluteAddress);
                }
                break;
              case AbsoluteIndirect: // JMP (absolute)
                absoluteAddress = (currentData << 8) + previousOperand;
                sprintf(output, "\t(0x%04X)",     absoluteAddress);
                break;
              case AbsoluteXIndexedIndirect: // JMP (absolute,X)
                absoluteAddress = (currentData << 8) + previousOperand;
                sprintf(output, "\t(0x%04X, X)",     absoluteAddress);
                break;
              case ZeroPageIndirectYIndexed: // (zeropage),Y
                sprintf(output, "\t(0x%02X), Y",    currentData);
                break;
              case XIndexedZeroPage: // zeropage, X
                sprintf(output, "\t0x%02X, X",      currentData);
                break;
              case YIndexedZeroPage: // zeropage, Y
                sprintf(output, "\t0x%02X, Y",      currentData);
                break;
              case XIndexedAbsolute: // absolute, X
                absoluteAddress = (currentData << 8) + previousOperand;
                sprintf(output, "\t0x%04X, X",  absoluteAddress);
                break;
              case YIndexedAbsolute: // absolute, Y
                absoluteAddress = (currentData << 8) + previousOperand;
                sprintf(output, "\t0x%04X, Y",  absoluteAddress);
                break;
              case Relative: // relative (for branches)
                // The range of the offset is 128 to +127 bytes from the next instruction.
                absoluteAddress = currentAddress + currentData - 255;
                sprintf(output, "\t0x%04X",     absoluteAddress);
                break;
              case Undefined: // what's this?
                sprintf(output, "\t0x%02X ???",     currentData);
                break;
              default:
                break;
            }
            Serial.println(output);
          }
          else if (operands == 2)
          {
            previousOperand = currentData;
          }
          else
          {
            Serial.println("BAD operands");
          }
          operands--;
        }

        else if ((currentAddress >= 0x00EC) && (currentAddress <= 0x00FF)) // implementation specific: what are your PORT addresses?
        {
            // port IO
            sprintf(output, "\t\t\t\t\t0x%04X\t0x%02X\t\t%c PORT",  currentAddress, currentData, (char)(currentRead? 'R' : 'W'));
            Serial.println(output);
        }
        else if ((currentAddress >= 0x0000) && (currentAddress < 0xC000)) // implementation specific: what is your data RAM?
        {
            // RAM
            sprintf(output, "\t\t\t\t\t0x%04X\t0x%02X\t\t%c RAM",  currentAddress, currentData, (char)(currentRead? 'R' : 'W'));
            Serial.println(output);
        }
        else if ((currentAddress >= 0xFFFA) && (currentAddress <= 0xFFFF)) 
        {
            // vectors
            sprintf(output, "\t\t\t\t\t0x%04X\t0x%02X\t\t%c VECTORS",  currentAddress, currentData, (char)(currentRead? 'R' : 'W'));
            Serial.println(output);
        }
        else if (currentSync && currentRead)
        {
            // https://www.pagetable.com/c64ref/6502/?cpu=65c02&tab=2 - good opcode resource
            // https://llx.com/Neil/a2/opcodes.html#insc02 - good instruction decoding resource
            char opName[5];
            
            operands = 0;
            addressingMode = Undefined;
            strcpy(opName, "???"); // what's this?!
            currentOpCode = currentData;

            // other instructions (not neatly in the groups which follow)
            switch (currentData)
            {
              case 0x00:
                strcpy(opName, "BRK");
                break;
              case 0x20:
                strcpy(opName, "JSR");
                operands = 2;
                addressingMode = Absolute;
                break;
              case 0x4C:
                strcpy(opName, "JMP");
                operands = 2;
                addressingMode = Absolute;
                break;
              case 0x6C:
                strcpy(opName, "JMP");
                operands = 2;
                addressingMode = AbsoluteIndirect;
                break;
              case 0x7C:
                strcpy(opName, "JMP");
                operands = 2;
                addressingMode = AbsoluteXIndexedIndirect;
                break;
              case 0x40:
                strcpy(opName, "RTI");
                break;
              case 0x60:
                strcpy(opName, "RTS");
                break;
              case 0x08:
                strcpy(opName, "PHP");
                break;
              case 0x28:
                strcpy(opName, "PLP");
                break;
              case 0x48:
                strcpy(opName, "PHA");
                break;
              case 0x68:
                strcpy(opName, "PLA");
                break;
              case 0x88:
                strcpy(opName, "DEY");
                break;
              case 0xA8:
                strcpy(opName, "TAY");
                break;
              case 0xC8:
                strcpy(opName, "INY");
                break;
              case 0xE8:
                strcpy(opName, "INX");
                break;
              case 0x18:
                strcpy(opName, "CLC");
                break;
              case 0x38:
                strcpy(opName, "SEC");
                break;
              case 0x58:
                strcpy(opName, "CLI");
                break;
              case 0x78:
                strcpy(opName, "SEI");
                break;
              case 0x98:
                strcpy(opName, "TYA");
                break;
              case 0xB8:
                strcpy(opName, "CLV");
                break;
              case 0xD8:
                strcpy(opName, "CLD");
                break;
              case 0xF8:
                strcpy(opName, "SED");
                break;
              case 0x8A:
                strcpy(opName, "TXA");
                break;
              case 0x9A:
                strcpy(opName, "TXS");
                break;
              case 0xAA:
                strcpy(opName, "TAX");
                break;
              case 0xBA:
                strcpy(opName, "TSX");
                break;
              case 0xCA:
                strcpy(opName, "DEX");
                break;
              case 0xEA:
                strcpy(opName, "NOP");
                break;
              
              // 65C02
              case 0x1A:
                strcpy(opName, "INC");
                break;
              case 0x3A:
                strcpy(opName, "DEC");
                break;
              case 0xDA:
                strcpy(opName, "PHX");
                break;
              case 0x5A:
                strcpy(opName, "PHY");
                break;
              case 0xFA:
                strcpy(opName, "PLX");
                break;
              case 0x7A:
                strcpy(opName, "PLY");
                break;
              case 0x89:      
                strcpy(opName, "BIT");
                addressingMode = Immediate;
                operands = 1;
                break;
              case 0x14:
                strcpy(opName, "TRB");
                addressingMode = ZeroPage;
                operands = 1;
                break;
              case 0x64:
                strcpy(opName, "STZ");
                addressingMode = ZeroPage;
                operands = 1;
                break;
              case 0x1C:
                strcpy(opName, "TRB");
                addressingMode = Absolute;
                operands = 2;
                break;
              case 0x9C:
                strcpy(opName, "STZ");
                addressingMode = Absolute;
                operands = 2;
                break;
              case 0x74:
                strcpy(opName, "STZ");
                addressingMode = XIndexedZeroPage;
                operands = 1;
                break;
              case 0x9E:
                strcpy(opName, "STZ");
                addressingMode = XIndexedAbsolute;
                operands = 2;
                break;
              
              case 0x10:
                strcpy(opName, "BPL");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0x30:
                strcpy(opName, "BMI");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0x50:
                strcpy(opName, "BVC");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0x70:
                strcpy(opName, "BVS");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0x80:
                strcpy(opName, "BRA");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0x90:
                strcpy(opName, "BCC");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0xB0:
                strcpy(opName, "BCS");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0xD0:
                strcpy(opName, "BNE");
                addressingMode = Relative;
                operands = 1;
                break;
              case 0xF0:
                strcpy(opName, "BEQ");
                addressingMode = Relative;
                operands = 1;
                break;
              
              default:
                switch (currentData & 0b00000011)
                {
                  case 0b00000001: // 'Group One' instructions
                    addressingMode = Undefined;
                    switch (currentData & 0b00011100)
                    {
                      case 0b00000000: // (zeropage,X)
                        addressingMode = XIndexedZeroPageIndirect;
                        operands = 1;
                        break;
                      case 0b00000100: // zeropage
                        addressingMode = ZeroPage;
                        operands = 1;
                        break;
                      case 0b00001000: // #immediate
                        addressingMode = Immediate;  
                        operands = 1;
                        break;
                      case 0b00010000: // (zeropage),Y
                        addressingMode = ZeroPageIndirectYIndexed;  
                        operands = 1;
                        break;
                      case 0b00010100: // zeropage,X
                        addressingMode = XIndexedZeroPage;  
                        operands = 1;
                        break;
                      case 0b00001100: // absolute
                        addressingMode = Absolute;  
                        operands = 2;
                        break;
                      case 0b00011000: // absolute, X
                        addressingMode = XIndexedAbsolute;  
                        operands = 2;
                        break;
                      case 0b00011100: // absolute, Y
                        addressingMode = YIndexedAbsolute;  
                        operands = 2;
                        break;
                    }
                    switch (currentData & 0b11100000)
                    {
                      case 0b00000000:
                        strcpy(opName, "ORA");
                        break;
                      case 0b00100000:
                        strcpy(opName, "AND");
                        break;
                      case 0b01000000:
                        strcpy(opName, "EOR");
                        break;
                      case 0b01100000:
                        strcpy(opName, "ADC");
                        break;
                      case 0b10000000:
                        strcpy(opName, "STA");
                        break;
                      case 0b10100000:
                        strcpy(opName, "LDA");
                        break;
                      case 0b11000000:
                        strcpy(opName, "CMP");
                        break;
                      case 0b11100000:
                        strcpy(opName, "SBC");
                        break;
                    }
                    break;
                  case 0b00000010: // 'Group Two' instructions
                    addressingMode = Undefined;
                    switch (currentData & 0b00011100)
                    {
                      case 0b00000000: // #immediate
                        addressingMode = Immediate;
                        operands = 1;
                        break;
                      case 0b00000100: // zeropage
                        addressingMode = ZeroPage;
                        operands = 1;
                        break;
                      case 0b00001000: // A
                        addressingMode = Accumulator;  
                        operands = 0;
                        break;
                      case 0b00001100: // absolute
                        addressingMode = Absolute;  
                        operands = 2;
                        break;
                      case 0b0010000: // (zeropage)
                        addressingMode = ZeroPageIndirect;  
                        operands = 2;
                        break;
                      case 0b00010100: // zeropage,X
                        addressingMode = XIndexedZeroPage;  
                        operands = 1;
                        break;
                      case 0b00011100: // absolute, X
                        addressingMode = XIndexedAbsolute;  
                        operands = 2;
                        break;
                    }
                    if (addressingMode == ZeroPageIndirect) // 65C02 exception
                    {
                      // 'Group One' opcodes
                      switch (currentData & 0b11100000)
                      {
                        case 0b00000000:
                          strcpy(opName, "ORA");
                          break;
                        case 0b00100000:
                          strcpy(opName, "AND");
                          break;
                        case 0b01000000:
                          strcpy(opName, "EOR");
                          break;
                        case 0b01100000:
                          strcpy(opName, "ADC");
                          break;
                        case 0b10000000:
                          strcpy(opName, "STA");
                          break;
                        case 0b10100000:
                          strcpy(opName, "LDA");
                          break;
                        case 0b11000000:
                          strcpy(opName, "CMP");
                          break;
                        case 0b11100000:
                          strcpy(opName, "SBC");
                          break;
                      }
                    }
                    else
                    {
                      switch (currentData & 0b11100000)
                      {
                        case 0b00000000:
                          strcpy(opName, "ASL");
                          break;
                        case 0b00100000:
                          strcpy(opName, "ROL");
                          break;
                        case 0b01000000:
                          strcpy(opName, "LSR");
                          break;
                        case 0b01100000:
                          strcpy(opName, "ROR");
                          break;
                        case 0b10000000:
                          switch (addressingMode)
                          {
                            case XIndexedZeroPage:
                              addressingMode = YIndexedZeroPage;
                              break;
                            default:
                              break;
                          }
                          strcpy(opName, "STX");
                          break;
                        case 0b10100000:
                          switch (addressingMode)
                          {
                            case XIndexedZeroPage:
                              addressingMode = YIndexedZeroPage;
                              break;
                            case XIndexedAbsolute:
                              addressingMode = YIndexedAbsolute;
                              break;
                            default:
                              break;
                          }
                          strcpy(opName, "LDX");
                          break;
                        case 0b11000000:
                          strcpy(opName, "DEC");
                          break;
                        case 0b11100000:
                          strcpy(opName, "INC");
                          break;
                      }
                    }
                    break;
                  case 0b00000000: // 'Group Three' instructions
                    addressingMode = Undefined;
                    switch (currentData & 0b00011100)
                    {
                      case 0b00000000: // #immediate
                        addressingMode = Immediate;
                        operands = 1;
                        break;
                      case 0b00000100: // zeropage
                        addressingMode = ZeroPage;
                        operands = 1;
                        break;
                      case 0b00001100: // absolute
                        addressingMode = Absolute;  
                        operands = 2;
                        break;
                      case 0b00010100: // zeropage,X
                        addressingMode = XIndexedZeroPage;  
                        operands = 1;
                        break;
                      case 0b00011100: // absolute, X
                        addressingMode = XIndexedAbsolute;  
                        operands = 2;
                        break;
                      default:
                        break;
                    }
                    switch (currentData & 0b11100000)
                    {
                      case 0b00000000:
                        strcpy(opName, "TSB");
                        break;
                      case 0b00100000:
                        strcpy(opName, "BIT");
                        break;
                      case 0b10000000:
                        strcpy(opName, "STY");
                        break;
                      case 0b10100000:
                        strcpy(opName, "LDY");
                        break;
                      case 0b11000000:
                        strcpy(opName, "CPY");
                        break;
                      case 0b11100000:
                        strcpy(opName, "CPX");
                        break;
                    }
                    break;
                case 0b00000011: // special Rockwell and WDC instructions
                    addressingMode = Undefined;
                    unsigned char bit = ((currentData & 0b01110000) >> 4) + '0';
                    switch (currentData & 0b00001100)
                    {
                      case 0b00000100:
                        sprintf(opName, "RMB%c", bit);
                        break;
                      case 0b10000100:
                        sprintf(opName, "SMB%c", bit);
                        break;
                      case 0b00001100:
                        sprintf(opName, "BBR%c", bit);
                        break;
                      case 0b10001100:
                        sprintf(opName, "BBS%c", bit);
                        break;
                    }
                    break;
                }
                break;
            }

            if (   (operands > 0) 
                && (currentOpCode != 0x20) // JSR is special: stack bytes are seen before operand bytes
              )
            {
              sprintf(output, "0x%04X\t0x%02X\t%s",  currentAddress, currentData, opName);
            }
            else
            {
              sprintf(output, "0x%04X\t0x%02X\t%s\n",  currentAddress, currentData, opName);
            }
            Serial.print(output);

            /*
            // For some reason the SYNC signal is not always correct following a single byte opcode.
            // This is a workaround but it cannot be used if the single byte opcode causes stack read/write
            opCodeMustFollow = false;
            if (operands == 0)
            {
              switch (currentData)
              {
                case 0x48: // PHA
                case 0x08: // PHP
                case 0xDA: // PHX
                case 0x5A: // PHY
                case 0x68: // PLA
                case 0x28: // PLP
                case 0xFA: // PLX
                case 0x7A: // PLY
                case 0x20: // JSR
                case 0x40: // RTI
                case 0x60: // RTS
                  break;
                default:
                  opCodeMustFollow = true;
                  break;
              }
            }
            */
        }
        else
        {
            // everything else (typically ignored extra bus activity like a conditional branch not taken)
            /*
            sprintf(output, "\t\t\t\t\t0x%04X\t0x%02X\t\t%c\t%c ?",  currentAddress, currentData, (char)(currentRead? 'R' : 'W'), (char)(currentSync? 'S' : ' '));
            Serial.println(output);
            */
        }

        i++;
        if (i >= 10240) { break; }
    }
}
