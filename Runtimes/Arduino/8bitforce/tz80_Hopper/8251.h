#ifndef _8251_H
#define _8251_H


////////////////////////////////////////////////////////////////////
// 8251 Peripheral
// emulate just enough so keyboard/display works thru serial port.
////////////////////////////////////////////////////////////////////
//

#define ADDR_8251_DATA          0x00
#define ADDR_8251_MODCMD        0x01

#define STATE_8251_RESET        0x01
#define STATE_8251_INITIALIZED  0x00
#define CMD_8251_INTERNAL_RESET 0x40
#define CMD_8251_RTS            0x20
#define CMD_8251_DTR            0x02
#define STAT_8251_TxRDY         0x01
#define STAT_8251_RxRDY         0x02
#define STAT_8251_TxE           0x04
#define STAT_DSR                0x80

byte reg8251_STATE;      // register to keep track of 8251 state: reset or initialized
byte reg8251_MODE;
byte reg8251_COMMAND;
byte reg8251_STATUS;
byte reg8251_DATA;

void intel8251_init()
{
  reg8251_STATE     = STATE_8251_RESET;
  reg8251_MODE      = 0b01001101;       // async mode: 1x baudrate, 8n1
  reg8251_COMMAND   = 0b00100111;       // enable tx/rx; assert DTR & RTS
  reg8251_STATUS    = 0b10000101;       // TxRDY, TxE, DSR (ready for operation). RxRDY=0
  reg8251_DATA      = 0x00;
}

////////////////////////////////////////////////////////////////////
// Serial Event
////////////////////////////////////////////////////////////////////

/*
  SerialEvent occurs whenever a new data comes in the
 hardware serial RX.  This routine is run between each
 time loop() runs, so using delay inside loop can delay
 response. Note: Multiple bytes of data may be available.
 */

inline __attribute__((always_inline))
void serialEvent8251() 
{
  const byte TRANSMIT_DELAY = 10;             // tx_delay to handle fast incoming chars
  static byte tx_delay = TRANSMIT_DELAY;
  
  if (tx_delay > 0)
    tx_delay--;
  else
  if (digitalReadFast(uP_INT_N) == LOW)
  {
    // If interrupt is already asserted
    // wait for it go high...
    return;
  }
  else
  if (Serial.available())
  {
    // if (reg8251_STATUS & CMD_8251_RTS)  // read serial byte only if RTS is asserted
    {
      // RxRDY bit for cpu
      reg8251_STATUS = reg8251_STATUS | STAT_8251_RxRDY;
    }

    digitalWriteFast(uP_INT_N, LOW);

    tx_delay = TRANSMIT_DELAY;
  }
  return;
}
#endif // _8251_H