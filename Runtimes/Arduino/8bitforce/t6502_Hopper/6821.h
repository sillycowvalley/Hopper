#ifndef _6821_H
#define _6821_H


////////////////////////////////////////////////////////////////////
// 6821 Peripheral
// emulate just enough so keyboard/display works thru serial port.
////////////////////////////////////////////////////////////////////
//

#define KBD   0xd010
#define KBDCR 0xd011
#define DSP   0xd012
#define DSPCR 0xd013
byte regKBD;
byte regKBDDIR;    // Dir register when KBDCR.bit2 == 0
byte regKBDCR;
byte regDSP;
byte regDSPDIR;    // Dir register when DSPCR.bit2 == 0
byte regDSPCR;


void m6821_init()
{
  regKBD    = 0x00;
  regKBDDIR = 0x00;
  regKBDCR  = 0x00;
  regDSP    = 0x00;
  regDSPDIR = 0x00;
  regDSPCR  = 0x00;
}

#endif // _6821_H