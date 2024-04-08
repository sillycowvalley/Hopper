#ifndef _SETUPHOLD_H
#define _SETUPHOLD_H

// ##################################################
// Adjust setup/hold times based on Teensy
// Teensy 3.5 = 120Mhz  (1x)
// Teensy 3.6 = 180Mhz  (1.5x)
// Teensy 4.1 = 600Mhz  (5x)
// ##################################################
#if (ARDUINO_TEENSY35)

  #define DELAY_UNIT()      asm volatile("nop\nnop\nnop\nnop\nnop\nnop\n")
  #define DELAY_FACTOR_H() {DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); }
  #define DELAY_FACTOR_L() {DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); }

#elif (ARDUINO_TEENSY36)

  #define DELAY_UNIT()      asm volatile("nop\nnop\nnop\nnop\nnop\nnop\nnop\nnop\nnop\n")
  #define DELAY_FACTOR_H() {DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); }
  #define DELAY_FACTOR_L() {DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); DELAY_UNIT(); }

#elif (ARDUINO_TEENSY41)

  #define DELAY_UNIT()      asm volatile("nop\nnop\nnop\nnop\nnop\nnop\n" \
                                         "nop\nnop\nnop\nnop\nnop\nnop\n")
                                         // "nop\nnop\nnop\nnop\nnop\nnop\n"
                                         // "nop\nnop\nnop\nnop\nnop\nnop\n"
                                         // "nop\nnop\nnop\nnop\nnop\nnop\n"
  #define DELAY_FACTOR_H() {DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); }
  #define DELAY_FACTOR_L() {DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); } // DELAY_UNIT(); }


#endif

#endif  // _SETUPHOLD_H