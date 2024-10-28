//#include <pins_arduino.h>
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
    ticks = 0;
    dindex = 0;
}


volatile byte dlog[10240];
volatile word alog[10240];

void OnClock() 
{
    bool sync = digitalRead(SYNC) != 0;
    //bool rw   = digitalRead(RW)   != 0;
    
    if (capturing)
    {
        dlog[dindex] = xDATA_IN();
        alog[dindex] = ADDR();
        ticks++;
        dindex++;
        if (dindex >= 10240)
        {
            capturing = false;
        }
    }
    ticks++;
}

void Dump()
{
    for (uint i=0; i < 10240; i++)  
    {
        Serial.print(" 0x"); Serial.print(i, HEX); Serial.print(" 0x"); Serial.print(alog[i], HEX); Serial.print(" 0x"); Serial.print(dlog[i], HEX);
        Serial.println();
    }
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
    
    ticks = 0;
    digitalWrite(LED_BUILTIN, HIGH);  // turn the LED on (HIGH is the voltage level)
    delay(500);                      // wait for a second
    digitalWrite(LED_BUILTIN, LOW);   // turn the LED off by making the voltage LOW
    delay(500);                      // wait for a second
    Serial.println(ticks);
}
