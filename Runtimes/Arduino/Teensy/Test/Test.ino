
#include "portmap.h"

#define PHI2 MEGA_PB0 // 65C02 clock
#define SYNC MEGA_PB1
#define RW   MEGA_PB2

void setup() {
    pinMode(LED_BUILTIN, OUTPUT);
    pinMode(PHI2, INPUT);
    pinMode(SYNC, INPUT);
    pinMode(RW, INPUT);

    configure_PINMODE_ADDR();
    configure_PINMODE_DATA();

    attachInterrupt(digitalPinToInterrupt(PHI2), OnClock, FALLING); // Data and Address are stable when PHI2 is falling
    Serial.begin(115200);
}

volatile uint ticks;
volatile byte data;
volatile word address;

void OnClock() 
{
    bool sync = digitalRead(SYNC) != 0;
    //bool rw   = digitalRead(RW)   != 0;
    
    if (!sync)
    {
        data    = xDATA_IN();
        address = ADDR();
        ticks++;
    }
}

// the loop function runs over and over again forever
void loop() {
    ticks = 0;
    digitalWrite(LED_BUILTIN, HIGH);  // turn the LED on (HIGH is the voltage level)
    delay(500);                      // wait for a second
    digitalWrite(LED_BUILTIN, LOW);   // turn the LED off by making the voltage LOW
    delay(500);                      // wait for a second
    Serial.println(ticks);
}
