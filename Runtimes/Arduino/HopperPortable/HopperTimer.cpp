#include "HopperTimer.h"

long nativeLongFromHopperLong(UInt hopperLong);
extern Bool Library_isrExists;

#ifdef USETIMERSTUBS
void External_TimerInitialize() {}
void External_TimerRelease()    {}
UInt External_TimerStartMS(UInt msInterval, TimerISRDelegate timerISRDelegate) { return 0; }
UInt External_TimerStartUS(UInt hrusInterval, TimerISRDelegate timerISRDelegate) { return 0; }
void External_TimerStop(UInt timerID) {}
UInt External_TimerAlarmMS(UInt msInterval, TimerISRDelegate timerISRDelegate) { return 0; }
UInt External_TimerAlarmUS(UInt hrusInterval, TimerISRDelegate timerISRDelegate) { return 0; }
void External_TimerCancel(UInt alarmID) {}
#endif

#ifdef USETIMER

#define MAX_HOPPER_TIMERS 16
#define MAX_HOPPER_ALARMS 16

#include <stdio.h>
#include "pico/stdlib.h"

// Note: modification of alarmSlots and timerSlots only happens
//       in the main execution flow, never in ISRs, so no mutex is required.
//       The only exception to this is that the alarmCallback will make
//       its slot available again. This should not be an issue (grabbing an
//       already in-use slot is the concern).

alarm_id_t        alarmSlots[MAX_HOPPER_ALARMS+1];
repeating_timer_t timerSlots[MAX_HOPPER_TIMERS+1];

bool timerCallback(struct repeating_timer *repeatingTime) 
{
    uint lparam          = (uint)(repeatingTime->user_data);

    HopperISRStruct isrStruct;
    isrStruct.interruptType = InterruptType::eTimer;
    isrStruct.timerID       = (UInt)((lparam >> 16) & 0xFFFF);
    isrStruct.pin           = 0;
    isrStruct.status        = 0;
    isrStruct.isrDelegate   = (UInt)(lparam &0xFFFF);
    isrQueue.push(isrStruct); // assuming interrupts are disabled inside an ISR so no conflict with External_ServiceInterrupts(..)

#ifdef DIAGNOSTICS
    Serial.print("T: 0x");
    Serial.print(isrStruct.isrDelegate, HEX);
    Serial.print(" ");
    Serial.println(isrStruct.timerID);
#endif    


    return true; // true to continue repeating, false to stop.
}

int64_t alarmCallback(alarm_id_t id, void *user_data) 
{
    uint lparam          = (uint)(user_data);

    HopperISRStruct isrStruct;
    isrStruct.interruptType = InterruptType::eTimer;
    isrStruct.timerID       = (UInt)((lparam >> 16) & 0xFFFF);
    isrStruct.pin           = 0;
    isrStruct.status        = 0;
    isrStruct.isrDelegate   = (UInt)(lparam &0xFFFF);
    isrQueue.push(isrStruct); // assuming interrupts are disabled inside an ISR so no conflict with External_ServiceInterrupts(..)

    alarmSlots[isrStruct.timerID] = -1; // free up this slot

#ifdef DIAGNOSTICS
    Serial.print("A: 0x");
    Serial.print(isrStruct.isrDelegate, HEX);
    Serial.print(" ");
    Serial.println(isrStruct.timerID);
#endif    

    return 0; // 0 to not reschedule the alarm
}

bool timerInitialized = false;
void External_TimerInitialize()
{
    if (timerInitialized)
    {
        External_TimerRelease();
    }
    for (UInt i = 0; i <= MAX_HOPPER_ALARMS; i++)
    {
        alarmSlots[i] = -1; // unused
    }
    for (UInt i = 0; i <= MAX_HOPPER_TIMERS; i++)
    {
        timerSlots[i].alarm_id = -1; // unused
    }
    alarm_pool_init_default();

    timerInitialized = true;    
#ifdef DIAGNOSTICS
    Serial.println("<Timer_Initialize>");
#endif    
}
void External_TimerRelease()
{
#ifdef DIAGNOSTICS
    Serial.print("<Timer_Release");
#endif    

    for (UInt i = 1; i <= MAX_HOPPER_ALARMS; i++)
    {
        if (alarmSlots[i] != -1)
        {
            External_TimerCancel(i);
        }
    }
    for (UInt i = 1; i <= MAX_HOPPER_TIMERS; i++)
    {
        if (timerSlots[i].alarm_id != -1)
        {
            External_TimerStop(i);
        }
    }
  
#ifdef DIAGNOSTICS
    Serial.println(">");
#endif    
    timerInitialized = false;
}

UInt External_TimerStart(UInt msInterval, TimerISRDelegate timerISRDelegate)
{
#ifdef DIAGNOSTICS
    Serial.print("<TimerStart ");
    Serial.print(msInterval);
#endif    
    UInt timerID = 0;
    for (UInt i = 1; i <= MAX_HOPPER_TIMERS; i++)
    {
        if (timerSlots[i].alarm_id == -1)
        {
            timerID = i;
            break;
        }
    }
    if (timerID != 0)
    {
        timerSlots[timerID].user_data = (void*)((uint)timerISRDelegate + (timerID << 16));
        if (!add_repeating_timer_ms(msInterval, timerCallback, timerSlots[timerID].user_data, &timerSlots[timerID]))
        {
            timerSlots[timerID].alarm_id = -1;
            timerID = 0;
        }
        else
        {
            Library_isrExists = true;
        }
    }
#ifdef DIAGNOSTICS
    Serial.print(" ");
    Serial.print(timerID);
    Serial.println(" >");
#endif    
    return timerID;
}
UInt External_TimerStartLong(UInt hrmsInterval, TimerISRDelegate timerISRDelegate)
{
    long msInterval = nativeLongFromHopperLong(hrmsInterval);
#ifdef DIAGNOSTICS
    Serial.print("<TimerStartLong ");
    Serial.print(msInterval);
#endif    

    UInt timerID = 0;
    for (UInt i = 1; i <= MAX_HOPPER_TIMERS; i++)
    {
        if (timerSlots[i].alarm_id == -1)
        {
            timerID = i;
            break;
        }
    }
    if (timerID != 0)
    {
        timerSlots[timerID].user_data = (void*)((uint)timerISRDelegate + (timerID << 16));
        if (!add_repeating_timer_ms(msInterval, timerCallback, timerSlots[timerID].user_data, &timerSlots[timerID]))
        {
            timerSlots[timerID].alarm_id = -1;
            timerID = 0;
        }
        else
        {
            Library_isrExists = true;
        }
    }
#ifdef DIAGNOSTICS
    Serial.print(" ");
    Serial.print(timerID);
    Serial.println(" >");
#endif   
    return timerID;
}
void External_TimerStop(UInt timerID)
{
    if ((timerID > 0) && (timerID <= MAX_HOPPER_TIMERS))
    {
        alarm_id_t id = timerSlots[timerID].alarm_id;
        if (-1 != id)
        {
            cancel_repeating_timer(&timerSlots[timerID]);
            timerSlots[timerID].alarm_id = -1;
#ifdef DIAGNOSTICS
            Serial.print("<TimerStop ");
            Serial.print(timerID);
            Serial.println(" >");
#endif
        }
    }
}

UInt External_TimerAlarm(UInt msInterval, TimerISRDelegate timerISRDelegate)
{
#ifdef DIAGNOSTICS
    Serial.print("<TimerAlarm ");
    Serial.print(msInterval);
#endif    

    UInt alarmID = 0;
    for (UInt i = 1; i <= MAX_HOPPER_TIMERS; i++)
    {
        if (alarmSlots[i] == -1)
        {
            alarmID = i;
            break;
        }
    }
    if (alarmID != 0)
    {
        void *userData = (void*)((uint)timerISRDelegate + (alarmID << 16));
        alarm_id_t id = add_alarm_in_ms(msInterval, alarmCallback, userData, false);
        if (id != -1)
        {
            alarmSlots[alarmID] = id;
            Library_isrExists = true;
        }
        else
        {
            alarmID = 0;
        }
    }
#ifdef DIAGNOSTICS
    Serial.print(" ");
    Serial.print(alarmID);
    Serial.println(" >");
#endif   
    return alarmID;
}
UInt External_TimerAlarmLong(UInt hrmsInterval, TimerISRDelegate timerISRDelegate)
{
    long msInterval = nativeLongFromHopperLong(hrmsInterval);
#ifdef DIAGNOSTICS
    Serial.print("<TimerAlarmLong ");
    Serial.print(msInterval);
#endif    

    UInt alarmID = 0;
    for (UInt i = 1; i <= MAX_HOPPER_TIMERS; i++)
    {
        if (alarmSlots[i] == -1)
        {
            alarmID = i;
            break;
        }
    }
    if (alarmID != 0)
    {
        void *userData = (void*)((uint)timerISRDelegate + (alarmID << 16));
        alarm_id_t id = add_alarm_in_ms(msInterval, alarmCallback, userData, false);
        if (id != -1)
        {
            alarmSlots[alarmID] = id;
            Library_isrExists = true;
        }
        else
        {
            alarmID = 0;
        }
    }
#ifdef DIAGNOSTICS
    Serial.print(" ");
    Serial.print(alarmID);
    Serial.println(" >");
#endif   
    return alarmID;
}
void External_TimerCancel(UInt alarmID)
{
    if ((alarmID > 0) && (alarmID <= MAX_HOPPER_ALARMS))
    {
        alarm_id_t id = alarmSlots[alarmID];
        if (-1 != id)
        {
            cancel_alarm(id);
            alarmSlots[alarmID] = -1;
#ifdef DIAGNOSTICS
            Serial.print("<TimerCancel ");
            Serial.print(alarmID);
            Serial.println(" >");
#endif
        }
    }
}

#endif
