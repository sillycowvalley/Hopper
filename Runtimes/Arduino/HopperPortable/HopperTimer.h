#ifndef HOPPERTIMER_H
#define HOPPERTIMER_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

#if defined(RP2040)
#define USETIMER
#else
#define USETIMERSTUBS
#endif

void External_TimerInitialize();
void External_TimerRelease();

UInt External_TimerStart(UInt msInterval, TimerISRDelegate timerISRDelegate);
UInt External_TimerStartLong(UInt hrmsInterval, TimerISRDelegate timerISRDelegate);
void External_TimerStop(UInt timerID);

UInt External_TimerAlarm(UInt msInterval, TimerISRDelegate timerISRDelegate);
UInt External_TimerAlarmLong(UInt hrmsInterval, TimerISRDelegate timerISRDelegate);
void External_TimerCancel(UInt alarmID);

#endif