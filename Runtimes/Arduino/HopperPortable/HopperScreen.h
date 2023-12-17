#ifndef HOPPERSCREEN_H
#define HOPPERSCREEN_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

void HRGraphics_ConfigureDisplay(Display display, UInt width, UInt height);
void HRGraphics_ConfigureSPI(Byte chipSelectPin, Byte dataCommandPin);
void HRGraphics_ConfigureSPIPort(Byte txPin, Byte clkPin);
void HRGraphics_ConfigureReset(Byte resetPin);
void HRGraphics_ConfigureI2C(Byte i2cAddress);
void HRGraphics_ConfigureMatrix(Byte clockPin, Byte dataPin, Byte intensity);
DisplayState HRGraphics_Begin();
void HRGraphics_End();

void HRScreen_Clear();
void HRGraphics_Clear(UInt colour);
UInt HRGraphics_Width_Get();
UInt HRGraphics_Height_Get();
Byte HRScreen_Columns_Get();
Byte HRScreen_Rows_Get();
void HRScreen_DrawChar(UInt col, UInt row, Char chr, UInt foreColour, UInt backColour);
void HRGraphics_DrawChar(UInt x, UInt y, Char chr, UInt foreColour, UInt backColour, Byte scale, Bool antiAliased);

void HRScreen_Print(Char ch);
void HRScreen_PrintLn();


void HRScreen_Suspend();
void HRScreen_Resume(Bool isInteractive);

void HRGraphics_SetPixel(Int x, Int y, UInt colour);

void HRGraphics_Line(UInt x1, UInt y1, UInt x2, UInt y2, UInt colour);
void HRGraphics_HorizontalLine(UInt x1, UInt y1, UInt x2, UInt y2, UInt colour);
void HRGraphics_VerticalLine(UInt x1, UInt y1, UInt x2, UInt y2, UInt colour);
void HRGraphics_Rectangle(UInt x, UInt y, UInt w, UInt h, UInt colour);
void HRGraphics_FilledRectangle(UInt x, UInt y, UInt w, UInt h, UInt colour);
void HRGraphics_Circle(UInt x, UInt y, UInt r, UInt colour);
void HRGraphics_FilledCircle(UInt x, UInt y, UInt r, UInt colour);

void HRGraphics_InvertDisplay(Bool invertColours);
void HRGraphics_FlipDisplay(Bool flipVertical);
void HRGraphics_Show(Bool on);


#endif // HOPPERSCREEN_H