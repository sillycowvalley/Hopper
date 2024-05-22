Here is a 16x16 table showing the syscalls from 0x00 to 0xFF, with unused slots clearly marked:

|       | 0    | 1    | 2    | 3    | 4    | 5    | 6    | 7    | 8    | 9    | A    | B    | C    | D    | E    | F    |
|-------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|
| **0x**0 | 00   | 01   | 02   | 03   | 04   | 05   | 06   | 07   | 08   | 09   | 0A   | 0B   | 0C   | 0D   | 0E   | 0F   |
|       | StringNewFromConstant | StringBuild | StringNew | StringBuildFront | ArrayNewFromConstant | TimeSecondsGet | StringLengthGet | TimeDelay | DiagnosticsDie | SerialConnect | StringGetChar | ArrayNew | ArrayCountGet | ArrayGetItem | ArraySetItem | SerialReadChar |
| **0x1** | 10   | 11   | 12   | 13   | 14   | 15   | 16   | 17   | 18   | 19   | 1A   | 1B   | 1C   | 1D   | 1E   | 1F   |
|       | SerialWriteChar | SerialIsAvailableGet | MemoryReadByte | MemoryWriteByte | MemoryAvailable | MemoryMaximum | MemoryAllocate | MemoryFree | ByteToHex | IntGetByte | IntFromBytes | ArraySlice | ArrayItemTypeGet | unused | unused | unused |
| **0x2** | 20   | 21   | 22   | 23   | 24   | 25   | 26   | 27   | 28   | 29   | 2A   | 2B   | 2C   | 2D   | 2E   | 2F   |
|       | PairNew | StringAppend | PairKey | StringInsertChar | PairValue | CharToString | unused | VariantBox | VariantUnBox | ScreenPrint | ScreenPrintLn | ScreenClear | ScreenSetCursor | ScreenColumnsGet | ScreenRowsGet | ScreenCursorXGet |
| **0x3** | 30   | 31   | 32   | 33   | 34   | 35   | 36   | 37   | 38   | 39   | 3A   | 3B   | 3C   | 3D   | 3E   | 3F   |
|       | ScreenCursorYGet | ScreenSuspend | ScreenResume | ScreenDrawChar | IntToFloat | IntToLong | UIntToLong | UIntToInt | LongToString | unused | LongToFloat | LongToInt | LongToUInt | LongNew | LongNewFromConstant | LongAdd |
| **0x4** | 40   | 41   | 42   | 43   | 44   | 45   | 46   | 47   | 48   | 49   | 4A   | 4B   | 4C   | 4D   | 4E   | 4F   |
|       | LongSub | LongDiv | LongMul | LongMod | LongEQ | LongLT | LongLE | LongGT | LongGE | LongNegate | FloatToString | unused | FloatNew | FloatNewFromConstant | FloatAdd | FloatSub |
| **0x5** | 50   | 51   | 52   | 53   | 54   | 55   | 56   | 57   | 58   | 59   | 5A   | 5B   | 5C   | 5D   | 5E   | 5F   |
|       | FloatDiv | FloatMul | FloatEQ | FloatLT | FloatLE | FloatGT | FloatGE | TimeMillisGet | ScreenShowCursorSet | SystemArgumentsGet | SystemCurrentDirectoryGet | SystemCurrentDirectorySet | SystemBeep | unused | unused | FileExists |
| **0x6** | 60   | 61   | 62   | 63   | 64   | 65   | 66   | 67   | 68   | 69   | 6A   | 6B   | 6C   | 6D   | 6E   | 6F   |
|       | FileNew | FileOpen | FileCreate | FileReadLine | FileRead | FileIsValid | FileAppend | FileFlush | FileDelete | FileGetSize | DirectoryExists | DirectoryNew | DirectoryIsValid | DirectoryOpen | DirectoryGetDirectoryCount | DirectoryGetFileCount |
| **0x7** | 70   | 71   | 72   | 73   | 74   | 75   | 76   | 77   | 78   | 79   | 7A   | 7B   | 7C   | 7D   | 7E   | 7F   |
|       | DirectoryGetFile | DirectoryGetDirectory | KeyboardReadKey | KeyboardIsAvailableGet | KeyboardToKey | KeyboardClickXGet | KeyboardClickYGet | KeyboardClickUpGet | KeyboardClickDoubleGet | KeyboardScrollDeltaGet | DiagnosticsOutputDebug | DiagnosticsAssert | unused | DiagnosticsSetError | TypesTypeOf | TypesValueTypeOf |
| **0x8** | 80   | 81   | 82   | 83   | 84   | 85   | 86   | 87   | 88   | 89   | 8A   | 8B   | 8C   | 8D   | 8E   | 8F   |
|       | TypesKeyTypeOf | TypesBoxTypeOf | TypesVerifyValueTypes | unused | WiFiConnect | WiFiIPGet | WiFiStatusGet | WiFiDisconnect | unused | DirectoryCreate | DirectoryDelete | RuntimePCGet | RuntimeSPGet | RuntimeBPGet | RuntimeCSPGet | RuntimeGetStackWord |
| **0x9** | 90   | 91   | 92   | 93   | 94   | 95   | 96   | 97   | 98   | 99   | 9A   | 9B   | 9C   | 9D   | 9E   | 9F   |
|       | RuntimeGetStackType | RuntimeGetCallStackWord | RuntimeExecute | RuntimeInline | RuntimeUserCodeGet | TimeTime_Get | TimeDate_Get | RuntimeInDebuggerGet | RuntimeDateTimeGet | MemoryReadProgramByte | MemoryWriteProgramByte | MemoryReadProgramWord | MemoryWriteProgramWord | FileGetDate | DirectoryGetDate | MemoryProgramOffsetSet |
| **0xA** | A0   | A1   | A2   | A3   | A4   | A5   | A6   | A7   | A8   | A9   | AA   | AB   | AC   | AD   | AE   | AF   |
|       | SerialWriteString | unused | DictionaryNew | SerialClose | SerialIsValid | unused | unused | unused | HardwareLEDSet | DictionaryCountGet | DictionarySet | DictionaryContains | DictionaryGet | DictionaryNext | DictionaryClear | TraceSet |
| **0xB** | B0   | B1   | B2   | B3   | B4   | B5   | B6   | B7   | B8   | B9   | BA   | BB   | BC   | BD   | BE   | BF   |
|       | TraceGet | DictionaryHashKey | ClipboardHasTextGet | ClipboardGetText | ClipboardSetText | unused | MemoryReadBit | MemoryWriteBit | CharToUpper | CharIsUpper | CharIsDigit | CharIsLetterOrDigit | CharIsLower | ByteToDigit | unused | CharIsHexDigit |
| **0xC** | C0   | C1   | C2   | C3   | C4   | C5   | C6   | C7   | C8   | C9   | CA   | CB   | CC   | CD   | CE   | CF   |
|       | CharToLower | StringStartsWith | StringContains | StringIndexOf | SystemWarp_Set | SystemWarp_Get | unused | LongInc | LongAddRef | LongMulRef | ArrayGetItemUInt | ArraySetItemUInt | FileGetTimeStamp | unused | FileGetTime | DirectoryGetTime |
| **0xD** | D0   | D1   | D2   | D3   | D4   | D5   | D6   | D7   | D8   | D9   | DA   | DB   | DC   | DD   | DE   | DF   |
|       | StringTrim | StringTrimLeft | StringTrimRight | StringPushImmediate | StringToUpper | StringToLower | ClipboardGetChar | MemoryReadWord | MemoryWriteWord | unused | unused | unused | MemoryReadCodeByte | MemoryWriteCodeByte | MemoryReadCodeWord | MemoryWriteCodeWord |
| **0xE** | E0   | E1   | E2  

 | E3   | E4   | E5   | E6   | E7   | E8   | E9   | EA   | EB   | EC   | ED   | EE   | EF   |
|       | LongGetByte | unused | FloatGetByte | LongFromBytes | unused | FloatFromBytes | UIntToFloat | SerialPortsGet | StringCompare | StringEndsWith | StringSubstring | StringReplace | FloatToUInt | FloatToLong | LongAddB | LongSubB |
| **0xF** | F0   | F1   | F2   | F3   | F4   | F5   | F6   | F7   | F8   | F9   | FA   | FB   | FC   | FD   | FE   | FF   |
|       | FloatSin | FloatCos | FloatATan2 | FloatSqrt | ListNew | ListCountGet | ListAppend | ListInsert | ListGetItem | ListGetItemAsVariant | ListSetItem | ListClear | ListRemove | ListContains | unused | unused |

The `unused` slots represent those hexadecimal values that are not assigned to any syscall.