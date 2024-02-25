unit RTCDevice
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/PiPico"
#endif

    uses "/Source/Library/RTCs/DS3231Driver"
    
    bool Begin()
    {
        return RTCDriver.begin(0, Board.GP20, Board.GP21, 0x68);
    }
    
}
