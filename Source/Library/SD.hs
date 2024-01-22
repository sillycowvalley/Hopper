unit SD
{
    byte SPIController { get library; set library; }
    
    byte CSPin  { get library; set library; }
    byte ClkPin { get library; set library; }
    byte TxPin  { get library; set library; } // MOSI
    byte RxPin  { get library; set library; } // MISO
    
    bool Mount(string rootFolder) library;   
    bool Eject() library; 
}
