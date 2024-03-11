unit Float
{
    byte GetByte(float this, byte index) system;
    float FromBytes(byte b0, byte b1, byte b2, byte b3) system;
    string ToString(float this) system;
    
    float Sin(float angle) system;
    float Cos(float angle) system;
    float ATan2(float y, float x) system;
    float Sqrt(float value) system;
}
