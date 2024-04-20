unit Diagnostics
{
    Die(uint error) system;
#ifdef CPU_Z80
    SetError(uint error) system;
#endif    
}
