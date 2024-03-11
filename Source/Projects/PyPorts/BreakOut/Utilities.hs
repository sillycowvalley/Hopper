unit Utilities
{
    uint rnd;
    
    uint Random(uint max)
    {
        // Pseudorandom generator from here: (fast and good enough)
        // https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
        rnd = rnd ^ (rnd << 7);
        rnd = rnd ^ (rnd >> 9);
        rnd = rnd ^ (rnd << 8);
        return (rnd % max);
    }
    RandomSeed(uint seed)
    {
        rnd = seed == 0 ? 1 : seed;
    }
}
