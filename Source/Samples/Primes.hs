program Primes
{

   uses "/Source/System/System"
   uses "/Source/System/Screen"   
   
   // Python benchmarks:
   // https://forum.micropython.org/viewtopic.php?t=2659
   // https://github.com/shaoziyang/micropython_benchmarks/blob/master/1.9.4-479/benchmark.py
   
   uint isPrime(uint n)
   {
       uint n2 = n/2;
       for (uint i=2; i <= n2; i++)
       {
           if ((n % i) == 0)
           {
               return 0;
           }
       }
       return 1;
   }
   {
       //for (uint j=0; j < 100; j++)
       //{
           long start = Millis;
           
           
           uint numPrimes = 0;
           for (uint i=2; i < 65535; i++)
           {
               numPrimes = numPrimes + isPrime(i);        
           }
           
           long end = Millis;
           long elapsed = end - start;
           float seconds = elapsed / 1000.0;
           
           PrintLn(numPrimes.ToString() + " primes, Elapsed time: " + seconds.ToString() + " seconds");
           PrintLn("(goal is < 1 second)");
       //}
   }
   
}
