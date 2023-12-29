import time

def Fibo(n):
  if n <= 1 : return n
  else      : return Fibo(n-1) + Fibo(n-2)

def Benchmark(name, func, arg=0, loops=1):
  elapsed = time.time()
  for count in range(loops):
    result = func(arg)
  elapsed = (time.time() - elapsed) / loops
  print("{}({}) = {} in {} seconds".format(name, arg, result, elapsed))

Benchmark("Fibo", Fibo, 24, 10) # Fibo(24) = 46368