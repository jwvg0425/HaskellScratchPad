factorial n = product [1..n]
factorial' n = if n == 0 then 1 else n * factorial' (n-1)

fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)