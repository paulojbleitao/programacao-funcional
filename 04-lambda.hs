--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
fix :: (a -> a) -> a
fix f = f (fix f)

pow = \x y -> if (y == 0) then 1 else x * pow x (y - 1)
fatorial = \x -> product [1..x] 
isPrime = \x -> (length $ filter (\y -> x `mod` y == 0) [1..x]) == 2
fib = \x -> if (x == 1 || x == 2) then 1 else fib (x - 1) + fib (x - 2)
mdc = \x y -> if (x == 0) then y else if (y == 0) then x else mdc y (x `mod` y)
mmc x y = \x y -> x * y `div` mdc x y
coprimo = \x y -> mdc x y == 1
goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast = \xs -> if (length xs == 0) then error "Lista vazia!" else if (length xs == 1) then head xs else meuLast $ tail xs
penultimo = \xs -> if (length xs == 0 || length xs == 1) then error "Lista sem penultimo" else if (length xs == 2) then head xs else penultimo $ tail xs
elementAt i xs = undefined
meuLength xs = undefined
meuReverso xs = undefined
isPalindrome xs = undefined
compress xs = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined