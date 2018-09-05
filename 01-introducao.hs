{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor :: Bool -> Bool -> Bool
xor a b
    | and [a, b] = False
    | otherwise  = or [a, b]

impl :: Bool -> Bool -> Bool
impl a b = or [not a, b]

equiv :: Bool -> Bool -> Bool
equiv a b = and [impl a b, impl b a]

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y = x * pow x (y - 1)

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial x = product [1..x]

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime x = (length $ filter (\y -> x `mod` y == 0) [1..x]) == 2

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x 0 = x
mdc 0 y = y
mdc x y = mdc y (x `mod` y)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = x * y `div` mdc x y

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = mdc x y == 1

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [(a, b) | a <- primos, b <- primos, a + b == x]
    where primos = filter isPrime [1..x]
