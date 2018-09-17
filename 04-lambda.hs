--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
fix :: (a -> a) -> a
fix f = f (fix f)

pow = fix (\f x y -> if (y == 0) then 1 else x * f x (y - 1))
fatorial = \x -> product [1..x] 
isPrime = \x -> (length $ filter (\y -> x `mod` y == 0) [1..x]) == 2
fib = fix (\f x -> if (x == 1 || x == 2) then 1 else f (x - 1) + f (x - 2))
mdc = fix (\f x y -> if (x == 0) then y else if (y == 0) then x else f y (x `mod` y))
mmc x y = \x y -> x * y `div` mdc x y
coprimo = \x y -> mdc x y == 1
goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast = fix (\f xs -> if (length xs == 0) then error "Lista vazia!" else if (length xs == 1) then head xs else f $ tail xs)
penultimo = fix (\f xs -> if (length xs == 0 || length xs == 1) then error "Lista sem penultimo" else if (length xs == 2) then head xs else f $ tail xs)
elementAt = fix(\f i xs -> if (i == 1) then head xs else f (i - 1) (tail xs))
meuLength = \xs -> sum [1 | x <- xs]
meuReverso = \xs -> if (length xs == 0) then [] else meuReverso (tail xs) ++ [head xs]

isPalindrome :: [Int] -> Bool
isPalindrome = \xs -> xs == (meuReverso xs)

compress :: [Int] -> [Int]
compress = fix (\f xs -> if (length xs == 0) then [] else if (head xs `elem` tail xs) then f (tail xs) else [head xs] ++ f (tail xs))

compact :: [Int] -> [Int]
compact = fix (\f xs -> if (length xs == 0) then [] else filter (== head xs) xs ++ f (filter (/= head xs) (tail xs)))

encode :: [Int] -> [(Int, Int)]
encode = fix (\f xs -> if (length xs == 0) then [] else [(head xs, length $ filter (== head xs) xs)] ++ f (filter (/= head xs) (tail xs)))

split = \xs i-> [take i xs, drop i xs]
slice = \xs imin imax -> [xs !! i | i <- [(imin - 1)..(imax - 1)]]
insertAt = \el pos xs -> (split xs (pos - 1)) !! 0 ++ [el] ++ (split xs (pos - 1)) !! 1

sort xs = undefined

mySum :: [Int] -> Int
mySum = \xs -> foldr (+) 0 xs

maxList :: [Int] -> Int
maxList = \(x:xs) -> foldr max x (x:xs)

buildPalindrome :: [a] -> [a]
buildPalindrome = fix (\f xs -> if (length xs == 0) then [] else [head xs] ++ f (tail xs) ++ [head xs])

mean :: [Int] -> Int
mean = \xs -> sum xs `div` length xs

myAppend :: [a] -> [a] -> [a]
myAppend = \xs ys -> foldr (:) ys xs
