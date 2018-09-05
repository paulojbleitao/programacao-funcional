{-
- Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!" 
-}
meuLast :: [Int] -> Int
meuLast [] = error "Lista vazia!"
meuLast [x] = x
meuLast (x:xs) = meuLast xs

{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
-}
penultimo :: [Int] -> Int
penultimo [] = error "Lista sem penultimo"
penultimo [_] = error "Lista sem penultimo"
penultimo [x,_] = x
penultimo (x:xs) = penultimo xs

{-
- Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}
elementAt :: Int -> [Int] -> Int
elementAt 1 (x:_) = x
elementAt i (x:xs) = elementAt (i - 1) xs

{-
- Retorna o tamanho de uma lista. 
-}
meuLength :: [Int] -> Int
meuLength xs = sum [1 | x <- xs]

{-
- Retorna o inverso de uma lista. 
-}
meuReverso :: [Int] -> [Int]
meuReverso [] = []
meuReverso (x:xs) = meuReverso xs ++ [x]

{-
- Diz se uma lista é palindrome. 
-}
isPalindrome :: [Int] -> Bool
isPalindrome xs = xs == meuReverso xs

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}
compress :: [Int] -> [Int]
compress [] = []
compress (x:xs)
    | (x `elem` xs) = compress xs
    | otherwise     = [x] ++ compress xs

{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
compact :: [Int] -> [Int]
compact [] = []
compact (x:xs) = equalElements ++ compact restOfTheElements
    where equalElements     = filter (== x) (x:xs)
          restOfTheElements = filter (/= x) xs

{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
encode :: [Int] -> [(Int, Int)]
encode [] = []
encode (x:xs) = [(x, length equalElements)] ++ encode restOfTheElements
    where equalElements     = filter (== x) (x:xs)
          restOfTheElements = filter (/= x) xs

{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}
split :: [Int] -> Int -> [[Int]]
split xs i = [take i xs, drop i xs]

{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}
slice :: [Int] -> Int -> Int -> [Int]
slice xs imin imax = [xs !! i | i <- [(imin - 1)..(imax - 1)]]

{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt :: Int -> Int -> [Int] -> [Int]
insertAt el pos xs = firstHalf ++ [el] ++ secondHalf
    where splitList  = split xs (pos - 1)
          firstHalf  = splitList !! 0
          secondHalf = splitList !! 1

{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos 
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}
minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

{-
- Calcula a soma de todos os elementos de uma lista usando foldr.
-}
mySum :: [Int] -> Int
mySum xs = foldr (+) 0 xs

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList :: [Int] -> Int
maxList (x:xs) = foldr max x (x:xs)

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}
buildPalindrome :: [a] -> [a]
buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++ [x]

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

mean :: [Int] -> Int
mean xs = sum' xs `div` meuLength xs

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}
myAppend :: [Int] -> [Int] -> [Int]
myAppend xs ys = foldr (:) ys xs
