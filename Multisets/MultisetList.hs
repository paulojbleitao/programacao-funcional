module MultisetList (
    MultisetList.insert,
    MultisetList.remove,
    MultisetList.search,
    MultisetList.union,
    MultisetList.intersection,
    MultisetList.minus,
    MultisetList.inclusion,
    MultisetList.sum,
    MultisetList.size
)
    where

{-
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde
 - cada elemento da lista consiste do dado em si e sua quantidade (um par).
 - Eh recomendavel que voce consulte a documentacao de Data.List
-}
import Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
-}
insert :: Int -> [(Int, Int)] -> [(Int, Int)]
insert element bag
    | element `elem` map fst bag = incrementExistent element bag
    | otherwise                  = bag ++ [(element, 1)]

incrementExistent :: Int -> [(Int, Int)] -> [(Int, Int)]
incrementExistent element [] = error "Lista vazia!"
incrementExistent element (x:xs)
    | element == fst x = (element, (snd x) + 1):xs
    | otherwise        = [x] ++ incrementExistent element xs

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura.
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove :: Int -> [(Int, Int)] -> [(Int, Int)]
remove element [] = error "Lista vazia!"
remove element (x:xs)
    | element == fst x && snd x > 1  = [(element, (snd x) - 1)] ++ xs
    | element == fst x && snd x == 1 = xs
    | otherwise                      = [x] ++ remove element xs
{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search :: Int -> [(Int, Int)] -> Int
search element [] = 0
search element (x:xs)
    | element == fst x = snd x
    | otherwise        = search element xs

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
union bag1 [] = bag1
union [] bag2 = bag2
union bag1 (x:xs)
    | fst x `elem` map fst bag1 = if (quantityInFirstBag > snd x)
                                  then [(fst x, quantityInFirstBag)] ++ MultisetList.union restOfBag1 xs
                                  else [x] ++ MultisetList.union restOfBag1 xs
    | otherwise                 = [x] ++ MultisetList.union restOfBag1 xs
    where quantityInFirstBag = search (fst x) bag1
          restOfBag1 = filter (\y -> fst y /= fst x) bag1

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
intersection _ [] = []
intersection [] _ = []
intersection bag1 (x:xs)
    | fst x `elem` map fst bag1 = if (quantityInFirstBag > snd x)
                                  then [x] ++ intersection bag1 xs
                                  else [(fst x, quantityInFirstBag)] ++ intersection bag1 xs
    | otherwise                 = intersection bag1 xs
    where quantityInFirstBag = search (fst x) bag1

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B).
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag.
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
minus [] _ = []
minus bag1 [] = bag1
minus (x:xs) bag2
    | fst x `elem` map fst bag2 = if (differenceBetweenQuantities > 0)
                                  then [(fst x, differenceBetweenQuantities)] ++ minus xs bag2
                                  else minus xs bag2
    | otherwise                 = [x] ++ minus xs bag2
    where differenceBetweenQuantities = snd x - (search (fst x) bag2)

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion :: [(Int, Int)] -> [(Int, Int)] -> Bool
inclusion [] _ = True
inclusion _ [] = False
inclusion (x:xs) bag2
    | fst x `elem` map fst bag2 = if (differenceBetweenQuantities <= 0)
                                  then inclusion xs bag2
                                  else False
    | otherwise                 = False
    where differenceBetweenQuantities = snd x - (search (fst x) bag2)

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas.
-}
sum :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
sum bag1 [] = bag1
sum [] bag2 = bag2
sum (x:xs) bag2 = [(fst x, snd x + search (fst x) bag2)] ++ MultisetList.sum xs restOfBag2
    where restOfBag2 = filter (\y -> fst y /= fst x) bag2

{-
 - Retorna a quantidade total de elementos no Bag
-}
size :: [(Int, Int)] -> Int
size [] = 0
size ((element, quantity):tail) = quantity + size tail