module MultisetMap ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
 - cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
 - Eh recomendavel que voce consulte a documentacao de Data.Map
 -}
import Data.Map.Strict as Map
import Data.Maybe

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert :: Int -> Map Int Int -> Map Int Int
insert elem bag
    | quantity /= Nothing = Map.update (\x -> Just (x + 1)) elem bag
    | otherwise           = Map.insert elem 1 bag
    where quantity = Map.lookup elem bag

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove :: Int -> Map Int Int -> Map Int Int
remove elem bag
    | quantity == Nothing   = bag
    | quantityMinusOne <= 0 = Map.delete elem bag
    | otherwise             = Map.update (\x -> Just quantityMinusOne) elem bag
    where quantity = Map.lookup elem bag
          quantityMinusOne = fromJust quantity - 1
{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search :: Int -> Map Int Int -> Int
search elem bag
    | quantity /= Nothing = fromJust quantity
    | otherwise           = 0
    where quantity = Map.lookup elem bag

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union :: Map Int Int -> Map Int Int -> Map Int Int
union bag1 bag2 = unionWith union' bag1 bag2

union' :: Int -> Int -> Int
union' e1 e2
    | e1 >= e2  = e1
    | otherwise = e2

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection :: Map Int Int -> Map Int Int -> Map Int Int
intersection bag1 bag2 = intersectionWith intersection' bag1 bag2

intersection' :: Int -> Int -> Int
intersection' e1 e2
    | e1 >= e2  = e2
    | otherwise = e1

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus :: Map Int Int -> Map Int Int -> Map Int Int
minus bag1 bag2 = differenceWith minus' bag1 bag2

minus' :: Int -> Int -> Maybe Int
minus' e1 e2
    | difference > 0 = Just difference
    | otherwise      = Nothing
    where difference = e1 - e2

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion :: Map Int Int -> Map Int Int -> Bool
inclusion bag1 bag2 = isSubmapOfBy inclusion' bag1 bag2

inclusion' :: Int -> Int -> Bool
inclusion' e1 e2
    | e1 <= e2  = True
    | otherwise = False

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum :: Map Int Int -> Map Int Int -> Map Int Int 
sum bag1 bag2 = unionWith (+) bag1 bag2

{-
 - Retorna a quantidade total de elementos no Bag
-}
size :: Map Int Int -> Int
size bag = Map.foldr (+) 0 bag
