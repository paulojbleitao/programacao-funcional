{-
Neste exercicio voce vai implementar um menu que permite o usuario escolher
a sua versao de multiconjunto implementada (multiset) com lista ou map.

Primeiramente o menu deve oferecer escolha de que implementação testar. depois ele
deve oferecer opcoes de invocar cada funcao presente no multiset e executa-la
quando escolhida.

Duas funcionalidades devem sempre estar presentes para cada estrutura: a impressão da estrutura em si
para que ela seja visualizada, e uma funcao que ordena os elementos da estrutura por chave/valor ou pela
quantidade de ocorrencias. No caso da função de ordenação ser invocada, ela deve imprimir a estrutura
apos a ordenação.
-}

import qualified MultisetList
import qualified MultisetMap
import qualified Data.List as List
import qualified Data.Map as Map

main :: IO()
main = do
    putStrLn "Escolha a implementação de Multiset desejada (map ou list)"
    putStr "> "
    option <- getLine
    chooseImplementation option

chooseImplementation :: String -> IO()
chooseImplementation "list" = listOptions []
chooseImplementation "map" = undefined
chooseImplementation _ = do
    putStrLn "Opção inválida, tente novamente."
    main

printFunctions :: IO()
printFunctions = do
    putStrLn "Escolha uma operação: "
    putStrLn "- insert"
    putStrLn "- remove"
    putStrLn "- search"
    putStrLn "- union"
    putStrLn "- intersection"
    putStrLn "- difference"
    putStrLn "- inclusion"
    putStrLn "- sum"
    putStrLn "- size"
    putStrLn "- print"
    putStrLn "- sort"
    putStrLn "- quit"

listOptions :: [(Int, Int)] -> IO()
listOptions list = do
    printFunctions
    putStr "> "
    function <- getLine
    callListFunction function list

callListFunction :: String -> [(Int, Int)] -> IO()
callListFunction "insert" list = do
    result <- simpleOperation MultisetList.insert list
    listOptions result
callListFunction "remove" list = do
    result <- simpleOperation MultisetList.remove list
    listOptions result
callListFunction "search" list = do
    simpleOperation MultisetList.search list
    listOptions list
callListFunction "union" list = do
    result <- setOperation MultisetList.union list
    listOptions result
callListFunction "intersection" list = do
    result <- setOperation MultisetList.intersection list
    listOptions result
callListFunction "difference" list = do
    result <- setOperation MultisetList.minus list
    listOptions result
callListFunction "inclusion" list = do
    result <- setOperation MultisetList.inclusion list
    listOptions list
callListFunction "sum" list = do
    result <- setOperation MultisetList.sum list
    listOptions result
callListFunction "size" list = do
    print $ MultisetList.size list
    listOptions list
callListFunction "print" list = do
    print list
    listOptions list
callListFunction "sort" list = do
    let result = List.sortBy (\x y -> fst x `compare` fst y) list
    print result
    listOptions result
callListFunction "quit" _ = do
    putStrLn ". + . * . *"
    putStrLn "*. Até * +"
    putStrLn ". * mais . *."
    putStrLn ". * + . * ."
callListFunction _ list = do
    putStrLn "Opção inválida, tente novamente."
    listOptions list

simpleOperation :: (Show b, Read t) => (t -> t1 -> b) -> t1 -> IO b
simpleOperation f list = do
    putStrLn "Qual número?"
    putStr "> "
    element <- getLine
    let result = f (read element) list
    print result
    return result

setOperation :: (Show b, Read t) => (t1 -> t -> b) -> t1 -> IO b
setOperation f list = do
    putStrLn "Com qual multiset?"
    putStr "> "
    multiset <- getLine
    let result = f list (read multiset)
    print result
    return result
