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
import qualified Data.Map as Map

main :: IO()
main = do
    putStrLn "Escolha a implementação de Multiset desejada (map ou list)"
    putStr "> "
    option <- getLine
    putStrLn option