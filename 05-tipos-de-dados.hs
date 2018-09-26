--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq, Show)

tripleFst :: (Triple a b c) -> a
tripleFst (Triple x _ _) = x

tripleSnd :: (Triple a b c) -> b
tripleSnd (Triple _ y _) = y

tripleThr :: (Triple a b c) -> c
tripleThr (Triple _ _ z) = z

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quad a a b b deriving (Eq, Show)

firstTwo :: (Quadruple a b) -> (a, a)
firstTwo (Quad x y _ _) = (x, y)

secondTwo :: (Quadruple a b) -> (b, b)
secondTwo (Quad _ _ w z) = (w, z)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a
                    | Tuple2 a b
                    | Tuple3 a b c
                    | Tuple4 a b c d
                    deriving (Eq, Show)

tuple1 :: (Tuple a b c d) -> Maybe a
tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a _) = Just a
tuple1 (Tuple3 a _ _) = Just a
tuple1 (Tuple4 a _ _ _) = Just a

tuple2 :: (Tuple a b c d) -> Maybe b
tuple2 (Tuple2 _ b) = Just b
tuple2 (Tuple3 _ b _) = Just b
tuple2 (Tuple4 _ b _ _) = Just b
tuple2 _ = Nothing

tuple3 :: (Tuple a b c d) -> Maybe c
tuple3 (Tuple3 _ _ c) = Just c
tuple3 (Tuple4 _ _ c _) = Just c
tuple3 _ = Nothing

tuple4 :: (Tuple a b c d) -> Maybe d
tuple4 (Tuple4 _ _ _ d) = Just d
tuple4 _ = Nothing 

-- implementado pelo professor
data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

value :: (BinaryTree a) -> Maybe a
value NIL = Nothing
value (Node a _ _) = Just a

--verifica se uma BT é uma BST
isBST :: Ord a => (BinaryTree a) -> Bool
isBST NIL = True
isBST (Node a NIL NIL) = True
isBST (Node a left NIL) = (Just a > value left) && isBST left
isBST (Node a NIL right) = (Just a < value right) && isBST right
isBST (Node a left right) = (Just a > value left) && (Just a < value right) && isBST left && isBST right

--insere uma nova chave na BST retornando a BST modificada
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x NIL = Node x NIL NIL
insert x (Node a left right)
    | x < a = (Node a (insert x left) right)
    | x > a = (Node a left (insert x right))

--retorna o Node da BST contendo o dado procurado ou entao NIL
search :: Ord a => a -> BinaryTree a -> BinaryTree a
search x NIL = NIL
search x (Node a left right)
    | x == a = (Node a left right)
    | x < a  = search x left
    | x > a  = search x right

--retorna o elmento maximo da BST
maxElement :: Ord a => BinaryTree a -> Maybe a
maxElement NIL = Nothing
maxElement (Node a _ NIL) = Just a
maxElement (Node _ _ right) = maxElement right

--retorna o elemento minimo da BST
minElement :: Ord a => BinaryTree a -> Maybe a
minElement NIL = Nothing
minElement (Node a NIL _) = Just a
minElement (Node _ left _) = minElement left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor = undefined

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined
