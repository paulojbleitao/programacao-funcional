data Part = AM | PM
    deriving (Eq, Ord, Show)

data TIME = Local Int Int Part
            | Total Int Int

{-
 - Defina TIME como instância da classe Eq de forma a que a
 - igualdade entre horas seja independente do formato em que hora está guardada.
 -}
instance Eq TIME where
    Local localHour localMin part == Total totalHour totalMin
        | part == AM = (localHour == totalHour) && (localMin == totalMin)
        | otherwise  = (localHour + 12 == totalHour) && (localMin == totalMin)

{-
 - Defina TIME como instância da classe Ord.
 -}
instance Ord TIME where
    Local hour1 min1 part1 `compare` Local hour2 min2 part2
        | hour1 == hour2 && part1 == part2 = min1 `compare` min2
        | part1 == part2                   = hour1 `compare` hour2
        | otherwise                        = part1 `compare` part2
    Total hour1 min1 `compare` Total hour2 min2
        | hour1 == hour2 = min1 `compare` min2
        | otherwise      = hour1 `compare` hour2

{-
 - Defina TIME como instância da classe Show, de modo a que a apresentação dos
 - termos (Local 10 35 AM), (Local 4 20 PM) e (Total 17 30) seja respectivamente:
 - 10:35 am, 4:20 pm e 17h30m.
 -}
instance Show TIME where
    show (Local hour min part) = show hour ++ ":" ++ show min ++ " " ++ show part
    show (Total hour min) = show hour ++ "h" ++ show min ++ "m"

{-
 - Declare TIME como instância da classe Enum, de forma a que succ
 - avance o relógio 1 minuto e pred recue o relógio 1 minuto.
 - Assuma que o sucessor de 11:59 pm é 00:00 am. Depois, faça o
 - interpretador calcular o valor das seguintes expressões:
 - [(Total 10 30)..(Total 10 35)] e [(Total 10 30),(Local 10 35 AM)..(Total 15 20)].
 -}
instance Enum TIME where
    fromEnum = totalMinutos
    toEnum = minutosParaTime
    succ (Total hour min)
        | hour == 23 && min + 1 == 60 = Total 00 00
        | hour < 24 && min + 1 == 60  = Total (hour + 1) 00
        | otherwise                   = Total hour (min + 1)

{-
 - Defina a função totalMinutos :: TIME -> Int que conta
 - o total de minutos de uma dada hora.
 -}
totalMinutos :: TIME -> Int
totalMinutos (Local hour min part)
    | part == AM = hour * 60 + min
    | otherwise  = (12 + hour) * 60 + min
totalMinutos (Total hour min) = hour * 60 + min

minutosParaTime :: Int -> TIME
minutosParaTime min = Total hour restOfMin
    where hour = min `div` 60
          restOfMin = min - (hour * 60)

{-
 - Defina a função seleciona :: TIME -> [(TIME,String)] -> [(TIME,String)] que
 - recebe uma hora e uma lista de horários de cinema, e seleciona os filmes que
 - começam depois de uma dada hora.
 -}
seleciona :: TIME -> [(TIME, String)] -> [(TIME, String)]
seleciona time list = filter (\x -> fst x > time) list
