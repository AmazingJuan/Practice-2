numAString = show

cadenaAEntero = read 

obtenerFactores num = filter (\x -> mod num x == 0) [1 .. div num 2]

aliquout num = sum (obtenerFactores num)

borrarElementos n = drop n

cogerElementos n = take n

obtenerLista inicio fin numero = cogerElementos fin (borrarElementos inicio (numAString numero))

obtenerPeriodo num = "20" ++ cogerElementos 2 lista ++ "-" ++ cogerElementos 1 (borrarElementos 2 lista)
    where lista = obtenerLista 0 3 num

obtenerEscuela num
    | suma == num = "Engineering"
    | suma > num = "Administrative"
    | otherwise = "Humanities"
    where suma = aliquout num

esPar num 
    | (mod num 2) == 0 = "even"
    | otherwise = "odd"

obtenerNum num ="num" ++ (show num)

addZeros str = 
    if length(str) == 3 then str
    else addZeros("0" ++ str)

main :: IO()
main = do
    numero <- readLn :: IO Int
    let periodo = obtenerPeriodo numero
    let escuela = obtenerEscuela (cadenaAEntero (obtenerLista 3 2 numero))
    let lastNum = cadenaAEntero (addZeros (borrarElementos  5 (show numero)))
    let tipo = esPar lastNum
    let num = obtenerNum lastNum
    putStrLn (periodo ++ " " ++ escuela ++ " " ++ num ++ " " ++ tipo)




