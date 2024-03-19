filtrarLista :: (a -> Bool) -> [a] -> [a]
filtrarLista _ [] = [] -- Caso base: si la lista está vacía, devuelve una lista vacía
filtrarLista f (x:xs)
    | f x       = x : filtrarLista f xs -- Si f x es True, añade x a la lista resultante y continua filtrando el resto de la lista
    | otherwise = filtrarLista f xs     -- Si f x es False, no añade x a la lista resultante pero continua filtrando el resto de la lista

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

longitudMayorA5 :: String -> Bool
longitudMayorA5 s = length s > 5

esPositivo :: Int -> Bool
esPositivo n = n > 0

main :: IO ()
main = do
    let listaNumeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let listaPares = filtrarLista esPar listaNumeros
    let listaCadenas = ["Haskell", "es", "genial", "y", "muy", "poderoso"]
    let listaLongitudMayorA5 = filtrarLista longitudMayorA5 listaCadenas
    let listaNegativos = filtrarLista esPositivo listaNumeros
    putStrLn "Lista de números:"
    print listaNumeros
    putStrLn "Lista filtrada de números pares:"
    print listaPares
    putStrLn "Lista de cadenas:"
    print listaCadenas
    putStrLn "Lista filtrada de cadenas con longitud mayor a 5:"
    print listaLongitudMayorA5
    putStrLn "Lista filtrada de números negativos:"
    print listaNegativos
