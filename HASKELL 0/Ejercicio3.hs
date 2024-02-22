Pares :: Int -> [Int]
Pares a = [x | x <- [0..a], even x]

main :: IO ()
main = do
    putStrLn "Ingrese un número:"
    numStr <- getLine
    let res = read numStr :: Int
    putStrLn $ "Lista" ++ show res ++ ": " ++ show (Pares res)
