factor :: Int -> Int
factor 0 = 1 
factor a= a * factor (a - 1) 

main :: IO ()
main = do
    putStrLn "Ingrese un numero:"
    numStr <- getLine
    let res= read numStr :: Int
    putStrLn $ "El factorial de " ++ show num ++ " es: " ++ show (factor res)
