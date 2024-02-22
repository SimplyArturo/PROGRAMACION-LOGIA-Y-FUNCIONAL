Pares :: [Int] -> [Int]
Pares [] = []                   
Pares (x:xs) 
  | even x     = x : Pares xs   
  | otherwise  = Pares xs   

main :: IO ()
main = do
    let lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    putStrLn $ "La lista original es: " ++ show lista
    putStrLn $ "La lista con números pares es: " ++ show (Pares lista)
