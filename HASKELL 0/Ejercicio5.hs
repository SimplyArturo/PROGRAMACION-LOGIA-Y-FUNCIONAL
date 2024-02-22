Lista :: [b] -> [b]
Lista [] = [] 
Lista (x:xs) = Lista xs ++ [x]

main :: IO ()
main = do
    putStrLn "Ingrese una lista utilice espacios para ingresarlo correctamente:"
    input <- getLine
    let res= map read (words input) :: [Int]
    putStrLn $ "Lista invertida: " ++ show (Lista res)
