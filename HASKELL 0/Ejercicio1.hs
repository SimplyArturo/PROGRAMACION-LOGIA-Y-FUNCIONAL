sumar :: [Int] -> Int
sumar [] = 0
sumar (x:xs) = x + sumar xs

main :: IO ()
main = do
    let suma= [1, 2, 3, 4, 5]
    putStrLn $ "La suma es:  " ++ show (sumar suma)
