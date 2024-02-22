lol :: Int -> Int
lol 0 = 1
lol 1 = 1
lol n = lol (n-1) + lol (n-2)

main :: IO ()
main = do
    let n = 10
    putStrLn $ "El término " ++ show n ++ " de la secuencia de Fibonacci es: " ++ show (lol n)
