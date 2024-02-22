Elementos :: [Int] -> [Int]
Elementos = map (*2)

main :: IO ()
main = do
    let lista = [1, 2, 3, 4, 5]
    putStrLn $ "La lista original es: " ++ show lista
    putStrLn $ "La lista con elementos duplicados es: " ++ show (Elementos lista)
