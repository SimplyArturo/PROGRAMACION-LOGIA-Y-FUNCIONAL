Cadena :: String -> Int
Cadena = length

main :: IO ()
main = do
    let long= "Cadena God"
    putStrLn $ "La longitud de la cadena es: " ++ show (Cadena long)
