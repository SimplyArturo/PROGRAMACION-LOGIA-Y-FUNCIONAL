import Data.Char (toLower, isAlpha)

Palindromo :: String -> Bool
Palindromo s = normalized == reverse normalized
  where normalized = map toLower $ filter isAlpha s

main :: IO ()
main = do
    let cadena1 = "Anita lava la tina"
        cadena2 = "Hola mundo"
    putStrLn $ "¿La cadena \"" ++ cadena1 ++ "\" es un palíndromo? " ++ show (Palindromo cadena1)
    putStrLn $ "¿La cadena \"" ++ cadena2 ++ "\" es un palíndromo? " ++ show (Palindromo cadena2)
