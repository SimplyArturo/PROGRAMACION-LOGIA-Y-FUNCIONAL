div :: Int -> [Int]
div a = divD a 1

divD :: Int -> Int -> [Int]
divD a b
  | a == b    = [a]
  | a > b && a `mod` b == 0 = m : (divD a (b+1))
  | a > b && a `mod` b /= 0  = divD a (b+1)

main :: IO ()
main = do
    let num = 20
    putStrLn $ "Los divisores de " ++ show num ++ " son: " ++ show (div num)
