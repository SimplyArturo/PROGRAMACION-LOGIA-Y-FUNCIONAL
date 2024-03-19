import Text.Printf

calcularFuncion :: String -> Double -> [(Int, Double)]
calcularFuncion "seno" valor = [(x, sin (fromIntegral x)) | x <- [1..round valor]]
calcularFuncion "coseno" valor = [(x, cos (fromIntegral x)) | x <- [1..round valor]]
calcularFuncion "tangente" valor = [(x, tan (fromIntegral x)) | x <- [1..round valor]]
calcularFuncion "exponencial" valor = [(x, exp (fromIntegral x)) | x <- [1..round valor]]
calcularFuncion "logaritmo" valor = [(x, log (fromIntegral x)) | x <- [1..round valor]]
calcularFuncion _ _ = error "Función no válida"

imprimirTabla :: [(Int, Double)] -> IO ()
imprimirTabla tabla = mapM_ (\(x, y) -> printf "%d -> %.6f\n" x y) tabla

main :: IO ()
main = do
    putStrLn "Calculadora Científica"
    putStrLn "Funciones disponibles: seno, coseno, tangente, exponencial, logaritmo"
    putStrLn "Ingrese la función a aplicar:"
    funcion <- getLine
    putStrLn "Ingrese el valor:"
    valor <- readLn :: IO Double
    let resultado = calcularFuncion funcion valor
    putStrLn "Tabla de resultados:"
    imprimirTabla resultado
