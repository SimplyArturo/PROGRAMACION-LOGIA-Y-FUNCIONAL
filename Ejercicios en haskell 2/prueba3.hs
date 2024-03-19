asignarCalificaciones :: [Int] -> [String]
asignarCalificaciones = map asignarCalificacion
    where
        asignarCalificacion nota
            | nota >= 95 && nota <= 100 = "Excelente"
            | nota >= 85 && nota <= 94  = "Notable"
            | nota >= 75 && nota <= 84  = "Bueno"
            | nota >= 70 && nota <= 74  = "Suficiente"
            | otherwise                 = "Desempeño insuficiente"

main :: IO ()
main = do
    let notas = [98, 85, 80, 72, 60, 100, 78]
    let calificaciones = asignarCalificaciones notas
    putStrLn "Notas:"
    print notas
    putStrLn "Calificaciones correspondientes:"
    print calificaciones
