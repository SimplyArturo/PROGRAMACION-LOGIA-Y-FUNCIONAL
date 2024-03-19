import Data.Char (toUpper)

asignarCalificacion :: Int -> String
asignarCalificacion nota
    | nota >= 95 && nota <= 100 = "Excelente"
    | nota >= 85 && nota <= 94  = "Notable"
    | nota >= 75 && nota <= 84  = "Bueno"
    | nota >= 70 && nota <= 74  = "Suficiente"
    | otherwise                 = "Desempeño insuficiente"

asignarCalificacionesAprobadas :: [(String, Int)] -> [(String, String)]
asignarCalificacionesAprobadas = map (\(asignatura, nota) -> (map toUpper asignatura, asignarCalificacion nota)) . filter (\(_, nota) -> nota >= 70)

main :: IO ()
main = do
    let notasAlumno = [("Matematicas", 80), ("Historia", 65), ("Ciencias", 90), ("Lenguaje", 72)]
    let calificacionesAprobadas = asignarCalificacionesAprobadas notasAlumno
    putStrLn "Calificaciones aprobadas del alumno:"
    print calificacionesAprobadas
