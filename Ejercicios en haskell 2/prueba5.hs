data Inmueble = Inmueble { año :: Int
                         , metros :: Int
                         , habitaciones :: Int
                         , garaje :: Bool
                         , zona :: Char
                         } deriving (Show)

calcularPrecio :: Inmueble -> Double
calcularPrecio inmueble =
    let precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)
        antiguedad = 2024 - año inmueble -- Suponiendo que el año actual es 2024
        descuento = fromIntegral antiguedad / 100
        precioConDescuento = precioBase * (1 - descuento)
        factorZona = if zona inmueble == 'B' then 1.5 else 1
    in precioConDescuento * factorZona

filtrarPorPresupuesto :: [Inmueble] -> Double -> [Inmueble]
filtrarPorPresupuesto inmuebles presupuesto =
    filter (\inmueble -> calcularPrecio inmueble <= presupuesto) inmuebles

main :: IO ()
main = do
    let listaInmuebles = [ Inmueble 2000 100 3 True 'A'
                         , Inmueble 2012 60 2 True 'B'
                         , Inmueble 1980 120 4 False 'A'
                         , Inmueble 2005 75 3 True 'B'
                         , Inmueble 2015 90 2 False 'A'
                         ]
    let presupuesto = 150000 -- Ejemplo de presupuesto
    let inmueblesEnPresupuesto = filtrarPorPresupuesto listaInmuebles presupuesto
    putStrLn "Inmuebles en presupuesto:"
    mapM_ print inmueblesEnPresupuesto
