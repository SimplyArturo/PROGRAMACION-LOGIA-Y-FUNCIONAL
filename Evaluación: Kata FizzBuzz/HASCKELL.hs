module Main where

import Data.List (intercalate)

-- Función para determinar si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    | n <= 1 = False
    | otherwise = not $ any (\x -> n mod x == 0) [2..intSqrt n]
    where
        intSqrt = floor . sqrt . fromIntegral

-- Función para convertir un número a palabras en español
numeroEnPalabras :: Int -> String
numeroEnPalabras n
    | n == 0 = "cero"
    | n < 20 = menosQue20 n
    | n < 100 = decenas (n div 10) ++ if n mod 10 /= 0 then " y " ++ menosQue20 (n mod 10) else ""
    | n < 1000 = centenas (n div 100) ++ if n mod 100 /= 0 then " " ++ numeroEnPalabras (n mod 100) else ""
    | n < 1000000 = miles (n div 1000) ++ if n mod 1000 /= 0 then " " ++ numeroEnPalabras (n mod 1000) else ""
    | otherwise = "Número fuera de rango"
  where
    miles :: Int -> String
    miles n
        | n == 1 = "mil"
        | n < 20 = menosQue20 n ++ " mil"
        | n < 100 = decenas (n div 10) ++ if n mod 10 /= 0 then " y " ++ menosQue20 (n mod 10) ++ " mil" else " mil"
        | n < 1000 = centenas (n div 100) ++ if n mod 100 /= 0 then " " ++ numeroEnPalabras (n mod 100) ++ " mil" else " mil"
        | otherwise = error "Número fuera de rango"

    menosQue20 :: Int -> String
    menosQue20 n
        | n == 1 = "uno"
        | n == 2 = "dos"
        | n == 3 = "tres"
        | n == 4 = "cuatro"
        | n == 5 = "cinco"
        | n == 6 = "seis"
        | n == 7 = "siete"
        | n == 8 = "ocho"
        | n == 9 = "nueve"
        | n == 10 = "diez"
        | n == 11 = "once"
        | n == 12 = "doce"
        | n == 13 = "trece"
        | n == 14 = "catorce"
        | n == 15 = "quince"
        | n == 16 = "dieciséis"
        | n == 17 = "diecisiete"
        | n == 18 = "dieciocho"
        | n == 19 = "diecinueve"
        | otherwise = error "Número fuera de rango"

    decenas :: Int -> String
    decenas n
        | n == 2 = "veinte"
        | n == 3 = "treinta"
        | n == 4 = "cuarenta"
        | n == 5 = "cincuenta"
        | n == 6 = "sesenta"
        | n == 7 = "setenta"
        | n == 8 = "ochenta"
        | n == 9 = "noventa"
        | otherwise = error "Número fuera de rango"

    centenas :: Int -> String
    centenas n
        | n == 1 = "ciento"
        | n == 2 = "doscientos"
        | n == 3 = "trescientos"
        | n == 4 = "cuatrocientos"
        | n == 5 = "quinientos"
        | n == 6 = "seiscientos"
        | n == 7 = "setecientos"
        | n == 8 = "ochocientos"
        | n == 9 = "novecientos"
        | otherwise = error "Número fuera de rango"

-- Función principal
fizzbuzz :: Int -> String
fizzbuzz n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numeroEnPalabras n

-- Función de entrada
main :: IO ()
main = mapM_ (putStrLn . fizzbuzz) [0..1000000]