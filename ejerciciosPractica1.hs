import Text.Show.Functions

sayHello name= "Hello "++ name ++"!"

--Aprovechando ecuaciones con guardas
signo :: Integer -> Integer
signo 0 = 0
signo nro   | nro>0 = 1
            | otherwise = -1

funcion1 nro = (doble.signo) nro -- Composicion de funciones

maximo :: Integer -> Integer -> Integer
maximo nro otroNro  | nro > otroNro = nro
                    | otherwise = otroNro

--Aprovechando Pattern Matching
esVocal :: Char -> Bool
esVocal 'a'= True
esVocal 'e'= True
esVocal 'i'= True
esVocal 'o'= True
esVocal 'u'= True
esVocal _ = False -- _ implica "para otros valores cualesquiera"

-- Ejercicio 1 - ecuaciones con guardas
calcular :: (Integer, Integer)->(Integer, Integer)
calcular (x,y)  | even x && odd y =(doble x, siguiente y)
                | even x = (doble x, y)
                | odd y = (x,siguiente y)
                | otherwise= (x,y)

doble:: Integer -> Integer
doble nro= nro*2
siguiente:: Int->Int
siguiente = (+1)

{-Ejercicio 2
    Con Pattern Matching-}
and'::Bool->Bool->Bool
and' True True = True
and' _ _ = False

--Con ecuaciones con guardas
and'' :: Bool->Bool->Bool
and'' cond1 cond2   | cond1 = cond2 --puede valer True o False
                    | otherwise = False

or'::Bool-> Bool->Bool
or' True _ = True
or' False expre = expre

--Ejercicio 3
type Nota = Integer -- simil typedef, 1ra letra Mayus.
type Alumno = (String, Nota, Nota, Nota)

notaMaxima::Alumno->Nota
notaMaxima (_,nota1,nota2,nota3) = (max nota1 . max nota2) nota3

notaMaxima'::Alumno->Nota
notaMaxima' (_,nota1,nota2,nota3) = (nota1 `max` nota2) `max` nota3

--Ejercicio 4
cuadruple :: Integer -> Integer
cuadruple nro = doble.doble nro

--Ejercicio 5
--Evaluar si el doble del siguiente de un num sumado 2 es >10 
esMayorA::Integer-> Integer
esMayorA num = ((>10).doble.siguiente.(+2)) num
