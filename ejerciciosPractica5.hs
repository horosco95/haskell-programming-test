
data Animal= Raton {nombre :: String, edad :: Float, peso :: Float, enfermedades :: [String]} deriving Show

-- Ejemplo de raton
cerebro :: Animal
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

--Ejercicio 1
modificarNombre funcion unRaton = unRaton { nombre = funcion.nombre $ unRaton} 

modificarEdad funcion unRaton = unRaton { edad = funcion.edad $ unRaton}

modificarPeso funcion unRaton = unRaton { peso= funcion.peso $ unRaton}

modificarEnfermedad funcion unRaton = unRaton {enfermedades = funcion.enfermedades $ unRaton}

--Ejercicio 2
hierbaBuena unRaton = modificarEdad sqrt unRaton

hierbaVerde unaEnfermedad unRaton = modificarEnfermedad (filter (/= unaEnfermedad )) unRaton

alcachofa unRaton = modificarPeso (pierdePeso) unRaton
pierdePeso num | num >2 = num-0.1*num
                |otherwise= num-0.05*num

hierbaMagica  = modificarEdad (const 0).modificarEnfermedad (const [])

--Ejercicio 3
--a
medicamento hierbas raton = foldl (\unRaton unaHierba -> unaHierba unRaton) raton hierbas

medicamento' hierbas raton = foldl (flip ($)) raton hierbas
--b
antiAge raton = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) raton
--antiAge (Raton "bicenterata" 256.0 0.2 [])