data Postulante = UnPostulante {nombre :: String, edad :: Int, remuneracion :: Float, conocimientos :: [String]} | Estudiante {legajo :: String, conocimientos:: [String]} deriving Show 

pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto González" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno::Nombre
apellidoDueno = "González"

-- 1 a
type Requisito = Postulante -> Bool
tieneConocimientos :: Puesto -> Requisito
tieneConocimientos puesto postulante = all (\unConociPuesto -> elem unConociPuesto (conocimientos postulante)) (conocimientoRequeridos puesto)

-- 1 b
edadAceptable :: Int -> Int -> Postulante -> Bool
edadAceptable edadMin edadMax postulante = edadMin <= edad postulante && edad postulante <= edadMax

-- 1 c
sinArreglo :: Postulante -> Bool
sinArreglo postulante =  ((apellidoDueno /=).last.words.nombre) postulante

-- 2 a
cumplenRequisitos :: [Postulante] -> [Requisito] -> [Postulante]
cumplenRequisitos postulantes requisitos = filter (cumpleTodosLosRequisitos requisitos) postulantes

cumpleTodosLosRequisitos :: [Requisito] -> Postulante -> Bool
cumpleTodosLosRequisitos requisitos unPostulante = all (\unRequisito -> unRequisito unPostulante)  requisitos
-- 2 b
--cumplenRequisitos [pepe, tito] [sinArreglo, edadAceptable 30 40, tieneConocimientos jefe, (\postulante -> not. elem "repetir logica". conocimientos $ postulante) ]

-- 3
incrementarEdad :: Postulante -> Postulante
incrementarEdad postulante= postulante {edad = edad postulante +1}

aumentarSueldo :: Float -> Postulante -> Postulante
aumentarSueldo porcentaje postulante = postulante {remuneracion = nuevoSueldo porcentaje postulante }

nuevoSueldo :: Float -> Postulante -> Float
nuevoSueldo porcentaje postulante = remuneracion postulante+((remuneracion postulante) * porcentaje) /100

-- a
actualizarPostulantes :: [Postulante] -> [Postulante]
actualizarPostulantes postulantes = [ aumentarSueldo 27.incrementarEdad $ postulante | postulante <- postulantes]

-- b
actualizarPostulantes' :: [Postulante] -> [Postulante]
actualizarPostulantes' = map (aumentarSueldo 27.incrementarEdad) 

postulantesInfinitos :: Postulante -> [Postulante]
postulantesInfinitos unPostulante = unPostulante : postulantesInfinitos unPostulante
{-
Es mas conveniente la 2da ya que es mas declarativa la definicion de funcion
que la 1ra, se delegan funciones (en este caso al map).
Se puede interpretar mejor la funcion.


Si la lista es infinita no termina
>actualizarPostulantes postulantesInfinitos tito
No termina
-}

-- 4 a
capacitar' :: Postulante -> String -> Postulante
capacitar' postulante conocimiento = postulante {conocimientos = (conocimientos postulante) ++ [conocimiento]}

-- 4 b
capacitar :: Postulante -> String -> Postulante
capacitar postulante conocimiento = postulante {conocimientos = aprender postulante conocimiento}

aprender :: Postulante -> String -> [String]
aprender (UnPostulante _ _ _ conocimientos) unConocimiento = agregarConocimiento conocimientos unConocimiento
aprender (Estudiante _ conocimientos) unConocimiento = agregarConocimiento (take (length conocimientos -1) conocimientos) unConocimiento

agregarConocimiento conocimientos unConocimiento = conocimientos ++ [unConocimiento]

-- 4 c
capacitacion :: Puesto -> Postulante -> Postulante
capacitacion unPuesto postulante = foldl capacitar postulante (conocimientoRequeridos unPuesto)