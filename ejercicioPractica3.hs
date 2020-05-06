import Text.Show.Functions
--Ejercicio 1
type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre::Nombre, notas::Notas} deriving Show
promediosAlumnos:: [Persona]->[(Nombre,Int)]
--promediosAlumnos listaAlum = map (\unAlumno->(nombre unAlumno,(promedio.notas)unAlumno)) listaAlum
promediosAlumnos  = map (\unAlumno->(nombre unAlumno,(promedio.notas)unAlumno))
--Modo point free

promedio listnotas= (sum listnotas) `div` (length listnotas)
--promediosAlumnos [(Alumno "juan" [8,6]), (Alumno "maria"[7,9,4]), (Alumno "ana" [6,2,4])]

--Ejercicio 2
promediosSinAplazos::[Notas]->[Int]
promediosSinAplazos  = map (promedio.filter(>=6)) 
--promediosSinAplazos [[8,6],[6,6,4]]

--Ejercicio 3
aprobo :: Persona->Bool --Dominio Persona -> Imagen Bool
aprobo = all (>=6).notas
--aprobo (Alumno "manuel" [8,6,2,4])

--Ejercicio 4
aprobaron::[Persona]->[Nombre]
aprobaron alumnos = (map nombre.filter aprobo) alumnos 
--aprobaron [Alumno "manuel" [8,6,2,4] , Alumno "elena" [7,9,4,5], Alumno "ana" [6,2,4,2], Alumno "pedro" [9,6,7,10]]

--Ejercicio 5
productos nombres precios = zip nombres precios
productos' nombres precios = zipWith (\nombres precios-> (nombres,precios))
--Con point free:
--productos  = zip 
--productos' nom precio = zipWith (\nom precio-> (nom,precio))

--zipWith (\ x y -> x+y) [2..5] [10..14]

--Ejercicio 6
data Flor= Flor {name :: String , aplicacion :: String , cantidadDeDemanda :: Int } deriving Show
rosa = Flor "rosa" "decorativo" 120
jazmin = Flor "jazmin" "aromatizante" 100
violeta = Flor "violeta" "infusion" 110
orquidea= Flor "orquidea" "decorativo" 150

flores = [orquidea,rosa,violeta,jazmin]

maximoSegun _ [a] = a
maximoSegun condicion (x:xs) = (mayorSegun condicion) x (maximoSegun condicion xs)

mayorSegun cond var1 var2 | cond var1 > cond var2 = var1
                          | otherwise = var2
--a: condicion=cantidadDeDemanda
longitudNombre::Flor->Int
longitudNombre = length.name
restoDivCantDemand4 flor = rem (cantidadDeDemanda flor) 4

--Ejercicio 7 a
lista_comida= ["lechuga","anana","alfajor de chocolate"]
--cantidadVocales =

--f1 lista = map () (filter () lista)