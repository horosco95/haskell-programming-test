--import Text.Show.Functions

--Tipos propios, Contructores - Funciones de Orden Superior
--Ejercicio 1
data Empleado= Comun {nombre::String, sueldoBasico::Double} |
            Jefe {nombre::String, sueldoBasico::Double, cantACargo::Double}

sueldo::Empleado->Double
sueldo (Comun _ salario) = salario
sueldo (Jefe _ salario cantidad) = salario + plus cantidad

plus :: Num a => a -> a
plus cantidad = 500*cantidad

--Ejercicio 2
data Bebida = Cafe {nombreBebida::String, azucar::Integer}|
            Gaseosa {sabor::String, azucar::Integer}

esEnergizante::Bebida->Bool
esEnergizante (Cafe "capuchino" _)= True
esEnergizante (Gaseosa "pomelo" cantAzucar)= cantAzucar>10 -- Evalua si cantidad de azucar es >10
esEnergizante _ = False

--Ejemplo:
type Nombre = String
type Edad = Integer
data Persona = Estudiante {nom::Nombre, edad::Edad} deriving Show
--La funcion deriving Show, permite utilizar los argumentos del Contr.

julia::Persona
julia = Estudiante "Julia" 20

cumplirAnios::Persona-> Persona
cumplirAnios alumno = alumno {edad = edad alumno +1}

--Ejercicio 3
find' :: (c -> Bool) -> [c] -> c
find' f lista = (head.filter f) lista

data Politico = Politico {proyectosPresentados::[String],sueldoPol::Float,edadPolit::Integer} deriving Show
politicos :: [Politico]
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]
--(a)
--find' ((<50).edadPolit) politicos
--(b)
--find' ((>3).length.proyectosPresentados) politicos
--(c)
--find' ((any) ((>3).length)proyectadosPresentados) politicos