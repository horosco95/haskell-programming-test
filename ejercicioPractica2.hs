import Text.Show.Functions

--Tipos propios, Contructores - Funciones de Orden Superior
--Ejercicio 1
data Empleado= Comun {nombre::String, sueldoBasico::Double} |
            Jefe {nombre::String, sueldoBasico::Double, cantACargo::Double}

sueldo::Empleado->Double
sueldo (Comun _ salario) = salario
sueldo (Jefe _ salario cantidad) = salario + plus cantidad

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

cumplirAños::Persona-> Persona
cumplirAños alumno = alumno {edad = edad alumno +1}