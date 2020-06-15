--Parcial FMI
--punto 1

--Representar al TAD Pais
type Recurso = String

data Pais = Pais {
    ingresoPerCapita :: Float,
    activosPublico :: Int,
    activosPrivado :: Int,
    recursosNaturales :: [Recurso],
    deuda :: Float
} deriving (Eq, Show)

--Generar al pais Namibia
namibia :: Pais
namibia = Pais 4140 400000 600000 ["Mineria","Ecoturismo"] 50

--punto 2
--prestarle plata
type Estrategia = Pais -> Pais

prestarPlata :: Float -> Pais -> Pais
prestarPlata cuanto pais = pais {deuda = deuda pais + cobrarIntereses cuanto}

cobrarIntereses :: Float -> Float
cobrarIntereses cuanto = 1.5* cuanto

--reducir x puestos de trabajo del sector publico
reducirPuestos :: Int-> Estrategia
reducirPuestos cantPuestos pais = pais {
    activosPublico = activosPublico pais - cantPuestos,
    ingresoPerCapita = ingresoPerCapita pais * (1-reduccionIngreso cantPuestos)
}

reduccionIngreso :: Int -> Float
reduccionIngreso cantPuestos | cantPuestos >100 = 0.2
                            | otherwise = 0.15

--darle a una empresa afin la explotacion de alguno de los recursos
explotar :: Recurso -> Estrategia
explotar recurso pais = pais {
    recursosNaturales = quitarRecurso recurso $ recursosNaturales pais,
    deuda = deuda pais - 20 
}
quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso lista = filter (/=recurso) lista

--
blindaje :: Estrategia
blindaje pais = (prestarPlata (pbi pais * 0.5) . reducirPuestos 500) pais

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * fromIntegral (poblacionActiva pais)

poblacionActiva :: Pais -> Int
poblacionActiva pais = activosPublico pais + activosPrivado pais

--punto 3
type Receta = [Estrategia]
receta :: Receta
receta = [prestarPlata 2000, explotar "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta
--aplicarReceta receta pais = foldl (\pais f -> f pais) pais receta

--punto 4
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (elem "Petroleo".recursosNaturales)

totalDeuda :: [Pais] -> Float
totalDeuda paises = foldr ((+).deuda) 0 paises

--punto 5
estaOrdenado :: Pais -> [Receta] -> Bool
--estaOrdenado _ [] = False
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas) 
        = revisarPbi receta1 pais <= revisarPbi receta2 pais && estaOrdenado pais (receta2:recetas)

revisarPbi :: Receta -> Pais -> Float
revisarPbi receta pais = pbi.aplicarReceta receta $ pais 

--punto 6
recursosNaturalesInfinitos :: [Recurso]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

--a) que ocurre con la funcion 4a?
--b) ¿y con la 4b?
pruebaInfinita1 = puedenZafar [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
-- no termina nunca, porque trata de buscar "Mineria" entre los recursos
pruebaInfinita2 = totalDeuda [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
-- se puede ya que al no evaluar los recursos solamente suma deuda,
-- esto se logra gracias a la evaluacion diferida, solo se evalua
-- lo que se necesita.
