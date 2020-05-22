import Text.Show.Functions
type Desgaste = Float
type Patente = String
type Fecha =(Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show

vehiculo1 :: Auto
vehiculo1 = Auto {
 patente = "AT001LN",
 desgasteLlantas= [0.5, 0.15, 0.31, 0.45] ,
 rpm = 2050,
 temperaturaAgua = 60,
 ultimoArreglo = (12,9,2016)
}

vehiculo2 :: Auto
vehiculo2 = Auto {
 patente = "DJV214",
 desgasteLlantas= [0.51, 0.23, 0.28, 0.33] ,
 rpm = 1850,
 temperaturaAgua = 80,
 ultimoArreglo = (12,9,2015)
}
vehiculo3 :: Auto
vehiculo3 = Auto {
 patente = "DJV215",
 desgasteLlantas= [0.5, 0.44, 0.66, 0.6] ,
 rpm = 2230,
 temperaturaAgua = 98,
 ultimoArreglo = (12,9,2018)
}

vehiculo4 :: Auto
vehiculo4 = Auto {
 patente = "DFH029",
 desgasteLlantas= [0.51, 0.71, 0.6, 0.75] ,
 rpm = 1700,
 temperaturaAgua = 70,
 ultimoArreglo = (12,9,2014)
}

--Punto 1
costoReparacion :: Auto -> Int
costoReparacion auto | esPatenteNueva.patente $ auto = 12500
                     | estaEntre "DJ" "NB" . patente $ auto = calculoPatental.patente $ auto
                     | otherwise = 15000

estaEntre :: Patente -> Patente -> Patente -> Bool
estaEntre cotaInf cotaSup unaPatente = ((cotaInf<=).take 2) unaPatente && ((cotaSup>=).take 2) unaPatente
esPatenteNueva :: Patente -> Bool
esPatenteNueva  = (==7).length

calculoPatental :: Patente->Int
calculoPatental patenteAuto | terminaEn '4' patenteAuto = ((3000*).length) patenteAuto
                            | otherwise = 20000
terminaEn ::  Char -> Patente -> Bool
terminaEn caracter = (caracter==).last

--Punto 2
--parte 1 - (Integrante A)
esAutoPeligroso::Auto->Bool
esAutoPeligroso  = (>0.5).head.desgasteLlantas 

--parte 2 - (Integrante B)
necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).anio.ultimoArreglo

--Punto 3
type Mecanico = Auto -> Auto
--parte 1 - (Integrante A)
alfa :: Mecanico
alfa vehiculo = vehiculo {rpm= min 2000 (rpm vehiculo) }

bravo :: Mecanico
bravo vehiculo = vehiculo {desgasteLlantas=[0, 0, 0, 0]}

charly :: Mecanico
charly = bravo.alfa

--parte 2 - (Integrante B)
tango :: Mecanico
tango = nada

zulu :: Mecanico
zulu = lima.aguaA90Grados

lima :: Mecanico
lima = cambio2Llantas

nada :: Auto -> Auto
nada vehiculo= vehiculo

aguaA90Grados :: Auto -> Auto
aguaA90Grados vehiculo = vehiculo {temperaturaAgua = 90}

cambio2Llantas :: Auto -> Auto
cambio2Llantas vehiculo = vehiculo {desgasteLlantas = (\[_,_,c,d]->[0,0,c,d]) (desgasteLlantas vehiculo)}

--Punto 4

auto1 = Auto { patente = "FG884NY", desgasteLlantas= [0.1,0.4,0.2,0] , rpm = 1950, temperaturaAgua = 80, ultimoArreglo = (2,5,2017)}
auto2 = Auto { patente = "BKQ647", desgasteLlantas= [0.2,0.5,0.6,0.1] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (27,9,2018)}
auto3= Auto { patente = "OFW321", desgasteLlantas= [0.1,0.1,0.1,0] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (5,1,2020)}

auto4= Auto { patente = "DVB429", desgasteLlantas= [0.1,0.4,0.2,0] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (5,1,2020)}
auto5= Auto { patente = "VBG546", desgasteLlantas= [0.3,0.5,0.6,0.1] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (5,1,2020)}
auto6= Auto { patente = "MDA381", desgasteLlantas= [0.1,0.1,0.1,0] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (5,1,2020)}

auto7= Auto { patente = "DSA429", desgasteLlantas= [0.1,0.4,0.2,0] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (5,1,2020)}

auto8= Auto { patente = "AP002TM", desgasteLlantas= [0.1,0.4,0.2,0.1] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (5,1,2020)}

--listaAutos1 = [auto1,auto2,auto3]

estanOrdenadosTOC::[Auto]->Bool
estanOrdenadosTOC [] = True
estanOrdenadosTOC (a:[]) = odd.(cantidadDesgaste.desgasteLlantas) $ a
estanOrdenadosTOC (a:b:cz) = (odd.cantidadDesgaste.desgasteLlantas $ a) && (even.cantidadDesgaste.desgasteLlantas $ b) && estanOrdenadosTOC cz

cantidadDesgaste::[Desgaste]->Int
--cantidadDesgaste :: (Num a) => [a] -> a
cantidadDesgaste llantasAuto= round.(*10).sum $ llantasAuto

--Punto 5
--listaTec1= [zulu,tango,charly]
--listaTec2= [charly,lima,bravo]

ordenReparacion :: Fecha -> [Mecanico] -> Auto -> Auto
ordenReparacion unaFecha listaTecnicos unAuto= (actualizarFecha unaFecha) (foldl (flip ($)) unAuto listaTecnicos)

actualizarFecha :: Fecha -> Auto -> Auto
actualizarFecha unaFecha unAuto= unAuto {ultimoArreglo = unaFecha} 
--Punto 6
--parte 1 - integrante a
--tecnicosQueDejanEnCondiciones listaTecnicos unAuto = map (not esAutoPeligroso (flip ($)) unAuto) listaTecnicos

--parte 2 - integrante b
auto9 = Auto { patente = "AT001LN", desgasteLlantas= [0.5, 0.15, 0.31, 0.45] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (12,9,2014)}
auto10 = Auto { patente = "DJV215", desgasteLlantas= [0.5, 0.15, 0.31, 0.45] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (12,9,2018)}
auto11 = Auto { patente = "DJV214", desgasteLlantas= [0.5, 0.15, 0.31, 0.45] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (12,9,2017)}
auto12 = Auto { patente = "DFH029", desgasteLlantas= [0.5, 0.15, 0.31, 0.45] , rpm = 2050, temperaturaAgua = 60, ultimoArreglo = (12,9,2015)}

--listaAutos2 = [auto9,auto10,auto11,auto12]

costoReparacionAutosConRevision :: [Auto] -> Int
costoReparacionAutosConRevision listaAutos = sumarCostoReparacionPorAuto.filtrarAutosNecesitenRevision $ listaAutos

sumarCostoReparacionPorAuto :: [Auto] -> Int
sumarCostoReparacionPorAuto = sum.map costoReparacion

filtrarAutosNecesitenRevision :: [Auto] -> [Auto]
filtrarAutosNecesitenRevision = filter (necesitaRevision)
--punto 7

--parte 2
autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0

autosInfinitos' :: Float -> [Auto]
autosInfinitos' n = Auto {
 patente = "AAA000",
 desgasteLlantas = [n, 0, 0, 0.3],
 rpm = 1500 + round n,
 temperaturaAgua = 90,
 ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)
{-
Ejemplo:

>costoReparacionAutosConRevision autosInfinitos
NO Termina
Como autosInfinitos genera una lista ilimitada de autos, al aplicar la
funcion costoReparacionAutosConRevision este irá calculando el costo 
de Reparacion por cada auto de la lista que necesite Revision, dicho 
proceso tambien es infinito. Por lo cual al aplicar la funcion sum 
sobre la lista resultante TAMPOCO termina de evaluar.

Es posible modificar la funcion para que tome los primeros N elementos
de la lista:
-}
costoReparacionAutosConRevisionPrimerosN n listaAutos = sumarCostoReparacionPorAuto.take n.filtrarAutosNecesitenRevision $ listaAutos

{-
Esta version si acepta lista infinita de autos ya que se encuentra limitado
por la funcion take n, gracias a que Haskell utiliza Lazy Evaluation, por
lo cual toma los primeros n elementos de la lista que se genera.
Cuando se alcancen los n elementos de la lista infinita se aplica las
funcion take n.
-}