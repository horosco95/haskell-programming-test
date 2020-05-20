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
