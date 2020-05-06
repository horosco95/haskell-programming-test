type Desgaste = Double
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
costoReparacion auto | length (patente auto) == 7 = 12500
                     | "DJ"<= patente auto && patente auto <="NB" = calculoPatental (patente auto)
                     | otherwise = 15000

calculoPatental::Patente->Int
calculoPatental patenteAuto | (('4'==).last) patenteAuto = ((3000*).length) patenteAuto
                            | otherwise = 20000

--Punto 2
--parte 1 - (Integrante A)
esAutoPeligroso::Auto->Bool
esAutoPeligroso  = (>0.5).head.desgasteLlantas 

--parte 2 - (Integrante B)
necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).anio.ultimoArreglo

--Punto 3
--parte 1 - (Integrante A)
type ServicioMecanico = Auto -> Auto
alfa :: ServicioMecanico
alfa vehiculo = vehiculo {rpm= min 2000 (rpm vehiculo) }

bravo :: ServicioMecanico
bravo vehiculo = vehiculo {desgasteLlantas=[0, 0, 0, 0]}

charly :: ServicioMecanico
charly = bravo.alfa
--parte 2 - (Integrante B)
tango :: ServicioMecanico
tango vehiculo= vehiculo

zulu :: ServicioMecanico
zulu vehiculo = lima (vehiculo {temperaturaAgua = 90})

lima :: ServicioMecanico
lima vehiculo = vehiculo {desgasteLlantas = (\[_,_,c,d]->[0,0,c,d]) (desgasteLlantas vehiculo)}
