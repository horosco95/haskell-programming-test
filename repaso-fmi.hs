import Text.Show.Functions
--punto 1
data Pais = UnPais {ingresoPerCapita::Float,deuda::Float,poblacionActiva::PoblacionActiva,recursosNaturales::[String]} deriving Show
type PoblacionActiva= (SectorPublico,SectorPrivado)
type SectorPublico = Float
type SectorPrivado = Float

namibia = UnPais {ingresoPerCapita=4140,deuda=50,poblacionActiva=(450000,600000),recursosNaturales=["mineria","ecoturismo"]}

--punto 2
otorgarPrestamo :: Float -> Pais -> Pais
otorgarPrestamo n unPais= unPais {deuda=incrementarDeuda n unPais}

incrementarDeuda n unPais = (*1.5) n + deuda unPais

reducirPuestosTrabajo :: SectorPublico -> Pais -> Pais
reducirPuestosTrabajo x unPais = reduccionIngresoSegun.menosXPuestosSectorPublico x$ unPais

menosXPuestosSectorPublico x unPais = unPais {poblacionActiva= (\(a,b)-> (a-x,b)).poblacionActiva $ unPais}
puestosSectorPublico (a,_) = a
reduccionIngresoSegun unPais | (>100).puestosSectorPublico.poblacionActiva $ unPais = menosIngreso 0.2 unPais
                        | otherwise = menosIngreso 0.15 unPais

menosIngreso x unPais = unPais {ingresoPerCapita= (*(1-x)).ingresoPerCapita $ unPais}

otorgarExplotacion :: String -> Pais -> Pais
otorgarExplotacion recurso unPais = disminucionDeuda.entregarRecurso recurso $ unPais

entregarRecurso recurso unPais = unPais {recursosNaturales= filter (/=recurso) . recursosNaturales $ unPais}
disminucionDeuda unPais = unPais {deuda= deuda unPais - 2} 

blindaje :: Pais -> Pais
blindaje unPais = reducirPuestosTrabajo 500.otorgarPrestamo ((/2).pbi $ unPais) $ unPais

pbi pais = (/1000000).(*(poblacionActivaTotal (poblacionActiva pais))).ingresoPerCapita $ pais
poblacionActivaTotal (a,b) = a+b

--punto 3
type Receta = [Pais->Pais]
receta1 = [otorgarPrestamo 200,otorgarExplotacion "mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unaReceta pais = foldl (flip ($)) pais unaReceta

{-
Se cumple el efecto colateral debido a que se realiza transformaciones sobre un mismo
pais, generando un pais con las funciones (recetas) aplicadas
-}

--punto 4
puedenZafar paises = filter (elem "Petroleo".recursosNaturales) paises

totalDeudaConFMI paises = sum.map deuda $ paises

{-
orden superior: en puedenZafar (filter)
aplicacion parcial: puedenZafar (elem "Petroleo".recursosNaturales)
composicion: totalDeudaConFMI (sum.map deuda)

La ventaja que hay es que permite abstraerse en como es la funcion, solo interesa
saber para pasarle los argumentos que requiere.
Otra ventaja es que termina siendo un codigo mas declarativo y expresivo
-}
--punto 5
--esDeOrdenPeorAMejor _ [receta] = True
--esDeOrdenPeorAMejor unPais (receta1:receta2:otrasRecetas) = receta1 `(esPeorQue unPais)` receta2 && receta2 `esPeorQue unPais` esDeOrdenPeorAMejor otrasRecetas 

--esPeorQue pais a b = pbi (aplicarReceta a pais) < 