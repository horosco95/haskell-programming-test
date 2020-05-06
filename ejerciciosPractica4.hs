--Ejercicio 1
cantidadDeElementos lista = foldl contar 0 lista
contar sem _ = sem + 1

cantidadDeElementos' lista = foldl (\sem _ -> sem + 1) 0 lista

cantidadDeElementos'' lista = foldr (\_ sem -> sem + 1) 0 lista

--Ejercicio 2
masGastador (cab:cola) = foldl masGasta cab cola

masGasta sem elemento  | snd sem > snd elemento = sem
                    | otherwise = elemento

masGastadorFoldl1 lista = foldl1 masGasta lista

masGastadorFoldr (cab:cola) = foldr (flip masGasta) cab cola

--Ejercicio 3
lista= [("ana",80),("pepe",40),("“juan”",300),("“maria”",120)]

monto list= foldl (\sem (_,gasto) -> sem+gasto) 0 list
monto' list= foldr (\(_,gasto) sem -> sem+gasto) 0 list

--Ejercicio 4
--foldl (\sem f -> f sem)  2 [(3+), (*2), (5+)]

--foldr (\f sem -> f sem)  2 [(3+), (*2), (5+)]

--foldr ($)  2 [(3+), (*2), (5+)]

--foldl (flip ($))  2 [(3+), (*2), (5+)]

--Ejercicio 5
type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data Proyecto = Proy {nombre::Nombre, inversionInicial::InversionInicial,profesionales::Profesionales} deriving Show
proyectos::[Proyecto]
proyectos = [(Proy "red social de arte" 20000 ["ing. en sistemas", "contador"]), (Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"]), (Proy "ventaChurros" 1000 ["cocinero"])]

maximoSegun condicion (cab:cola) = foldl (maximo condicion) cab cola 

maximo cond proyecto otroProyecto | cond proyecto > cond otroProyecto = proyecto
                                | otherwise = otroProyecto

--mayorInversionInic elemento1 elemento2 
--    | inversionInicial elemento1 > inversionInicial elemento2 = elemento1
--    | otherwise = elemento2

{-maximoSegun inversionInicial proyectos
=> Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}

maximoSegun (length.profesionales) proyectos
=> Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]}

maximoSegun (length.words.nombre) proyectos
=> Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
-}
--Con foldr:
maximoSegunFoldr condicion (cab:cola) = foldr (maximo condicion) cab cola 
