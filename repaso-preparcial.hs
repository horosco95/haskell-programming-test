psicosis :: Pelicula
psicosis = UnaPelicula {nombre="Psicosis", duracion=109,genero="Terror",origen="Estados Unidos"}
perfumeDeMujer :: Pelicula
perfumeDeMujer= UnaPelicula "Perfume de Mujer" 150 "Drama" "Estados Unidos" 
elSaborDeLasCervezas :: Pelicula
elSaborDeLasCervezas = UnaPelicula "El sabor de las cervezas" 95 "Drama" "Iran" 
lasTortugasTambienVuelan :: Pelicula
lasTortugasTambienVuelan = UnaPelicula "Las tortugas también vuelan" 103 "Drama" "Iran" 
juan :: Usuario
juan = UnUsuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer] 60

julia = UnUsuario "julia" "estandar" 25 "Argentina" (replicate 22 elSaborDeLasCervezas) 70
--1
data Pelicula = UnaPelicula {nombre::String,duracion::Int,genero::String,origen::String }deriving (Show, Eq)
data Usuario = UnUsuario {nombreU::String,categoria::String,edad::Int,paisResidencia::String,peliculasVistas::[Pelicula],estadoSalud::Int}deriving Show

--2
ver :: Pelicula -> Usuario -> Usuario
ver unaPelicula usuario = usuario { peliculasVistas = peliculasVistas usuario ++ [unaPelicula] }

--3
premiarUsuariosFieles :: [Usuario] -> [Usuario]
premiarUsuariosFieles usuarios = map premiarUsuario usuarios
premiarUsuario :: Usuario -> Usuario
premiarUsuario usuario | esUsuarioFiel usuario = subirCategoria usuario
                    | otherwise = usuario

esUsuarioFiel :: Usuario -> Bool
esUsuarioFiel usuario = (>20).length.filter noEsDeEstadosUnidos.peliculasVistas $ usuario

noEsDeEstadosUnidos :: Pelicula -> Bool
noEsDeEstadosUnidos pelicula = origen pelicula /= "Estados Unidos"

subirCategoria :: Usuario -> Usuario
subirCategoria usuario = usuario {categoria= siguienteCategoria . categoria $ usuario}

siguienteCategoria :: String -> String
siguienteCategoria "basica" = "estandar"
siguienteCategoria "estandar" = "premium"
siguienteCategoria "premium" = "premium"

--4
type Criterio = Pelicula -> Bool
teQuedasteCorto :: Criterio
teQuedasteCorto pelicula = (<35).duracion $ pelicula 
cuestionDeGenero :: [String] -> Criterio
cuestionDeGenero generos pelicula = elem (genero pelicula) generos
cuestionDeGenero' generos pelicula = any (\gen -> (gen==).genero $ pelicula) generos

deDondeSaliste :: String -> Criterio
deDondeSaliste unOrigen pelicula = (==unOrigen).origen $ pelicula 

vaPorEseLado :: (Eq a) => Pelicula -> (Pelicula -> a) -> Criterio
vaPorEseLado unaPelicula f otraPelicula = (f unaPelicula) == (f otraPelicula)

--5
basePeliculas :: [Pelicula]
basePeliculas= [lasTortugasTambienVuelan,psicosis,elSaborDeLasCervezas,perfumeDeMujer]
sugerirPeliculas :: Usuario -> [Criterio] -> [Pelicula]
sugerirPeliculas usuario criterios = take 3 . filter (cumpleCondiciones usuario criterios) $ basePeliculas

cumpleCondiciones :: Usuario -> [Criterio] -> Pelicula -> Bool
cumpleCondiciones usuario criterios pelicula = (not.vioPelicula usuario) pelicula && cumpleCriterios criterios pelicula

vioPelicula :: Usuario -> Pelicula -> Bool
vioPelicula usuario pelicula = elem pelicula (peliculasVistas usuario)

cumpleCriterios:: [Criterio] -> Pelicula -> Bool
cumpleCriterios criterios pelicula = all (\f -> f pelicula) criterios

--Segunda Parte: Maraton de series
--1
data UnCapitulo = CapituloSerie {nombreS::String,generoS::String,duracionS::Int,origenS::String,afectaUsuario::(Usuario->Usuario)} 

--2
consumeSerie :: Usuario -> UnCapitulo -> Usuario
consumeSerie usuario capitulo = (afectaUsuario capitulo) usuario
--3
losSimuladores :: UnCapitulo
losSimuladores = CapituloSerie "Los Simuladores" "Accion" 120 "Argentina" (\(UnUsuario nom cat edad origen peli salud) -> UnUsuario nom cat edad origen peli (salud `div` 2))

--4
maraton :: Usuario -> [UnCapitulo] -> Usuario
maraton usuario serie = foldl consumeSerie usuario serie
--Plus:
mejoraUsuario :: Usuario -> Usuario
mejoraUsuario unUsuario = unUsuario {estadoSalud = estadoSalud unUsuario + 50}

losSimuladoresII :: UnCapitulo
losSimuladoresII = CapituloSerie "Los Simuladores" "Accion" 120 "Argentina" mejoraUsuario
--5: Este programa no termina, por lo que maraton afectaria al usuario sin un fin

--6
--maraton juan (take 3. repeat $ losSimuladores)