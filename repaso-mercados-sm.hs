import Text.Show.Functions

data Accion = Accion {
    simbolo :: String,
    precios :: [Float]
} deriving Show

data Usuario = Usuario {
    cartera :: Float,
    accionesUsuario :: [TituloAccion]
}
data TituloAccion = TituloAccion {
    simboloTitulo :: String,
    cantidad :: Int,
    precioCompra :: Float
} deriving Show

--punto 2
mapCondicional :: (b -> Bool) -> (b -> b) -> [b] -> [b]
mapCondicional condicion f lista = map (segunCondicion condicion f) lista
segunCondicion :: (p -> Bool) -> (p -> p) -> p -> p
segunCondicion c f elem | c elem = f elem
                        | otherwise = elem
{-
Para que esta funcion tenga sentido, la transformacion tiene que ser de tipo a->a
es decir a puede ser cualquier tipo (excepto Bool y Function) pero tiene que 
devolver del mismo tipo 
-}

encontrar :: (c -> Bool) -> [c] -> c
encontrar condic lista =  head.filter condic $ lista

cuantasTieneDe :: String -> Usuario -> Int
cuantasTieneDe unSimbolo usuario = cantidad.encontrar ((==unSimbolo).simboloTitulo). accionesUsuario $ usuario

--punto 3
nuevoPrecioAccion :: Float -> Accion ->  Accion
nuevoPrecioAccion unPrecio accion = accion {precios= unPrecio:precios accion}

nuevoPrecio :: [Accion] -> String -> Float -> [Accion]
nuevoPrecio acciones unSimbolo unPrecio = mapCondicional ((==unSimbolo).simbolo) (nuevoPrecioAccion unPrecio) acciones

precioActual :: Accion -> Float
precioActual = head.precios

--punto 4
--estadoActual usuario acciones = 

--punto 5
--pagarDividendos usuarios simbolo cantDividendos = mapCondicional ((==1).encontrar ((==simbolo).simboloTitulo).accionesUsuario) (actualizarEfectivo cantDividendos simbolo) usuarios
--tieneEseSimbolo simbolo usuario = (==simbolo).simboloTitulo . accionesUsuario $ usuario

--actualizarEfectivo cantDividendos simbolo usuario = usuario {cartera =cartera usuario+(*cantDividendos).cuantasTieneDe simbolo usuario}

--