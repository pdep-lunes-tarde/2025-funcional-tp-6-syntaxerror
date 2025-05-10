{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elem" #-}
module Library where
import PdePreludat


data Ingrediente =
    Carne | Pan | PanIntegral | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente PanIntegral =3
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- PARTE 1 

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = precioBase hamburguesa + sum (map precioIngrediente (ingredientes hamburguesa))

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa { ingredientes = ingredientes hamburguesa ++ [ingrediente] }


sacarIngrediente :: Hamburguesa -> Ingrediente -> Hamburguesa
sacarIngrediente (Hamburguesa p ingrs) ing =
    Hamburguesa p (filter (/= ing) ingrs)


tiene :: Ingrediente -> Hamburguesa -> Bool
tiene ingrediente = any (== ingrediente) . ingredientes

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
  | tiene Carne hamburguesa = agregarIngrediente Carne hamburguesa
  | tiene Pollo hamburguesa = agregarIngrediente Pollo hamburguesa
  | tiene PatiVegano hamburguesa = agregarIngrediente PatiVegano hamburguesa
  | otherwise     = hamburguesa

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa { precioBase = floor (precioBase hamburguesa * (1 - porcentaje / 100)) }


cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

pdepBurger :: Hamburguesa
pdepBurger =
  descuento 20 $
    agregarIngrediente Cheddar $
    agregarIngrediente Panceta $
    agrandar $
    agrandar cuartoDeLibra

-- PARTE 2

dobleCuarto :: Hamburguesa
dobleCuarto = Hamburguesa (precioBase cuartoDeLibra) (ingredientes cuartoDeLibra ++[Carne, Cheddar])

bigPdep :: Hamburguesa
bigPdep= Hamburguesa (precioBase dobleCuarto) (ingredientes dobleCuarto ++[Curry])

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = Hamburguesa (precioBase hamburguesa*0.7) (ingredientes hamburguesa ++[Papas])

-- PARTE 3
convertirVeggie :: Ingrediente -> Ingrediente
convertirVeggie Carne = PatiVegano
convertirVeggie Cheddar = QuesoDeAlmendras
convertirVeggie Panceta = BaconDeTofu
convertirVeggie otro = otro

cambiarPan ::Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie (Hamburguesa precio ingredientes) =
    Hamburguesa precio (map convertirVeggie ingredientes)

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati (Hamburguesa precio ingredientes)=
  Hamburguesa precio (map cambiarPan ingredientes)

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati (hacerVeggie dobleCuarto)

