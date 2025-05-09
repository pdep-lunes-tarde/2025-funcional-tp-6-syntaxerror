module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- PARTE 1 

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = precioBase hamburguesa + sum (map precioIngrediente (ingredientes hamburguesa))

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa { ingredientes = ingredientes hamburguesa ++ [ingrediente] }
 
tiene :: Ingrediente -> Hamburguesa -> Bool
tiene ingrediente = any (== ingrediente) . ingredientes

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
  | tiene Carne hamburguesa = agregarIngrediente Carne hamburguesa
  | tiene Pollo hamburguesa = agregarIngrediente Pollo hamburguesa
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