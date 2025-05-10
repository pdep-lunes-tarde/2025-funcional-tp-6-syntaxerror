module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
import GHC.Generics (prec)
import Data.Functor.Contravariant (comparisonEquivalence)


correrTests :: IO ()
correrTests = hspec $ do
    describe "Hamburguesas" $ do
        it "precio final de hamburguesa sin ingredientes" $ do
            precioFinal (Hamburguesa 50 []) `shouldBe` 50

        it "precio final suma base e ingredientes" $ do
            precioFinal (Hamburguesa 10 [Carne, Cheddar, Pan]) `shouldBe` 42

        it "agregar ingrediente al final" $ do
            ingredientes (agregarIngrediente BaconDeTofu (Hamburguesa 5 [Pan])) `shouldBe` [Pan, BaconDeTofu]

        it "agrandar agrega carne si ya tiene carne" $ do
            ingredientes (agrandar (Hamburguesa 5 [Pan, Carne])) `shouldBe` [Pan, Carne, Carne]

        it "agrandar agrega pollo si no tiene carne pero sí pollo" $ do
            ingredientes (agrandar (Hamburguesa 5 [Pollo, Pan])) `shouldBe` [Pollo, Pan, Pollo]

        it "agrandar no cambia nada si no tiene carne ni pollo" $ do
            ingredientes (agrandar (Hamburguesa 5 [Pan, Cheddar])) `shouldBe` [Pan, Cheddar]

        it "descuento al 50%" $ do
            precioBase (descuento 50 (Hamburguesa 40 [])) `shouldBe` 20

        it "pdepBurger tiene precio final 110" $ do
            precioFinal pdepBurger `shouldBe` 110

        it "dobleCuarto tiene precio final 84" $ do
            precioFinal dobleCuarto `shouldBe` 84

        it "bigPdep tiene precio final 89" $ do
            precioFinal bigPdep `shouldBe` 89 

        it "bigPdep del dia tiene precio final 88" $ do
            precioFinal (delDia dobleCuarto) `shouldBe` 88

        it "hacerVeggie a un bigPedp" $ do
            hacerVeggie bigPdep `shouldBe` Hamburguesa 20 [Pan, PatiVegano, QuesoDeAlmendras, Pan, PatiVegano, QuesoDeAlmendras, Curry]
        it "cambiarPanDePati a bigPdep" $ do
            cambiarPanDePati bigPdep `shouldBe` Hamburguesa 20 [PanIntegral,Carne,Carne,Cheddar,Cheddar,Curry,PanIntegral]

        it "dobleCuartoVegano tiene precio final 76" $ do
            precioFinal dobleCuartoVegano `shouldBe` 76