module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

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
