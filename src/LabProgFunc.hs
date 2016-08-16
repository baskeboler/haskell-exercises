{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LabProgFunc where

import Control.Applicative (pure)
import Control.DeepSeq (NFData, ($!!))
import Control.Monad (void)
import Data.Map (Map, empty, foldMapWithKey, singleton)
import GHC.Generics (Generic)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

data Expresión
    = Suma Expresión Expresión
    | Resta Expresión Expresión
    | Multiplicación Expresión Expresión
    | División Expresión Expresión
    | Negativo Expresión
    | Literal Integer
    deriving (Eq, Read, Show)
-- Auxiliares

binaria :: Expresión -> Bool
binaria
    = \ case
        Literal n -> False
        Negativo e -> False
        _ -> True

operando1 :: Expresión -> Expresión
operando1
    = \ case
        Suma e _ -> e
        Resta e _ -> e
        Multiplicación e _ -> e
        División e _ -> e
        _ -> error "No es expresión binaria"
operando2 :: Expresión -> Expresión
operando2
    = \ case
        Suma _ e-> e
        Resta _ e -> e
        Multiplicación _ e -> e
        División _ e -> e
        _ -> error "No es expresión binaria"

-- Fin auxiliares

t1, t2, t3 :: Expresión
t1 = Literal 42
t2 = Suma (Literal 27) t1
t3 = Suma (Multiplicación t2 (Multiplicación t2 (Literal 1))) (Negativo (División (Suma t1 (Literal 0)) (Literal 3)))

evaluar :: Expresión -> Double
evaluar
  = \ case
    Suma e1 e2 -> evaluar e1 + evaluar e2
    Resta e1 e2 -> evaluar e1 - evaluar e2
    Multiplicación e1 e2 -> evaluar e1 * evaluar e2
    División e1 e2 -> evaluar e1 / evaluar e2
    Negativo e -> (-1) * evaluar e
    Literal n -> fromInteger n

operaciones :: Expresión -> Integer
operaciones
  = \ case
        Suma e1 e2 -> 1 + operaciones e1 + operaciones e2
        Resta e1 e2 -> 1 + operaciones e1 + operaciones e2
        Multiplicación e1 e2 -> 1 + operaciones e1 + operaciones e2
        División e1 e2 -> 1 + operaciones e1 + operaciones e2
        Negativo e -> 1 +  operaciones e
        Literal n -> 0
sumaLiterales :: Expresión -> Integer
sumaLiterales
    = \ case
        Literal n -> n
        Negativo e -> sumaLiterales e
        Suma e1 e2 -> sumaLiterales e1 + sumaLiterales e2
        Resta e1 e2 -> sumaLiterales e1 + sumaLiterales e2
        Multiplicación e1 e2 -> sumaLiterales e1 + sumaLiterales e2
        División e1 e2 -> sumaLiterales e1 + sumaLiterales e2

literales :: Expresión -> [Integer]
literales
    = \ case
        Literal n -> [n]
        Negativo e -> literales e
        Suma e1 e2 -> literales e1 ++ literales e2
        Resta e1 e2 -> literales e1 ++ literales e2
        Multiplicación e1 e2 -> literales e1 ++ literales e2
        División e1 e2 -> literales e1 ++ literales e2

altura :: Expresión -> Integer
altura
    = \ case
        Literal _ -> 0
        Negativo e -> 1 + altura e
        e
            | binaria e ->
                1 + max (altura $ operando1 e) (altura $ operando2 e)