{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Applicative (pure)
import Control.DeepSeq (NFData, ($!!))
import Control.Monad (void)
import Data.Map (Map, empty, foldrWithKey, singleton)
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
                
-- suma e1 e2 = Suma e1 e2                

newtype Fix f = Fx (f (Fix f)) 

-- outF :: ExprF a
-- outF (Fx (f (Fix f))) 
    -- = f (Fix f)

data ExprF a 
    = SumaF a a
    | RestaF a a 
    | MultF a a
    | DivF a a 
    | NegF a
    | Const Int 
    deriving (Eq, Show, Read)
toExF :: Expresión -> Expr
toExF (Literal l )= Fx (Const $ fromInteger l)
toExF (Suma a b)= Fx (SumaF (toExF a) (toExF b))
toExF (Resta a b)= Fx (RestaF (toExF a) (toExF b))
toExF (Multiplicación a b)= Fx (MultF (toExF a) (toExF b))
toExF (División a b)= Fx (DivF (toExF a) (toExF b))
toExF (Negativo a)= Fx (NegF (toExF a))

instance Functor ExprF where
    fmap f (Const e) = Const e 
    fmap f (NegF e) = NegF (f e)
    fmap f (SumaF a b) = SumaF (f a) (f b)
    fmap f (RestaF a b) = RestaF (f a) (f b)
    fmap f (MultF a b) = MultF (f a) (f b)
    fmap f (DivF a b) = DivF (f a) (f b)
    
type Expr = Fix ExprF 
    -- deriving (Eq, Read, Show)
instance Show Expr where
    show (Fx e) = "Fx " ++ show e 
    
type Algebra f a = f a -> a
type SimpleA = Algebra ExprF Int
alg :: SimpleA

-- alg :: ExprF Int -> Int
alg (Const i) = i
alg (NegF n) = -n 
alg (SumaF n m) = n + m
alg (RestaF n m) = n - m
alg (MultF n m) =  n *  m
alg (DivF n m) = n `div` m

type ExprInitAlg = Algebra ExprF (Fix ExprF)

ex_init_alg :: ExprF (Fix ExprF) -> Fix ExprF
ex_init_alg = Fx

unFix :: Fix f -> f (Fix f)
unFix (Fx x)=x

g = alg . (fmap g) . unFix
cata :: Functor f => Algebra f a -> Fix f -> a 
cata alg = alg . fmap (cata alg) . unFix

eval :: Fix ExprF -> Int
eval = alg . (fmap eval) . unFix

-- suma' :: Expr -> Expr -> Expr
-- suma' n = cata phi where
--     phi (Const m) = n+n
--     phi (Cons m) = s m 
    
-- mult' :: Expr -> Expr -> Expr
-- mult' n = cata phi where 
--     phi (Literak 0) = z
--     phi (Cons m) = suma' n m
    
-- s :: Expr -> Expr
-- s = Fx . Fix
z :: Expr 
z = Fx (Const 0)
e = Fx (SumaF (Fx (Const 5)) (Fx (Const 3)))
cataExpresión
    :: (a -> a -> a)
    -> (a -> a -> a)
    -> (a -> a -> a)
    -> (a -> a -> a)
    -> (a -> a)
    -> (Integer -> a)
    -> Expresión -> a
cataExpresión
    suma
    resta
    multiplicación
    división
    negativo
    literal
    = undefined

type Atributos
    = Map String String
newtype Documento
    = Documento Elemento
    deriving Show
data Elemento
    = Elemento String Atributos [Elemento]
    | Texto String
    deriving Show
--instance Show Elemento where
myshow (Texto t) = t
myshow (Elemento name attrs elems)= openTag ++ inside ++ closeTag
    where openTag = "<" ++ name  ++ attribs ++">"
          closeTag = "</" ++ name ++ ">"
          inside = foldr (++) "" $ map myshow elems
          attribs = foldrWithKey (\k v b -> b  ++ " " ++ k ++ "=\"" ++ v++ "\"") "" attrs
htmlE, headE, bodyE, divE :: [Elemento] -> Elemento
htmlE = Elemento "html" (singleton "xmlns" "http://www.w3.org/1999/xhtml")
headE = Elemento "head" empty
bodyE = Elemento "body" empty
divE = Elemento "div" empty

styleE, titleE, h1E :: String -> Elemento
styleE  s = Elemento "style" (singleton "type" "text/css") [Texto s]
titleE s = Elemento "title" empty [Texto s]
h1E s = Elemento "h1" empty [Texto s]
pE s = Elemento "p" empty [Texto s]

showP :: Show a => a -> Elemento
showP = pE . show

pp=Documento $ htmlE $ [headE [styleE estilo], bodyE $ [showP "Hola"]]
class RenderXHTML a where
    render :: a -> String

instance RenderXHTML Documento where
    render (Documento raiz)
        = myshow raiz

expresionDoc :: Expresión -> Documento
expresionDoc e = pp
estilo :: String
estilo
    = unlines
        [ "div, p {"
        , " border: 1px solid black;"
        , " float: left;"
        , " margin: 1em;"
        , "}"
        , "h1 {"
        , " clear: both;"
        , "}"]

deriving instance Generic Expresión
instance NFData Expresión
main :: IO ()
main = do
    args <- getArgs
    case args of
        (nombreArchivo : expresiónTexto : _) -> do
            expresión <- pure $!! read expresiónTexto
            putStrLn $ render $ expresionDoc$  expresión
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Uso: " ++ progName ++ " ARCHIVO.xhtml EXPRESIÓN"