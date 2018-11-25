{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Program
    (Program,compile,sampleProgram,readProgram, sampleIf) where

import Prelude hiding (print)
import Evaluator
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.List

--this allows us to write a program using our defined language
type Program = Writer Statement ()


-- instance Semigroup Statement where (<>) = mappend
instance Monoid Statement where
  mempty = Pass
  mappend a b = a `Seq` b


int = Const . I
bool = Const . B
var = Var

class SmartAssignment a where
  assign :: String -> a -> Statement

instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
  assign = Assign

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

-- THIS IS NOT NECESSARY, DO NOT GET CONFUSED

class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr
  (./) :: a -> b -> Expr
  (.+) :: a -> b -> Expr


instance PrettyExpr String String where
  x .* y = Var x `Mul` Var y
  x .- y = Var x `Sub` Var y
  x ./ y = Var x `Div` Var y
  x .+ y = Var x `Add` Var y

instance PrettyExpr String Int where
  x .* y = Var x `Mul` Const (I y)
  x .- y = Var x `Sub` Const (I y)
  x ./ y = Var x `Div` Const (I y)
  x .+ y = Var x `Add` Const (I y)

-- And finally some convenience functions for our syntactic sugar:

infixl 1 .=
(.=) :: String -> Expr -> Program
var .= val = tell $ assign var val


-- if is a keyword in Haskell so I can't hide it. I renamed it so:

iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

-- This is why I wanted to hide the system function "print":

print :: Expr -> Program
print e = tell $ Print e

try :: Program -> Program -> Program
try block recover = tell $ Try (compile block) (compile recover)

--function for reading in a program file
readProgram :: String -> Program
readProgram p = writer((),statement)
                  where statement = mconcat thestatements
                        thestatements = map( \x -> read x ::Statement) $ lines p
                  -- map over the program's lines (p), and for every x (line), create a new statement


sampleProgram :: Program
sampleProgram = do
           "arg"     .= int 10
           "scratch" .= var "arg"
           "total"   .= int 1
           while ( var "scratch" `Gt` int 1 ) (
            do "total"   .=  "total" .* "scratch"
               "scratch" .= "scratch" .- (1::Int)
            )
           print $ var "total"
           "total"   .=  "total" ./ (4::Int)
           print $ var "total"
           "total"   .=  "total" .+ (4::Int)
           print $ var "total"

sampleIf :: Program
sampleIf = iif ( var "scratch" `Gt` int 1 ) ("total"   .=  "total" ./ (4::Int)) ("total"   .=  "total" ./ (4::Int))
