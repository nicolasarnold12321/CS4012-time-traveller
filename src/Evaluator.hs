{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Evaluator
    ( Expr(..), Env, Eval, Statement(..), Name, Val(..),ProgramState(..),
    lookup,runEval, eval,getEnv,reformatStatement,putStrC,colorRed,colorGreen ,colorYellow,  colorBlue ,  colorMagenta ,  colorCyan,colorReset
) where
import Prelude hiding (lookup)

import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

data Val = I Int | B Bool | N --N for null
  deriving (Eq,Read, Show)

-- constants for the colour of the txt
colorRed = "\x1b[31m"
colorGreen = "\x1b[32m"
colorYellow = "\x1b[33m"
colorBlue = "\x1b[34m"
colorMagenta = "\x1b[35m"
colorCyan = "\x1b[36m"
colorReset = "\x1b[0m"


-- Defining a const

-- Const (I 10), Const (B False) or Var "nic"
data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var String
   deriving (Eq,Read,Show)

type Name = String
type Env = Map.Map Name Val
data ProgramState= ProgramState {envs :: [Env], statements :: [Statement]}


lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing ->throwError ("Unknown variable "++k)

type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity $ runExceptT $ runReaderT ex env

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) =   evali (+) e0 e1
eval (Sub e0 e1) =   evali (-) e0 e1
eval (Mul e0 e1) =   evali (*) e0 e1
eval (Div e0 e1) =   evali div e0 e1

eval (And e0 e1) =  evalb (&&) e0 e1
eval (Or e0 e1) =  evalb (||) e0 e1

eval (Not e0  ) = evalb (const not) e0 (Const (B True))
                       where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) =   evalib (==) e0 e1
eval (Gt e0 e1) =  evalib (>) e0 e1
eval (Lt e0 e1) =   evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env

getEnv :: ProgramState -> Env
getEnv cp = case length (envs cp) of
                    0 -> Map.empty
                    _ -> head $ envs cp

data Statement = Assign String Expr
               | If Expr Statement Statement
               | Ifc Expr --allows us to store only the conditon to the stack
               | While Expr Statement
               | Whilec Expr --allows us to store only the conditon to the stack
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Debug Statement --allows us to build a statement stack
               | Break Expr Statement --breakpoint statement
               | Pass
      deriving (Eq, Read, Show) --read used for the parsing of the file, allows me to create a 'program'

--reformats the statement so that only the conditions are present
reformatStatement:: Statement -> String
reformatStatement (Whilec c)="While "++show c++")"
reformatStatement (While c _)= "While "++show c++")"
reformatStatement (Ifc c) = "If ("++show c++")"
reformatStatement (If c _ _) = "If ("++show c++")"
reformatStatement s = ""++show s

--function for colourful txt
putStrC :: String -> String -> IO ()
putStrC c s = putStr $ c ++ s ++ colorReset
