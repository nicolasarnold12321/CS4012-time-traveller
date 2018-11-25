module Main where

import Interpreter
import Data.List
import System.Environment(getArgs)

main :: IO ()
main = do
    filename <- getArgs
    file <- readFile $ filename !! 0 --gets head of the list ie. first arg
    let p=readProgram file --read in the program
    run p
