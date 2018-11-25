module Interpreter
   (Program, run , readProgram, sampleProgram, compile,Env(..), sampleIf
    ) where

import Evaluator
import Program
import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import System.Console.ANSI
import System.Exit

--Run here is of type Monad
type Run a = StateT ProgramState (ExceptT String IO) a
runRun p =  runExceptT ( runStateT p ProgramState{envs=[],statements=[]}) --needs to be modified (Map empty is the set of vars)

set :: (Name, Val) -> Run ()
set (s,i) = state (\table ->
    let newEnv = Map.insert s i $ getEnv table
    in ((), ProgramState {envs = newEnv : tail (envs table), statements = statements table} ))

exec :: Statement -> Run ()
exec (Assign s v) = do env <- get
                       prev  <- return $ runEval (getEnv env) (eval v) `catchError` (\_ -> return N) --check if the value is null
                       case prev of
                          Right N  ->  do
                                       warningError v
                                       giveOption (Assign s v)
                          _ ->
                                  do
                                     put ProgramState{ envs = getEnv env:envs env, statements = Assign s v:statements env}
                                     Right val <- return $ runEval (getEnv env) (eval v)
                                     set (s,val)

exec (Seq s0 s1) = exec (Debug s0) >> exec (Debug s1)

exec (Print e) = do env <- get
                    prev  <- return $ runEval (getEnv env) (eval e) `catchError` (\_ -> return N) --check if the value is null
                    case prev of
                      Right N  ->  do
                                   warningError e
                                   giveOption (Print e)
                      _ ->
                              do
                                put ProgramState{ envs = getEnv env:envs env, statements = Print e:statements env}
                                Right val <- return prev
                                liftIO $ putStrC colorCyan "---OUTPUT---\n"
                                liftIO $ System.print val
                                return ()

exec (If cond s0 s1) = do env <- get
                          prev  <- return $ runEval (getEnv env) (eval cond) `catchError` (\_ -> return N) --check if the value is null
                          case prev of
                            Right N  ->  do
                                         warningError cond
                                         giveOption (If cond s0 s1)
                            _ ->
                                    do
                                      addStatement (Ifc cond) --adds if condtional part to avoid seq runing and ruining the order of execution
                                      Right (B val) <- return prev
                                      if val then exec (Debug s0) else exec (Debug s1)

exec (Break cond s) = do env <- get
                         prev  <- return $ runEval (getEnv env) (eval cond) `catchError` (\_ -> return N) --check if the value is null
                         case prev of
                           Right N  ->  do
                                        warningError cond
                                        giveOption (Break cond s)
                           _ -> do
                                      addStatement (Break cond s)
                                      Right (B val) <- return prev
                                      if val then
                                        put ProgramState {envs = envs env, statements = statements env} else
                                          (do
                                                     warning val cond
                                                     giveOption s)

exec (While cond s) = do env <- get
                         prev  <- return $ runEval (getEnv env) (eval cond) `catchError` (\_ -> return N) --check if the value is null
                         case prev of
                           Right N  ->  do
                                        warningError cond
                                        giveOption (While cond s)
                           _ ->
                                  do
                                   Right (B val) <-return prev
                                   addStatement (Whilec cond) --adds while condtional part to avoid seq runing and ruining the order of execution
                                   when val $ exec (Debug s) >> exec (Debug (While cond s))

exec (Try s0 s1) = catchError (exec s0) (\e -> exec s1)
exec (Debug (Seq s0 s1))= exec (Seq s0 s1)
exec (Debug s)= prompt s
exec (Whilec cond)= addStatement (Whilec cond) --adds the condition back to the stack so that we can show it
exec (Ifc cond)= addStatement (Ifc cond) --adds the condition back to the stack so that we can show it
exec Pass = return ()

int = Const . I
bool = Const . B
var = Var

warningError:: Expr -> Run ()
warningError s= liftIO $ putStrC colorRed ("---WARNING---\nWARNING: THE VAR:" ++ show  s ++ " DOES NOT EXIST\n---WARNING---\n\n")

-- extractVariable:: Expr -> String
-- extractVariable (Gt s e)= "Either :"show s++"Or :"show e

run :: Program -> IO ()
run program = do result <- runExceptT $ (runStateT $ exec $ compile program) ProgramState {envs=[],statements=[]}
                 case result of
                      Right ( (), env ) -> return ()
                      Left exn -> putStrC colorRed ("Uncaught exception: "++exn)

--adds the statement to the environment
addStatement :: Statement -> Run ()
addStatement currentstate = do
                            env <- get
                            put ProgramState{envs = getEnv env:envs env, statements = currentstate:statements env}


warning:: Bool -> Expr -> Run ()
warning v s=   liftIO $ putStrC colorRed ("WARNING: BREAKPOINT TRIGGERED for:" ++ show s ++ "\nVALUE OF THE CONDITION IS: "++ show v ++"\n")

giveOption:: Statement->Run()
giveOption s=  do
                liftIO $ putStrC colorMagenta "Don't worry, we can recover this! Remember this is a time machine..\n"
                prompt s

 --allows the user to interact with the time traveller
prompt:: Statement -> Run ()
prompt s = do st <- get
              printPromptInfo s
              liftIO $ putStrC colorCyan final
              input <- liftIO getLine
              case input of
                "n" -> do
                        liftIO clearScreen
                        exec s
                "back" -> goBack s
                "inspect" -> do
                        liftIO clearScreen
                        showVariables s
                        prompt s
                "state" -> do
                            liftIO clearScreen
                            showStatements s
                            exec (Debug s)
                _ -> do
                  liftIO clearScreen
                  liftIO $ putStrC colorRed "Unrecognised input\n"
                  prompt s
              where final= case s of
                            Pass -> "Last chance to do something (go back in time)!!\n"
                            _ -> ""

--steps back in time for the debugging
goBack:: Statement -> Run ()
goBack s= do
   st <- get
   liftIO clearScreen
   case length (statements st) of
      0 -> do
           liftIO $ putStrC colorRed "This is the first statement, we cannot go back further\n"
           exec (Debug s)
      _ -> do
           put ProgramState {envs = tail $ envs st, statements = tail $ statements st} -- gets rid of the recently added statement
           exec (Debug (head $ statements st)) --get the last statement and execute it
           exec (Debug s) --now run the statement again


printPromptInfo :: Statement ->Run()
printPromptInfo s = do
                          liftIO $ putStrC colorGreen "The Current Statement\n"
                          liftIO $ putStrC colorReset ("> " ++ reformatStatement s ++ "\n")
                          liftIO $ putStrC colorCyan "n:"
                          liftIO $ putStrC colorGreen " to go forward\n"
                          liftIO $ putStrC colorCyan "inspect:"
                          liftIO $ putStrC colorGreen " to show the variables\n"
                          liftIO $ putStrC colorCyan "state:"
                          liftIO $ putStrC colorGreen "to show the stack of statements\n"
                          liftIO $ putStrC colorCyan "back:"
                          liftIO $ putStrC colorGreen "to go back\n"
                          liftIO $ putStrC colorBlue "\n"

--loops through the stack of statements, and prints them out one by one
showStatements:: Statement -> Run ()
showStatements s = do
    env <- get
    printPromptInfo s
    case length $ statements env of
        0 -> liftIO $ putStrC colorRed "No statements have been executed\n"
        _ -> forM_ (zip [0..] (reverse $ statements env)) $ \(i, e) -> do
                liftIO $ putStrC  colorYellow  ("--- Statement " ++ show (i+1) ++ " ---\n")
                liftIO $ putStrC colorCyan ("This is the Statment:\n"++reformatStatement e++"\n")

--loops through the variables, and prints them out one by one
showVariables :: Statement -> Run ()
showVariables s = do
    env <- get
    printPromptInfo s
    case length $ envs env of
        0 -> liftIO $ putStrC colorRed "No variables have been set.\n"
        _ -> do
                let e = head $ envs env
                liftIO $ putStrC  colorYellow  "--- Variables  ---\n"
                liftIO $ putStrC colorGreen (Map.foldrWithKey (\k v r -> r ++ show k ++ ": " ++ show v ++ "\n") "" e ++ "\n")
