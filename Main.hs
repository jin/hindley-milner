module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment

import Syntax (Expr)
import Parser
import Infer

inferType :: Expr -> String
inferType e = case infer e of
  Left err -> show err
  Right ty -> show ty

process :: String -> IO ()
process line = case parseExpr line of
  Left err -> print err
  Right e -> do 
    putStrLn line
    putStrLn (show e ++ ": " ++ inferType e)
    putStrLn ""

-- https://hackage.haskell.org/package/haskeline-0.7.3.1/docs/System-Console-Haskeline.html
runRepl :: IO ()
runRepl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO (process input) >> loop

loadFile :: String -> IO ()
loadFile filePath = do
  src <- readFile filePath
  mapM_ process [line | line <- lines src, line /= ""]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    (x:_) -> loadFile x
