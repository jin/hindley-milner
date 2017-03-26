module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment

import Parser

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ast -> print ast 

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
