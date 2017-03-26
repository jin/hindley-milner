import Control.Monad.Trans
import System.Console.Haskeline

import Parser

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO (process input) >> loop
