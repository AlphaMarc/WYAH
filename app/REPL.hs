module REPL where

import           Control.Monad.IO.Class   (liftIO)
import           SimpleLang.Syntax
import           System.Console.Haskeline



process :: String -> IO ()
process s =
    let exs = parseExpr s in
    case exs of
      Right ev  -> case eval ev of
        Just evaled -> print evaled
        Nothing     -> print "Cannot evaluate"
      Left err  -> print err



loop :: InputT IO ()
loop = do
  maybeInput <- getInputLine "λ >"
  case maybeInput of
    Nothing    -> outputStrLn "Au revoir"
    Just input -> liftIO (process input) >> loop


main :: IO ()
main = runInputT defaultSettings loop
