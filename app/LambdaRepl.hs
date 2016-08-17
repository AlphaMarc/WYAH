module Main where


import           Control.Monad.IO.Class   (liftIO)
import           LambdaCalc.Eval
import           LambdaCalc.Parser
import           LambdaCalc.Syntax
import           System.Console.Haskeline



loop :: InputT IO ()
loop = do
          maybeInput <- getInputLine "λ >"
          case maybeInput of
            Nothing    -> outputStrLn "Au revoir"
            Just input -> liftIO (process input) >> loop


process :: String -> IO ()
process s =
    let exs = parseExpr s in
    case exs of
      Right ev  -> print (runEval ev)
      Left err  -> print err


main :: IO ()
main = runInputT defaultSettings loop
