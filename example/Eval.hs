
import Control.Monad (when)
import qualified Data.Text as Text
import Language.Eval (eval)
import System.Environment (getArgs)
import System.Exit (exitFailure)

usage :: IO ()
usage = do
    putStrLn "usage: Eval \"1+1\"\n"
    exitFailure

main = do
    args <- getArgs
    when (length args /= 1) usage
    let x = Text.pack (head args)
    print $ eval x []
