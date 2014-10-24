
import Control.Monad (when)
import qualified Data.Text as Text
import Language.Eval (eval)
import System.Environment (getArgs)
import System.Exit (exitFailure)

usage :: IO ()
usage = putStrLn "usage: Eval \"1+1\"\n"

main = do
    args <- Text.pack <$> getArgs
    when (length args /= 1) (usage >>= exitFailure)
    print $ eval (head args) []
