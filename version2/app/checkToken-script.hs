module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utils                (stringToWriteScriptValidator)
import           Cardano.Api                 as API

main :: IO (Either (FileError ()) ())
main = do
    [file, addrString] <- getArgs
    stringToWriteScriptValidator file addrString