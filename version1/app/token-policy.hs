module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utils                (stringToWriteTestMintingPolicy)
import           Cardano.Api                 as API

main :: IO (Either (FileError ()) ())
main = do
    [file, addrString] <- getArgs
    stringToWriteTestMintingPolicy file addrString