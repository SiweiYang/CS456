-- file: CS456/A1/RTPClient.hs
module RTPClient where

import System.Environment (getArgs)

main :: IO ()
main = do
         filename : _ <- getArgs
	 putStrLn filename