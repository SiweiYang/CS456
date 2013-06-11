-- file: CS456/A1/RTPClient.hs
module RTPClient where

import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (addrAddress)

import System.Environment (getArgs)
import Control.Monad (unless)
import Data.ByteString (pack, unpack, readFile)

import Prelude hiding (readFile)

main :: IO ()
main = do
         timeout : filename : _ <- getArgs
	 putStrLn filename
	 content <- readFile filename
	 let packets = createPackets content

	 let hostname = "localhost"
	 let port = "513"

	 addr <- targetSocketInfo hostname port
	 sock <- dynamicSocket hostname port

	 sendPackets sock packets (addrAddress addr)
       where
         --sendPackets :: Socket -> Packet -> SockAddr -> IO ()
         sendPackets sock (packet:packets) addr = do
	                                          sendRTP sock packet addr
						  unless (length packets == 0) (sendPackets sock packets addr)