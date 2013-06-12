-- file: CS456/A1/RTPClient.hs
module RTPServer where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..))
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (sClose, addrAddress, SockAddr(SockAddrInet))

import System.Environment (getArgs)
import Control.Monad (unless)
import Data.ByteString (pack, unpack, writeFile)

import Prelude hiding (writeFile)

main :: IO ()
main = do
         filename : _ <- getArgs
	 putStrLn filename
         
	 let hostname = "localhost"
	 let port = "513"

	 sock <- staticSocket hostname port

	 receivePackets sock
         sClose sock
       where
         --receivePackets :: Socket -> IO ()
         receivePackets sock = do
	                         (SockAddrInet port hostname, packet) <- recvRTP sock                                 
                                 putStrLn (show hostname)
                                 putStrLn (show port)
                                 putStrLn (show packet)
				 unless (pt packet == EOT) (receivePackets sock)