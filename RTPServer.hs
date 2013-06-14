-- file: CS456/A1/RTPClient.hs
module RTPServer where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), RTPStack(RTPStack))
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (sClose, Socket, addrAddress, SockAddr(SockAddrInet, SockAddrInet6))

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

	 RTPStack ps sw ew <- receivePackets sock (RTPStack [] 0 0)
         putStrLn (show ps)
         sClose sock
       where
         --receivePackets :: Socket -> RTPStack -> IO RTPStack
         receivePackets sock (RTPStack ps sw ew)= do
                                                    --(SockAddrInet port hostname, packet) <- recvRTP sock
                                                    (a, packet) <- recvRTP sock
                                                    putStrLn (show a)
                                                    --putStrLn (show hostname)
                                                    --putStrLn (show port)
                                                    putStrLn (show packet)
                                                    if pt packet == EOT
                                                    then return (RTPStack ps sw ew)
                                                    else receivePackets sock (RTPStack ((packet, 0):ps) sw ew)
                                                    