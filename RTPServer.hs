-- file: CS456/A1/RTPClient.hs
module RTPServer where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), RTPStack(RTPStack), modifyPacketStore)
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (sClose, Socket, addrAddress, SockAddr(SockAddrInet, SockAddrInet6))

import System.Environment (getArgs)
import Control.Monad (unless)
import Data.ByteString (pack, unpack, writeFile)

import Prelude hiding (writeFile)

cps :: Socket -> RTPStack -> IO RTPStack
cps sock stack = do
                   (a, packet) <- recvRTP sock
                   let ps' = modifyPacketStore packet ps
                   putStrLn $ "PKT RECV " ++ (show packet)
                   if (pt packet) == EOT
                   then do
                     sendRTP sock (Packet EOT (sn packet) 12 []) a
                     return (RTPStack ps' sw ew)
                   else do
                     sendRTP sock (Packet ACK (sn packet) 12 []) a
                     cps sock (RTPStack ps' sw ew)
                 where
                   RTPStack ps sw ew = stack

main :: IO ()
main = do
         filename : _ <- getArgs
         putStrLn filename
         let hostname = "localhost"
         let port = "1513"

         sock <- staticSocket hostname port

         --RTPStack ps sw ew <- receivePackets sock (RTPStack [] 0 0)
         RTPStack ps sw ew <- cps sock (RTPStack [] 0 0)
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
                                                    
