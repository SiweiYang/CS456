-- file: CS456/A1/RTPClient.hs
module RTPServer where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), RTPStack(RTPStack), sortPacketStoreBySN, checkTrailingPacketStack, modifyPacketStore, onReceivingGBNDAT, onReceivingSRDAT)
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets, mergePackets)
import Network.Socket (sClose, Socket, addrAddress, socketPort, SockAddr(SockAddrInet, SockAddrInet6))
import Network.BSD (getHostName)

import System.Environment (getArgs)
import Control.Monad (unless)
import Data.ByteString (pack, unpack)
import qualified Data.ByteString (writeFile)
import Data.Maybe

onReceivingEOT :: RTPStack -> Packet -> Maybe RTPStack
onReceivingEOT (RTPStack ps sw ew) (Packet EOT sn pl payload) = if sn < sw || sn > ew then Nothing
                                                                else if (checkTrailingPacketStack ps sw) == sn then Just (RTPStack ps sw ew)
                                                                else Nothing

cps :: Socket -> RTPStack -> IO RTPStack
cps sock stack = do
                   mpair <- recvRTP sock
                   if isNothing mpair
                   then do
                     putStrLn $ "RECV NOTHING"
                     cps sock stack
                   else do
                     let Just (a, packet) = mpair
                     case pt packet of
                       EOT -> if isNothing (onReceivingEOT stack packet)
                              then do
                                putStrLn $ "WINDOW" ++ " " ++ (show sw) ++ " " ++ (show ew) ++ " " ++ (show (checkTrailingPacketStack ps sw))
                                putStrLn $ "Packet out of order on EOT"
                                cps sock stack
                              else do
                                sendRTP sock (Packet EOT (sn packet) 12 []) a 0
                                return stack
                       DAT -> do
                              let mpair = onReceivingSRDAT stack packet
                              if isNothing mpair
                              then do
                                putStrLn $ "Packet rejected"
                                cps sock stack
                              else do
                                let Just (stack', packet') = mpair
                                sendRTP sock packet' a 0
                                cps sock stack'
                       ACK -> do
                                putStrLn $ "Not supposed to receive ACK"
                                cps sock stack
                 where
                   RTPStack ps sw ew = stack

main :: IO ()
main = do
         filename : _ <- getArgs
         
         hostname <- getHostName
         sock <- staticSocket hostname "0"
         port <- socketPort sock
         writeFile "recvInfo" (hostname ++ " " ++ (show port))

         --RTPStack ps sw ew <- receivePackets sock (RTPStack [] 0 0)
         RTPStack ps sw ew <- cps sock (RTPStack [] 0 4294967295)
         let content = mergePackets (dropWhile (\p->(pt p)==EOT) (map (\(p, t)->p) (sortPacketStoreBySN ps)))
         Data.ByteString.writeFile filename content
         sClose sock
                                                    
