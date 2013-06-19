-- file: CS456/A1/RTPClient.hs
module RTPClient where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), RTPStack(RTPStack), modifyPacketStore, updatePacketStore, timeoutPacketStore, sortPacketStoreBySN, sortPacketStoreByTime, allFreeDATPackets, allActiveDATPackets, onReceivingEOT)
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (Socket, SockAddr,  SocketOption(..), setSocketOption, addrAddress)

import System.Environment (getArgs)
import System.Timeout (timeout)
import Control.Monad (unless)
import Data.ByteString (pack, unpack, readFile)
import Data.Word (Word32)
import Data.Maybe

import Prelude hiding (readFile)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

data OperationalRTPStack = OperationalRTPStack {start :: UTCTime, packets :: [Packet], stack :: RTPStack}

cps :: Socket -> SockAddr -> Word32 -> RTPStack -> UTCTime -> IO ()
cps sock addr ttw stack start = if (length freePackets) == 0
                    then do
                      ct <- getCurrentTime
                      let d = floor (diffUTCTime ct start) * 1000
                      let activePackets = allActiveDATPackets stack d
                      if (length activePackets) == 0
                      then do
                        let t = timeoutPacketStore ps
                        let timewait = (t - d) * 1000
                        putStrLn $ "RECV TIMEOUT " ++ (show timewait)
                        result <- timeout 1 (recvRTP sock)
                        putStrLn $ "RECV TERMINATE"
                        if isNothing result
                        -- Initiate another loop when nothing is received
                        then cps sock addr ttw stack start
                        else do
                          let Just (_, packet) = result
                          if (pt packet) == EOT
                          then if isNothing (onReceivingEOT stack packet)
                               -- Report out of order EOT
                               then error "Problem detected on the received EOT"
                               else do
                                 putStrLn $ "PKT RECV " ++ (show packet)
                                 -- Terminate after receiving legite EOT
                                 return ()
                          else if (pt packet) == DAT
                               -- Report illegal DAT
                               then error "Not supposed to receive DAT"
                               else do
                                 let ps' = modifyPacketStore packet ps
                                 putStrLn $ "PKT RECV " ++ (show packet)
                                 -- Initiate another loop after a legite ACK received
                                 cps sock addr ttw (RTPStack ps' sw ew) start
                      else do
                        let packet = (head activePackets)
                        sendRTP sock packet addr
                        ct <- getCurrentTime
                        let d = floor (diffUTCTime ct start) * 1000
                        let ps' = updatePacketStore (packet, d+ttw) ps
                        putStrLn $ "PKT SEND " ++ (show packet)
                        putStrLn $ "PKT TIMEOUT " ++ (show (d+ttw))
                        -- Initiate another loop after one active packet is sent
                        cps sock addr ttw (RTPStack ps' sw ew) start
                    else do
                      let packet = (head freePackets)
                      sendRTP sock packet addr
                      ct <- getCurrentTime                      
                      let d = floor ((diffUTCTime ct start) * 1000)
                      putStrLn $ "TIME DIFF " ++ (show d) ++ " [" ++ (show (diffUTCTime ct start)) ++ "] from the start"
                      let ps' = updatePacketStore (packet, d+ttw) ps
                      putStrLn (show (take 5 ps'))
                      putStrLn $ "PKT SEND " ++ (show packet)
                      putStrLn $ "PKT TIMEOUT " ++ (show (d+ttw))
                      -- Initiate another loop after one free packet is sent
                      cps sock addr ttw (RTPStack ps' sw ew) start
                    where
                      RTPStack ps sw ew = stack
                      freePackets = allFreeDATPackets stack

main :: IO ()
main = do
         timeoutvalue : filename : _ <- getArgs
	 putStrLn $ filename
	 content <- readFile filename
	 let packets = createPackets 0 content
         let ps = sortPacketStoreBySN (foldr modifyPacketStore [] packets)

	 let ttw = read timeoutvalue :: Word32
         let hostname = "localhost"
	 let port = "513"

	 addr <- targetSocketInfo hostname port
	 sock <- dynamicSocket hostname port
         setSocketOption sock SO_RCVTIMEO 100

	 --sendPackets sock packets (addrAddress addr)
         start <- getCurrentTime
         cps sock (addrAddress addr) ttw (RTPStack ps 0 9) start
       where
         --sendPackets :: Socket -> Packet -> SockAddr -> IO ()
         sendPackets sock (packet:packets) addr = do
	                                          sendRTP sock packet addr
						  unless (length packets == 0) (sendPackets sock packets addr)