-- file: CS456/A1/RTPClient.hs
module RTPClient where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), RTPStack(RTPStack), queryPacketStore, modifyPacketStore, updatePacketStore, timeoutPacketStack, sortPacketStoreBySN, sortPacketStoreByTime, onReceivingSRACK, onReceivingGBNACK, allFreeDATPackets, allActiveDATPackets)
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (Socket, SockAddr,  SocketOption(..), setSocketOption, addrAddress)

import System.Environment (getArgs)
import System.Timeout (timeout)
import Control.Monad (unless)
import Data.ByteString (pack, unpack)
import qualified Data.ByteString (readFile)
import Data.Word (Word32)
import Data.Maybe

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

onReceivingEOT :: RTPStack -> Packet -> Maybe RTPStack
onReceivingEOT (RTPStack ps sw ew) (Packet EOT sn pl payload) = if sn < sw || sn > ew
                                                                then Nothing
                                                                else if isNothing mpair || (pt p') /= EOT
                                                                     then Nothing
                                                                     else Just (RTPStack ps sw ew)
                                                                where
                                                                  -- check whether an EOT been sent with same SN
                                                                  mpair = queryPacketStore ps sn
                                                                  Just (p', t') = mpair
                                                                     
cps :: Socket -> SockAddr -> Word32 -> RTPStack -> UTCTime -> IO RTPStack
cps sock addr ttw stack start = do
                                  ct <- getCurrentTime
                                  let d = floor ((diffUTCTime ct start) * 1000)
                                  let RTPStack ps sw ew = stack
                                  let freePackets = allFreeDATPackets stack
                                  let activePackets = allActiveDATPackets stack d
                                    
                                  if null freePackets && null activePackets
                                  then do
                                    putStrLn $ "WINDOW " ++ (show sw) ++ " - " ++ (show ew)
                                    let t = timeoutPacketStack stack
                                    if t == 0 then do
                                      -- no pending or on going transmission, EOT
                                      let packet = Packet EOT (fromIntegral (length ps)) 12 []
                                      let ps' = updatePacketStore (packet, d+ttw) ps
                                      sendRTP sock packet addr (d+ttw)
                                      
                                      cps sock addr ttw (RTPStack ps' sw ew) start
                                    else do
                                      let timewait = t - d
                                      putStrLn $ "RECV TIMEOUT " ++ (show t) ++ " - " ++ (show d) ++ " = " ++ (show timewait)
                                      result <- timeout (fromIntegral timewait * 1000) (recvRTP sock)
                                      let Just mpair = result
                                      if isNothing result || isNothing mpair then do                                      
                                        -- Initiate another loop when nothing is received
                                        putStrLn $ "RECV NOTHING VALID"
                                        cps sock addr ttw stack start
                                      else do
                                        let Just (_, packet) = mpair
                                        case pt packet of
                                          EOT -> if isNothing (onReceivingEOT stack packet) then do
                                                   -- Report out of order EOT
                                                   putStrLn $ "Problem detected on the received EOT"
                                                   cps sock addr ttw stack start
                                                 else do
                                                   let ps' = modifyPacketStore packet ps
                                                   -- Terminate after receiving legite EOT
                                                   return (RTPStack ps' sw ew)
                                          DAT -> do
                                                   -- Report illegal DAT
                                                   putStrLn "Not supposed to receive DAT"
                                                   cps sock addr ttw stack start
                                          ACK -> do
                                                   let mstack = onReceivingSRACK stack packet
                                                   if isNothing mpair then do
                                                     putStrLn $ "Problem detected on the received Packet"
                                                     cps sock addr ttw stack start
                                                   else do
                                                     putStrLn $ "Updating stack"
                                                     let Just stack' = mstack
                                                     -- Initiate another loop after a legite ACK received
                                                     cps sock addr ttw stack' start
                                  else do
                                    let packet = head (freePackets++activePackets)
                                    let ps' = updatePacketStore (packet, d+ttw) ps
                                    sendRTP sock packet addr (d+ttw)
                                    -- Initiate another loop after one free packet is sent
                                    cps sock addr ttw (RTPStack ps' sw ew) start

main :: IO ()
main = do
         timeoutvalue : filename : _ <- getArgs
         putStrLn $ "Transferring file: " ++ filename
         
         content <- readFile "channelInfo"
         let [hostname, port] = (words content)
         addr <- targetSocketInfo hostname port
         sock <- dynamicSocket hostname port
         
         content <- Data.ByteString.readFile filename
         let packets = createPackets 0 content
         let ps = sortPacketStoreBySN (foldr modifyPacketStore [] packets)
         let ttw = read timeoutvalue :: Word32

         start <- getCurrentTime
         RTPStack ps sw ew <- cps sock (addrAddress addr) ttw (RTPStack ps 0 (fromIntegral (length ps))) start
         return ()
