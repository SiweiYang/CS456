-- file: CS456/A1/RTPOperations.hs
module RTPOperations where

import Data.Word
import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), serializePacket, deserializePacket, renumberPacket, sortPacketStoreByTime)
import Network.Socket (Socket, SockAddr, withSocketsDo, AddrInfo, getAddrInfo, addrAddress, SockAddr, HostName, addrFamily, SocketType(Datagram), defaultProtocol, socket, bindSocket)
import Network.Socket.ByteString (sendTo, recvFrom)
import Data.ByteString (ByteString, empty, pack, unpack, append)
import Data.List (sortBy)
import Control.Monad (unless)

targetSocketInfo :: HostName -> String -> IO AddrInfo
targetSocketInfo hostname port = withSocketsDo $ do
                                   addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
                                   --return (addrAddress (head addrinfos))
                                   return (head addrinfos)

staticSocket :: HostName -> String -> IO Socket
staticSocket hostname port = withSocketsDo $ do
                               addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
                               let serveraddr = head addrinfos :: AddrInfo
                               sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
                               bindSocket sock (addrAddress serveraddr)
                               return sock

dynamicSocket :: HostName -> String -> IO Socket
dynamicSocket hostname port = withSocketsDo $ do
                                addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
                                let serveraddr = head addrinfos :: AddrInfo
                                sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
                                return sock

sendRTP :: Socket -> Packet -> SockAddr -> IO Int
sendRTP sock packet addr = sendTo sock (pack (serializePacket packet)) addr

recvRTP :: Socket -> IO (SockAddr, Packet)
recvRTP sock = withSocketsDo $ do
                 (msg, addr) <- recvFrom sock 512
                 let bytes = unpack msg
                 return (addr, (deserializePacket bytes))

createPackets :: Word32 -> ByteString -> [Packet]
createPackets start msg = map (\(piece, id) -> Packet DAT id (fromIntegral (12 + (length piece))) piece) (zip pieces [start..])
                          where
                            splitor :: [[a]] -> [a] -> [[a]]
                            splitor groups [] = groups
                            splitor groups elements = group : (splitor [] next)
                                                      where
                                                        group = take 500 elements
                                                        next = drop 500 elements
                            pieces = splitor [] (unpack msg)

mergePackets :: [Packet] -> ByteString
mergePackets packets = foldl (\bytes p -> append bytes (pack (payload p))) empty orderedPackets
                       where
                         orderedPackets = sortBy (\p1 p2 -> compare (sn p1) (sn p2)) packets

refreshPacketStore :: Socket -> [(Packet, Word32)] -> Word32 -> SockAddr -> IO ()
refreshPacketStore sock ps threshold addr = sender sock addr threshold (sortPacketStoreByTime ps)
                                            where
                                              sender :: Socket -> SockAddr -> Word32 -> [(Packet, Word32)] -> IO ()
                                              sender sock addr threshold [] = do
                                                                                return ()
                                              sender sock addr threshold ((packet, t):ps) = do
                                                                                              unless (threshold > t) ((sendRTP sock packet addr) >>= (\i -> return ()))
                                                                                              sender sock addr threshold ps
--main = do
--  let p1 = Packet ACK 0 0 []
--  t <- targetSocket "localhost" "514"
--  s <- staticSocket "localhost" "512"
--  i <- sendRTP s p1 (addrAddress t)
--  return i

-- dat <- (Data.ByteString.readFile "RTPTypes.hs")
-- let packets = createPackets dat
-- mergePackets packets
