-- file: CS456/A1/RTPTypes.hs
module RTPTypes where

import Data.Word
import Data.Bits((.|.), shiftL, shiftR)

splitWord :: Word32 -> [Word8]
splitWord w = [ fromIntegral (shiftR w 24),
                fromIntegral (shiftR w 16),
                fromIntegral (shiftR w 8),
                fromIntegral w]

--for big-endian conversion (for little-endian, reverse the list ;)

joinWords :: [Word8] -> Word32
joinWords = foldl accum 0
  where
    accum a o = (shiftL a 8) .|. (fromIntegral o)


data PacketType = DAT | ACK | EOT deriving (Show)
serializePacketType :: PacketType -> Word32
serializePacketType DAT = 0
serializePacketType ACK = 1
serializePacketType EOT = 2

deserializePacketType :: Word32 -> PacketType
deserializePacketType 0 = DAT
deserializePacketType 1 = ACK
deserializePacketType 2 = EOT

type SequenceNumber = Word32
type PacketLength = Word32
type PayLoad = [Word8]

data Packet = Packet {pt :: PacketType, sn :: SequenceNumber, pl :: PacketLength, payload :: PayLoad} deriving (Show)
serializePacket :: Packet -> [Word8]
serializePacket (Packet pt sn pl payload) = (splitWord (serializePacketType pt)) ++
                                   (splitWord sn) ++
				   (splitWord pl) ++
				   payload

deserializePacket :: [Word8] -> Packet
deserializePacket bytes = Packet pt sn pl payload
                          where
			    ptw = take 4 bytes
			    snw = drop 4 (take 8 bytes)
			    plw = drop 8 (take 12 bytes)
			    payload = drop 12 bytes
			    pt = deserializePacketType (joinWords ptw)
			    sn = joinWords snw
			    pl = joinWords plw

renumberPacket :: SequenceNumber -> Packet -> Packet
renumberPacket number (Packet pt sn pl payload) = (Packet pt number pl payload)

p1 = Packet ACK 0 0 []
p2 = Packet ACK 1 0 []
p3 = Packet ACK 0 1 []