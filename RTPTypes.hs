-- file: CS456/A1/RTPTypes.hs
module RTPTypes where

import Data.Word
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.List (sortBy)

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


data PacketType = DAT | ACK | EOT deriving (Eq, Show)
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

data Packet = Packet {pt :: PacketType, sn :: SequenceNumber, pl :: PacketLength, payload :: PayLoad}
instance Show Packet where
                           show (Packet pt sn pl payload) = "Packet {pt = " ++ show pt ++ ", sn = " ++ show sn ++ ", pl = " ++ show pl ++ ""
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

data RTPStack = RTPStack {ps :: [(Packet, Word)], sw, ew :: Word}

p1 = Packet ACK 0 0 []
p2 = Packet ACK 1 0 []
p3 = Packet ACK 0 1 []