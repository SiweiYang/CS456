-- file: CS456/A1/RTPTypes.hs
module RTPTypes where

import Data.Word (Word8, Word32)
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


data PacketType = DAT | ACK | EOT deriving (Ord, Eq, Show)
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
                           --show (Packet pt sn pl payload) = "Packet {pt = " ++ show pt ++ ", sn = " ++ show sn ++ ", pl = " ++ show pl ++ ""
                           show (Packet pt sn pl payload) = show pt ++ " " ++ show sn ++ " " ++ show pl
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

data RTPStack = RTPStack {ps :: [(Packet, Word32)], sw, ew :: Word32} deriving (Show)
sortPacketStoreByTime :: [(Packet, Word32)] -> [(Packet, Word32)]
sortPacketStoreByTime ps = sortBy comparator ps
                           where
                             comparator (p1, t1) (p2, t2) = if (pt p1) == (pt p2) then timeComparator (p1, t1) (p2, t2) else compare (pt p1) (pt p2)
                             timeComparator (p1, t1) (p2, t2) = case (t1, t2) of
                               (0, 0) -> compare (sn p1) (sn p2)
                               (0, _) -> GT
                               (_, 0) -> LT
                               (_, _) -> compare t1 t2
sortPacketStoreBySN :: [(Packet, Word32)] -> [(Packet, Word32)]
sortPacketStoreBySN ps = sortBy comparator ps
                           where
                             comparator (p1, t1) (p2, t2) = compare (sn p1) (sn p2)

-- Not sure if needed
checkPacketStore :: [(Packet, Word32)] -> Word32 -> Bool
checkPacketStore [] num = False
checkPacketStore ((p, _):ps) num = if (sn p) == num
                                 then True
                                 else checkPacketStore ps num

timeoutPacketStore :: [(Packet, Word32)] -> Word32
timeoutPacketStore ps = if (pt packet) == DAT && timeout > 0
                        then timeout
                        else 0
                        where
                          ps' = sortPacketStoreByTime ps
                          (packet, timeout) = head ps'

-- Note the number returned isn't inclusive
checkTrailingPacketStack :: [(Packet, Word32)] -> Word32 -> Word32
checkTrailingPacketStack ps num = checkTrailingPacketStackUnsafe (sortPacketStoreBySN ps) num

checkTrailingPacketStackUnsafe :: [(Packet, Word32)] -> Word32 -> Word32
checkTrailingPacketStackUnsafe [] num = num
checkTrailingPacketStackUnsafe ((p, _):ps) num = if (sn p) == num && (pt p) == ACK
                                               then checkTrailingPacketStackUnsafe ps (num+1)
                                               else checkTrailingPacketStackUnsafe ps num

-- wouldn't de-duplication
modifyPacketStore :: Packet -> [(Packet, Word32)] -> [(Packet, Word32)]
modifyPacketStore packet [] = [(packet, 0)]
modifyPacketStore packet ((p, t):ps) = if (sn p) == (sn packet)
                                    then (packet, 0):ps
                                    else (p, t):(modifyPacketStore packet ps)
updatePacketStore :: (Packet, Word32) -> [(Packet, Word32)] -> [(Packet, Word32)]
updatePacketStore (packet, timeout) [] = [(packet, timeout)]
updatePacketStore (packet, timeout) ((p, t):ps) = if (sn p) == (sn packet)
                                                  then (packet, timeout):ps
                                                  else (p, t):(updatePacketStore (packet, timeout) ps)

onReceivingGBNDAT :: RTPStack -> Packet -> Maybe (RTPStack, Packet)
onReceivingGBNDAT (RTPStack ps sw ew) packet = if sn' < sw || sn' > ew
                                            then Nothing
                                            else Just (RTPStack ps' sw ew, (Packet ACK ((checkTrailingPacketStack ps' sn')-1) 12 []))
                                            where
                                              sn' = (sn packet)
                                              ps' = modifyPacketStore packet ps
onReceivingGBNACK :: RTPStack -> Packet -> Maybe RTPStack
onReceivingGBNACK (RTPStack ps sw ew) (Packet ACK sn pl payload) = if sn < sw || sn > ew
                                                  then Nothing
                                                  else Just ns
                                                  where
                                                    ns = RTPStack (modifyPacketStore (Packet ACK sn pl payload) ps) (sn+1) (sn+10)

onReceivingSRDAT :: RTPStack -> Packet -> Maybe (RTPStack, Packet)
onReceivingSRDAT (RTPStack ps sw ew) packet = if sn' < sw || sn' > ew
                                              then Nothing
                                              else Just (RTPStack ps' sw ew, (Packet ACK sn' 12 []))
                                              where
                                                sn' = (sn packet)
                                                ps' = modifyPacketStore packet ps
onReceivingSRACK :: RTPStack -> Packet -> Maybe RTPStack
onReceivingSRACK (RTPStack ps sw ew) (Packet ACK sn pl payload) = if sn < sw || sn > ew
                                                    then Nothing
                                                    else Just (RTPStack ps' sw' ew')
                                                    where
                                                      ps' = modifyPacketStore (Packet ACK sn pl payload) ps
                                                      sw' = checkTrailingPacketStack ps' sw
                                                      ew' = sw'+9

onReceivingEOT :: RTPStack -> Packet -> Maybe RTPStack
onReceivingEOT (RTPStack ps sw ew) (Packet EOT sn pl payload) = if sn < sw || sn > ew
                                                                then Nothing
                                                                else if (length expectedEOTs) == 0
                                                                     then Nothing
                                                                     else Just (RTPStack ps' sw ew)
                                                                where
                                                                  expectedEOTs = map (\(p, t) -> p) (filter (\(p, t) -> (pt p) == EOT) ps)
                                                                  ps' = modifyPacketStore (Packet EOT sn pl payload) ps

allFreeDATPackets :: RTPStack -> [Packet]
allFreeDATPackets (RTPStack ps sw ew) = map (\(p, t) -> p) fps
                                        where
                                          fps = filter (\(p, t) -> if (pt p) == DAT && t == 0 && (sn p) >= sw && (sn p) <= ew then True else False) ps
allActiveDATPackets :: RTPStack -> Word32 -> [Packet]
allActiveDATPackets (RTPStack ps sw ew) threshold = map (\(p, t) -> p) fps
                                        where
                                          fps = filter (\(p, t) -> if (pt p) == DAT && t > 0 && t <= threshold && (sn p) >= sw && (sn p) <= ew then True else False) ps

p1 = Packet ACK 0 0 []
p2 = Packet ACK 1 0 []
p3 = Packet ACK 0 1 []

ps' = [] :: [(Packet, Word32)]
packets = [(Packet ACK 0 12 []), (Packet ACK 1 12 []), (Packet ACK 2 12 []), (Packet DAT 3 12 []), (Packet ACK 4 12 []), (Packet ACK 6 12 []), (Packet DAT 7 12 []), (Packet ACK 8 12 [])]
ps'' = foldr modifyPacketStore ps' packets
stack = RTPStack ps'' 0 10