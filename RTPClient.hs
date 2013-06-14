-- file: CS456/A1/RTPClient.hs
module RTPClient where

import RTPTypes (PacketType(..), SequenceNumber, PacketLength, PayLoad, Packet(..), RTPStack(RTPStack))
import RTPOperations (targetSocketInfo, staticSocket, dynamicSocket, sendRTP, recvRTP, createPackets)
import Network.Socket (addrAddress)

import System.Environment (getArgs)
import Control.Monad (unless)
import Data.ByteString (pack, unpack, readFile)

import Prelude hiding (readFile)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

data operationalRTPStack = operationalRTPStack {start :: UTCTime, packets :: [Packet], stack :: RTPStack}

main :: IO ()
main = do
         timeoutvalue : filename : _ <- getArgs
	 putStrLn filename
	 content <- readFile filename
	 let packets = createPackets 0 content

	 let timeout = read timeoutvalue
         let hostname = "localhost"
	 let port = "513"

	 addr <- targetSocketInfo hostname port
	 sock <- dynamicSocket hostname port

	 sendPackets sock packets (addrAddress addr)
         sendRTP sock (Packet EOT 0 12 []) (addrAddress addr)
         return ()
       where
         --sendPackets :: Socket -> Packet -> SockAddr -> IO ()
         sendPackets sock (packet:packets) addr = do
	                                          sendRTP sock packet addr
						  unless (length packets == 0) (sendPackets sock packets addr)