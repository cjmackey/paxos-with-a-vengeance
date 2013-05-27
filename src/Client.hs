module Main where

import Action
import Model
import Data.Serialize

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Data.ByteString(ByteString)
import qualified Data.Text as T
import Data.Word

import System.Environment (getArgs)
import Control.Monad

main = withSocketsDo $ do
  args <- getArgs
  let times = case args of
                (x:xs) -> read x
                _ -> 1 :: Int
  print "client"
  addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  let cmd = Command "Example" (map (MText . T.pack) (drop 1 args))
  replicateM (times-1) (sendCommand sock cmd >> recvCommandResult sock)
  sendCommand sock cmd
  recvCommandResult sock >>= print
  sClose sock


sendCommand sock cmd = do
  --print cmd
  let cmdB = encode cmd
  sendAll sock (encode (fromIntegral (B.length cmdB) :: Word64))
  sendAll sock cmdB
  --print "done sending command"

recvCommandResult sock = do
  lB <- recvAll sock 8
  let l = (case decode lB of
               Right x -> x
               Left s -> error s
            ) :: Word64
  --print l
  outputB <- recvAll sock (fromIntegral l)
  let output = (case decode outputB of
                  Right x -> x
                  Left s -> error s) :: Model
  return output  

recvAll :: Socket -> Int -> IO ByteString
recvAll s nr = go nr []
  where
    go 0 x = return $ B.concat (Prelude.reverse x)
    go n x = do
        bs <- recv s n
        if B.length bs == 0
            then return (error "socket closed...")
            else go (n - B.length bs) (bs:x)

