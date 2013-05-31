module Data.NodeState where

import Data.ModelTree(ModelTree)
import qualified Data.ModelTree as MT
import Model
import Data.Protocol.Command(Command(Command), runCommand)

import Control.Concurrent(forkIO)
import Control.Concurrent.MVar(MVar, putMVar, newMVar, takeMVar)

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.ByteString as B
import Data.ByteString(ByteString)

import qualified Data.Text as T

import Data.Sequence()
import Data.Word(Word64)
import Data.Serialize(decode, encode)

data NodeState = SoloNodeState ModelTree

data ClientDescription = ClientDescription Socket [MT.TreePath]

empty :: NodeState
empty = SoloNodeState MT.empty
tree (SoloNodeState mt) = mt
setTree mt (SoloNodeState _) = SoloNodeState mt

-- NOTE: we'll probably want to move a lot of this out of here to other file(s)...


run :: NodeState -> IO ()
run ns = do
  mns <- newMVar ns
  print "server"
  addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  runLoop sock mns

runLoop sock mns = do
  (conn, _) <- accept sock
  forkIO (talk conn mns)
  runLoop sock mns

talk conn mns = do
  --hSetBuffering conn NoBuffering
  --putStrLn "on a connection"
  lenB <- recvAll conn 8
  let len = (case decode lenB of
               Right x -> x
               Left s -> error s
            ) :: Word64
  --print len
  commandB <- recvAll conn (fromIntegral len)
  let command = (case decode commandB of
                   Right x -> x
                   Left s -> Command "Nuthin" []
                ) :: Command
  --print command
  ns <- takeMVar mns
  let (ns', output) = case runCommand command (tree ns) of
                        Left err -> (ns, MText (T.pack err))
                        Right (o, t', _) -> (setTree t' ns, o)
  putMVar mns ns'
  let outputB = encode output
  sendAll conn (encode (fromIntegral (B.length outputB) :: Word64))
  sendAll conn outputB
  talk conn mns
  

recvAll :: Socket -> Int -> IO ByteString
recvAll s nr = go nr []
  where
    go 0 x = return $ B.concat (Prelude.reverse x)
    go n x = do
        bs <- recv s n
        if B.length bs == 0
            then return (error "socket closed...")
            else go (n - B.length bs) (bs:x)
--        go (n - B.length bs) (bs:x)

