module Network.Socket.ByteString.RecvAll (recvAll) where

import Network.Socket.ByteString (recv)
import Network.Socket (Socket)

import qualified Data.ByteString as B
import Data.ByteString(ByteString)


{- stolen/modified from http://hackage.haskell.org/packages/archive/commsec-keyexchange/0.1.1/doc/html/src/Network-CommSec-KeyExchange.html -}


recvAll :: Socket -> Int -> IO ByteString
recvAll s nr = go nr []
  where
    go 0 x = return $ B.concat (Prelude.reverse x)
    go n x = do
        bs <- recv s n
        if B.length bs == 0
            then return bs -- indicating that they dropped or closed connection.
            else go (n - B.length bs) (bs:x)

