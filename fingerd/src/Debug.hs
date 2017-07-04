module Main where

import           Control.Monad             (forever)
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
    (s, _) <- accept sock
    printAndKickback s
    close s
    where
        printAndKickback conn = do
            msg <- recv conn 1024
            print msg
            sendAll conn msg

main :: IO ()
main = do -- withSocketsDo $ do
    addrinfos <- getAddrInfo
                (Just (defaultHints
                        { addrFlags = [AI_PASSIVE] }))
                Nothing (Just "12345") -- 79 is the real port, only available when run as root
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr)
                Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    logAndEcho sock
    close sock
