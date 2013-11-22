module ChannelTest where

import Concurrent(Channel)
import Concurrent(ConcBase)

main :: IO()
main = do c1 <- newChan :: IO(Chan Int)
          c2 <- newChan :: IO(Chan Int)
          forkIO (client c1 c2)
          forkIO (server c2 c1)

client :: Chan Int -> Chan Int -> IO ()
client cin cout = undefined

server :: Chan Int -> Chan Int -> IO ()
server cin cout = undefined
