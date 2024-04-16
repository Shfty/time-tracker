module TimeTracker.Input where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import TimeTracker.Time

data Input = Tick | Char !Char deriving (Eq, Ord, Show)
type Channel = Chan Input

tickChan :: Channel -> IO ()
tickChan chan = writeChan chan Tick

inputThread :: Channel -> IO a
inputThread chan = do
    getChar >>= writeChan chan . Char
    inputThread chan

tickThread :: Float -> Channel -> IO a
tickThread tick chan = do
    tickChan chan
    threadDelay $ secondsToMicros tick
    tickThread tick chan

-- Fork tick and input threads
forkThreads :: Float -> IO Channel
forkThreads tick = do
    -- Create channel
    chan <- newChan

    -- Perform initial tick to ensure MVar is populated
    tickChan chan

    -- Fork threads
    forkIO $ tickThread tick chan
    forkIO $ inputThread chan

    -- Return channel
    return chan

