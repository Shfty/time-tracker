module TimeTracker.Time where

secondsToMicros :: Float -> Int
secondsToMicros = round . (* 1000000)

