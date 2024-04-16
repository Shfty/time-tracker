module TimeTracker.Time where

import Data.Time (NominalDiffTime, secondsToNominalDiffTime)

secondsToMicros :: Float -> Int
secondsToMicros = round . (* 1000000)

-- NominalDiffTime of zero length
zeroDiffTime :: NominalDiffTime
zeroDiffTime = secondsToNominalDiffTime 0

