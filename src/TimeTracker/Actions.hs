module TimeTracker.Actions where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.State
import Data.Foldable
import Data.Map (lookup)
import Data.Time (diffUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as C
import Data.Time.Format
import Data.Yaml
import System.Directory
import System.FilePath
import System.Log.Logger
import TimeTracker

-- Monadic error handler
handleError :: AsyncException -> TimeTracker
handleError UserInterrupt = quit
handleError e = throwM e

-- print lifted into the TimeTracker monad.
print' :: (Show a) => a -> TimeTracker
print' = lift . Prelude.print

-- putStrLn lifted into the TimeTracker monad.
putStrLn' :: String -> TimeTracker
putStrLn' = lift . Prelude.putStrLn

-- putStr lifted into the TimeTracker monad.
putStr' :: String -> TimeTracker
putStr' = lift . Prelude.putStr

-- Get a UTCTime within the TimeTracker monad.
getCurrentTime :: TimeTrackerIO UTCTime
getCurrentTime = lift C.getCurrentTime

-- Fetch the current UTC time and add it to the current state
pushTimeStamp :: TimeTrackerIO Timestamp
pushTimeStamp = do
    time <- getCurrentTime
    modify $ mapTimestamps (<> [time])
    return time

isPaused :: TimeTrackerIO Bool
isPaused = gets (even . length . timestamps)

setPaused :: TimeTracker
setPaused = do
    paused <- isPaused
    unless paused $ void pushTimeStamp
    saveProject

setRunning :: TimeTracker
setRunning = do
    paused <- isPaused
    when paused $ void pushTimeStamp
    saveProject

-- Pause or resume the task by adding an entry to the timestamps array.
togglePause :: TimeTracker
togglePause = do
    paused <- isPaused
    if paused
        then setRunning
        else setPaused
    printSummary

-- NominalDiffTime of zero length
zeroDiffTime :: NominalDiffTime
zeroDiffTime = secondsToNominalDiffTime 0

-- Convert a list of timestamps into a list of pairs,
-- using the current time in cases where only one element is available
timestampPairs :: Timestamps -> TimeTrackerIO [(Timestamp, Timestamp)]
timestampPairs (x : y : tail) = do
    tail' <- timestampPairs tail
    return $ (x, y) : tail'
timestampPairs [x] = do
    y <- getCurrentTime
    return [(x, y)]
timestampPairs [] = return []

-- Convert a list of timestamps into (start, end) pairs,
-- diff them to produce a list of durations, and sum it into a total
totalTime :: TimeTrackerIO NominalDiffTime
totalTime = do
    ts <- getTimestamps
    pairs <- timestampPairs ts
    let diffs = uncurry (flip diffUTCTime) <$> pairs
    return $ foldl' (+) zeroDiffTime diffs

-- Log the current timestamp state for debugging
logTimestamps :: TimeTracker
logTimestamps = do
    ts <- getTimestamps
    lift $ debugM "TimeTracker.Timestamps" $ show ts

clearScreen :: TimeTracker
clearScreen = putStr' "\ESC[2J"

moveToTopLeft :: TimeTracker
moveToTopLeft = putStr' "\ESC[H\ESC0\ESC0"

-- Print the current project path
printProjectPath :: TimeTracker
printProjectPath = do
    path <- getProjectPath
    putStrLn' $ "Project: " ++ takeBaseName path

-- Print whether the tracker is paused or running
printPaused :: TimeTracker
printPaused = do
    paused <- isPaused
    if paused
        then putStrLn' "Paused"
        else putStrLn' "Running"

-- Print the total time for this project
printTotal :: TimeTracker
printTotal = do
    total <- totalTime
    putStrLn' $ "Total: " ++ formatTime defaultTimeLocale "%H Hours %M Minutes %S Seconds" total

-- Print a summary of the current task state.
printSummary :: TimeTracker
printSummary = do
    clearScreen
    moveToTopLeft
    printProjectPath
    putStrLn' ""
    printPaused
    printTotal
    putStrLn' ""
    putStrLn' "Space: Toggle Pause | Return: Refresh"
    logTimestamps

-- End the program.
quit :: TimeTracker
quit = do
    setPaused
    putStrLn' "Quitting..."

-- Attempt to load the project from its path,
-- initializing a new one if it does not exist,
-- and throwing any other errors that occur during YAML parsing
loadProject :: TimeTracker
loadProject = do
    path <- getProjectPath

    lift $ infoM "TimeTracker.Load" $ "Loading project file from " ++ path ++ "..."

    exists <- lift $ doesFileExist path

    if not exists
        then do
            lift $ infoM "TimeTracker.Load" "Project file does not exist, initializing..."
            saveProject
        else do
            file <- lift $ decodeFileEither path
            case file of
                Left e -> do
                    error $ show e
                Right f -> do
                    lift $ infoM "TimeTracker.Load" "Done."
                    modify $ mapTimestamps (const f)

-- Save the project to its given path
saveProject :: TimeTracker
saveProject = do
    path <- getProjectPath
    lift $ infoM "TimeTracker.Save" $ "Saving project file to " ++ path ++ "..."
    ts <- getTimestamps
    lift $ encodeFile path ts
