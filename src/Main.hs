import Control.Monad.Catch
import Control.Monad.State
import Data.Map
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Log.Logger
import TimeTracker
import TimeTracker.Actions
import TimeTracker.Bindings
import TimeTracker.Config

import Control.Concurrent
import Control.Concurrent.Timer

-- Configure stdin for transparent per-char input
configureStdin :: IO ()
configureStdin = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering

inputThread :: Chan Char -> IO a
inputThread chan = do
    getChar >>= writeChan chan
    inputThread chan

outputThreadWith :: (Char -> TimeTrackerIO a) -> Chan Char -> TimeTrackerIO a
outputThreadWith f chan = do
    lift (readChan chan) >>= f
    outputThreadWith f chan

outputThread :: Chan Char -> TimeTracker
outputThread = outputThreadWith $ lift . putStrLn . ("Input: " ++) . show

secondsToMicros :: Int -> Int
secondsToMicros = (* 1000000)

tickThread chan = do
    writeChan chan '\n'
    threadDelay $ secondsToMicros 1
    tickThread chan

inputLoop' :: Chan Char -> TimeTracker
inputLoop' chan = do
    i <- lift (readChan chan)
    let bindings' = bindings (inputLoop' chan)
    let binding = Data.Map.lookup i bindings'
    catch (fromMaybe (inputLoop' chan) binding) handleError

main = do
    configYaml <- loadConfig

    -- Extract base directory path and create it if missing
    baseDir <- configMaybe "baseDir" configYaml
    infoM "TimeTracker.Main" $ "Base Directory: " ++ baseDir
    createDirectoryIfMissing True baseDir

    -- Retrieve target project from CLI arguments
    args <- getArgs
    project <- case args of
        [project] -> return project
        _otherwise -> do
            errorM "TimeTracker.Main" "Error: No project specified"
            exitFailure

    -- Derive project file path from base dir and project name
    let projectFile = baseDir </> project <.> "yaml"

    -- Configure stdin for transparent per-char input
    configureStdin

    -- Fork input and tick threads
    chan <- newChan
    forkIO $ inputThread chan
    forkIO $ tickThread chan

    -- Enter main loop
    runTimeTracker projectFile $ do
        loadProject
        putStrLn' ""
        printProjectPath
        putStrLn' ""
        printSummary
        inputLoop' chan
