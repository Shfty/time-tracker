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
import TimeTracker.Input
import Control.Concurrent

tickHz = 15
tickRate = 1.0 / tickHz

-- Configure stdin for transparent per-char input
configureStdin :: IO ()
configureStdin = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering

-- Main loop
mainLoop :: Channel -> TimeTracker
mainLoop chan = do
    let cont = mainLoop chan
    i <- lift (readChan chan)
    let bindings' = bindings cont
    let binding = Data.Map.lookup i bindings'
    catch (fromMaybe cont binding) handleError

-- Entry point
main = do
    -- Load config
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

    -- Fork threads and receive communication channel
    chan <- forkThreads tickRate

    -- Enter main loop
    runTimeTracker projectFile $ do
        loadProject
        mainLoop chan
