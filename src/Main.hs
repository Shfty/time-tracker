import Control.Monad.State
import Control.Monad.Catch
import Data.Map
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Log.Logger
import TimeTracker
import TimeTracker.Actions
import TimeTracker.Bindings
import TimeTracker.Config

main = do
    configYaml <- loadConfig

    -- Extract base directory path and create it if missing
    baseDir <- configMaybe "baseDir" configYaml
    infoM "TimeTracker.Main" $ "Base Directory: " ++ baseDir
    createDirectoryIfMissing True baseDir

    -- Retrieve target project from CLI arguments
    args <- getArgs
    let project = case args of
            [project] -> project
            _otherwise -> error "No project specified"

    -- Derive project file path from base dir and project name
    let projectFile = baseDir </> project <.> "yaml"

    -- Configure stdin for transparent per-char input
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering

    -- Enter main loop
    runTimeTracker projectFile $ do
        loadProject
        putStrLn' ""
        printProjectPath
        putStrLn' ""
        printSummary
        inputLoop

