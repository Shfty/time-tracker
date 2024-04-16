{-# LANGUAGE TemplateHaskell #-}

module TimeTracker where

import Control.Monad.State
import Data.Aeson.TH
import Data.Time
import System.Directory
import System.FilePath
import TimeTracker.Time

-- Buffer of timestamps
type Timestamp = UTCTime
type Timestamps = [Timestamp]

data Project = Project
    { name :: !String
    , deadline :: !UTCTime
    , timestamps :: !Timestamps
    }

$(deriveJSON defaultOptions ''Project)

defaultProject :: String -> NominalDiffTime -> IO Project
defaultProject name deadline = do
    now <- getCurrentTime
    return $ Project name (addUTCTime deadline now) [now]

mapName :: (String -> String) -> Project -> Project
mapName f proj = proj{name = f $ name proj}

getName :: TimeTrackerIO String
getName = gets (name . project)

mapDeadline :: (UTCTime -> UTCTime) -> Project -> Project
mapDeadline f proj = proj{deadline = f $ deadline proj}

getDeadline :: TimeTrackerIO UTCTime
getDeadline = gets (deadline . project)

mapTimestamps :: (Timestamps -> Timestamps) -> Project -> Project
mapTimestamps f proj = proj{timestamps = f $ timestamps proj}

getTimestamps :: TimeTrackerIO Timestamps
getTimestamps = gets (timestamps . project)

data Context = Context
    { projectPath :: !FilePath
    , project :: !Project
    }

mapProjectPath :: (FilePath -> FilePath) -> Context -> Context
mapProjectPath f proj = proj{projectPath = f $ projectPath proj}

getProjectPath :: TimeTrackerIO FilePath
getProjectPath = gets projectPath

mapProject :: (Project -> Project) -> Context -> Context
mapProject f proj = proj{project = f $ project proj}

getProject :: TimeTrackerIO Project
getProject = gets project

-- Program monad
type TimeTrackerT = StateT Context
type TimeTrackerIO = TimeTrackerT IO
type TimeTracker = TimeTrackerIO ()

runTimeTrackerT :: (Monad m, MonadIO m) => FilePath -> FilePath -> NominalDiffTime -> TimeTrackerT m a -> m a
runTimeTrackerT baseDir project deadline tt = do
    let projectDir = baseDir </> project
    liftIO $ createDirectoryIfMissing True projectDir

    -- Derive project file path from base dir and project name
    let projectFile = projectDir </> "project.yaml"

    proj <- liftIO $ defaultProject project deadline
    fst <$> runStateT tt (Context projectFile proj)

runTimeTracker :: FilePath -> FilePath -> NominalDiffTime -> TimeTracker -> IO ()
runTimeTracker = runTimeTrackerT
