module TimeTracker where

import Control.Monad.State
import Data.Time

-- Buffer of timestamps
type Timestamp = UTCTime
type Timestamps = [Timestamp]

data Context = Context
    { projectPath :: !FilePath
    , timestamps :: !Timestamps
    }

mapProjectPath :: (FilePath -> FilePath) -> Context -> Context
mapProjectPath f ctx = ctx{projectPath = f $ projectPath ctx}

getProjectPath :: TimeTrackerIO FilePath
getProjectPath = do
    ctx <- get
    return $ projectPath ctx

mapTimestamps :: (Timestamps -> Timestamps) -> Context -> Context
mapTimestamps f ctx = ctx{timestamps = f $ timestamps ctx}

getTimestamps :: TimeTrackerIO Timestamps
getTimestamps = do
    ctx <- get
    return $ timestamps ctx

-- Program monad
type TimeTrackerT = StateT Context
type TimeTrackerIO = TimeTrackerT IO
type TimeTracker = TimeTrackerIO ()

runTimeTrackerT :: (Monad m) => FilePath -> TimeTrackerT m a -> m a
runTimeTrackerT projectPath tt = fst <$> runStateT tt (Context projectPath [])

runTimeTracker :: FilePath -> TimeTracker -> IO ()
runTimeTracker = runTimeTrackerT
