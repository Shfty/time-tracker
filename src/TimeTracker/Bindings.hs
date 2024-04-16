module TimeTracker.Bindings where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Map
import Data.Maybe
import TimeTracker
import TimeTracker.Actions
import TimeTracker.Input

-- Mapping from input character to action.
bindings :: TimeTracker -> Map Input TimeTracker
bindings cont =
    fromList
        [ (Tick, tick cont)
        , (Char ' ', togglePause >> cont)
        , (Char 'q', quit)
        ]
