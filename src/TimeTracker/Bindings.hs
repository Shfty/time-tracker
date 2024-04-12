module TimeTracker.Bindings where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Map
import Data.Maybe
import TimeTracker
import TimeTracker.Actions

-- Mapping from input character to action.
bindings :: TimeTracker -> Map Char TimeTracker
bindings inputLoop =
    fromList
        [ (' ', togglePause >> inputLoop)
        , ('\n', printSummary >> inputLoop)
        , ('q', quit)
        ]

-- Read a character from standard input,
-- try to lookup a corresponding binding,
-- and run it if successful.
inputLoop :: TimeTracker
inputLoop = do
    i <- lift getChar
    let bindings' = bindings inputLoop
    let binding = Data.Map.lookup i bindings'
    catch (fromMaybe inputLoop binding) handleError
