module TimeTracker.Config where

import Data.Map
import System.Directory
import Data.Yaml
import System.Log.Logger

type Config = Map String String

loadConfig = do
    -- Load config.yaml file from disk
    configPath <- getXdgDirectory XdgConfig "time-tracker/config.yaml"
    infoM "TimeTracker.Config" $ "Loading config from " ++ configPath ++ "..."

    -- Decode YAML into a String-String map
    configYaml <- decodeFileThrow configPath :: IO Config
    debugM "TimeTracker.Config" $ "Config: " ++ show configYaml

    return configYaml

configMaybe :: (MonadFail m) => String -> Config -> m String
configMaybe key config = do
    -- Extract base directory value
    case Data.Map.lookup key config of
        Just value -> return value
        _otherwise -> fail $ "No " ++ key ++ " key in config.yaml"

