module Main where

import BroadcastChan (newBChanListener, newBroadcastChan)
import Commands (runCommand)
import Config
import Control.Concurrent (forkIO, newChan)
import Control.Lens ((^.))
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Yaml (ToJSON (toJSON), encode)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database (findOnline, listOnline, listPlants)
import Database.Persist.Sqlite (createSqlitePool, runMigration, runSqlPool)
import Discovery (foldOnline, runMqtt)
import Models (migrateAll)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Paths_plantrs (getDataDir)
import Schedule (schedulePlants)
import Server (AppEnv (AppEnv), app)
import Text.Printf (printf)

main :: IO ()
main = do
  -- load configuration file
  cfg <- loadYamlSettingsArgs [toJSON defaultConfig] useEnv :: IO Configuration
  -- print current configuration
  putBSLn $ encode cfg
  -- load database
  pool <- runStderrLoggingT $ case cfg ^. db of
    Sqlite path -> createSqlitePool path 5
  -- run migrations
  runSqlPool (runMigration migrateAll) pool
  -- new broadcast channel
  broadChan <- newBroadcastChan
  -- broadcast receiver generator
  let getSubChan = newBChanListener broadChan
  -- messages to the broker
  pubChan <- newChan
  -- online status messages
  online <- newChan
  -- online aggregate status
  onlineStatus <- newMVar Set.empty
  -- set up command handler
  let commander = runCommand (cfg ^. topics) (getSubChan, pubChan)
  -- broker uri
  let uri = fromJust $ parseURI $ toString (cfg ^. mqtt)
  -- start broker service
  _ <- forkIO $ runMqtt uri (cfg ^. topics) (broadChan, pubChan) online
  -- match up online clients with the db
  _ <- forkIO $ foldOnline pool onlineStatus online
  -- schedule crons
  schedules <- schedulePlants pool commander =<< listPlants pool
  -- find directory for static files
  serveDir <- getDataDir
  -- make app state
  let env =
        AppEnv
          pool
          commander
          schedules
          (listOnline pool onlineStatus)
          (findOnline pool onlineStatus)
          serveDir
  -- spawn webserver on port
  putStrLn $ printf "Running server on port %d..." (cfg ^. port)
  run (cfg ^. port) $ app env
