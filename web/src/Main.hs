module Main where

import BroadcastChan (newBChanListener, newBroadcastChan)
import Commands (runCommand)
import Config
import Control.Concurrent (forkIO, newChan)
import Control.Lens ((^.))
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Yaml (ToJSON (toJSON))
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database (listPlants)
import Database.Persist.Sqlite (createSqlitePool, runMigration, runSqlPool)
import Discovery (foldOnline, runMqtt)
import Models (migrateAll)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Schedule (schedulePlants)
import Server (app)
import Text.Printf (printf)

main :: IO ()
main = do
  -- load configuration file
  cfg <- loadYamlSettingsArgs [toJSON defaultConfig] useEnv :: IO Configuration
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
  schedules <- schedulePlants commander =<< listPlants pool
  -- build web app
  api <- app pool commander schedules onlineStatus
  -- spawn webserver on port
  putStrLn $ printf "Running server on port %d..." (cfg ^. port)
  run (cfg ^. port) api
