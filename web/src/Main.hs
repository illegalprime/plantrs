module Main where

import Commands (runCommand)
import Config
import Control.Concurrent (forkIO, newChan)
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
  Configuration {port, mqtt, topics, db} <- loadYamlSettingsArgs [toJSON defaultConfig] useEnv
  -- load database
  pool <- runStderrLoggingT $ case db of
    Sqlite path -> createSqlitePool path 5
  -- run migrations
  runSqlPool (runMigration migrateAll) pool
  -- messages from the broker
  rx <- newChan
  -- messages to the broker
  tx <- newChan
  -- online status messages
  online <- newChan
  -- online aggregate status
  onlineStatus <- newMVar Set.empty
  -- set up command handler
  let commander = runCommand topics (rx, tx)
  -- broker uri
  let uri = fromJust $ parseURI $ toString mqtt
  -- start broker service
  _ <- forkIO $ runMqtt uri topics (rx, tx) online
  -- match up online clients with the db
  _ <- forkIO $ foldOnline pool onlineStatus online
  -- schedule crons
  schedules <- schedulePlants commander =<< listPlants pool
  -- build web app
  api <- app pool commander schedules onlineStatus
  -- spawn webserver on port
  putStrLn $ printf "Running server on port %d..." port
  run port api
