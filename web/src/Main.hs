module Main where

import Config
import Control.Concurrent (newChan)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Yaml (ToJSON (toJSON))
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.Persist.Sqlite (createSqlitePool, runMigration, runSqlPool)
import Discovery (initDiscover, mqttRouter)
import Models (migrateAll)
import Network.MQTT.Client
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
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
  -- list of online clients
  clients <- newMVar Set.empty
  -- messages from the broker
  msgs <- newChan
  -- connect to MQTT broker
  let uri = fromJust $ parseURI $ toString mqtt
  let callback = mqttRouter pool topics msgs clients
  mc <- connectURI mqttConfig {_msgCB = callback} uri
  -- subscribe & init client discovery
  initDiscover topics mc
  -- build web app
  api <- app pool topics mc msgs clients
  -- spawn webserver on port
  putStrLn $ printf "Running server on port %d..." port
  run port api
