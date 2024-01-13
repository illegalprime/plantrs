module Server (
  app,
) where

import Api
import Commands (Commander)
import Data.Set qualified as Set
import Database (findPlant, labelPlant, listPlants)
import Database.Persist.Sql (ConnectionPool)
import Models (Plant (Plant, plantName))
import Network.Wai.Middleware.Cors (simpleCors)
import Paths_plantrs (getDataDir)
import Servant

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Text
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> AddReq -> Handler Text
addHandler commander plant AddReq {a, b} = do
  liftIO $ commander plant $ Add a b

discoverHandler :: ConnectionPool -> MVar (Set Text) -> Handler [OnlinePlant]
discoverHandler db onlineState = do
  plants <- listPlants db
  online <- readMVar onlineState
  pure $ map (decorateOnline online) plants
  where
    isOnline online Plant {plantName} = Set.member plantName online
    decorateOnline online plant = OnlinePlant {plant, online = isOnline online plant}

infoHandler :: ConnectionPool -> Text -> Handler (Maybe Plant)
infoHandler = findPlant

labelHandler :: ConnectionPool -> Text -> LabelReq -> Handler ()
labelHandler db plant LabelReq {label} = labelPlant db plant label

server :: FilePath -> ConnectionPool -> Commander -> MVar (Set Text) -> Server AppApi
server static db cmd clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> infoHandler db
    :<|> labelHandler db
    :<|> discoverHandler db clients
    :<|> serveDirectoryFileServer static

app :: ConnectionPool -> Commander -> MVar (Set Text) -> IO Application
app db commander clients = do
  -- directory to serve static files from
  static <- getDataDir
  -- build app description
  pure $ simpleCors $ serve appApi $ server static db commander clients
