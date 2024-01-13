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
import Text.Blaze.Html5 (Html)
import View qualified

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Text
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> AddReq -> Handler Text
addHandler commander plant AddReq {a, b} = do
  liftIO $ commander plant $ Add a b

infoHandler :: ConnectionPool -> Text -> Handler (Maybe Plant)
infoHandler = findPlant

labelHandler :: ConnectionPool -> Text -> LabelReq -> Handler ()
labelHandler db plant LabelReq {label} = labelPlant db plant label

indexHandler :: (MonadIO m) => m [OnlinePlant] -> m Html
indexHandler = fmap View.index

server :: FilePath -> ConnectionPool -> Commander -> MVar (Set Text) -> Server AppApi
server static db cmd clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> infoHandler db
    :<|> labelHandler db
    :<|> readPlants
    :<|> indexHandler readPlants
    :<|> serveDirectoryWebApp static
  where
    readPlants = allPlants db clients

app :: ConnectionPool -> Commander -> MVar (Set Text) -> IO Application
app db commander clients = do
  -- directory to serve static files from
  static <- getDataDir
  -- build app description
  pure $ simpleCors $ serve appApi $ server static db commander clients

allPlants :: (MonadIO m) => ConnectionPool -> MVar (Set Text) -> m [OnlinePlant]
allPlants db onlineState = do
  plants <- listPlants db
  online <- readMVar onlineState
  pure $ map (decorateOnline online) plants
  where
    isOnline online Plant {plantName} = Set.member plantName online
    decorateOnline online plant = OnlinePlant {plant, online = isOnline online plant}
