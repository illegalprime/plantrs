module Views.Detail where

import Api (HasOnline (online), HasPlant (plant), OnlinePlant)
import Control.Lens ((^.))
import Data.Time (TimeZone, UTCTime)
import Models (label, name)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx qualified as X
import Text.Printf (printf)
import Views.Common qualified as C

index :: (UTCTime, TimeZone) -> OnlinePlant -> Html
index now oPlant = C.page $ do
  -- plant name & short info
  detailBanner now oPlant
  -- various plant details / actions
  let plantActions =
        [ scheduleCard oPlant
        ]
  -- display plant actions as columns
  H.section ! A.class_ "section" $ do
    H.div ! A.class_ "columns" $ do
      forM_ plantActions $ \a -> do
        H.div ! A.class_ "column is-half" $ a

detailBanner :: (UTCTime, TimeZone) -> OnlinePlant -> Html
detailBanner now oPlant = do
  let isOnline = oPlant ^. online
      onlineClass = if isOnline then "is-success" else "is-danger"
      pLabel = fromString . toString $ oPlant ^. plant . label
      labelReq = X.hxPost $ fromString $ printf "/%s/label" $ oPlant ^. plant . name
  H.section ! C.classes ["hero", onlineClass, "mt-5"] $ do
    H.div ! A.class_ "hero-body" $ do
      H.form ! A.class_ "title" ! A.onsubmit "this[0].blur()" ! labelReq $ do
        H.input ! A.name "_label" ! X.hxSwap "none" ! A.class_ "title-edit" ! A.type_ "text" ! A.value pLabel
        H.i ! A.class_ "fa-regular fa-pen-to-square fa-2xs" $ ""
      H.p ! A.class_ "subtitle mb-0" $ do
        C.exceptToHtml $ C.displayNextWater now (oPlant ^. plant)
      H.p ! A.class_ "subtitle" $ do
        H.i $ if isOnline then "Online" else "Offline"

scheduleCard :: OnlinePlant -> Html
scheduleCard oPlant = do
  H.div ! A.class_ "card" $ do
    H.div ! A.class_ "card-content" $ do
      H.p ! A.class_ "title" $ "Watering Schedule"
      H.hr
      H.p ! A.class_ "subtitle" $ do
        H.i "Currently (UTC): "
        C.exceptToHtml $ C.displaySchedule (oPlant ^. plant)
      H.hr
      scheduleForm oPlant

scheduleForm :: OnlinePlant -> Html
scheduleForm oPlant = do
  H.form ! X.hxPost "/simple-schedule" $ do
    C.bulmaField (Just "Water duration (seconds):") $ do
      H.input ! A.name "_volume" ! A.class_ "input" ! A.type_ "number" ! A.placeholder "8"
    C.bulmaField (Just "Time of day:") $ do
      H.input ! A.name "_time" ! A.class_ "input" ! A.type_ "time"
    C.bulmaField (Just "Repeat every (days):") $ do
      H.input ! A.name "_repeat" ! A.class_ "input" ! A.type_ "number" ! A.placeholder "2"
    let pName = fromString . toString $ oPlant ^. plant . name
    H.input ! A.type_ "hidden" ! A.name "_name" ! A.value pName
    C.bulmaField Nothing $ do
      H.button ! A.class_ "button is-primary" $ "Update"

-- TODO: display history
-- TODO: edit cover image
-- TODO: show instructional terminal card with info to connect devices
