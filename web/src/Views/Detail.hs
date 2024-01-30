module Views.Detail where

import Control.Lens ((^.), (^?))
import Data.Time (TimeZone, UTCTime)
import HttpApi (HasOnline (online), HasPlant (plant), OnlinePlant)
import Models qualified as M
import MqttApi as Q
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx qualified as X
import Text.Printf (printf)
import Views.Common qualified as C

index :: (UTCTime, TimeZone) -> OnlinePlant -> [M.ActivityLog] -> Html
index now oPlant history = C.page $ do
  -- plant name & short info
  detailBanner now oPlant
  -- various plant details / actions
  let plantActions =
        [ scheduleCard (snd now) oPlant
        , activityCard (snd now) history
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
      pLabel = H.toValue $ oPlant ^. plant . M.label
      labelReq = X.hxPost $ H.stringValue $ printf "/%s/label" $ oPlant ^. plant . M.name
  H.section ! C.classes ["hero", onlineClass, "mt-5"] $ do
    H.div ! A.class_ "hero-body" $ do
      H.form ! A.class_ "title" ! A.onsubmit "this[0].blur()" ! labelReq $ do
        H.input ! A.name "_label" ! X.hxSwap "none" ! A.class_ "title-edit" ! A.type_ "text" ! A.value pLabel
        H.i ! A.class_ "fa-regular fa-pen-to-square fa-2xs" $ ""
      H.p ! A.class_ "subtitle mb-0" $ do
        C.exceptToHtml $ C.displayNextWater now (oPlant ^. plant)
      H.p ! A.class_ "subtitle" $ do
        H.i $ if isOnline then "Online" else "Offline"

scheduleCard :: TimeZone -> OnlinePlant -> Html
scheduleCard zone oPlant = do
  H.div ! A.class_ "card" $ do
    H.div ! A.class_ "card-content" $ do
      H.p ! A.class_ "title" $ "Watering Schedule"
      H.p ! A.class_ "subtitle" $ do
        H.i "Currently: "
        C.exceptToHtml $ C.displaySchedule zone (oPlant ^. plant)
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
    let pName = H.toValue $ oPlant ^. plant . M.name
    H.input ! A.type_ "hidden" ! A.name "_name" ! A.value pName
    C.bulmaField Nothing $ do
      H.button ! A.class_ "button is-primary" $ "Update"

activityCard :: TimeZone -> [M.ActivityLog] -> Html
activityCard zone history = do
  H.div ! A.class_ "card" $ do
    H.div ! A.class_ "card-content" $ do
      H.p ! A.class_ "title" $ "Watering History"
      H.table ! A.class_ "table activity-log" $ do
        H.thead $ do
          H.tr $ do
            H.th "Date"
            H.th "Volume"
            H.th "Successful"
        H.tbody $ forM_ history $ \log -> do
          H.tr $ do
            H.td $ H.toHtml $ C.displayTime zone $ log ^. M.time
            H.td $ H.toHtml $ fmtVolume $ log ^. M.command ^? Q._Drive
            H.td $
              if log ^. M.successful
                then H.i ! A.class_ "fa-solid fa-check" ! A.style "color:#48c78e;" $ ""
                else H.i ! A.class_ "fa-solid fa-xmark" ! A.style "color:#f14668;" $ ""
  where
    fmtVolume = maybe ("?" :: Text) ((<> "s") . show)

-- TODO: unify success/error colors
-- TODO: edit cover image
-- TODO: show instructional terminal card with info to connect devices
