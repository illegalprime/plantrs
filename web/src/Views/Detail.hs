module Views.Detail where

import Api (HasOnline (online), HasPlant (plant), OnlinePlant)
import Control.Lens ((^.))
import Data.Time (TimeZone, UTCTime)
import Models (label, name, waterCron, waterVolume)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx qualified as X
import Text.Printf (printf)
import Views.Common qualified as C

index :: (UTCTime, TimeZone) -> OnlinePlant -> Html
index now oPlant = C.page $ do
  let isOnline = oPlant ^. online
      onlineClass = if isOnline then "is-success" else "is-danger"
      pLabel = fromString . toString $ oPlant ^. plant . label
      labelReq = X.hxPost $ fromString $ printf "/%s/label" $ oPlant ^. plant . name
  H.section ! C.classes ["hero", onlineClass, "mt-5"] $ do
    H.div ! A.class_ "hero-body" $ do
      H.form ! A.class_ "title" ! A.onsubmit "this[0].blur()" ! labelReq $ do
        H.input ! A.name "_label" ! X.hxSwap "none" ! A.class_ "title-edit" ! A.type_ "text" ! A.value pLabel
        H.i ! A.class_ "fa-regular fa-pen-to-square fa-2xs" $ ""
      H.p ! A.class_ "subtitle" $ do
        H.b $ if isOnline then "Online" else "Offline"
        H.span ! A.class_ "pl-5" $ ""
        C.displaySmallSchedule
          now
          (oPlant ^. plant . waterCron)
          (oPlant ^. plant . waterVolume)

-- TODO: display & edit schedule/volume
-- TODO: display history
-- TODO: edit cover image
-- TODO: show instructional terminal card with info to connect devices
