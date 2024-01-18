module View where

import Api (HasOnline (online), HasPlant (plant), OnlinePlant)
import Control.Lens ((^.))
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, formatTime, utcToZonedTime)
import Models (label, name, waterCron, waterVolume)
import System.Cron (nextMatch, parseCronSchedule)
import Text.Blaze.Html qualified as A
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx qualified as X
import Text.Printf (printf)

toastId :: String
toastId = "toaster"

index :: [OnlinePlant] -> (UTCTime, TimeZone) -> Html
index plants now = page $ do
  title
  plantCards plants now
  H.div ! A.id (fromString toastId) ! X.hxExt "remove-me" $ ""

plantCards :: [OnlinePlant] -> (UTCTime, TimeZone) -> Html
plantCards plants now = do
  H.section ! A.class_ "section" ! X.hxGet "/plant-cards" ! X.hxTrigger "every 5s" ! X.hxSwap "outerHTML" $ do
    H.div ! A.class_ "container is-max-desktop" $ do
      H.div ! A.class_ "columns" $ do
        forM_ plants (plantCard now)

plantCard :: (UTCTime, TimeZone) -> OnlinePlant -> Html
plantCard now oPlant = do
  let waterReq = X.hxPost $ fromString $ printf "/%s/water?t=5" $ oPlant ^. plant . name
  H.div ! A.class_ "column is-one-third" $ do
    H.div ! A.class_ "card" $ do
      plantImage
      H.div ! A.class_ "card-content" $ do
        H.div ! A.class_ "content" $ do
          H.h2 $ H.toHtml $ oPlant ^. plant . label
          H.div ! A.class_ "block is-vcentered" $ do
            onlineIndicator (oPlant ^. online)
            case oPlant ^. plant . waterCron of
              Nothing -> H.p $ H.i "no water schedule"
              Just cron -> displaySmallSchedule now cron $ oPlant ^. plant . waterVolume
          H.div ! A.class_ "block has-text-right" $ do
            H.button
              ! A.class_ "button"
              ! X.hxTrigger "click"
              ! A.customAttribute "hx-indicator" ".spinner"
              ! A.customAttribute "hx-disabled-elt" "this"
              ! X.hxSwap "none"
              ! waterReq
              $ do
                H.p ! A.class_ "spinner mb-0" $ do
                  "Working..."
                  H.i ! A.class_ "ml-3 fa-solid fa-spinner fa-spin" $ ""
                H.p ! A.class_ "spinner-hide" $ do
                  "Water Now"
                  H.i ! A.class_ "ml-3 fa-solid fa-droplet" $ ""

onlineIndicator :: Bool -> Html
onlineIndicator isOnline = H.p $ do
  H.i ! A.class_ "fa-sharp fa-solid fa-circle fa-sm" ! A.style (style isOnline) $ ""
  " " <> text isOnline
  where
    text True = "Online"
    text False = "Offline"
    style True = "color: #4adc4b;"
    style False = "color: #d94c50;"

displaySmallSchedule :: (UTCTime, TimeZone) -> Text -> Word32 -> Html
displaySmallSchedule (now, zone) cron vol = case nextTime of
  Left err -> H.i $ H.toHtml err
  Right Nothing -> H.i "No upcoming watering."
  Right (Just next) -> H.p $ H.toHtml $ text $ fmtTime next
  where
    text :: String -> String
    text = printf "Will water %ds on %s" vol
    fmtTime = formatTime defaultTimeLocale "%b %e, %l:%M%P"
    nextTime = utcToZonedTime zone <<$>> second (`nextMatch` now) (parseCronSchedule cron)

plantImage :: Html
plantImage = do
  H.div ! A.class_ "card-image" $ do
    H.figure ! A.class_ "image is-4by3" $ do
      H.img ! A.src "/images/house-plants.webp"

header :: Html
header = do
  H.head $ do
    H.title "Plant.rs"
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.link
      ! A.rel "stylesheet"
      ! A.href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
    H.link
      ! A.rel "stylesheet"
      ! A.href "/main.css"
    H.script
      ! A.src "https://unpkg.com/htmx.org@1.9.10"
      $ ""
    H.script
      ! A.src "https://unpkg.com/htmx.org@1.9.10/dist/ext/remove-me.js"
      $ ""
    H.script
      ! A.src "https://kit.fontawesome.com/4abb0f2a3e.js"
      $ ""

title :: Html
title = do
  H.nav
    ! A.class_ "level mt-5 mb-0"
    $ H.p
      ! A.class_ "is-size-1 level-item has-text-centered"
    $ "Plant.rs"

page :: Html -> Html
page content = do
  H.docTypeHtml $ do
    header
    H.body content

toast :: Bool -> Text -> Html
toast isError text = do
  H.div
    ! A.class_ ("notification " <> colorErr isError)
    ! A.customAttribute "remove-me" "3s"
    $ do
      H.b $ H.toHtml text
  where
    colorErr True = "is-danger"
    colorErr False = "is-success"
