module Views.Common where

import Data.Time (TimeZone, UTCTime, defaultTimeLocale, formatTime, utcToZonedTime)
import System.Cron (nextMatch, parseCronSchedule)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx qualified as X
import Text.Printf (printf)

page :: Html -> Html
page content = do
  H.docTypeHtml $ do
    header
    H.body $ do
      title
      content
      toaster

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
    $ H.a
      ! A.href "/"
      ! A.class_ "is-size-1 level-item has-text-centered"
    $ "Plant.rs"

toaster :: Html
toaster = do
  H.div ! A.id "toaster" ! X.hxExt "remove-me" $ ""

toast :: Bool -> Text -> Html
toast isError text = do
  H.div
    ! A.class_ ("notification " <> colorErr isError)
    ! H.customAttribute "remove-me" "3s"
    $ do
      H.b $ H.toHtml text
  where
    colorErr True = "is-danger"
    colorErr False = "is-success"

classes :: [Text] -> H.Attribute
classes = A.class_ . fromString . toString . unwords

displaySmallSchedule :: (UTCTime, TimeZone) -> Maybe Text -> Word32 -> Html
displaySmallSchedule _ Nothing _ = H.i "No water schedule."
displaySmallSchedule (now, zone) (Just cron) vol = case nextTime of
  Left err -> H.i $ H.toHtml err
  Right Nothing -> H.i "No upcoming watering."
  Right (Just next) -> H.toHtml $ text $ fmtTime next
  where
    text :: String -> String
    text = printf "Will water %ds on %s." vol
    fmtTime = formatTime defaultTimeLocale "%b %e, %l:%M%P"
    nextTime = utcToZonedTime zone <<$>> second (`nextMatch` now) (parseCronSchedule cron)
