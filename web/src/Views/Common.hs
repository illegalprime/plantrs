module Views.Common where

import Control.Error (failWith)
import Control.Lens ((^.))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans.Except (except)
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, formatTime, utcToZonedTime)
import Models (Plant, waterCron, waterVolume)
import System.Cron (CronSchedule, defaultOpts, describe, nextMatch, parseCronSchedule)
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
classes = A.class_ . H.toValue . unwords

displayNextWater :: (UTCTime, TimeZone) -> Plant -> Except String String
displayNextWater (time, zone) p = do
  cron <- parseCron $ p ^. waterCron
  next <- failWith "No upcoming watering." $ nextMatch cron time
  let zonedTime = utcToZonedTime zone next
  let nextTime = formatTime defaultTimeLocale "%b %e, %l:%M%P" zonedTime
  pure $ printf "Will water %ds on %s." (p ^. waterVolume) nextTime

displaySchedule :: Plant -> Except String String
displaySchedule p = do
  cron <- parseCron (p ^. waterCron)
  pure $ printf "%s, for %ds." (describe defaultOpts cron) (p ^. waterVolume)

parseCron :: Maybe Text -> Except String CronSchedule
parseCron mCron = do
  failWith "No watering schedule." mCron >>= except . parseCronSchedule

exceptToHtml :: Except String String -> Html
exceptToHtml = either (H.i . H.toHtml) H.toHtml . runExcept

bulmaField :: Maybe Html -> Html -> Html
bulmaField fLabel field = do
  H.div ! A.class_ "field" $ do
    forM_ fLabel $ H.label ! A.class_ "label"
    H.div ! A.class_ "control" $ field
