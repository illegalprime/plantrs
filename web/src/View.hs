module View where

import Api (OnlinePlant (OnlinePlant, online, plant))
import Models (Plant (Plant, plantLabel, plantName))
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx qualified as X
import Text.Printf (printf)

index :: [OnlinePlant] -> Html
index plants = page $ do
  title
  plantCards plants

plantCards :: [OnlinePlant] -> Html
plantCards plants = do
  H.section ! A.class_ "section" $ do
    H.div ! A.class_ "container is-max-desktop" $ do
      H.div ! A.class_ "columns" $ do
        forM_ plants plantCard

plantCard :: OnlinePlant -> Html
plantCard OnlinePlant {online, plant} = do
  let Plant {plantLabel, plantName} = plant
  -- TODO: add ui success/failure feedback & timeout
  let waterReq = X.hxPost $ fromString $ printf "/%s/water?t=5" plantName
  H.div ! A.class_ "column is-one-third" $ do
    H.div ! A.class_ "card" $ do
      plantImage
      H.div ! A.class_ "card-content" $ do
        H.div ! A.class_ "content" $ do
          H.h2 (H.toHtml plantLabel)
          H.div ! A.class_ "block is-vcentered" $ do
            onlineIndicator online
          H.div ! A.class_ "block has-text-right" $ do
            H.button ! A.class_ "button" ! X.hxTrigger "click" ! X.hxSwap "none" ! waterReq $ do
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
    H.script
      ! A.src "https://unpkg.com/htmx.org@1.9.10"
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
