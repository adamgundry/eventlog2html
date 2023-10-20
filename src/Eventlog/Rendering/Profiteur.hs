{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Eventlog.Rendering.Profiteur where

import Data.FileEmbed (embedFile)
import Data.String
import Data.Text.Encoding (decodeUtf8)
import Eventlog.Javascript
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BC8
import qualified Profiteur.Core as Profiteur

embedProfiteurIframe :: Profiteur.NodeMap -> H.Html
embedProfiteurIframe node_map =
  H.iframe ! A.id "profiteur-iframe"
           ! A.srcdoc ( fromString $ renderHtml $ profiteurReportToHtml node_map)
           ! A.width "100%"
           ! A.height "100%"
           $ mempty

profiteurReportToHtml :: Profiteur.NodeMap -> Html
profiteurReportToHtml nodeMap =
  H.docTypeHtml $ do
    withReportHeader $ do
      encodedProfToHtml nodeMap
      mconcat profiteurCssAssets
      mconcat profiteurJsAssets
    reportBody

profiteurCssAssets :: [Html]
profiteurCssAssets = Prelude.map (H.style . H.preEscapedToHtml)
  [ decodeUtf8 $(embedFile "javascript/profiteur/main.css") ]

profiteurJsAssets :: [Html]
profiteurJsAssets = Prelude.map (H.script . H.preEscapedToHtml)
  [ jquery
  , decodeUtf8 $(embedFile "javascript/profiteur/unicode.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/model.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/resizing-canvas.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/node.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/selection.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/zoom.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/details.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/sorting.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/tree-map.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/tree-browser.js")
  , decodeUtf8 $(embedFile "javascript/profiteur/main.js")
  ]

withReportHeader :: Html -> Html
withReportHeader innerHtml =
    H.head $ do
      H.meta ! charset "UTF-8"
      innerHtml

encodedProfToHtml :: Profiteur.NodeMap -> Html
encodedProfToHtml nodeMap =
  let jsonProf = BC8.toStrict $ "var $prof = " <> Aeson.encode nodeMap <> ";"
  in H.script ! A.type_ "text/javascript" $ fromString (BC8.unpack jsonProf)

reportBody :: Html
reportBody =
  H.body $ do
    H.div ! A.id "details-tree" $ do
      H.div ! A.id "details" $ mempty
      H.div ! A.id "tree" $ mempty
    H.div ! A.id "map" $ mempty
