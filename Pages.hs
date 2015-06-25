{-# LANGUAGE OverloadedStrings #-}

module Pages (template, renderText, renderDir) where


import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Web.Scotty                    as S

import qualified Data.Text.Lazy                as T

import           Data.Monoid

renderText :: T.Text -> Html
renderText text = html $ body $ p $ toHtml text


renderDir :: String -> [String] -> [String] -> Html
renderDir dir fs ds =
  let frows = foldl1 (>>) $ fmap
              (\f -> tr . td .
                     (a ! href (toValue $ mconcat ["/files/", dir, f])) $ toHtml f) fs
      drows = foldl1 (>>) $ fmap
              (\d -> tr. td .
                     (a ! href (toValue $ mconcat ["/files/", dir, d, "/"])) $ toHtml $ d) ds
  in html $ body $ table $ do
    tr (td ! A.style "border-bottom:1px solid black" $ "files")
    frows
    tr (td ! A.style "border-bottom:1px solid black" $ "directories")
    drows


template :: String -> Html -> Html
template title body = html $ do H.head $ do H.title (toHtml title)
                                            defaultcss
                                H.body $ do body
                                            theFooter
theFooter :: Html
theFooter = H.footer $ do
            a ! href "/files/"  $ "file serving"
            toHtml ("      " :: String)
            a ! href "#" $ "file uploading"
            toHtml ("      " :: String)
            a ! href "#" $ "Donnerisms"
            toHtml ("      " :: String)
            a ! href "#" $ "Donnerfile"



defaultcss :: Html
defaultcss = H.link ! A.href "/static/css/default.css" ! A.title "compact" ! A.rel "stylesheet" ! A.type_ "text/css"
