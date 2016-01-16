{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Pages (template
             , renderText
             , renderDir
             , uploadPage
             , homePage
             , donnerPage
             , donnerAddPage
             , addTorrentPage
             , torrentStatusPage
             ) where

import           Types

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Data.List                   (isSuffixOf, sort)
import           Data.Monoid
import qualified Data.Text.Lazy              as T

-- not used
torrentStatusPage :: Html
torrentStatusPage = do
  H.span ! A.id "res" $ mempty
  script ! type_ "text/javascript" $ toHtml $ unlines
             [ ""
             , "var getTorrentData = function () {"
             , "  var x = new XMLHttpRequest();"
             , "  x.addEventListener(\"load\", listener)"
             , "  x.open(\"GET\", \"/torrentstatusraw\", true);"
             , "  x.send();"
             , "}"
             , "var listener = function () {"
             , "  document.getElementById(\"res\").innerHTML = this.response;"
             , "}"
             , "var t = setInterval(getTorrentData, 1000);"
             , "" ]


addTorrentPage :: Html
addTorrentPage = template "add a torrent" $
                   H.form ! A.method "POST" ! action "/torrentadd" $ do
                                              input ! type_ "text" ! name "magnet" ! size "70"
                                              input ! type_ "submit" ! value "ADD"

renderText :: T.Text -> Html
renderText text = html $ body $ p $ toHtml text

homePage :: Html
homePage = renderText "home"

renderDir :: String -> [FileEntry] -> [FileEntry] -> Html
renderDir dir fs ds =
  let dir' = if null dir then dir else reverse (dropWhile (== '/') $ reverse dir) ++ "/"
      fs' = sort fs
      ds' = sort ds
      frows = foldl (>>) mempty $ fmap
              (\f -> tr $ do
                  td (a ! href (toValue $ mconcat ["/files/", dir', feName f]) $
                      toHtml $ feName f)
                  td $ toHtml $ feLastModified f
                  td $ toHtml $ feSize f) fs'

      drows = foldl (>>) mempty $ fmap
              (\d -> tr $ do
                  td (a ! href (toValue $ mconcat ["/files/", dir', feName d, "/"]) $
                      toHtml $ feName d ++ "/")
                  td $ toHtml $ feLastModified d
                  td $ toHtml $ feSize d) ds'
      ls = frows >> drows
  in html $ body $ H.div ! class_ "container-fluid" $ table ! class_ "table" $ do
    thead $
      tr $ do
        th "files"
        th "last modified"
        th "size"
    tbody ! class_ "fs-body" $ ls


uploadPage :: Html
uploadPage = template "upload form" $ do
  link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.0.1/dropzone.css"
  H.form ! action "/uploaded" ! class_ "dropzone" ! A.id "customdropzone" $ do
    H.select ! A.id "folder" ! A.name "folder" $ do
      H.option ! A.selected "" ! A.value "default" $ "default"
      H.option ! A.value "movies" $ "movies"
      H.option ! A.value "tv" $ "tv"
      H.option ! A.value "music" $ "music"

template :: String -> Html -> Html
template title body = do
  docType
  html ! lang "en" $ do
    H.head $ do meta ! charset "utf-8"
                meta ! name "viewport" ! content
                  "width=device-width, initial-scale=1, minimum-scale=1.0, maximum-scale=1.0"
                link ! rel "shortcut icon" ! type_ "image/x-icon" ! href "/static/favicon.ico"
                H.title $ toHtml title
                defaultIncludes
    H.body $ do body
                theFooter

templateWithHead :: String -> Html -> Html -> Html
templateWithHead title h body = do
  docType
  html ! lang "en" $ do
    H.head $ do meta ! charset "utf-8"
                meta ! name "viewport" ! content
                  "width=device-width, initial-scale=1, minimum-scale=1.0, maximum-scale=1.0"
                link ! rel "shortcut icon" ! type_ "image/x-icon" ! href "/static/favicon.ico"
                H.title $ toHtml title
                defaultIncludes
                h
    H.body $ do body
                theFooter


theFooter :: Html
theFooter =
  nav ! class_ "navbar navbar-default navbar-fixed-bottom" $
    H.div ! class_ "container" $
      H.div ! class_ "footer" $ do
        a ! class_ "navbar-link" ! href "/login"      $ "login"
        a ! class_ "navbar-link" ! href "/files"      $ "file serving"
        a ! class_ "navbar-link" ! href "/upload"     $ "file uploading"
        a ! class_ "navbar-link" ! href "/donnerator" $ "donnerisms"
        a ! class_ "navbar-link" ! href "/donnerfile" $ "donnerfile"
        a ! class_ "navbar-link" ! href "/donneradd"  $ "add to donnerFile"

defaultIncludes :: Html
defaultIncludes = do script mempty !
                       src "https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.0.1/dropzone.js"
                     script mempty !
                       src "https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.0.1/dropzone.css"
                     link ! rel "stylesheet" ! -- bootstrap
                       href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                     script mempty !
                       src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                     script mempty !
                       src "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                     script mempty ! src "/static/js/custom-drop-form.js"
                     H.link ! A.href "/static/css/default.css" !
                       A.title "compact" ! A.rel "stylesheet" ! A.type_ "text/css"


donnerPage :: String -> Html
donnerPage ism = template "Donnerator Output" $ p $ toHtml ism

donnerAddPage :: Html
donnerAddPage = template "Add to Donnerfile" $ do
  p "add line to the donnerfile"
  p "no punctuation EXCEPT A PERIOD AT THE END OF EVERY DONNERISM"
  p "capitalization is irrelevant"
  p "seriously, that period is important"
  H.form ! A.method "POST" ! action "/donneradd" $ do
    input ! type_ "text" ! name "donner_line" ! size "70"
    input ! type_ "submit" ! value "SUBMIT"
