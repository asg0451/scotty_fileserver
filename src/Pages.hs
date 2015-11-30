{-# LANGUAGE OverloadedStrings #-}
module Pages (template
             , renderText
             , renderDir
             , uploadPage
             , homePage
             , donnerPage
             , donnerAddPage
             , videojstest
             , addTorrentPage
             ) where

import           Types

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Data.List                   (isSuffixOf, sort)
import           Data.Monoid
import qualified Data.Text.Lazy              as T


addTorrentPage :: Html
addTorrentPage = template "add a torrent" $ do
                   H.form ! A.method "POST" ! action "/torrentadd" $ do
                                              input ! type_ "text" ! name "magnet" ! size "70"
                                              input ! type_ "submit" ! value "ADD"

renderText :: T.Text -> Html
renderText text = html $ body $ p $ toHtml text

homePage :: Html
homePage = renderText "home"

renderDir :: String -> [FileEntry] -> [FileEntry] -> Html
renderDir dir fs ds =
  let dir' = if null dir then dir else (reverse $ dropWhile (== '/') $ reverse dir) ++ "/"
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
    thead $ do
      tr $ do
        th "files"
        th "last modified"
        th "size"
    tbody ! class_ "fs-body" $ do
      ls


uploadPage :: Html
uploadPage = template "upload form" $ do
  link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/dropzone/4.0.1/dropzone.css"
  H.form mempty ! action "/uploaded" ! class_ "dropzone" ! A.id "customdropzone"

template :: String -> Html -> Html
template title body = do
  docType
  html ! lang "en" $ do
    H.head $ do meta ! charset "utf-8"
                meta ! name "viewport" ! content
                  "width=device-width, initial-scale=1, minimum-scale=1.0, maximum-scale=1.0"
                link ! rel "shortcut icon" ! type_ "image/x-icon" ! href "/favicon.ico"
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
                link ! rel "shortcut icon" ! type_ "image/x-icon" ! href "/favicon.ico"
                H.title $ toHtml title
                defaultIncludes
                h
    H.body $ do body
                theFooter


theFooter :: Html
theFooter =
  nav ! class_ "navbar navbar-default navbar-fixed-bottom" $ do
    H.div ! class_ "container" $ do
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

videojstest :: Html
videojstest = templateWithHead "video.js Test"
              (do H.link ! href "//vjs.zencdn.net/4.12/video-js.css" ! rel "stylesheet"
                  script mempty ! src "//vjs.zencdn.net/4.12/video.js")
              (do video ! A.id "example_video_1" ! class_ "video-js vjs-default-skin"
                    ! controls mempty ! preload "auto" ! width "640" ! height "264"
--                    ! poster "http://video-js.zencoder.com/oceans-clip.png"
                    $ do source ! src "/served_files/tv/Rick_and_Morty/Rick and Morty S01E06.mkv" ! type_ "video/webm"
                         p ! class_ "vjs-no-js" $ "To view this video please enable JavaScript...")
