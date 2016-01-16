{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Pages
import           Types

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.ByteString.Char8                as B (elem, pack, unpack)
import           Data.ByteString.Lazy.Char8           as BL (writeFile)
import           Data.Functor                         ((<$>))
import           Data.List                            (isPrefixOf, lookup)
import           Data.Monoid                          ((<>))
import qualified Data.Text.Lazy                       as T
import qualified Data.Text.Lazy.IO                    as TIO
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime)
import           Data.Time.Format                     (defaultTimeLocale,
                                                       formatTime)
import           Prelude                              as P
import           System.Directory                     (doesDirectoryExist,
                                                       doesFileExist,
                                                       getDirectoryContents)
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Posix.Escape                  (escape)
import           Text.Blaze                           (preEscapedString)
import           Text.Blaze.Html5                     as H (a, div, p, script,
                                                            table, tbody,
                                                            toHtml, (!))
import           Text.Blaze.Html5.Attributes          as A (class_, href, id,
                                                            type_)
-- import           System.Locale                        (defaultTimeLocale)
import qualified System.Posix.Files                   as F
import           System.Process

import           Data.Time.Clock                      (NominalDiffTime)
import           Network.Wai.Handler.Warp             (Settings (..),
                                                       defaultSettings, setPort)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import qualified Network.Wai.Middleware.HttpAuth      as Auth
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    as N (fileContent,
                                                            fileName)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Web.Scotty                           as S
import           Web.Scotty.Login.Session

-- run with environment var PORT set to whatever port
-- for auth, make a file "auth.txt"
-- containing two lines: username and then password

main = do envPort <- getEnv "PORT"
          !envHttpAuth <- getEnv "AUTH" -- True or False
          h <- openFile "auth.txt" ReadMode
          c <- hGetContents h
          let l = P.lines c
              !authp = read envHttpAuth :: Bool
          initializeCookieDb'
          scottyOpts
            (Options 1 (setPort (read envPort) defaultSettings)) $ do
              middleware $
                addHeaders [(B.pack "X-Clacks-Overhead", B.pack "GNU Terry Pratchett")]
              middleware logStdoutDev
              middleware $ staticPolicy $
                hasPrefix "static/" -- <|> hasPrefix "served_files/"
              when (P.length l == 2 && authp) $ do
                let usn = head l
                    passwd = l !! 1
                middleware $ Auth.basicAuth
                  (\u p -> return $ u == B.pack usn && p == B.pack passwd) " Welcome "
              routes

routes :: ScottyM ()
routes = do S.get "/" $ blaze $ template "HOME" homePage

            loginRoutes

            authedRoutes

            fileRoutes

            S.notFound $ html "not here"


fileRoutes :: ScottyM ()
fileRoutes = do
  get "/files/" $ serveDir ""

  get (regex "^/files/(.+)$") $ do
               (f :: String) <- param "1"
               b <- liftIO $ doesFileExist (prefix <> f)
               unless b next
               liftIO $ print $ "opening file: " ++ f
               file' f

  get (regex "^/files/(.+)$") $ do
               dir <- param "1"
               liftIO $ print $ "opening dir: " ++ dir
               serveDir dir


authedRoutes :: ScottyM ()
authedRoutes = do
  getAuthed "/upload" $ blaze uploadPage

  postAuthed "/uploaded" $ do
    fs <- files
    dir <- param "folder"
--    liftIO $ print ps
    liftIO $ handleFiles dir fs
    html $ T.pack $ show fs

  get "/donnerator" $ do
    dism <- liftIO getDonnered
    blaze $ donnerPage dism

  getAuthed "/donneradd" $ blaze donnerAddPage

  postAuthed "/donneradd" $ do
    (line :: String) <- param "donner_line"
    liftIO $
      appendFile "/home/miles/ruby/donnerator/donnerisms.txt" $ line ++ "\n"
    redirect "/donnerfile"

  getAuthed "/donnerfile" $
    file "/home/miles/ruby/donnerator/donnerisms.txt"

  getAuthed "/torrentadd" $ blaze addTorrentPage

  post "/torrentadd" $ do -- de-authed
                 (magnet :: String) <- param "magnet"
                 unless (null magnet) $ void $ liftIO $
                        spawn $ "transmission-remote -a '" ++ escape magnet ++ "'"
                 redirect "/torrentstatus"

  getAuthed "/torrentstatus" $ do
                 js <- liftIO $ readFile "static/js/torrentstatus.js"
                 blaze $ template "torrents" $ do
                                 script ! type_ "text/javascript" $ preEscapedString $ js
                                 H.div ! class_ "container-fluid" $
                                  table ! A.id "res" ! class_ "table" $ tbody ! class_ "fs-body" $ mempty

  getAuthed "/torrentstatusraw" $ do
                 res <- liftIO $ shellWithOut "transmission-remote --debug -l 2>&1 | sed '1,/200 OK/d'| sed '1,/got response/d'| grep -A 1 -- '--------'| head -n 2| tail -n 1"
                 S.json res

  postAuthed "/removetorrent" $ do
                 (id :: String) <- param "torrentid"
                 res <- liftIO $ shellWithOut $ "transmission-remote -t " ++ escape id ++ "  -r"
                 return ()

loginRoutes :: ScottyM ()
loginRoutes = do
  S.get "/login" $ S.html $ T.pack $ unlines
    ["<form method=\"POST\" action=\"/login\">"
    , "<input type=\"text\" name=\"username\">"
    , "<input type=\"password\" name=\"password\">"
    , "<input type=\"submit\" name=\"login\" value=\"login\">"
    , "</form>"]
  S.post "/login" $ do
    (usn :: String) <- param "username"
    (pass :: String) <- param "password"
    if usn == "guest" && pass == "password"
      then do addSession'
              liftIO $ print "adding session"
              redirect "/"
      else redirect "/denied"

  S.get "/denied" $ blaze $ do
                p "ya gotta login man"
                a ! href "/login" $ "login"

blaze = S.html . renderHtml

file' :: String -> ActionM ()
file' f = file $ prefix <> f

dirInfo :: String -> IO ([FileEntry], [FileEntry])
dirInfo p = do let path = if p /= "" then prefix ++ p ++ "/" else prefix
               entries <- getDirectoryContents path
               fs <- filterM (doesFileExist . (path ++ )) entries
               ds <- filterM (doesDirectoryExist . ( path ++)) entries

               fattrs <- mapM (F.getFileStatus . (path ++)) fs
               dattrs <- mapM (F.getFileStatus . (path ++)) ds

               -- will have to write zipWith4 if we add more properties to FileEntry
               let fs'  = zipWith3 FileEntry fs (map (strTime . F.modificationTimeHiRes) fattrs) (map (showSize . fromIntegral . F.fileSize) fattrs)
                   ds'  = zipWith3 FileEntry ds (map (strTime . F.modificationTimeHiRes) dattrs) (map (showSize . fromIntegral . F.fileSize) dattrs)
                   ds'' = filter (\f -> feName f `notElem` [".",".."]) ds'

               print path >> print entries >> print fs' >> print ds'' -- debugging

               return (fs', ds'')

  where strTime = formatTime defaultTimeLocale "%F" . posixSecondsToUTCTime
        showSize :: Int -> String
        showSize n
          | n < 1000    = show n                   ++ " b"
          | n < 1000000 = show (n `P.div` 1000)    ++ " k"
          | otherwise   = show (n `P.div` 1000000) ++ " m"

serveDir p = do (fs, ds) <- liftIO $ dirInfo p
                blaze $ template p $ renderDir p fs ds

prefix :: String
prefix = "served_files/"

-- type File = (Text, FileInfo ByteString) -- (field in form where came from, info)
-- FileInfo { fileName :: ByteString, fileContentType :: ByteString, fileContent :: c }
handleFiles :: FilePath -> [S.File] -> IO ()
handleFiles dir fs = let fis = map snd fs
                         fis' = filter (\f -> not ('/' `B.elem` fileName f))  fis
                     in void $ forM fis' $ \f -> do
                       let fname = B.unpack $ fileName f
                           path = case dir of
                                    "default" -> prefix </> fname
                                    "movies"  -> prefix </> "movies" </> fname
                                    "tv"      -> prefix </> "tv" </> fname
                                    "music"   -> prefix </> "music" </> fname
                                    otherwise -> prefix </> fname
                       unlessM (orM [doesFileExist path, doesDirectoryExist path]) $
                               BL.writeFile path $ fileContent f

getDonnered :: IO String
getDonnered =  do (_, Just hout, _, pHandle) <- createProcess (proc "./donnerate.sh" []) { std_out = CreatePipe, cwd = Just "/home/miles/ruby/donnerator" }
                  waitForProcess pHandle
                  c <- hGetContents' hout
                  hClose hout
                  return c


--------------------------------------------------
whenM :: Monad m => m Bool -> m () -> m ()
whenM p a = p >>= \b -> if b then a else return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p a = p >>= \b -> if (not b) then a else return ()

orM :: Monad m => [m Bool] -> m Bool
orM = (fmap or) . sequence

spawn = createProcess . shell

shellWithOut :: String -> IO String
shellWithOut s = do
  (_, Just hout, __, pHandle) <- createProcess (shell s) { std_out = CreatePipe }
  waitForProcess pHandle
  c <- hGetContents' hout
  hClose hout
  return c

hGetContents' h = hGetContents h >>= \s -> length s `seq` return s

conf :: SessionConfig
conf = defaultSessionConfig { syncInterval       = 30 -- seconds
                            , expirationInterval = 1800 -- seconds
                            }

{-
data SessionConfig = SessionConfig { dbName             :: String
                                   , syncInterval       :: Int --seconds
                                   , expirationInterval :: NominalDiffTime
                                   }
-}

authCheck' = authCheck  (redirect "/denied")
addSession' = addSession conf
initializeCookieDb' = initializeCookieDb conf

getAuthed  r a = S.get  r $ authCheck' a
postAuthed r a = S.post r $ authCheck' a
