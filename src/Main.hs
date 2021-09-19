{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
module Main where
import           Pages
import           Types

import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.ByteString.Char8                as B (elem, pack, unpack)
import           Data.ByteString.Lazy.Char8           as BL (writeFile)
import           Data.List                            (isSuffixOf)
import           Data.Monoid                          ((<>))
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime)
import           Data.Time.Format                     (defaultTimeLocale,
                                                       formatTime)
import           Prelude                              as P
import           System.Directory                     (doesDirectoryExist,
                                                       doesFileExist,
                                                       getDirectoryContents,
                                                       removeFile)
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
import           Network.Wai.Handler.Warp             (defaultSettings, setPort)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import qualified Network.Wai.Middleware.HttpAuth      as Auth
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    as N (fileContent,
                                                            fileName)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Web.Scotty                           as S
import           Web.Scotty.Login.Session

import Data.String.Interpolate (i)
import System.Exit
import Control.Exception (catch)
import Data.List.Split (splitOn)
import Data.String
import Data.Maybe
import Data.Aeson

data SlackTestMessage = SlackTestMessage {
  token :: String, challenge :: String, _type :: String
  } deriving Show
instance FromJSON SlackTestMessage where
  parseJSON  = withObject "SlackTestMessage" $ \o -> SlackTestMessage
    <$> o .: "token" <*> o .: "challenge" <*> o .: "type"

-- run with environment var PORT set to whatever port
-- for auth, make a file "auth.txt"
-- containing two lines: username and then password

main = do compileScss >>= \res -> unless res $ die "scss compilation failure"
          envPort <- getEnv "PORT"
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
                hasPrefix "static/" <|> hasPrefix "static_src/" -- for sourcemaps in dev
              when (P.length l == 2 && authp) $ do
                let usn = head l
                    passwd = l !! 1
                middleware $ Auth.basicAuth
                  (\u p -> return $ u == B.pack usn && p == B.pack passwd) " Welcome "
              routes

----- scss build
compileScss :: IO Bool
compileScss = do
  cssFilePaths <- filter (isSuffixOf ".scss") <$> getDirectoryContents "static_src/css"
  -- remove files
  (sequence_ $ removeFile . ("static/css" ++ ) <$> cssFilePaths) `catch` (\(e :: IOError) -> return ())
  results <- sequence $ (\path -> system $ [i| scss static_src/css/#{path} static/css/#{minusExt path ++ ".css"} |]) <$> cssFilePaths
  return $ all (== ExitSuccess) results
    where
      minusExt :: String -> String
      minusExt path = concat $ init $ splitOn "." path
------
routes :: ScottyM ()
routes = do S.get "/" $ blaze $ template "HOME" homePage

            loginRoutes

            authedRoutes

            fileRoutes

            slackRoutes

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



slackRoutes :: ScottyM ()
slackRoutes = do
  post "/slackthing/events/" $ do
    poop <- jsonData
    text $ T.pack $ challenge $ fromJust poop


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

  getAuthed "/torrentstatus" $ blaze torrentStatusPage

  getAuthed "/torrentstatusraw" $ do
                 res <- liftIO $ shellWithOut "transmission-remote --debug -l 2>&1 | sed '1,/200 OK/d'| sed '1,/got response/d'| grep -A 1 -- '--------'| head -n 2| tail -n 1"
                 S.json res

  postAuthed "/removetorrent" $ do
    (id :: String) <- param "torrentid"
    _ <- liftIO $ shellWithOut $ "transmission-remote -t " ++ escape id ++ "  -r"
    return ()
  postAuthed "/torrentmove" $ do
    (num :: Int) <- param "num"
    (dir :: String) <- param "dir"
    unless (num >= 0 && not (null dir)) $ do
      liftIO $spawn $ "transmission-remote -t " ++ (escape $ show num) ++ " -- move " ++
        escape dir ++ " && transmission-remote -t " ++ (escape $ show num) ++ " -v"
      redirect "/torrentstatus"



loginRoutes :: ScottyM ()
loginRoutes = do
  S.get "/login" $ do
    params <- params
    let redirect = T.unpack <$> lookup "redirect" params
        redirect' = fromMaybe "/" redirect
    S.html $ T.pack $ unlines
      ["<div class=form-wrapper>"
      , "<form method=\"POST\" action=\"/login\">"
      , "<input type=\"text\" name=\"username\">"
      , "<input type=\"password\" name=\"password\">"
      , "<input type=\"text\" style=\"display: none;\" name=\"redirect\" value=\"" <> redirect' <> "\">"
      , "<input type=\"submit\" name=\"login\" value=\"login\">"
      , "</form>"
      , "</div>"
      , "<script> "
      , "(function() { document.querySelector('form input[type=\"text\"]').focus(); })()"
      , "</script>"
      ]
  S.post "/login" $ do
    (usn :: String) <- param "username"
    (pass :: String) <- param "password"
    ps <- params
    let redirectUrl = T.unpack <$> lookup "redirect" ps
        redirectUrl' = maybe "/" urlDecode redirectUrl
    if usn == "guest" && pass == "password"
      then do addSession'
              liftIO $ print "adding session"
              redirect $ T.pack redirectUrl'
      else redirect "/denied"

  S.get "/denied" $ blaze $ do
                p "ya gotta login man"
                a ! href "/login" $ "login"

  S.get "/logout" $ removeSession conf

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
                       let fname = subSemis $ B.unpack $ fileName f
                           path = case dir of
                                    "default"      -> prefix </> fname
                                    "movies"       -> prefix </> "movies" </> fname
                                    "tv"           -> prefix </> "tv" </> fname
                                    "music"        -> prefix </> "music" </> fname
                                    "torrentwatch" -> prefix </> "torrentwatch" </> fname
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
whenM p a = p >>= \b -> when b a

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p a = p >>= \b -> unless b a

orM :: Monad m => [m Bool] -> m Bool
orM = fmap or . sequence

subSemis :: String -> String
subSemis = foldr (\c a -> if c == ';' then ('\\' : ';' : a) else (c:a)) []


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
                            , debugMode          = True
                            }

{-
data SessionConfig = SessionConfig { dbName             :: String
                                   , syncInterval       :: Int --seconds
                                   , expirationInterval :: NominalDiffTime
                                   }
-}

authCheck' = authCheck (redirect "/denied")
addSession' = addSession conf
initializeCookieDb' = initializeCookieDb conf

-- getAuthed :: String -> ActionT T.Text IO () -> ScottyM ()
getAuthed  r a = S.get  (fromString r) $ authCheck (redirect $ T.pack $ "/login?redirect=" <> urlEncode r) a
postAuthed r a = S.post r $ authCheck' a

-- TODO
urlEncode s = s
urlDecode s = s
