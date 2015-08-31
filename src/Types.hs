module Types where



data FileEntry = FileEntry { feName         :: String
                           , feLastModified :: String
                           , feSize         :: String
                           }
               deriving (Eq, Ord, Show)
