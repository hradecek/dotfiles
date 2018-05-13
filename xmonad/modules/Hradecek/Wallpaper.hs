module Hradecek.Wallpaper
  (
    randomWallpaper
  , nextWallpaper
  , prevWallpaper
  ) where

import Control.Monad.Extra    ( ifM )
import Control.Monad.IO.Class ( MonadIO )
import Data.Text              ( pack, unpack, strip, splitOn )
import Data.Maybe             ( fromJust, listToMaybe )
import System.Directory       ( getHomeDirectory, listDirectory )
import System.FilePath.Posix  ( takeFileName, (</>) )
import System.Random          ( randomRIO )
import XMonad.Util.Run        ( runProcessWithInput )

feh :: String
feh = "/usr/bin/feh"

randomWallpaper :: FilePath -> IO ()
randomWallpaper dir = do
  all <- getAllWallpapers dir
  new <- randElem all
  setWallpaper dir new

nextWallpaper, prevWallpaper :: FilePath -> IO ()
nextWallpaper = setNextPrev nextElem
prevWallpaper = setNextPrev prevElem

setNextPrev fun dir = do
  all <- getAllWallpapers dir
  curr <- getCurrentWallpaper
  let new = fromJust $ fun curr all
  setWallpaper dir new

setWallpaper dir new = runProcessWithInput feh ["--bg-scale", dir </> new] [] >> return ()

getAllWallpapers :: FilePath -> IO [FilePath]
getAllWallpapers = listDirectory

getCurrentWallpaper :: IO FilePath
getCurrentWallpaper = takeFileName . unpack . last . init . splitOn (pack "'") . strip . pack <$> (fehbg >>= readFile)
  where fehbg = (</> ".fehbg") <$> getHomeDirectory


nextElem, prevElem :: Eq a => a -> [a] -> Maybe a
nextElem e xs = listToMaybe . drop 1 . dropWhile (/= e) $ xs ++ (take 1 xs)
prevElem e xs = nextElem e $ reverse xs

randElem :: [a] -> IO a
randElem xs = pick xs
  where pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
