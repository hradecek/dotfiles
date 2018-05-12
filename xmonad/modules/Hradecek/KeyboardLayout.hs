module Hradecek.KeyboardLayout
  (
    cycleLayout
  ) where

import Control.Monad.Extra  ( ifM )
import Data.List            ( findIndex )
import Data.Text            ( pack, strip, unpack )
import Data.Maybe           ( fromJust )
import Data.List.Split      ( splitOn )
import System.Process       ( readProcess )
import System.Posix.Process ( executeFile, forkProcess )
import XMonad.Core          ( spawn )

import Hradecek.Utils

setxkbmap :: FilePath
setxkbmap = "/usr/bin/setxkbmap"

cycleLayout :: [String] -> IO ()
cycleLayout layouts = ifM (isInstalled setxkbmap) (cycleLayout' layouts) $ return ()
  where cycleLayout' ls = do
          current <- getLayout
          let next = ((`mod` (length ls)) . (+1)) <$> findIndex (\l -> l == current) ls
          setLayout $ layouts !! (fromJust next)

setLayout :: String -> IO ()
setLayout l = spawn $ setxkbmap ++ " " ++ l

getLayout :: IO String
getLayout =  parseLayout <$> readProcess setxkbmap ["-query"] []
  where parseLayout = unpack . strip . pack . last . splitOn ":" . last . lines

