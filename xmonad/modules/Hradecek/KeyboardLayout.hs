module Hradecek.KeyboardLayout
  (
    cycleLayout
  ) where

import Control.Monad.Extra    ( ifM )
import Control.Monad.IO.Class ( MonadIO )
import Data.List              ( findIndex )
import Data.Text              ( pack, strip, unpack )
import Data.Maybe             ( fromJust )
import Data.List.Split        ( splitOn )
import XMonad.Util.Run        ( runProcessWithInput )

import Hradecek.Utils

setxkbmap :: FilePath
setxkbmap = "/usr/bin/setxkbmap"

cycleLayout :: MonadIO m => [String] -> m (Maybe String)
cycleLayout layouts = ifM (isInstalled setxkbmap) (cycleLayout' layouts) $ return Nothing
  where cycleLayout' ls = do
          current <- getLayout
          let next = ls !! (fromJust $ ((`mod` (length ls)) . (+1)) <$> findIndex (\l -> l == current) ls)
          setLayout next
          return $ Just next

setLayout :: MonadIO m => String -> m ()
setLayout l = runProcessWithInput setxkbmap [l] "" >> return ()

getLayout :: MonadIO m => m String
getLayout =  parseLayout <$> runProcessWithInput setxkbmap ["-query"] []
  where parseLayout = unpack . strip . pack . last . splitOn ":" . last . lines

