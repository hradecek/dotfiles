module Hradecek.Utils
  (
    isInstalled,
  ) where

import Control.Monad.IO.Class ( MonadIO )
import XMonad.Util.Run        ( runProcessWithInput )

which :: String
which = "which"

isInstalled :: MonadIO m => String -> m Bool
isInstalled what = let which' = runProcessWithInput which [what] []
                   in  fromOutput <$> which'

fromOutput :: String -> Bool
fromOutput out = if null out then False else True
