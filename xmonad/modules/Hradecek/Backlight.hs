module Hradecek.Backlight
  (
    getBrightness
  , setBrightness
  ) where

import Control.Monad.Extra ( (&&^), ifM )
import Data.Functor        ( (<$>) )
import Data.Text           ( Text (..), pack, strip )
import Data.Text.Read      ( decimal )
import System.Process      ( readProcess )
import Hradecek.Utils      ( isInstalled )
import XMonad.Core         ( spawn )

xbacklight :: FilePath
xbacklight = "/usr/bin/xbacklight"

getBrightness :: IO (Maybe Int)
getBrightness = ifM (isInstalled xbacklight) brightness' $ return Nothing
  where brightness' = fromString . pack <$> readProcess "xbacklight" ["-get"] []

setBrightness :: Integer -> IO ()
setBrightness value = ifM ((isInstalled xbacklight) &&^ (isPositive value)) (set' value) (return ())
  where
    isPositive = return . (>= 1)
    set' v = spawn $ xbacklight ++ " -set " ++ show v

fromString :: Text -> Maybe Int
fromString str = let result = decimal $ strip str
                 in either (\_ -> Nothing) (\(i, t) -> Just i) result

