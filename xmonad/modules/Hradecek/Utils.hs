module Hradecek.Utils
  (
    isInstalled,
  ) where

import System.Exit    ( ExitCode (..) )
import System.Process ( readProcessWithExitCode )

isInstalled :: String -> IO Bool
isInstalled what = let which = readProcessWithExitCode "which" [what] []
                   in  fromExitCode . exitCode <$> which

exitCode :: (ExitCode, String, String) -> ExitCode
exitCode (e, _, _) = e

fromExitCode :: ExitCode -> Bool
fromExitCode ExitSuccess = True
fromExitCode _           = False
