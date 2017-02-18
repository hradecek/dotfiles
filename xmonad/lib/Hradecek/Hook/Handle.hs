{-# LANGUAGE DeriveDataTypeable #-}
module Hradecek.Hook.Handle (
    handleEventHook
  ) where

import XMonad hiding (handleEventHook)

import XMonad.Util.Timer

import XMonad.Actions.ShowText

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ScreenCorners

import Data.Monoid

import Control.Applicative

import qualified XMonad.Util.ExtensibleState as XS

data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
    initialValue = TID 0

clockEventHook :: Event -> X All
clockEventHook e = do
  (TID t) <- XS.get
  handleTimer t e $ do
    startTimer 1 >>= XS.put . TID
    ask >>= logHook . config
    return Nothing
  return $ All True

handleEventHook :: Event -> X All
handleEventHook e = foldl1 (<+>) $
  [ clockEventHook
  , docksEventHook
  , handleTimerEvent
  , fullscreenEventHook
  , screenCornerEventHook
  ] <*> [e]
