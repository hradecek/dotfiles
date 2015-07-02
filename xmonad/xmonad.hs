--------------------------------------------------------------------------------
--                               XMONAD CONFIG                                --
-- File        : ~/.xmonad/xmonad.hs                                          --
-- Author      : hradecek <ivohradek@gmail.com>                               --
-- Stability   : unstable                                                     --
-- Description : Xmonad + Default dzen2 configuration                         --
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
--                                  IMPORTS                                   --
--------------------------------------------------------------------------------
import XMonad

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.Run
import XMonad.Util.Font
import XMonad.Util.Timer
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad

import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns

import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Colour.SRGB
import Data.String.Utils

import Control.Concurrent
import Control.Applicative

import System.IO
import System.Dzen
import System.Exit
import System.Random
import System.Directory
import System.FilePath.Posix

import Graphics.X11.ExtraTypes.XF86

import qualified Control.Exception as E

import qualified Data.Map as M
import qualified Data.Colour.Names as C

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
--------------------------------------------------------------------------------
--                                 LOG HOOK                                   --
--------------------------------------------------------------------------------
logHook' :: [X ()] -> X ()
logHook' = sequence_

--------------------------------------------------------------------------------
--                               STARTUP HOOK                                 --
--------------------------------------------------------------------------------
startupHook' :: X ()
startupHook' =
      setWMName "LG3D"
  <+> setDefaultCursor xC_left_ptr
  <+> (liftIO $ threadDelay 1000000)
  <+> (startTimer 1 >>= XS.put . TID)

--------------------------------------------------------------------------------
--                                EVENT HOOK                                  --
--------------------------------------------------------------------------------
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
    initialValue = TID 0

handleEventHook' :: Event -> X All
handleEventHook' e =
      clockEventHook e
  <+> docksEventHook e
  <+> handleTimerEvent e
  <+> fullscreenEventHook e

clockEventHook :: Event -> X All
clockEventHook e = do
  (TID t) <- XS.get
  handleTimer t e $ do
    startTimer 1 >>= XS.put . TID
    ask >>= logHook . config
    return Nothing
  return $ All True
  
--------------------------------------------------------------------------------
--                                WINDOW HOOK                                 --
--------------------------------------------------------------------------------
manageHook' :: ManageHook
manageHook' =
      manageDocks
  <+> windowsHook
  <+> dynamicMasterHook

windowsHook :: ManageHook
windowsHook = composeAll . concat $
  [
    [ isDialog --> doFloat ]
  , [ isFullscreen --> doFullFloat ]
  ]

--------------------------------------------------------------------------------
--                                LAYOUT HOOK                                 --
--------------------------------------------------------------------------------
layoutHook' = avoidStruts
  $ onWorkspace (workspaces' !! 2) webLayouts
  $ onWorkspace (workspaces' !! 3) develLayouts
  $ onWorkspace (workspaces' !! 4) develLayouts
  $ allLayouts
    where
      webLayouts =
            tiled
        ||| tabbed shrinkText tabConfigTheme
      develLayouts =
            tiled
        ||| tabbed shrinkText tabConfigTheme
      allLayouts =
            tiled
        ||| threeCol
        ||| tabbed shrinkText tabConfigTheme
      delta = 2/100
      ratio = 1/2
      nmaster = 1
      tiled = Tall nmaster delta ratio
      threeCol = ThreeCol nmaster delta ratio

--------------------------------------------------------------------------------
--                               KEY BINDINGS                                 --
--------------------------------------------------------------------------------
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask  .|. shiftMask   , xK_q         ), io (exitWith ExitSuccess))
  , ((modMask                  , xK_q         ), spawn "xmonad --recompile && xmonad --restart")
  , ((modMask                  , xK_c         ), kill)
  , ((mod1Mask                 , xK_F4        ), kill)
  , ((modMask                  , xK_Tab       ), windows W.swapMaster)
  , ((modMask                  , xK_j         ), windows W.swapDown)
  , ((modMask                  , xK_k         ), windows W.swapUp)
  , ((mod1Mask                 , xK_j         ), windows W.focusDown)
  , ((mod1Mask                 , xK_k         ), windows W.focusUp)
  , ((modMask                  , xK_t         ), withFocused $ windows . W.sink)
  , ((modMask                  , xK_h         ), sendMessage Shrink)
  , ((modMask                  , xK_l         ), sendMessage Expand)
  , ((mod1Mask .|. controlMask , xK_Right     ), nextWS)
  , ((mod1Mask .|. controlMask , xK_l         ), nextWS)
  , ((mod1Mask .|. controlMask , xK_Left      ), prevWS)
  , ((mod1Mask .|. controlMask , xK_h         ), prevWS)
  , ((mod1Mask .|. shiftMask   , xK_Right     ), shiftToNext)
  , ((mod1Mask .|. shiftMask   , xK_l         ), shiftToNext)
  , ((mod1Mask .|. shiftMask   , xK_Left      ), shiftToPrev)
  , ((mod1Mask .|. shiftMask   , xK_h         ), shiftToPrev)
  , ((modMask                  , xK_b         ), sendMessage ToggleStruts)
  , ((modMask                  , xK_space     ), sendMessage NextLayout)
  , ((modMask  .|. shiftMask   , xK_space     ), setLayout $ XMonad.layoutHook conf)

  , ((modMask  .|. shiftMask   , xK_Return    ), spawn $ XMonad.terminal conf)
  , ((mod1Mask .|. shiftMask   , xK_f         ), spawn "/usr/bin/firefox" )
  , ((mod1Mask .|. shiftMask   , xK_v         ), spawn "/usr/bin/virtualbox")
  , ((mod1Mask .|. shiftMask   , xK_Return    ), shellPrompt shellConfig')
  , ((modMask                  , xK_x         ), spawn "/usr/bin/xcalib -invert -alter")
  , ((modMask                  , xK_s         ), spawn "sleep 1; xset dpms force off")
  , ((mod1Mask                 , xK_Escape    ), spawn "/home/ivo/.xmonad/bin/changeLayout.pl")

  , ((mod1Mask .|. controlMask , xK_Page_Up   ), io setPrevWallpaper >>= spawn)
  , ((mod1Mask .|. controlMask , xK_Page_Down ), io setNextWallpaper >>= spawn)
  , ((mod1Mask .|. controlMask , xK_End       ), io setRandomWallpaper >>= spawn)

  , ((mod1Mask                 , xK_Down      ), spawn "mpc toggle")
  , ((mod1Mask                 , xK_Left      ), spawn "mpc prev")
  , ((mod1Mask                 , xK_Right     ), spawn "mpc next")
  , ((mod1Mask                 , xK_Up        ), spawn "mpc stop")

  , ((0                    , xF86XK_AudioMute ), spawn "/usr/bin/amixer set Master toggle")
  , ((0             , xF86XK_AudioRaiseVolume ), spawn "/usr/bin/amixer set Master 5%+")
  , ((0             , xF86XK_AudioLowerVolume ), spawn "/usr/bin/amixer set Master 5%-")
  ]
  
--------------------------------------------------------------------------------
--                              MOUSE BINDINGS                                --
--------------------------------------------------------------------------------
mouseBindings' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
  , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
  , ((modMask .|. shiftMask, button1), \_ -> shiftToPrev)
  , ((modMask .|. shiftMask, button3), \_ -> shiftToNext)
  ]


--------------------------------------------------------------------------------
--                                   DZEN                                     --
--------------------------------------------------------------------------------
data DF = DF
  { xDF  :: Int
  , yDF  :: Int
  , wDF  :: Int
  , hDF  :: Int
  , taDF :: String
  , fgDF :: String
  , bgDF :: String
  , fnDF :: String
  } deriving (Data, Typeable)

instance Show DF where
  show df = concat $ zipWith (\a v -> a ++ " '" ++ v ++ "' ") (gs df) (fs <*> [df])
        where
          gs = map (replace "DF" "" . (++) "-") . constrFields. toConstr
          fs = [show . xDF, show . yDF, show . wDF, show . hDF, taDF, fgDF, bgDF, fnDF]

data DzenBox = DB
  { colDB  :: DColour
  , bgDB   :: DColour
  , fgDB   :: DColour
  , liDB   :: FilePath
  , riDB   :: FilePath
  , hDB    :: Int
  }

dzenBoxStyle :: DzenBox -> String -> String
dzenBoxStyle box text = toString $
      fg (colDB box) (icon (liDB box)
  +++ rect 1920 (hDB box)
  +++ pos (-1920)
  +++ fg (fgDB box) (str text)
  +++ fg (colDB box) (icon (riDB box)))
  +++ fg (bgDB box) (rect 1920 (hDB box))
  +++ pos (-1920)

dzenBoxStyleL :: DzenBox -> Logger -> Logger
dzenBoxStyleL = (fmap . fmap) . dzenBoxStyle

dzenSpawnPipe :: (MonadIO m, Show a) => a -> m Handle
dzenSpawnPipe df = spawnPipe $ "/usr/bin/dzen2 " ++ show df ++ " -p -e onstart=lower"

--------------------------------------------------------------------------------
--                                SETTINGS                                    --
--------------------------------------------------------------------------------
yRes                  = 768
xRes                  = 1366
dzenFont              = "xft:monofur:size=8:antialias=true:hinting=true"
dzenHeight            = 14
dzenBoxFullIcon       = "/home/ivo/.xmonad/icons/xbm/boxFull.xbm"
dzenBoxLeftIcon       = "/home/ivo/.xmonad/icons/xbm/boxLeft.xbm"
dzenBoxRightIcon      = "/home/ivo/.xmonad/icons/xbm/boxRight.xbm"
dzenBoxSmallRightIcon = "/home/ivo/.xmonad/icons/xbm/boxSmallRight.xbm"
dzenBoxSmallLeftIcon  = "/home/ivo/.xmonad/icons/xbm/boxSmallLeft.xbm"
dzenBoxSmallFullIcon  = "/home/ivo/.xmonad/icons/xbm/boxSmallFull.xbm"

workspaces' :: [WorkspaceId]
workspaces' =  map show [1..9]

workspacesNames :: [WorkspaceId]
workspacesNames =
  [ "Main"
  , "Web"
  , "Web2"
  , "Development1"
  , "Development2"
  , "Alternative1"
  , "Alternative2"
  , "Alternative3"
  , "Alternative4"
  ]

shellConfig' :: XPConfig
shellConfig' = defaultXPConfig
  { font              = "xft:monofur:size=9:antialias=true:hinting=true"
  , height            = fromIntegral dzenHeight
  , bgColor           = sRGB24show C.black
  , fgColor           = sRGB24show C.lightgray
  , bgHLight          = sRGB24show C.dodgerblue
  , fgHLight          = sRGB24show C.lightgray
  , position          = Top
  , borderColor       = sRGB24show C.black
  , historySize       = 100
  , autoComplete      = Nothing
  , historyFilter     = deleteConsecutive
  , completionKey     = xK_Tab
  , promptBorderWidth = 1
  }

tabConfigTheme :: Theme
tabConfigTheme = defaultTheme
  { activeColor         = sRGB24show C.dodgerblue
  , inactiveColor       = sRGB24show C.darkgray
  , urgentColor         = sRGB24show C.darkorange
  , activeBorderColor   = sRGB24show C.darkgray
  , inactiveBorderColor = sRGB24show C.darkgray
  , urgentBorderColor   = sRGB24show C.darkgray
  , fontName            = "xft:monofur:size=9:antialias=true:hinting=true"
  , decoHeight          = 14
  }

dzenBottomFlags :: DF
dzenBottomFlags = DF
  { xDF  = 0
  , yDF  = yRes - dzenHeight
  , wDF  = xRes
  , hDF  = dzenHeight
  , taDF = "l"
  , bgDF = sRGB24show C.black
  , fgDF = sRGB24show C.lightgray
  , fnDF = dzenFont
  }

dzenTopFlags :: DF
dzenTopFlags = DF
  { xDF  = 0
  , yDF  = 0
  , wDF  = xRes
  , hDF  = dzenHeight
  , taDF = "l"
  , bgDF = sRGB24show C.black
  , fgDF = sRGB24show C.lightgray
  , fnDF = dzenFont
  }

darkorangeDzenBoxLR :: DzenBox
darkorangeDzenBoxLR = DB
  { colDB = C.darkorange
  , bgDB  = C.black
  , fgDB  = C.black
  , liDB  = dzenBoxLeftIcon
  , riDB  = dzenBoxRightIcon
  , hDB   = dzenHeight
  }

dodgerblueDzenBoxL :: DzenBox
dodgerblueDzenBoxL = DB
  { colDB = C.dodgerblue
  , bgDB  = C.black
  , fgDB  = C.black
  , liDB  = dzenBoxSmallLeftIcon
  , riDB  = dzenBoxSmallFullIcon
  , hDB   = dzenHeight
  }

dodgerblueDzenBoxR :: DzenBox
dodgerblueDzenBoxR = DB
  { colDB = C.dodgerblue
  , bgDB  = C.black
  , fgDB  = C.black
  , liDB  = dzenBoxSmallFullIcon
  , riDB  = dzenBoxSmallRightIcon
  , hDB   = dzenHeight
  }

dimgrayDzenBoxLR :: DzenBox
dimgrayDzenBoxLR = DB
  { colDB = C.dimgray
  , bgDB  = C.black
  , fgDB  = C.silver
  , liDB  = dzenBoxLeftIcon
  , riDB  = dzenBoxRightIcon
  , hDB   = dzenHeight
  }

dodgerblueDzenBoxLR :: DzenBox
dodgerblueDzenBoxLR = DB
  { colDB = C.dodgerblue
  , bgDB  = C.black
  , fgDB  = C.black
  , liDB  = dzenBoxLeftIcon
  , riDB  = dzenBoxRightIcon
  , hDB   = dzenHeight
  }

topBarLogHook :: Handle -> X ()
topBarLogHook h = dynamicLogWithPP defaultPP
    { ppOutput = hPutStrLn h
    , ppOrder  = \(_:a:b:x) -> [a,b]
    , ppSep    = " "
    , ppExtras = [ ]
    }

bottomBarLogHook :: Handle -> X ()
bottomBarLogHook h = dynamicLogWithPP defaultPP
  { ppSep             = " "
  , ppOrder           = \(ws:_:_:x) -> [ws] ++ x
  , ppWsSep           = "-"
  , ppOutput          = hPutStrLn h
  , ppExtras          = [batteryL, dateL, uptimeL]
  , ppHidden          = dzenBoxStyle dodgerblueDzenBoxLR
  , ppUrgent          = dzenBoxStyle dodgerblueDzenBoxLR
  , ppCurrent         = dzenBoxStyle dodgerblueDzenBoxLR
  , ppVisible         = dzenBoxStyle dodgerblueDzenBoxLR
  , ppHiddenNoWindows = dzenBoxStyle dimgrayDzenBoxLR
  }

dzenClickWorkspace ws =
     "^ca(1,"
  ++ xdo "w;"
  ++ xdo id
  ++ ")"
  ++ "^ca(3,"
  ++ xdo "w;"
  ++ xdo id
  ++ ")"
  ++ ws
  ++ "^ca()^ca()"
  where
    wsIdxToString Nothing  = "1"
    wsIdxToString (Just n) = show $ mod (n+1) $ length workspaces'

    id                     = wsIdxToString (elemIndex ws workspaces')
    xdo key                = "/usr/bin/xdotool key super+" ++ key

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

nextElem, prevElem :: Eq a => a -> [a] -> Maybe a
nextElem e xs = listToMaybe . drop 1 . dropWhile (/= e) $ xs ++ (take 1 xs)
prevElem e xs = nextElem e $ reverse xs
randElem xs = pick xs

setRandomWallpaper = do
    allWall <- getAllWallpapers
    newWall <- randElem allWall
    return $ "/usr/bin/feh --bg-scale \"/data/Pictures/Wallpapers/Selected/" ++ newWall ++ "\""

setNextWallpaper = do
    allWall <- getAllWallpapers
    curWall <- getCurrentWallpaper
    let newWall = fromJust $ nextElem curWall allWall
    return $ "/usr/bin/feh --bg-scale \"/data/Pictures/Wallpapers/Selected/" ++ newWall ++ "\""

setPrevWallpaper = do
    allWall <- getAllWallpapers
    curWall <- getCurrentWallpaper
    let newWall = fromJust $ prevElem curWall allWall
    return $ "/usr/bin/feh --bg-scale \"/data/Pictures/Wallpapers/Selected/" ++ newWall ++ "\""

getCurrentWallpaper :: IO [Char]
getCurrentWallpaper =
        init
    <$> rstrip
    <$> takeFileName
    <$> readFile "/home/ivo/.fehbg"

getAllWallpapers :: IO [FilePath]
getAllWallpapers =
        drop 2
    <$> getDirectoryContents "/data/Pictures/Wallpapers/Selected"


(|++|) :: Logger -> Logger -> Logger
l1 |++| l2 = (liftA2 . liftA2) (++) l1 l2

labelL :: String -> Logger
labelL = return . return

batteryL =
       (dzenBoxStyleL dodgerblueDzenBoxL $ labelL "BATTERY")
  |++| (labelL " ")
  |++| (dzenBoxStyleL dodgerblueDzenBoxR battery)

dateL =
       (dzenBoxStyleL dodgerblueDzenBoxL $ labelL "TIME")
  |++| (labelL " ")
  |++| (dzenBoxStyleL dodgerblueDzenBoxR $ date "%T")

uptimeL =
       (dzenBoxStyleL dodgerblueDzenBoxL $ labelL "UPTIME")
  |++| (labelL " ")
  |++| (dzenBoxStyleL dodgerblueDzenBoxR uptime)

uptime :: Logger
uptime = fileToLogger format "0" "/proc/uptime"
  where
    u x = read (takeWhile (/= '.') x) :: Integer
    h x = div (u x) 3600
    hr x = mod (u x) 3600
    m x = div (hr x) 60
    s x = mod (hr x) 60
    format x = (show $ h x) ++ "h " ++ (show $ m x) ++ "m " ++ (show $ s x) ++ "s"
initNotNull :: String -> String
initNotNull [] = "0\n"
initNotNull xs = init xs

tailNotNull :: [String] -> [String]
tailNotNull [] = ["0\n"]
tailNotNull xs = tail xs

fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
  let readWithE f1 e1 p1 = E.catch (do
    contents <- readFile p1
    return $ f1 (initNotNull contents)) ((\_ -> return e1) :: E.SomeException -> IO String)
  str <- liftIO $ readWithE f e p
  return $ return str

--------------------------------------------------------------------------------
--                                   MAIN                                     --
--------------------------------------------------------------------------------
main :: IO ()
main = do
  topBar    <- dzenSpawnPipe dzenTopFlags
  bottomBar <- dzenSpawnPipe dzenBottomFlags
  xmonad XConfig {
      keys               = keys'
    , logHook            = logHook' [topBarLogHook topBar, bottomBarLogHook bottomBar]
    , modMask            = mod4Mask
    , terminal           = "/usr/bin/urxvtc"
    , layoutHook         = layoutHook'
    , manageHook         = manageHook'
    , workspaces         = workspaces'
    , borderWidth        = 1
    , startupHook        = startupHook'
    , mouseBindings      = mouseBindings'
    , handleEventHook    = handleEventHook'
    , clickJustFocuses   = True
    , focusFollowsMouse  = True
    , normalBorderColor  = sRGB24show C.black
    , focusedBorderColor = sRGB24show C.lightgray
  }
