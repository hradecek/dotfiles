--------------------------------------------------------------------------------
--                               XMONAD CONFIG                                --
-- File        : ~/.xmonad/xmonad.hs                                          --
-- Author      : hradecek <ivohradek@gmail.com>                               --
-- Stability   : unstable                                                     --
-- Description : Xmonad + dzen2                                               --
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
import XMonad.Hooks.ScreenCorners

import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Magnifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators hiding ((|||))

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
startupHook' = foldl1 (<+>) $
  [ setWMName "LG3D"
  , setDefaultCursor xC_left_ptr
  , (liftIO $ threadDelay 1000000)
  , (startTimer 1 >>= XS.put . TID)
  , corn
  ] where
      corn = addScreenCorners
               [ (SCLowerRight, nextWS)
               , (SCUpperRight, prevWS)
               , (SCLowerLeft , prevWS)
               , (SCUpperLeft , shellPrompt shellConfig')
               ]


--------------------------------------------------------------------------------
--                                EVENT HOOK                                  --
--------------------------------------------------------------------------------
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
    initialValue = TID 0

handleEventHook' :: Event -> X All
handleEventHook' e = foldl1 (<+>) $
  [ clockEventHook
  , docksEventHook
  , handleTimerEvent
  , fullscreenEventHook
  , screenCornerEventHook
  ] <*> [e]


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
manageHook' = foldl1 (<+>) $
   [ manageDocks
   , windowsHook
   , dynamicMasterHook
   ]

windowsHook :: ManageHook
windowsHook = composeAll . concat $
  [ [ className =? c --> doShift (workspaces' !! 1) | c <- webs     ]
  , [ className =? c --> doShift (workspaces' !! 2) | c <- graphics ]
  , [ className =? c --> doCenterFloat              | c <- floats   ]
  , [ isDialog       --> doFloat                                    ]
  , [ isFullscreen   --> doFullFloat                                ]
  ] where
      webs     = ["Chromium", "Firefox", "Opera"]
      graphics = ["Gimp", "gimp", "GIMP"]
      floats   = [ "Choose a file"
                 , "Open Image"
                 , "File Operation Progress"
                 , "Firefox Preferences"
                 , "Rename File"
                 , "Copying files"
                 , "Moving files"
                 , "File Properties"
                 , "Replace"
                 , "Quit GIMP"
                 ]
--------------------------------------------------------------------------------
--                                LAYOUT HOOK                                 --
--------------------------------------------------------------------------------
layoutHook' =
    avoidStruts
  $ minimize
  $ maximize
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
        ||| mag
        ||| Grid
        ||| threeCol
        ||| tabbed shrinkText tabConfigTheme
      delta    = 2/100
      ratio    = 1/2
      nmaster  = 1
      mag      = magnifier (Tall 1 (3/100) (1/2))
      tiled    = Tall nmaster delta ratio
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
  , ((mod1Mask                 , xK_f         ), sendMessage $ JumpToLayout "Full")

  , ((modMask  .|. shiftMask   , xK_Return    ), spawn $ XMonad.terminal conf)
  , ((mod1Mask .|. shiftMask   , xK_Return    ), shellPrompt shellConfig')
  , ((modMask                  , xK_x         ), spawn "/usr/bin/xcalib -invert -alter")
  , ((modMask                  , xK_s         ), spawn "sleep 1; xset dpms force off")
  , ((mod1Mask                 , xK_Escape    ), spawn "/home/ivo/.xmonad/bin/changeLayout.pl")

  , ((mod1Mask .|. controlMask , xK_Page_Up   ), io setPrevWallpaper >>= spawn)
  , ((mod1Mask .|. controlMask , xK_Page_Down ), io setNextWallpaper >>= spawn)
  , ((mod1Mask .|. controlMask , xK_End       ), io setRandomWallpaper >>= spawn)

  , ((0                        , xK_Print     ), spawn "/usr/bin/scrot '%F-%T_$wx$h.png' -z -e 'mv $f ~'")

  , ((mod1Mask                 , xK_Down      ), spawn "mpc toggle")
  , ((mod1Mask                 , xK_Left      ), spawn "mpc prev")
  , ((mod1Mask                 , xK_Right     ), spawn "mpc next")
  , ((mod1Mask                 , xK_Up        ), spawn "mpc stop")

  , ((0                    , xF86XK_AudioMute ), spawn "/usr/bin/amixer set Master toggle")
  , ((0             , xF86XK_AudioRaiseVolume ), spawn "/usr/bin/amixer set Master 5%+")
  , ((0             , xF86XK_AudioLowerVolume ), spawn "/usr/bin/amixer set Master 5%-")
  ] ++
  [ ((m .|. modMask , k                       ), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
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
  { bgDB   :: DColour
  , fgDB   :: DColour
  , liDB   :: FilePath
  , riDB   :: FilePath
  , hDB    :: Int
  }

dzenBoxStyleText :: DzenBox -> String -> String
dzenBoxStyleText box text = toString $ ignoreBg False $
     fg (bgDB box) (icon (liDB box))
 +++ bg (bgDB box) (fg (fgDB box) (str text))
 +++ fg (bgDB box) (icon (riDB box))

dzenBoxStyleIcon :: DzenBox -> FilePath -> String
dzenBoxStyleIcon box file = toString $ ignoreBg False $
     fg (bgDB box) (icon (liDB box))
 +++ bg (bgDB box) (fg (fgDB box) (icon file))
 +++ fg (bgDB box) (icon (riDB box))

dzenBoxStyleTextL :: DzenBox -> Logger -> Logger
dzenBoxStyleTextL = (fmap . fmap) . dzenBoxStyleText

dzenBoxStyleIconL :: DzenBox -> Logger -> Logger
dzenBoxStyleIconL = (fmap . fmap) . dzenBoxStyleIcon

dzenSpawnPipe :: (MonadIO m, Show a) => a -> m Handle
dzenSpawnPipe df = spawnPipe $ "/usr/bin/dzen2 " ++ show df ++ " -p -e onstart=lower"

--------------------------------------------------------------------------------
--                                SETTINGS                                    --
--------------------------------------------------------------------------------
yRes                  = 768
xRes                  = 1366
dzenBg                = sRGB24show C.black
dzenFg                = sRGB24show base3
dzenFont              = "xft:monofur:size=8:antialias=true:hinting=true"
dzenHeight            = 14
dzenBoxFullIcon       = "/home/ivo/.xmonad/icons/xbm/boxFull.xbm"
dzenBoxLeftIcon       = "/home/ivo/.xmonad/icons/xbm/boxLeft.xbm"
dzenBoxRightIcon      = "/home/ivo/.xmonad/icons/xbm/boxRight.xbm"
dzenBoxSmallRightIcon = "/home/ivo/.xmonad/icons/xbm/boxSmallRight.xbm"
dzenBoxSmallLeftIcon  = "/home/ivo/.xmonad/icons/xbm/boxSmallLeft.xbm"
dzenBoxSmallFullIcon  = "/home/ivo/.xmonad/icons/xbm/boxSmallFull.xbm"

base0   = sRGB24read "#839496"
base1   = sRGB24read "#93a1a1"
base2   = sRGB24read "#eee8d5"
base3   = sRGB24read "#fdf6e3"

base00  = sRGB24read "#657b83"
base01  = sRGB24read "#586e75"
base02  = sRGB24read "#073642"
base03  = sRGB24read "#002b36"

red     = sRGB24read "#dc322f"
blue    = sRGB24read "#268bd2"
cyan    = sRGB24read "#2aa198"
green   = sRGB24read "#859900"
orange  = sRGB24read "#cb4b16"
violet  = sRGB24read "#6c71c4"
yellow  = sRGB24read "#b58900"
magenta = sRGB24read "#d33682"

workspaces' :: [WorkspaceId]
workspaces' =  map show [1..9]

workspacesNames' :: [WorkspaceId]
workspacesNames' =
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
  , bgColor           = sRGB24show base03
  , fgColor           = sRGB24show base3
  , bgHLight          = sRGB24show blue
  , fgHLight          = sRGB24show base3
  , position          = Top
  , borderColor       = sRGB24show base3
  , historySize       = 0
  , autoComplete      = Nothing
  , historyFilter     = deleteConsecutive
  , completionKey     = xK_Tab
  , promptBorderWidth = 0
  }

tabConfigTheme :: Theme
tabConfigTheme = defaultTheme
  { activeColor         = sRGB24show blue
  , activeBorderColor   = sRGB24show base01
  , inactiveColor       = sRGB24show base03
  , inactiveBorderColor = sRGB24show base01
  , urgentColor         = sRGB24show red
  , urgentBorderColor   = sRGB24show base01
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
  , bgDF = dzenBg
  , fgDF = dzenFg
  , fnDF = dzenFont
  }

dzenTopFlags :: DF
dzenTopFlags = DF
  { xDF  = 0
  , yDF  = 0
  , wDF  = xRes
  , hDF  = dzenHeight
  , taDF = "l"
  , bgDF = dzenBg
  , fgDF = dzenFg
  , fnDF = dzenFont
  }

dzenBoxSmallR :: DzenBox
dzenBoxSmallR = DB
  { liDB  = dzenBoxSmallFullIcon
  , riDB  = dzenBoxSmallRightIcon
  , hDB   = dzenHeight
  }

dzenBoxSmallL :: DzenBox
dzenBoxSmallL = DB
  { liDB  = dzenBoxSmallLeftIcon
  , riDB  = dzenBoxSmallFullIcon
  , hDB   = dzenHeight
  }

dzenBoxSmallLR :: DzenBox
dzenBoxSmallLR = DB
  { liDB  = dzenBoxSmallLeftIcon
  , riDB  = dzenBoxSmallRightIcon
  , hDB   = dzenHeight
  }

dzenBoxL :: DzenBox
dzenBoxL  = DB
  { liDB  = dzenBoxLeftIcon
  , riDB  = dzenBoxFullIcon
  , hDB   = dzenHeight
  }

dzenBoxR :: DzenBox
dzenBoxR  = DB
  { liDB  = dzenBoxFullIcon
  , riDB  = dzenBoxRightIcon
  , hDB   = dzenHeight
  }

dzenBoxLR :: DzenBox
dzenBoxLR  = DB
  { liDB  = dzenBoxLeftIcon
  , riDB  = dzenBoxRightIcon
  , hDB   = dzenHeight
  }

topBarLogHook :: Handle -> X ()
topBarLogHook h = dynamicLogWithPP defaultPP
    { ppOutput = hPutStrLn h
    , ppOrder  = \(_:_:_:x) -> x
    , ppSep    = " "
    , ppExtras = [ logLayout, workspaceL, focusL ]
    }

bottomBarLogHook :: Handle -> X ()
bottomBarLogHook h = dynamicLogWithPP defaultPP
  { ppSep             = " "
  , ppOrder           = \(ws:_:_:x) -> [ws] ++ x
  , ppWsSep           = "-"
  , ppOutput          = hPutStrLn h
  , ppExtras          = [batteryL, dateL, uptimeL, tempL, brightL, memoryL]
  , ppHidden          = ds blue   base3
  , ppUrgent          = ds red    base3
  , ppCurrent         = ds orange base3
  , ppHiddenNoWindows = ds base03 base01
  } where
      ds b f = dzenBoxStyleText dzenBoxLR { bgDB = b, fgDB = f }

(|+>) :: Logger -> Logger -> Logger
l1 |+> l2 = (liftA2 . liftA2) (++) l1 l2

labelL :: String -> Logger
labelL = return . return

left  = dzenBoxStyleTextL $ dzenBoxL { bgDB = base01, fgDB = base03 }
right = dzenBoxStyleTextL $ dzenBoxR { bgDB = base03, fgDB = base01 }

batteryL =
      (left $ labelL "BATTERY")
  |+> (labelL " ")
  |+> (right battery)

dateL =
      (left $ labelL "TIME")
  |+> (labelL " ")
  |+> (right $ date "%T")

uptimeL =
      (left $ labelL "UPTIME")
  |+> (labelL " ")
  |+> (right uptime)

memoryL =
      (left $ labelL "MEM")
  |+> (labelL " ")
  |+> (right $ memUsage [percMemUsage, totMBMemUsage])

workspaceL =
      (left $ labelL "WORKSPACE")
  |+> (labelL " ")
  |+> (right $ onLogger namedWorkspaces logCurrent)
    where
      namedWorkspaces w = workspacesNames' !! (mod ((read w::Int) - 1) 10)

focusL =
      (left $ labelL "FOCUS")
  |+> (right $ shortenL 100 logTitle)

brightL =
      (left $ labelL "BRIGHT")
  |+> (labelL " ")
  |+> (right $ brightPerc 15)

tempL = concatWithSpaceL
  [ left $ labelL "CPU"
  , right $ cpuTemp 3 70 $ sRGB24show red
  ]

concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL [] = return $ return ""
concatWithSpaceL (x:xs) = x |+> (labelL " ") |+> concatWithSpaceL xs

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

initL :: Logger -> Logger
initL = (fmap . fmap) initNotNull

fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
  let readWithE f1 e1 p1 = E.catch (do
      contents <- readFile p1
      return $ f1 (initNotNull contents)) ((\_ -> return e1) :: E.SomeException -> IO String)
  str <- liftIO $ readWithE f e p
  return $ return str

-- Battery percent
batPercent :: Int -> String -> Logger
batPercent v c = fileToLogger format "N/A" "/sys/class/power_supply/BAT0/capacity" where
    format x = if ((read x::Int) <= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

-- Battery status
batStatus :: Logger
batStatus = fileToLogger (\x -> x) "AC Conection" "/sys/class/power_supply/BAT0/status"

-- Brightness percenn
brightPerc :: Int -> Logger
brightPerc p = fileToLogger format "0" "/sys/class/backlight/ideapad/actual_brightness" where
    format x = (show $ div ((read x::Int) * 100) p) ++ "%"

-- wifi signal
wifiSignal :: Logger
wifiSignal = fileToLogger format "N/A" "/proc/net/wireless" where
    format x = if (length $ lines x) >= 3 then (initNotNull ((words ((lines x) !! 2)) !! 2) ++ "%") else "Off"

-- CPU temperature
cpuTemp :: Int -> Int -> String -> Logger
cpuTemp n v c = initL $ concatWithSpaceL $ map (fileToLogger divc "0") pathtemps where
    pathtemps = map (++"/thermal_zone/temp") $ map ("/sys/bus/acpi/devices/LNXTHERM:0"++) $ take n $ map show [0..]
    divc x = crit $ div (read x::Int) 1000
    crit x = if (x >= v) then "^fg(" ++ c ++ ")" ++ show x ++ "°^fg()" else (show x ++ "°")

-- Memory usage
memUsage :: [(String -> String)] -> Logger
memUsage xs = initL $ concatWithSpaceL $ map funct xs where
    funct x = fileToLogger x "N/A" "/proc/meminfo"

_memUsed x = (_memValues x !! 0) - (_memValues x !! 2)  --new format
_memPerc x = div (_memUsed x * 100) (_memValues x !! 0)
_memValues x = map (getValues x) $ take 4 [0..] where
    getValues x n = read (words (lines x !! n) !! 1)::Int

freeBMemUsage x  = (show $ _memValues x !! 1) ++ "B"
freeMBMemUsage x = (show $ div (_memValues x !! 1) 1024) ++ "MB"
totBMemUsage     = (++ "B") . show . _memUsed
totMBMemUsage    = (++ "MB") . show . (`div` 1024) . _memUsed
percMemUsage     = (++ "%") . show . _memPerc

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
    , normalBorderColor  = sRGB24show base03
    , focusedBorderColor = sRGB24show base3
  }
