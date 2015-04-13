{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Main inspiration:
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey's_darcs_xmonad.hs

-- Misc
import XMonad
import XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, catMaybes)
import Data.List (isPrefixOf, partition, (\\))
import Control.Monad (liftM2, when, unless)
import Control.Exception (catch)
import System.Directory
import System.Locale
import System.Time
import Data.Monoid(mempty, mappend, All(..))
import Text.Regex.PCRE((=~))

import XMonad.Config.Desktop (desktopConfig)

----- Own packages
import XMonad.Hooks.UrgencyExtra
import XMonad.Layout.TopicExtra as TE
import XMonad.Layout.WorkspaceDirAlt
import XMonad.Util.ScratchpadExtra
import XMonad.Util.ScratchpadAlt (scratchpadSpawnActionCustom,
                                  scratchpadManageHook,
                                  scratchpadFilterOutWorkspace)

----- Hooks
import XMonad.Hooks.FadeInactive (fadeIf, fadeOutLogHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook (UrgencyHook (..),
                                 RemindWhen (..),
                                 UrgencyConfig (..),
                                 withUrgencyHookC,
                                 focusUrgent,
                                 urgencyConfig)

----- Layout
import qualified XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Simplest
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

----- Actions
import XMonad.Actions.CycleWS
  (prevScreen, nextScreen, swapPrevScreen, swapNextScreen)
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.TopicSpace
  (TopicConfig (..), checkTopicConfig, switchTopic)
import XMonad.Actions.DynamicWorkspaces
  (addWorkspacePrompt, addHiddenWorkspace, renameWorkspace, removeWorkspace, addWorkspace)
import XMonad.Actions.CopyWindow
  (copyToAll, killAllOtherCopies, wsContainingCopies)

----- Prompt
import XMonad.Prompt (defaultXPConfig, fgColor, bgColor, mkXPrompt, XPConfig)
import XMonad.Prompt.Shell
import XMonad.Prompt.Input (inputPrompt, (?+))
import XMonad.Prompt.Workspace (Wor(Wor))


----- Util
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)

myLayout = ResizableTall 1 (3/100) (5/7) [] |||
           -- Tabbed.tabbedBottom Tabbed.CustomShrink myTabbedTheme
           Full

-- Don't show text in tabs.
instance Tabbed.Shrinker Tabbed.CustomShrink where
  shrinkIt _ _ = []

myTabbedTheme =
  Tabbed.defaultTheme
  { Tabbed.inactiveBorderColor = "#000000"
  , Tabbed.inactiveColor       = "#000000"
  , Tabbed.activeColor         = "#BB0000"
  , Tabbed.activeBorderColor   = "#BB0000"
  , Tabbed.urgentBorderColor   = "#FF0000"
  , Tabbed.decoHeight          = 3
  }

myManageHook =
  [ title     =? "Calendar"   --> doShift "organise"
  , title     =? "GMail"      --> doShift "organise"
  , className =? "Gimp"       --> viewShift "gimp"
  , className =? "VirtualBox"      --> do name <- title
                                          case (name =~ "( \\(.*\\))?( \\[[^\\]]+\\])? - Oracle VM VirtualBox$") :: (String,String,String) of
                                            (_,"",_) -> return mempty
                                            (n,_,_)  -> do let ws = "vm-" ++ n
                                                           liftX $ addHiddenWorkspace ws
                                                           doShift ws
  ]
    where
      viewShift = doF . liftM2 (.) W.greedyView W.shift

myTopics =
  [ "im"
  , "web"
  , "organise"
  , "reading"
  , "inkscape"
  , "gimp"
  , "multimedia"
  , "procrastination"
  , "wireshark"
  , "idapro"
  , "virtualbox"
  , "download"
    -- Configuration
  , "emacs"
  , "xmonad"
  , "install"
  , "config"
    -- Coding
  , "sml"
  , "haskell"
  , "python"
    -- Projects
  , "pwntools"
  , "treasure-hunt"
  , "bitcoin"
    -- Misc
  , "background"
  , "windows"
  , "anon"
  ]

exec s = spawn $ "exec " ++ s

myTerminal = "xterm"
myBrowser = "chromium"
edit s = exec ("emacs " ++ s)
term = exec myTerminal
browser s = exec ("chromium " ++ s)
newBrowser s = exec ("chromium --new-window " ++ s)
appBrowser s = exec ("chromium --app=\"" ++ s ++ "\"")

myTopicConfig = TopicConfig
  { topicDirs = M.fromList []
  , topicActions =
       M.fromList $
       -- [ ("im", safeSpawn myTerminal ["-e", "ssh", "irssi@yesimparanoid.com", "-t", "screen", "-DR", "irc"])
       -- [ ("im", safeSpawn myTerminal ["-e", "ssh", "irc@fa.ntast.dk", "-t", "screen", "-DR", "irc"])
       [ ("im", safeSpawn myTerminal ["-e", "ssh", "lolbox.pwnies.dk", "-t", "screen", "-DR", "irc"])
       -- [ ("im", term)
       , ("web", browser "")
       , ("organise", appBrowser "http://gmail.com" >>
                      appBrowser "http://calendar.google.com")
       , ("multimedia", appBrowser "https://soundcloud.com/explore/hardcore%2Btechno")
       , ("procrastination", newBrowser
                             "xkcd.com \
                             \facebook.com \
                             \smbc-comics.com \
                             \phdcomics.com/comics.php")
       , ("virtualbox", exec "virtualbox")
       , ("reading", exec "evince")
       , ("emacs", edit "~/.emacs.d/settings/global-key-bindings.el")
       , ("xmonad", edit "~/.xmonad/xmonad.hs" >>
                    newBrowser
                    "http://xmonad.org/xmonad-docs/xmonad-contrib/index.html")
       , ("install", term)
       , ("pwntools", newBrowser "https://github.com/Gallopsled/pwntools" >>
                      term)
       , ("treasure-hunt", edit "~/pwnies/treasure-hunt/chal" >>
                           term)
       , ("haskell", newBrowser "www.haskell.org/hoogle/")
       , ("inkscape", exec "inkscape")
       , ("gimp", exec "gimp")
       , ("bitcoin", newBrowser "http://bitcoinity.org/markets \
                                \http://bitcoinwisdom.com/bitcoin/difficulty \
                                \https://bitcointalk.org/")
       ]
  , defaultTopicAction = const $ return ()
  , defaultTopic = "web"
  , maxTopicHistory = 10
  }

setWorkspaceDirs layout =
  set "treasure-hunt"   "~/projects/treasurehunt"                              $
  set "pwntools"        "~/projects/pwntools/pwnlib"                           $
  set "download"        "~/downloads"                                          $
  set "study"           "~/study"                                              $
  set "sml"             "~/code/sml"                                           $
  set "haskell"         "~/code/haskell"                                       $
  set "python"          "~/code/python"                                        $
  workspaceDir "~" layout
  where set ws dir = onWorkspace ws (workspaceDir dir layout)

br0nsConfig =
  withUrgencyHookC LibNotifyUrgencyHook urgencyConfig { remindWhen = Every 10 } $
  desktopConfig
       { modMask = mod4Mask
       , manageHook = manageHook desktopConfig <+>
                      composeAll myManageHook <+>
                      scratchpadManageHook (W.RationalRect 0.05 0.05 0.9 0.9)
       , layoutHook = smartBorders $
                      setWorkspaceDirs myLayout
       , borderWidth = 0
       , focusFollowsMouse = False
       , logHook = fadeOutLogHook $ fadeIf isUnfocusedOnCurrentWS 0.8
       , XMonad.workspaces = myTopics
       , terminal = myTerminal
       , handleEventHook = myEventHook
       }
       `removeKeysP` (["M-q"] ++ ["M-" ++ m ++ k | m <- ["", "S-"], k <- map show [1..9 :: Int]])
       `additionalKeysP` myKeys

myEventHook :: Event -> X All
myEventHook = deleteUnimportant (=~ "^(scratchpad|vm)-") callback
  where callback dead = withDir $ \tag dir ->
                  when (tag `elem` dead && tag =~ "^scratchpad-" && dir =~ ('^' : myScratchpadDir)) $ io $ deleteIfEmpty dir

deleteIfEmpty dir = do contents <- getDirectoryContents dir
                       liftIO $ putStrLn dir
                       when (null $ contents \\ [".", ".."]) $ removeDirectory dir
                    `catch` \(_e :: IOError) -> return ()

main = do
  liftIO $ do x <- doesDirectoryExist myScratchpadDir
              unless x (createDirectory myScratchpadDir)
  checkTopicConfig myTopics myTopicConfig
  xmonad $ br0nsConfig

myKeys =
  -- Rebind mod-q
  [ ("M-S-<Esc>", spawn "~/.cabal/bin/xmonad --recompile && ~/.cabal/bin/xmonad --restart")
  -- Application launcher
  , ("M-p", exec "rofi -show run")
  -- Bring up a menu for pass
  , ("M-C-p", exec "~/.xmonad/passmenu")
  -- Lock and suspend
  , ("M-C-l", exec "slock")
  , ("M-C-<Backspace>", exec "~/.xmonad/suspend")
  -- GSSelect
  , ("M-g", goToSelected myGSConfig)
  -- Window stack
  , ("M-a", windows W.swapMaster >> cycleRecentWindows [xK_Super_L] xK_a xK_q)
  -- Workspace navigation
  , ("M-S-z", shiftToSelectedWS True myGSConfig)
  , ("M-z", goToSelectedWS  myTopicConfig True myGSConfig)
  -- Screen navigation
  , ("M-<Left>", prevScreen)
  , ("M-<Right>", nextScreen)
  , ("M-S-<Left>", swapPrevScreen)
  , ("M-S-<Right>", swapNextScreen)
  -- Window resizing
  , ("M-S-h", sendMessage MirrorExpand)
  , ("M-S-l", sendMessage MirrorShrink)
  -- Dynamic workspaces
  , ("M-d", changeDir myXPConfig)
  , ("M-n", addWorkspacePrompt myXPConfig)
  , ("M-m", addWorkspaceMoveWindowPrompt myXPConfig)
  , ("M-C-S-<Backspace>", killAll >> myRemoveWorkspace)
  , ("M-r", renameWorkspace myXPConfig)
  , ("M-s", do dir <- liftIO $ formatCalendarTime defaultTimeLocale (myScratchpadDir ++ "/%Y-%m-%d-%H:%M:%S")  `fmap` (getClockTime >>= toCalendarTime)
               liftIO $ createDirectory dir
               newScratchpad
               changeDir_ dir)
  -- Scratchpad
  , ("M-S-<Space>", scratchpadSpawnActionCustom "term" "xterm -name scratchpad-term")
  , ("M-C-<Space>", scratchpadSpawnActionCustom "python" "PYTHONPATH=~/projects/pwntools/ xterm -name scratchpad-python -e ipython -c 'from pwn import *' --no-confirm-exit -i")
  -- Global window
  , ("M-S-g", toggleGlobal)
  -- Focus urgent
  , ("M-u", focusUrgent)
  -- Notifications
  , ("M-8", exec "notify-send -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\" --icon=dialog-information")
  , ("M-9", exec "notify-send -t 2000 Battery \"$(acpi)\" --icon=dialog-information")
  , ("M-0", exec "notify-send -t 1500 Date \"$(date)\" --icon=dialog-information")
  ]

-- Remove workspace unless it's a topic
myRemoveWorkspace :: X ()
myRemoveWorkspace = do
  s <- gets windowset
  case s of
    StackSet {current = W.Screen { workspace = Workspace { tag = this } } } -> do
      withDir $ \tag dir -> when (tag == this && tag =~ "^scratchpad-" && dir =~ ('^' : myScratchpadDir)) $ io $ deleteIfEmpty dir
      when (this `notElem` myTopics) removeWorkspace

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { fgColor = "#a8a3f7"
  -- , bgColor = "#ff3c6d"}
  , bgColor = "#3f3c6d"
  }

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig {gs_navigate = navNSearch}

withSelectedWS :: (WindowSpace -> X ()) -> Bool -> GSConfig WindowSpace -> X ()
withSelectedWS callback inclEmpty conf = do
  mbws <- gridselectWS inclEmpty conf
  case mbws of
    Just ws -> callback ws
    Nothing -> return ()

-- Includes empty window spaces if {True}
gridselectWS :: Bool -> GSConfig WindowSpace -> X (Maybe WindowSpace)
gridselectWS inclEmpty conf =
  withWindowSet $ \ws -> do
    let hid = W.hidden ws
        vis = map W.workspace $ W.visible ws
        all = scratchpadFilterOutWorkspace $ hid ++ vis
        wss = if inclEmpty
              then let (nonEmp, emp) = partition nonEmptyWS all
                   in nonEmp ++ emp
              else Prelude.filter nonEmptyWS all
        ids = map W.tag wss
    gridselect conf $ zip ids wss

myScratchpadDir :: String
myScratchpadDir = "/tmp/scratchpads"

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")
