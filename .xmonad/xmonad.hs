{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- Main inspiration:
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey's_darcs_xmonad.hs

-- Misc
import XMonad
import XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, catMaybes)
import Data.List (isPrefixOf, partition, (\\))
import Control.Monad (liftM2, when, unless)
import Control.Applicative ((<$>))
import Control.Exception (catch)
import System.Directory
import System.FilePath
import System.Locale
import System.Time
import Data.Monoid(mempty, mappend, All(..))
import Text.Regex.PCRE((=~))

import XMonad.Config (defaultConfig)

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
import XMonad.Hooks.EwmhDesktops (ewmh)

----- Layout
import qualified XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Simplest
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.WindowNavigation
import XMonad.Layout.TwoPane
import XMonad.Layout.ToggleLayouts

----- Actions
import XMonad.Actions.CycleWS
  (prevScreen, nextScreen, swapPrevScreen, swapNextScreen, shiftPrevScreen, shiftNextScreen)
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
import XMonad.Util.Cursor

-- XXX: focusUp, ... only on mapped windows

myLayout =
         -- Tall 1 (3/100) (4/7) |||
         -- ResizableTall 1 (3/100) (4/7) [] |||
         -- Tabbed.tabbedBottom Tabbed.CustomShrink myTabbedTheme
         toggleLayouts
         (Full |||
          TwoPane (3/100) (1/2)
         )
         (multiCol [1] 4 (3/100) (4/7)
         )

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

------------------------------------------------------------------------
--                               TOPICS                               --
------------------------------------------------------------------------

exec s = do
  let prog : args = words s
  path <- liftIO $ expandUser prog
  safeSpawn path args
  where
    expandUser path =
      if head path /= '~'
      then return path
      else do
        let tilde : suffix = splitPath path
        -- `init` because `splitPath "~/foo"` => `["~/", "foo"]`
        prefix <- case init tilde of
          [_]      -> getHomeDirectory
          _ : user -> return $ "/home/" ++ user
        let path = joinPath $ prefix : suffix
        return path

myTerminal = "xterm"
myBrowser = "chromium"
myEditor = "emacs"

edit files = safeSpawn myEditor files
term = exec myTerminal
browser urls = safeSpawn myBrowser $ "--" : urls
newBrowser urls = safeSpawn myBrowser $ "--new-window" : "--" : urls
appBrowser url = safeSpawn myBrowser ["--app=" ++ url]

myTopics =
  [ "today"

    -- "Tasks"
  , "anon"
  , "background"
  , "download"
  , "im"
  , "installing"
  , "music"
  , "organise"
  , "procrastination"
  , "reading"
  , "web"

    -- Configuration
  , "bash"
  , "emacs"
  , "gdb"
  , "git"
  , "pkgs"
  , "xmonad"

    -- Programming
  , "haskell"
  , "python"
  , "sml"

    -- Projects
  , "bitcoin"
  , "blog"
  , "bootstrap"
  , "ddmin"
  , "projects"
  , "pwntools"
  , "treasure-hunt"
  ] ++ programTopics

programTopics =
  [ "darktable"
  , "gimp"
  , "idapro"
  , "inkscape"
  , "virtualbox"
  , "windows"
  , "wireshark"
  ]

myTopicConfig = TopicConfig
  { topicDirs = M.fromList []
  , topicActions =
       M.fromList $
       [ ("today",
          exec "today"
         )

         -- Tasks
       , ("anon",
          exec "torbrowser-launcher"
         )
       , ("download",
          exec "deluge"
         )
       , ("im",
          exec $ myTerminal ++ " -e ssh lolbox.pwnies.dk -t screen -DR irc"
         )
       , ("music",
          appBrowser "https://soundcloud.com/explore/trance"
         )
       , ("organise",
          do appBrowser "http://gmail.com"
             appBrowser "http://calendar.google.com"
             edit ["~/.when/calendar"]
         )
       , ("procrastination",
          newBrowser [ "xkcd.com"
                     , "facebook.com"
                     , "smbc-comics.com"
                     , "phdcomics.com/comics.php"
                     ]
         )
       , ("web",
          browser []
         )

         -- Configuration
       , ("bash",
          edit ["~/.bashrc"]
         )
       , ("emacs",
          edit ["~/.emacs.d/settings/global-key-bindings.el"]
         )
       , ("gdb",
          edit ["~/.gdbinit"]
         )
       , ("git",
          edit ["~/.gitconfig"]
         )
       , ("pkgs",
          edit [ "~/.pkgs/dowant.pkgs"
               , "~/.pkgs/ignore.pkgs"
               , "~/.pkgs/delete.pkgs"]
         )
       , ("xmonad",
          do edit ["~/.xmonad/xmonad.hs"]
             newBrowser ["http://xmonad.org/xmonad-docs/xmonad-contrib/"]
         )

         -- Programming
       , ("haskell",
          newBrowser ["www.haskell.org/hoogle/"]
         )

         -- Projects
       , ("bitcoin",
          newBrowser [ "http://bitcoinity.org/markets"
                     , "http://bitcoinwisdom.com/bitcoin/difficulty"
                     , "https://www.hashnest.com"
                     , "https://www.bitstamp.net"
                     ]
         )
       , ("projects",
          edit ["~/projects/NOTES.md"]
         )
       , ("pwntools",
          do newBrowser ["https://github.com/Gallopsled/pwntools"]
             term
         )
       , ("treasure-hunt",
          do edit ["~/pwnies/treasure-hunt/chal"]
             term
         )

       ] ++ [(p, exec p) | p <- programTopics]


  , defaultTopicAction = const $ return ()
  , defaultTopic = "today"
  , maxTopicHistory = 10
  }

setWorkspaceDirs layout =
  -- Tasks
  set "download"        "~/downloads"                                          $

  -- Projects
  set "blog"            "~/projects/blog"                                      $
  set "bootstrap"       "~/projects/bootstrap"                                 $
  set "ddmin"           "~/projects/ddmin"                                     $
  set "projects"        "~/projects"                                           $
  set "pwntools"        "~/projects/pwntools/pwnlib"                           $
  set "treasure-hunt"   "~/projects/treasurehunt"                              $

  -- Programming
  set "haskell"         "~/code/haskell"                                       $
  set "python"          "~/code/python"                                        $
  set "sml"             "~/code/sml"                                           $
  workspaceDir "~" layout
  where set ws dir = onWorkspace ws (workspaceDir dir layout)

br0nsConfig =
  withUrgencyHookC LibNotifyUrgencyHook urgencyConfig { remindWhen = Every 10 } $
  ewmh defaultConfig
       { modMask = mod4Mask

       , manageHook      = composeAll myManageHook <+>
                           scratchpadManageHook (W.RationalRect 0.05 0.05 0.9 0.9)
       , layoutHook      = smartBorders $
                           setWorkspaceDirs myLayout
       , logHook         = fadeOutLogHook $ fadeIf isUnfocusedOnCurrentWS 0.8
       , handleEventHook = myEventHook
       , startupHook     = do
         setDefaultCursor xC_left_ptr
         exec "today"

       , borderWidth = 0
       , focusFollowsMouse = False
       , XMonad.workspaces = myTopics
       , terminal = "exec " ++ myTerminal
       }
       -- Unbind quit and switching between workspaces with the number keys
       `removeKeysP` (["M-q"] ++ ["M-" ++ m ++ k | m <- ["", "S-"], k <- map show [1..9 :: Int]])
       -- My key bindings
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

  -- Volume
  , ("<XF86AudioLowerVolume>", exec "volume -")
  , ("<XF86AudioRaiseVolume>", exec "volume +")
  , ("<XF86AudioMute>",        exec "volume toggle")

  -- Display
  , ("<XF86Display>", exec "xrandr-cycle")
  , ("M-o", exec "xrandr-cycle")

  -- Panel brightness
  , ("<XF86MonBrightnessUp>", exec "~/.xmonad/brightness +")
  , ("<XF86MonBrightnessDown>", exec "~/.xmonad/brightness -")

  -- GridSelect
  , ("M-g", goToSelected myGSConfig)

  -- Toggle layouts
  , ("M-<Space>", sendMessage ToggleLayout)
    -- M-|
  , ("M-S-\\", sendMessage NextLayout)

  -- Window stack
  , ("M-a", windows W.swapMaster >> cycleRecentWindows [xK_Super_L] xK_a xK_q)

  -- Workspace navigation
  , ("M-S-z", shiftToSelectedWS True False myGSConfig)
  , ("M-z", goToSelectedWS myTopicConfig True False myGSConfig)

  -- Screen navigation
  , ("M-<Left>", prevScreen)
  , ("M-<Right>", nextScreen)
  , ("M-C-<Left>", shiftPrevScreen >> prevScreen)
  , ("M-C-<Right>", shiftNextScreen >> nextScreen)
  , ("M-<Up>", swapNextScreen)
  , ("M-<Down>", swapPrevScreen)
  , ("M-C-<Up>", swapNextScreen >> nextScreen)
  , ("M-C-<Down>", swapPrevScreen >> prevScreen)

  -- Window resizing
  -- , ("M-S-h", sendMessage MirrorExpand)
  -- , ("M-S-l", sendMessage MirrorShrink)

  -- Dynamic workspaces
  , ("M-d", changeDir myXPConfig)
  , ("M-n", addWorkspacePrompt myXPConfig)
  , ("M-S-n", addWorkspaceMoveWindowPrompt myXPConfig)
  , ("M-C-S-<Backspace>", killAll >> myRemoveWorkspace)
  , ("M-r", renameWorkspace myXPConfig)
  , ("M-s", do dir <- liftIO $ formatCalendarTime defaultTimeLocale (myScratchpadDir ++ "/%Y-%m-%d-%H.%M.%S")  `fmap` (getClockTime >>= toCalendarTime)
               liftIO $ createDirectory dir
               newScratchpad
               changeDir_ dir)

  -- Scratchpad
  , ("M-S-<Space>", scratchpadSpawnActionCustom "term" "xterm -name scratchpad-term")
  , ("M-S-p", scratchpadSpawnActionCustom "python" "PYTHONPATH=~/projects/pwntools/ xterm -name scratchpad-python -e ipython -c 'from pwn import *' --no-confirm-exit -i")
  , ("M-S-h", scratchpadSpawnActionCustom "haskell" "xterm -name scratchpad-haskell -e ghci")
  -- , ("M-S-s", scratchpadSpawnActionCustom "python" "PYTHONPATH=~/projects/pwntools/ xterm -name scratchpad-python -e ipython -c 'from pwn import *' --no-confirm-exit -i")

  -- Global window
  , ("M-S-g", toggleGlobal)

  -- Focus urgent
  , ("M-u", focusUrgent)

  -- Notifications
  , ("M-0", notify "$(date +\"%A %B %d\")" "$(date +\"%F %H:%M\")")
  , ("M-9", notify "" "$(acpi)")
  , ("M-8", do ws <- currentWorkspace
               notify "" ws)
  , ("M-7", notify "" "$(when)")
  , ("M-6", notify "" "$(~/.xmonad/dwmr)")
  , ("M-5", notify "" "$(df -h)")
  ]

notify title body = spawn $ "notify -t 2 \"" ++ title ++ "\" \"" ++ body ++ "\""

-- Remove workspace unless it's a topic
myRemoveWorkspace :: X ()
myRemoveWorkspace = do
  s <- gets windowset
  case s of
    StackSet {current = W.Screen { workspace = Workspace { tag = this } } } -> do
      withDir $ \tag dir -> when (tag == this && tag =~ "^scratchpad-" && dir =~ ('^' : myScratchpadDir)) $ io $ deleteIfEmpty dir
      when (this `notElem` myTopics) removeWorkspace

-- visibleWorkspaces :: X [WorkspaceId]

currentWorkspace :: X WorkspaceId
currentWorkspace = W.tag . W.workspace . W.current <$> gets windowset

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { fgColor = "#a8a3f7"
  -- , bgColor = "#ff3c6d"}
  , bgColor = "#3f3c6d"
  }

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig {gs_navigate = navNSearch}

myScratchpadDir :: String
myScratchpadDir = "/tmp/scratchpads"

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")
