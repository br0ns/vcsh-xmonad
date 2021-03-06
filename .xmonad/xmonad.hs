{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Main inspiration:
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey's_darcs_xmonad.hs

-- Misc
import XMonad
import XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, catMaybes, fromMaybe)
import Data.List (isPrefixOf, partition, (\\), find)
import Control.Monad (liftM, liftM2, when, unless)
import Control.Applicative ((<$>))
import Control.Exception (catch)
import Control.Arrow (second)
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
import XMonad.Layout.Reflect

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

-- Ref: https://www.reddit.com/r/xmonad/comments/fhzw3
findScreenByTag i = gets (W.screens . windowset)
                    >>= return . find ((== i) . (W.tag . W.workspace))
data SLS sl ol a = SLS ScreenId sl ol deriving (Show, Read)
instance (LayoutClass sl a, LayoutClass ol a)
      => LayoutClass (SLS (sl a) (ol a)) a where

    -- glance at the current screen to determine which layout to run
    runLayout (W.Workspace i (SLS s sl ol) ms) r = do
        mts <- findScreenByTag i
        case liftM ((== s) . W.screen) mts of
            Just True -> fmap (second . fmap $ \nsl -> SLS s nsl ol)
                       $ runLayout (W.Workspace i sl ms) r
            _ -> fmap (second . fmap $ \nol -> SLS s sl nol)
               $ runLayout (W.Workspace i ol ms) r

    -- route messages to both sub-layouts (ick)
    handleMessage l@(SLS s sl ol) m = do
        msl <- handleMessage sl m
        mol <- handleMessage ol m
        return $ if isNothing msl && isNothing mol
         then Nothing
         else Just $ SLS s (fromMaybe sl msl) (fromMaybe ol mol)

-- | A utility constructor for SLS which helps to get the types right
mksls :: (LayoutClass sl a, LayoutClass ol a)
      => Int -> (sl a) -> (ol a) -> SLS (sl a) (ol a) a
mksls i = SLS (S i)

myLayout = mksls 0 layout $ reflectHoriz layout
  where
    layout = toggleLayouts
      (Full |||
       TwoPane (3/100) (1/2)
      )
      (multiCol [1] 4 (3/100) (4/7))

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

expandUser :: FilePath -> X FilePath
expandUser path = liftIO $
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

exec s = do
  let prog : args = words s
  path <- expandUser prog
  safeSpawn path args
  where

myTerminal = "~/.xmonad/xterm-acwd"
myBrowser = "google-chrome"
myEditor = "emacs"

edit files = safeSpawn myEditor files
term = exec myTerminal

-- Symlink ~/.config/google-chrome to ~/.config/google-chrome-DEFAULT
browser profile args urls = do
  profileDir <- expandUser $ "~/.config/google-chrome-" ++ profile
  safeSpawn myBrowser $
    ["--user-data-dir=" ++ profileDir] ++ args ++ "--" : urls

newBrowser profile = browser profile ["--new-window"]
appBrowser profile url = browser profile ["--app=" ++ url] []

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
  , "fish"
  , "emacs"
  , "gdb"
  , "git"
  , "pkgs"
  , "xmonad"

    -- Programming
  , "haskell"
  , "python"
  , "sml"

    -- Internet
  , "bitcoin"
  , "stocks"

    -- Projects
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
          exec $ myTerminal ++ " -e ssh fa.ntast.dk -t screen -DR irc"
         )
       , ("music",
          appBrowser "default" "https://soundcloud.com/explore/trance"
         )
       , ("organise",
          do appBrowser "default" "http://gmail.com"
             appBrowser "phone" "http://gmail.com"
             appBrowser "phone" "http://calendar.google.com"
             edit ["~/.when/calendar"]
         )
       , ("procrastination",
          newBrowser "default"
           [ "xkcd.com"
           , "facebook.com"
           , "smbc-comics.com"
           , "phdcomics.com/comics.php"
           ]
         )
       , ("web",
          newBrowser "default" []
         )

         -- Configuration
       , ("bash",
          edit ["~/.bashrc"]
         )
       , ("fish",
          edit ["~/.config/fish/config.fish"]
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
             newBrowser "default" ["https://hackage.haskell.org/package/xmonad-contrib"]
         )

         -- Programming
       , ("haskell",
          newBrowser "default" ["www.haskell.org/hoogle/"]
         )

         -- Internet
       , ("bitcoin",
          newBrowser "default"
           [ "http://bitcoinity.org/markets"
           , "http://bitcoinwisdom.com/bitcoin/difficulty"
           , "https://www.hashnest.com"
           , "https://www.bitstamp.net"
           ]
         )

       , ("stocks",
           let
             stock xchg symb =
               "https://www.google.dk/search?q=" ++ xchg ++ "%3A" ++ symb
             nasdaq   = stock "nasdaq"
             helsinki = stock "hel"
           in
             newBrowser "default"
             [ nasdaq   "nvda"  -- Nvidia
             , nasdaq   "tsla"  -- Tesla
             , nasdaq   "amd"   -- AMD
             , nasdaq   "intc"  -- Intel
             , nasdaq   "msft"  -- Microsoft
             , nasdaq   "fb"    -- Facebook
             , helsinki "nokia" -- Nokia
             , nasdaq   "googl" -- Alphabet (Google)
             , nasdaq   "amzn"  -- Amazon
             ]
         )

         -- Projects
       , ("projects",
          edit ["~/projects/NOTES.md"]
         )
       , ("pwntools",
          do newBrowser "default" ["https://github.com/Gallopsled/pwntools"]
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
       -- Unbind quit/restart and switching between workspaces with the #-keys
       `removeKeysP` (["M-q", "M-S-q"
                      ,"M-w", "M-S-w"
                      ,"M-e", "M-S-e"
                      ,"M-r", "M-S-r"
                      ] ++
                      ["M-" ++ m ++ k |
                        m <- ["", "S-"],
                        k <- map show [1..9 :: Int]])
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
  , ("M-m",                    exec "volume toggle --active-window")

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
  , ("M-S-p", scratchpadSpawnActionCustom "python" "xterm -name scratchpad-python -e ipython --profile=scratchpad")
  , ("M-S-h", scratchpadSpawnActionCustom "haskell" "xterm -name scratchpad-haskell -e ghci")

  -- Global window
  , ("M-S-g", toggleGlobal)

  -- Focus urgent
  , ("M-u", focusUrgent)

  -- Insert unicode character
  , ("M-i", exec "~/.xmonad/unicodemenu")

  -- Notifications
  , ("M-0", notify "$(date +\"%A %B %d\")" "$(date +\"%F %H:%M\")")
  , ("M-9", notify "" "$(acpi)")
  , ("M-8", do ws <- currentWorkspace
               notify "" ws)
  , ("M-7", do ws <- gets windowset
               case W.peek ws of
                Just w -> do nm <- getName w
                             notify "" $ show nm
                otherwise -> return ()
    )
  , ("M-3", notify "" "$(when)")
  , ("M-2", notify "" "$(~/.xmonad/dwmr)")
  , ("M-1", notify "" "$(df -h)")
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
