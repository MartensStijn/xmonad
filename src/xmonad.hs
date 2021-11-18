import qualified Data.Map as M
import Control.Monad (liftM2)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicIcons (IconConfig (..), appIcon,
                                  dynamicIconsPP,
                                  iconsFmtReplace,
                                  iconsGetFocus,
                                  wrapUnwords)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Run (safeSpawn, unsafeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Hooks.SetWMName (setWMName)
import qualified XMonad.StackSet as W

myModMask, altMask :: KeyMask
myModMask = mod4Mask
altMask = mod1Mask

myNumRow = [xK_ampersand
           , xK_bracketleft
           , xK_braceleft
           , xK_braceright
           , xK_parenleft
           , xK_equal
           , xK_asterisk
           , xK_parenright
           , xK_plus]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [
       -- launch a terminal
         ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

       -- close focused window
       , ((modm .|. shiftMask, xK_c)    , kill1)

       -- close all windows on the current workspace
       , ((modm .|. controlMask, xK_c)  , killAll)

       -- Rotate through the available layout algorithms
       , ((modm, xK_space)              , sendMessage NextLayout)

       -- Move focus to the next window
       , ((modm, xK_j)                  , windows W.focusDown)

       -- Move focus to the previous window
       , ((modm, xK_k)                  , windows W.focusUp)

       -- Shrink the master area
       , ((modm, xK_h)                  , sendMessage Shrink)

       -- Expand the master area
       , ((modm, xK_l)                  , sendMessage Expand)

       -- Push window back into tiling
       , ((modm, xK_t)                  , withFocused $ windows . W.sink)

       -- Push all windows on the current workspace into tiling
       , ((modm .|. shiftMask, xK_t)    , sinkAll)

       -- Restart xmonad
       , ((modm, xK_q), unsafeSpawn "xmonad --recompile; xmonad --restart")
    ]
    
    ++
       --
       -- mod-[1..9], Switch to workspace N
       -- mod-shift-[1..9], Move client to workspace N
       --
       [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) myNumRow
       , (f, m) <-
         [ (W.greedyView                   , 0)
         , (W.shift                        , shiftMask)
         , (liftM2 (.) W.greedyView W.shift, controlMask)
         ]
       ]


myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"
  -- spawnOnce "feh --bg-scale --randomize ~/Pictures/Wallpapers/*"
  spawnOnce "setxkbmap -layout us -variant dvp"

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myFgColor = "#D8DEE9"

topPP :: X PP
topPP =
  pure
    $   def { ppCurrent = xmobarBorder "Bottom" myFgColor 4
            , ppUrgent  = xmobarBorder "Bottom" "#CD3C66" 4
            , ppVisible = xmobarBorder "Bottom" "#98a0b3" 1
       --     , ppSep     = circleSep
            , ppExtras = [logLayoutOnScreen 0, shortenL 50 (logTitleOnScreen 0)]
            , ppOrder   = \(ws : _ : _ : extras) -> ws : extras
            }

secondaryPP :: ScreenId -> X PP
secondaryPP s = pure $ def
  { ppOrder  = \(_ : _ : _ : extras) -> extras
  , ppExtras = [ logCurrentOnScreen s
               , logLayoutOnScreen s
               , shortenL 50 $ logTitleOnScreen s
               , logWhenActive s (logConst "*")
               ]
  }

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure $ statusBarProp "xmobar 0" topPP
barSpawner 1 = pure $ statusBarProp "xmobar 1" topPP


main :: IO ()
main = do
  xmonad . dynamicEasySBs barSpawner . ewmhFullscreen . ewmh $ def
    { 
      -- variables
      terminal = "kitty"
    , modMask = myModMask

      -- key bindings
    , keys = myKeys

      -- hooks, layouts
    , startupHook = myStartupHook
    }

