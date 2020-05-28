import System.Exit
import Data.Bits ((.|.))

import XMonad.Config
import XMonad.Core
import XMonad.Main
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Layout ((|||))
import XMonad.Layout.NoBorders (smartBorders, SmartBorder)
import qualified XMonad.Layout.Spacing as Gaps
import qualified XMonad.Layout as L

import Graphics.X11.Types
import Graphics.X11.ExtraTypes.XF86

type LBoth = L.Choose
type SmartTall = ModifiedLayout SmartBorder L.Tall
type SmartFull = ModifiedLayout SmartBorder L.Full
type TallGaps = ModifiedLayout Gaps.Spacing SmartTall
type MyLayout = LBoth TallGaps (LBoth (L.Mirror TallGaps) SmartFull)

-- The Default modifier
dMod :: KeyMask
dMod = mod4Mask

-- Removes unwanted keybinds from the defaults
remMyKeys :: XConfig MyLayout -> XConfig MyLayout
remMyKeys = (flip removeKeys) [(dMod .|. shiftMask, xK_q)]

-- Adds my keybinds to the default kbinds
addMyKeys :: XConfig MyLayout -> XConfig MyLayout
addMyKeys = (flip additionalKeys) [
  ((dMod              , xK_w), spawn "firefox"),
  ((dMod .|. shiftMask, xK_e), io (exitWith ExitSuccess)),
  -- Media keys
  ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
  ((0, xF86XK_AudioPrev), spawn "playerctl previous"),
  ((0, xF86XK_AudioNext), spawn "playerctl next"),
  ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
  ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
  ((0, xF86XK_AudioMute), spawn "pactl set-sink-volume @DEFAULT_SINK@ toggle")]

-- Desconstruct XConfig to extract the mask and returns togglekeybind
toggleMyBar :: XConfig Layout -> (KeyMask, KeySym)
toggleMyBar XConfig { modMask = defMask } = (defMask, xK_b)

-- Starts stuff up!
myStartupHook :: X ()
myStartupHook = do
  spawn "dunst"
  spawn "picom"
  spawn "feh ~/Imagens/wallpapers/ --bg-fill --randomize"

myWindowGaps :: Gaps.Border
myWindowGaps = Gaps.Border { Gaps.top = size
                           , Gaps.bottom = size
                           , Gaps.left = size
                           , Gaps.right = size }
  where size = 10

-- myScreenGaps :: Gaps.Border
-- myScreenGaps = Gaps.Border { Gaps.top = size
--                            , Gaps.bottom = size
--                            , Gaps.left = size
--                            , Gaps.right = size }
--   where size = 5

myLayouts :: MyLayout a
myLayouts = tiled ||| L.Mirror tiled ||| smartBorders L.Full
  where
    vanillaTall = L.Tall nmaster delta ratio
    smartTall = smartBorders vanillaTall
    -- Smart tiling?, screeng gap, use screen gap?, window gap, use window gap?
    tiled = Gaps.spacingRaw True myWindowGaps True myWindowGaps True smartTall
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100



-- Builds a config transformator to add bar support (def is default PP)
myBar :: XConfig MyLayout -> IO (XConfig (ModifiedLayout AvoidStruts MyLayout))
myBar = statusBar "xmobar" xmobarPP toggleMyBar

-- Override some of the config defaults
myConfig :: XConfig MyLayout
myConfig = def
  { modMask = dMod -- Use Super instead of Alt
  , startupHook = myStartupHook
  , terminal = "alacritty"
  , layoutHook = myLayouts
  , focusedBorderColor = "#7851a9"
  -- more changes
  }

main :: IO ()
main = xmonad =<< myBar ((remMyKeys . addMyKeys) myConfig)
