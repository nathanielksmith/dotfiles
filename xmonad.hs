import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

main :: IO ()
main = do 
  xmonad $ ewmh defaultConfig 
    { modMask = mod4Mask
    , manageHook = manageHook defaultConfig <+> manageDocks
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook <+> fullscreenEventHook
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    }