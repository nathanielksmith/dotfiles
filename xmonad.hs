import XMonad
import XMonad.Util.EZConfig(additionalKeys)

main = do
    spawn "gnome-settings-daemon"
    xmonad $ defaultConfig {
        modMask = mod4Mask,
        borderWidth = 4,
        normalBorderColor = "#3A1957",
        focusedBorderColor = "#535719"
        } `additionalKeys`
        [ ((mod4Mask, xK_r), spawn "dmenu_run") ]
