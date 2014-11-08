tint2 &
nitrogen --restore &
pnmixer &
xfce4-volumed &
xfce4-power-manager &
thunar --daemon &

if egrep -iq 'touchpad' /proc/bus/input/devices; then
    synclient VertTwoFingerScroll=1 &
    synclient TapButton1=1 &
    synclient HorizTwoFingerScroll=1 &
    synclient TapButton2=2 &
fi

# xscreensaver -no-splash &

xset r rate 250 25 &
xset b off &
