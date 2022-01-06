#!/bin/env bash

xlayoutdisplay &
lxsession &
# /home/tkilian/Projects/picom-dccsillag/build/src/picom &
picom &
deadd-notification-center &
nitrogen --restore &
nm-applet &
reds &
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1 &
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1 &
volctl &
cbatticon &
onboard &
plank &
conky -c ~/.conky/Gotham/Gotham &
gsettings set org.cinnamon.desktop.default-applications.terminal exec tilix &
