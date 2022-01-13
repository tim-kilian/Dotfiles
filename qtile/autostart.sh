#!/bin/env bash

xrdb -merge ~/.Xresources &

xlayoutdisplay &
lxsession &
# /home/tkilian/Projects/picom-dccsillag/build/src/picom &
picom &
deadd-notification-center &
nitrogen --restore &
nm-applet &
reds &
cbatticon &
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1 &
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1 &
volctl &
onboard &
plank &
# conky -c ~/.conky/Gotham/Gotham &
gsettings set org.cinnamon.desktop.default-applications.terminal exec tilix &
# reds &

firefox &
