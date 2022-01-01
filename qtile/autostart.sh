#!/bin/env bash

lxsession &
# /home/tkilian/Projects/picom-dccsillag/build/src/picom &
picom &
nitrogen --restore &
nm-applet &
reds &
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1 &
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1 &
volctl &
cbatticon &
onboard &
gsettings set org.cinnamon.desktop.default-applications.terminal exec tilix &
