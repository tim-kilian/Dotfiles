#!/bin/env bash

lxsession &
/home/tkilian/Projects/picom-dccsillag/build/src/picom &
nitrogen --restore &
nm-applet &
reds &
xinput set-prop 13 313 1 &
xinput set-prop 13 305 1 &
umonitor --listen --daemonize &
volctl &
cbatticon &
onboard &
gsettings set org.cinnamon.desktop.default-applications.terminal exec tilix &
