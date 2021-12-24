#!/bin/bash

SCRIPT_DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

##
# zsh
##

if type -p zsh > /dev/null; then
    sudo rm -rf ~/.zshrc > /dev/null 2>&1
    sudo rm -rf ~/.p10k.zsh > /dev/null 2>&1

    ln -sf $SCRIPT_DIR/zsh/.zshrc ~/.zshrc
    ln -sf $SCRIPT_DIR/zsh/.p10k.zsh ~/.p10k.zsh

    echo "Configured zsh"
fi

##
# xmonad
##

if type -p xmonad > /dev/null; then
    mkdir -p ~/.xmonad

    sudo rm -rf ~/.xmonad/xmonad.hs > /dev/null 2>&1
    sudo rm -rf ~/.xmonad/xmobar.hs > /dev/null 2>&1
    sudo rm -rf ~/.xmonad/xpm > /dev/null 2>&1

    ln -sf $SCRIPT_DIR/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
    ln -sf $SCRIPT_DIR/xmonad/xmobar.hs ~/.xmonad/xmobar.hs
    ln -sf $SCRIPT_DIR/xmonad/xpm ~/.xmonad/xpm

    echo "Configured xmonad"
fi

##
# qtile
##

if type -p qtile > /dev/null; then
    mkdir -p ~/.config/qtile/

    sudo rm -rf ~/.config/qtile/config.py > /dev/null 2>&1
    sudo rm -rf ~/.config/qtile/autostart.sh > /dev/null 2>&1
    sudo rm -rf ~/.config/qtile/custom > /dev/null 2>&1

    ln -sf $SCRIPT_DIR/qtile/config.py ~/.config/qtile/config.py
    ln -sf $SCRIPT_DIR/qtile/autostart.sh ~/.config/qtile/autostart.sh
    ln -sf $SCRIPT_DIR/qtile/custom ~/.config/qtile/custom

    echo "Configured qtile"
fi

##
# picom
##

if type -p picom > /dev/null; then
    mkdir -p ~/.config/picom/

    sudo rm -rf ~/.config/picom/picom.conf > /dev/null 2>&1

    ln -sf $SCRIPT_DIR/picom/picom.conf ~/.config/picom/picom.conf

    echo "Configured picom"
fi

##
# dunst
##

if type -p dunst > /dev/null; then
    mkdir -p ~/.config/dunst/

    sudo rm -rf ~/.config/dunst/dunstrc > /dev/null 2>&1

    ln -sf $SCRIPT_DIR/dunst/dunstrc ~/.config/dunst/dunstrc

    echo "Configured dunst"
fi


##
# oblogout
##

if type -p oblogout > /dev/null; then
    sudo rm -rf /etc/oblogout.conf > /dev/null 2>&1
    sudo rm -rf /usr/share/themes/solarized-squares64 > /dev/null 2>&1

    sudo ln -sf $SCRIPT_DIR/oblogout/oblogout.conf /etc/oblogout.conf
    sudo ln -sf $SCRIPT_DIR/oblogout/solarized-squares64 /usr/share/themes/solarized-squares64

    echo "Configured oblogout"
fi
