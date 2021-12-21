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
# picom 
##

if type -p picom > /dev/null; then
    mkdir -p ~/.config/picom/

    sudo rm -rf ~/.config/picom/picom.conf > /dev/null 2>&1

    ln -sf $SCRIPT_DIR/picom/picom.conf ~/.config/picom/picom.conf

    echo "Configured picom"
fi
