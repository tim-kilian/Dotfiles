#!/bin/bash

SCRIPT_DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

##
# xprofile
##

rm ~/.xprofile
ln -sf $SCRIPT_DIR/xprofile/.xprofile ~/.xprofile
echo "Configured xprofile"

##
# icons
##

#cp -r $SCRIPT_DIR/icons/* ~/.icons/
#echo "Configured icons"

##
# zsh
##

if type -p zsh >/dev/null; then
    sudo rm -rf ~/.zshrc >/dev/null 2>&1
    sudo rm -rf ~/.p10k.zsh >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/zsh/.zshrc ~/.zshrc
    ln -sf $SCRIPT_DIR/zsh/.p10k.zsh ~/.p10k.zsh

    echo "Configured zsh"
fi

##
# alacritty
##

if type -p alacritty >/dev/null; then
    sudo rm -rf ~/.config/alacritty/alacritty.yml >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

    echo "Configured alacritty"
fi

##
# xmonad
##

if type -p xmonad >/dev/null; then
    mkdir -p ~/.xmonad

    sudo rm -rf ~/.xmonad/xmonad.hs >/dev/null 2>&1
    sudo rm -rf ~/.xmonad/xmobar.hs >/dev/null 2>&1
    sudo rm -rf ~/.xmonad/xpm >/dev/null 2>&1
    sudo rm -rf ~/.xmonad/xmobar >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
    ln -sf $SCRIPT_DIR/xmonad/xmobar.hs ~/.xmonad/xmobar.hs
    ln -sf $SCRIPT_DIR/xmonad/xpm ~/.xmonad/xpm
    ln -sf $SCRIPT_DIR/xmonad/xmobar ~/.xmonad/xmobar

    echo "Configured xmonad"
fi

##
# qtile
##

if type -p qtile >/dev/null; then
    mkdir -p ~/.config/qtile/

    sudo rm -rf ~/.config/qtile/config.py >/dev/null 2>&1
    sudo rm -rf ~/.config/qtile/default_config.py >/dev/null 2>&1
    sudo rm -rf ~/.config/qtile/autostart.sh >/dev/null 2>&1
    sudo rm -rf ~/.config/qtile/custom >/dev/null 2>&1
    sudo rm -rf ~/.config/qtile/scripts >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/qtile/config.py ~/.config/qtile/config.py
    ln -sf $SCRIPT_DIR/qtile/default_config.py ~/.config/qtile/default_config.py
    ln -sf $SCRIPT_DIR/qtile/autostart.sh ~/.config/qtile/autostart.sh
    ln -sf $SCRIPT_DIR/qtile/custom ~/.config/qtile/custom
    ln -sf $SCRIPT_DIR/qtile/scripts ~/.config/qtile/scripts

    echo "Configured qtile"
fi

##
# picom
##

if type -p picom >/dev/null; then
    mkdir -p ~/.config/picom/

    sudo rm -rf ~/.config/picom/picom.conf >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/picom/picom.conf ~/.config/picom/picom.conf

    echo "Configured picom"
fi

##
# dunst
##

if type -p dunst >/dev/null; then
    mkdir -p ~/.config/dunst/

    sudo rm -rf ~/.config/dunst/dunstrc >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/dunst/dunstrc ~/.config/dunst/dunstrc

    echo "Configured dunst"
fi

##
# deadd
##

if type -p deadd-notification-center >/dev/null; then
    mkdir -p ~/.config/deadd/

    sudo rm -rf ~/.config/deadd/deadd.conf >/dev/null 2>&1
    sudo rm -rf ~/.config/deadd/deadd.css >/dev/null 2>&1

    ln -sf $SCRIPT_DIR/deadd/deadd.conf ~/.config/deadd/deadd.conf
    ln -sf $SCRIPT_DIR/deadd/deadd.css ~/.config/deadd/deadd.css

    echo "Configured deadd"
fi

##
# oblogout
##

if type -p oblogout >/dev/null; then
    sudo rm -rf /etc/oblogout.conf >/dev/null 2>&1
    sudo rm -rf /usr/share/themes/solarized-squares64 >/dev/null 2>&1

    sudo ln -sf $SCRIPT_DIR/oblogout/oblogout.conf /etc/oblogout.conf
    sudo ln -sf $SCRIPT_DIR/oblogout/solarized-squares64 /usr/share/themes/solarized-squares64

    echo "Configured oblogout"
fi
