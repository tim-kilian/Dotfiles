# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  source /etc/profile.d/vte.sh
fi

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=1000000
setopt autocd
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/tkilian/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export ZSH="/home/tkilian/.config/oh-my-zsh"
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
zstyle ':omz:update' mode auto
ENABLE_CORRECTION="true"
plugins=(git npm docker docker-compose kubectl zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh
export LANG=en_US.UTF-8
export XCURSOR_THEME=Breeze
export EDITOR='vim'

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

alias ls='lsd'
alias reds="redshift-gtk -l $(curl -s https://location.services.mozilla.com/v1/geolocate\?key=geoclue | jq '.location.lat, .location.lng' | tr '\n' ':' | sed 's/:$//')"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PATH="/home/tkilian/.stack/snapshots/x86_64-linux-tinfo6/d395f12bd61d86081b31c564bd77fda9d97938095ecb09ef8825fed15b345c3f/8.10.7/bin:/home/tkilian/.stack/compiler-tools/x86_64-linux-tinfo6/ghc-8.10.7/bin:/home/tkilian/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.7/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export _JAVA_AWT_WM_NONREPARENTING=1
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=kvantum
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

TRAPWINCH () {
  BAR=$(printf '=%.0s' {1..$COLUMNS})
}

#[ -f "/home/tkilian/.ghcup/env" ] && source "/home/tkilian/.ghcup/env" # ghcup-env
[ -f "/home/tkilian/.ghcup/env" ] && source "/home/tkilian/.ghcup/env" # ghcup-env
