# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/ianpan/.oh-my-zsh"

ZSH_THEME="clean"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

plugins=(
    git
    colored-man-pages
)

source $ZSH/oh-my-zsh.sh

# Use English (instead of German etc.)
export LANG=en_US.UTF-8

alias tu='top -u'
alias gindent="gindent -gnu -bad -bbo --line-length79"
alias ec='emacsclient -create-frame --alternate-editor="" --no-wait --quiet'
alias enw='emacs -nw -q -l ~/emacs-simple-init.el' # for quick edits
export EDITOR=/usr/local/bin/vim

# Zsh command syntax highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export PATH="$PATH:/Library/TeX/texbin"
