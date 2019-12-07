export ZSH="/Users/ianpan/.oh-my-zsh"

ZSH_THEME="clean"

HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"

plugins=(
    git
    colored-man-pages
)

source $ZSH/oh-my-zsh.sh
export LANG=en_US.UTF-8 # Use English (instead of German etc.)
export EDITOR='vim'
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

PATH="/usr/local/opt/llvm/bin:$PATH"
PATH="$PATH:/Users/the_happy_hacker/opt/anaconda3/bin"
export PATH
