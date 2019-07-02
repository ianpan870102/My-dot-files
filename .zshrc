export ZSH="/Users/ianyepan/.oh-my-zsh"
ZSH_THEME="clean"

HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"

plugins=(
  git
  colored-man-pages
)

source $ZSH/oh-my-zsh.sh
export LANG=en_US.UTF-8
export EDITOR='vim'
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
