export ZSH="/Users/the_happy_hacker/.oh-my-zsh"

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
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # (brew install)

PATH="/usr/local/opt/llvm/bin:$PATH" # (brew install llvm)
PATH="/usr/local/sbin:$PATH"
# PATH="$PATH:/Users/the_happy_hacker/opt/anaconda3/bin"
export PATH

# fzf and fd (brew install)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type file --hidden --no-ignore'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
