export ZSH="/home/ianpan/.oh-my-zsh"

ZSH_THEME="clean"
DISABLE_AUTO_UPDATE=true

plugins=(git zsh-syntax-highlighting colored-man-pages)

alias vi="nvim"

export EDITOR=/usr/bin/nvim

# export DISPLAY=:0 # WSL1
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 # WSL2

export LIBGL_ALWAYS_INDIRECT=1
export GDK_SCALE=1.0
export GDK_DPI_SCALE=1
export NO_AT_BRIDGE=1

PATH="/home/ianpan/.local/bin:$PATH"
PATH="$PATH:/usr/local/sml/bin" # SML/NJ
PATH="$PATH:/home/ianpan/.deno/bin" # deno
PATH="/usr/share:$PATH"
export PATH

source $ZSH/oh-my-zsh.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
