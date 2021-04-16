export ZSH="/home/ianpan/.oh-my-zsh"

ZSH_THEME="clean"
DISABLE_AUTO_UPDATE=true

plugins=(git zsh-syntax-highlighting colored-man-pages)

# alias vi="neovide.exe --wsl --multiGrid"
alias vi="neovide.exe --wsl"

export EDITOR=/usr/bin/nvim

# For WSL1:
# export DISPLAY=:0

# For WSL2:
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
export LIBGL_ALWAYS_INDIRECT=1
# export GDK_SCALE=2
# export GDK_DPI_SCALE=0.5

xset r rate 200 40 # need to be set after export DISPLAY

PATH="/home/ianpan/.local/bin:$PATH"
PATH="$PATH:/usr/local/sml/bin" # SML/NJ
PATH="$PATH:/home/ianpan/.deno/bin" # deno
PATH="/usr/share:$PATH"
export PATH

source $ZSH/oh-my-zsh.sh

export FZF_DEFAULT_OPTS='--color=dark'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources # load mouse cursor theme
