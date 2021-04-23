PROMPT='ianpan@ubuntu:%1~/ %# '
alias vi="neovide.exe --wsl"
export EDITOR="neovide.exe --wsl"

# For WSL1:
# export DISPLAY=:0

# For WSL2:
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
export LIBGL_ALWAYS_INDIRECT=1
# export GDK_SCALE=2
# export GDK_DPI_SCALE=0.5

xset r rate 200 50 # need to be set after export DISPLAY

HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

PATH="/home/ianpan/.local/bin:$PATH"
PATH="/usr/share:$PATH"
export PATH

export FZF_DEFAULT_OPTS='--color=dark'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources # load mouse cursor theme
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
