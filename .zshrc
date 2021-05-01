PROMPT='ianpan@arch:%1~/ %# '
alias vi="neovide.exe --wsl"
export EDITOR="neovide.exe --wsl"

# For WSL2 X-server VcXsrv
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
export LIBGL_ALWAYS_INDIRECT=1

xset r rate 200 50 # key repeat rate, set after DISPLAY

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

autoload -U compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' # case insensitive "cd" etc.

bindkey -e # Emacs-style keybindings in zsh

export FZF_DEFAULT_OPTS='--color=dark'
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

cd ~ # sometimes Windows Terminal messes up default dir
