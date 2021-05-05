# PROMPT='ianpan@arch:%1~/ %# '
PROMPT='%F{33}i%f%F{39}a%f%F{38}np%f%F{44}an%f%F{50}@%f%F{43}a%f%F{44}rch%f%F{36}:%1~/%f %F{44}%#%f ' # Arch
# PROMPT='%F{202}i%f%F{208}a%f%F{214}npa%f%F{220}n%F{221}@%f%F{220}ub%f%F{214}un%f%F{208}tu:%f%F{202}%1~/%f %F{214}%#%f ' #Ubuntu
alias vi="neovide.exe --wsl"
export EDITOR="neovide.exe --wsl"
alias l="ls -lah"

# For WSL2 X-server VcXsrv
export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
export LIBGL_ALWAYS_INDIRECT=1

xset r rate 200 50 # key repeat rate

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
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' # case insensitive completion

bindkey -e # Emacs-style keybindings in zsh

export PATH=$HOME/.local/bin:$PATH

export FZF_DEFAULT_OPTS='--layout=reverse --color=dark'
export FZF_DEFAULT_COMMAND='fd --type f'
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
