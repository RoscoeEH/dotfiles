

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

eval "$(direnv hook bash)"   # or `zsh` if using zsh

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
        if [ -f "$rc" ]; then
            . "$rc"
        fi
    done
fi
unset rc

# Variables
export EDITOR='vi'
export VISUAL='vi'

export TERMINAL='alacritty'

. "$HOME/.cargo/env"


PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '

if echo $XDG_SESSION_TYPE | grep -q "tty"
then
    sway
fi

fastfetch
export OLLAMA_HOST=100.124.54.119:11434

# opencode
export PATH=/home/roscoe/.opencode/bin:$PATH
