# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

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
export EDITOR='vim'
export VISUAL='vim'

export TERMINAL='urxvt'

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
# ssh-add -l | grep -q "btq_github_ssh" || ssh-add ~/.ssh/btq_github_ssh
export PATH=$HOME/riscv/bin:$PATH
