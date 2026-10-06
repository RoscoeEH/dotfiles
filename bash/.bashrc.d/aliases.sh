alias dnfi='sudo dnf install -y'
alias dnfu='sudo dnf update'
alias dnfr='sudo dnf remove -y'
alias updateall='sudo ~/Tools/update-packages.sh'

alias py='python3'

alias cb='cargo build'
alias cr='cargo run'
alias cc='cargo clean'

alias vpn='nmcli connection'

alias shredd='shred -uz --iterations=10'

alias tarzip='tar -czf'
alias untar='tar -xvf'

alias mnt='~/Tools/open-and-mnt.sh'
alias eject='~/Tools/eject-drive.sh'

alias settimeest="timedatectl set-timezone America/New_York"
alias settimepst="timedatectl set-timezone America/Los_Angeles"
alias settimecst="timedatectl set-timezone America/Detroit"
alias settimemst="timedatectl set-timezone America/Denver"
alias settimejst="timedatectl set-timezone Asia/Tokyo"

alias jfon="sudo systemctl start jellyfin.service; pkill -RTMIN+5 waybar"
alias jfoff="sudo systemctl stop jellyfin.service; pkill -RTMIN+5 waybar"

alias setaudio="pactl set-default-sink"
alias getaudio="~/Tools/audio-info.sh"

alias sshsync="rsync -avh --progress --partial --checksum"

alias r="ranger"

alias tst="~/Tools/toggle-tailscale.sh"
alias ts="tailscale"

alias gethex='openssl rand -hex'
alias compare='~/Tools/hash-compare.sh'

alias o='~/Tools/mime-open.sh'
