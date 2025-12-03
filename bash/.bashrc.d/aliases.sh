alias dnfi='sudo dnf install'
alias dnfu='sudo dnf update'
alias dnfr='sudo dnf remove'
alias updateall='sudo ~/Tools/update-packages.sh'

alias py='python3'

alias cb='cargo build'
alias cr='cargo run'
alias cc='cargo clean'

alias vpns='nordvpn c Seattle'
alias vpnny='nordvpn c New_York'
alias vpn='nordvpn'

alias shredd='shred -uz --iterations=10'

alias wirein='sudo wg-quick up wg0'
alias wireout='sudo wg-quick down wg0'

alias zip='tar -czf'
alias extract='tar -xvf'

alias mnt='udisksctl mount -b'
alias eject='~/Tools/eject-drive.sh'

alias settimeest="timedatectl set-timezone America/New_York"
alias settimepst="timedatectl set-timezone America/Los_Angeles"
alias settimecst="timedatectl set-timezone America/Detroit"
alias settimemst="timedatectl set-timezone America/Denver"
alias settimejst="timedatectl set-timezone Asia/Tokyo"

alias jfon="sudo systemctl start jellyfin; pkill -RTMIN+5 i3blocks"
alias jfoff="sudo systemctl stop jellyfin; pkill -RTMIN+5 i3blocks"

alias setaudio="pactl set-default-sink"
alias getaudio="~/Tools/audio-info.sh"

alias sshsync="rsync -avh --progress --partial --checksum"
