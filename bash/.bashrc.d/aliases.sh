alias apti='sudo apt install -y'
alias aptu='sudo apt update'
alias aptr='sudo apt remove -y'
alias updateall='sudo ~/Tools/update-packages.sh'

alias py='python3'

alias cb='cargo build'
alias cr='cargo run'
alias cc='cargo clean'

alias shredd='shred -uz --iterations=10'

alias tarzip='tar -czf'
alias untar='tar -xvf'

alias mnt='udisksctl mount -b'
alias eject='~/Tools/eject-drive.sh'

alias settimeest="timedatectl set-timezone America/New_York"
alias settimepst="timedatectl set-timezone America/Los_Angeles"
alias settimecst="timedatectl set-timezone America/Detroit"
alias settimemst="timedatectl set-timezone America/Denver"
alias settimejst="timedatectl set-timezone Asia/Tokyo"

alias setaudio="pactl set-default-sink"
alias getaudio="~/Tools/audio-info.sh"

alias r="ranger"

alias sshclone="~/Tools/ssh-clone.sh"
