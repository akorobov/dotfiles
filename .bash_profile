host=$(hostname -s)

export GOPATH=~/dev/go

export PATH=${HOME}/bin:$PATH
export PATH=$(echo /opt/*/bin  | tr ' ' ':'):$PATH
export PATH=$(echo ~/dev/tools/*/bin | tr ' ' ':'):$GOPATH/bin:$HOME/.node/bin:$PATH

[[ -r ~/.bash_profile.local ]] && . ~/.bash_profile.local
[[ -r ~/.bash_profile.${host} ]] && . ~/.bash_profile.${host}
