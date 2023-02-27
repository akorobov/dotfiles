# macos sdk/headers
if [ $(uname -s) = "Darwin" ]; then
    clsdk="/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
    if [ -d "$clsdk" ]; then
        export SDKROOT="$clsdk"
    fi
fi

# need to be moved to .zprofile.local
export SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk
export GOPATH=~/dev/go
export PATH=${HOME}/bin:$PATH
export PATH=$(echo /opt/*/bin  | tr ' ' ':'):$PATH
export PATH=$(echo ~/dev/tools/*/bin | tr ' ' ':'):$GOPATH/bin:$HOME/.node/bin:$PATH


host=$(hostname -s)
[[ -r ~/.zprofile.local ]] && . ~/.zprofile.local
[[ -r ~/.zprofile.${host} ]] && . ~/.zprofile.${host}
