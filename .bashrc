export HISTSIZE=2000

export PATH=~/bin:$PATH

# append history
shopt -s histappend

#if [ "prompt" == $?prompt ]; then
#  exit
#fi

export EDITOR=vi
alias gl='git log --abbrev-commit'
alias glt='git log --graph --full-history --pretty=format:"%h%x09%d%x20%s"'
alias glta='git log --graph --full-history --pretty=format:"%h%x09%cr%x09%<(20,trunc)%an%x09%s"'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gst='git stash'
alias gpr="git pull -r"

[ -f "$HOME/.lesspipe.sh" ] && {
    LESSOPEN="|$HOME/.lesspipe.sh %s"
    LESSKEY="$HOME/.lesskey"
}

ct () {
    perl -e "print scalar localtime($1),\"\n\""
}

gmt () {
    perl -e "print scalar gmtime($1),\"\n\""
}

yday () {
    perl -e "\$a=(localtime($1))[7];print\"\$a\n\""
}

function make-cp () {
    cp=$(for i in $@; do
             [ -d $i ] &&
                 { jars=$(echo $i/*jar); [ "$i/*jar" != "$jars" ] && echo $jars || echo $i;} || echo $i;
         done);
    echo $cp | tr ' ' ':';
}

if [[ -f ~/.pythonrc ]]; then
	export PYTHONSTARTUP=~/.pythonrc
fi

[[ -r ~/.bashrc.local ]] && . ~/.bashrc.local
