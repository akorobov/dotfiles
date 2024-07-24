export HISTSIZE=2000
export HISTFILE="${HOME}/.zsh_history"

setopt inc_append_history

# aliases
export EDITOR=vi
alias gl='git log --abbrev-commit'
alias glt='git log --graph --full-history --pretty=format:"%h%x09%d%x20%s"'
alias glta='git log --graph --full-history --pretty=format:"%h%x09%cr%x09%<(20,trunc)%an%x09%s"'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gst='git stash'
alias gpr="git pull -r"
alias gco="git checkout"
alias git-describe-branches='for line in $(git branch); do
     description=$(git config branch.$line.description)
     if [ -n "$description" ]; then
       echo "$line     $description"
     fi
done'

export KUBECONFIG=$(find ~/.kube/ -name \*yaml | xargs | tr ' ' ':')

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

function dux () { du -smx "$@" | sort -rn -k 1,1;  }

function set-title () { echo -ne "\e]1;$*\a" }
if [[ -f ~/.pythonrc ]]; then
	export PYTHONSTARTUP=~/.pythonrc
fi

#zstyle ':completion:*' completer _complete
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
#autoload -Uz compinit
#compinit

export WORDCHARS='*?.-[]~=&;!#$%^(){}<>'

set -o emacs

export PROMPT='%1~%f ∀ '

host=$(hostname -s)
[[ -r ~/.zshrc.local ]] && . ~/.zshrc.local
[[ -r ~/.zshrc.${host} ]] && . ~/.zshrc.${host}

