export EDITOR=vim
#export PATH=/home/vilmibm/bin:/home/vilmibm/.gem/ruby/2.4.0/bin:/home/vilmibm/.gem/ruby/2.5.0/bin:$PATH
export VENVDIR=$HOME/.virtualenvs

mesg y

alias sgg="cd ~/src/github/github"
alias sge="cd ~/src/github/enterprise-web"

venv () {
  source $VENVDIR/$1/bin/activate
}

mkvenv () {
  python3 -mvenv $VENVDIR/$1
}

colors () {
  for i in {0..255} ; do
    printf "\x1b[38;5;${i}mcolour${i}\n"
  done
}

gack () {
  ack --ignore-dir=vendor --ignore-dir=tmp --ignore-dir=log $@
}

f () {
  find . -name "*$1*" -type f -not -path '*/\.*'
}

ff () {
  IFS="*"
  arg="*$**"
  echo $arg
  find . -name "$arg" -type f -not -path '*/\.*'
}

mirror () {
  wget --mirror --convert-links --adjust-extension --page-requisites --no-parent $1
}

paste () {
  filename="`pwgen | head -n1`.txt"
  cat - > "/home/vilmibm/public_html/paste/$filename"
  echo "https://tilde.town/~vilmibm/paste/$filename"
}

# only execute on non-mac work machine
if [ "$(uname)" != "Darwin" ]; then
  if [ -f ~/.ssh/work ]; then
    if ! pgrep -u "$USER" ssh-agent > /dev/null; then
        ssh-agent > ~/.ssh-agent-env
        ssh-add ~/.ssh/work
        ssh-add ~/.ssh/id_rsa
    fi
    if [[ "$SSH_AGENT_PID" == "" ]]; then
        eval "$(<~/.ssh-agent-env)"
    fi
  fi
fi

