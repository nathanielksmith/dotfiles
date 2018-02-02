export EDITOR=vim
export PATH=/home/vilmibm/bin:/home/vilmibm/.gem/ruby/2.4.0/bin:$PATH
export VENVDIR=/home/vilmibm/.virtualenvs

venv () {
  source $VENVDIR/$1/bin/activate
}

mkvenv () {
  python3 -mvenv $VENVDIR/$1
}

sln () {
  ln -s `pwd`/$1 $2
}

alias be="bundle exec"

colors () {
  for i in {0..255} ; do
    printf "\x1b[38;5;${i}mcolour${i}\n"
  done
}

blublock () {
  redshift -O 3700
}

unblockblu () {
  redshift -O 6500
}

tea () {
  sleep $1m && echo "your tea steeped for $1 minutes" | write $(whoami) 2&>/dev/null
}

pygrep () {
  find . -name "*.py" | xargs grep $1
}
