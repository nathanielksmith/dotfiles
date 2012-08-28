git clone git@github.com:nathanielksmith/oh-my-zsh.git
ln -s `pwd`/.zshrc `pwd`/../
ln -s `pwd`/oh-my-zsh `pwd`/../.oh-my-zsh
ln -s `pwd`/.vimrc `pwd`/../
ln -s `pwd`/.vim `pwd`/../
ln -s `pwd`/.tmux.conf `pwd`/../

[[ -d `pwd`/../.xmonad ]] && ln -s `pwd`/xmonad.hs `pwd`/../.xmonad/
