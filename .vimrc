"execute pathogen#infect()
"set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim


syntax on

set ts=4
set autoindent
set expandtab
set wildmode=longest,list
set number
set incsearch
set list listchars=tab:>-,trail:.,extends:>
set t_Co=256
set cursorline
set cursorcolumn
set laststatus=2

filetype plugin on
colorscheme zenburn

nnoremap q <Esc>
noremap <F1> <Esc>
" sane uparrow/downarrow with wrapped lines
nmap j gj
nmap k gk
" C-e to go b#
nmap <C-e> :e#<CR>

let g:posero_default_mappings = 1
iab >>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
iab pos_pa POSERO>> let b:posero_push_all = 1

au BufWritePost *.coffee silent CoffeeMake! | cwindow | redraw!
