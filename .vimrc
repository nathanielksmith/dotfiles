syntax on

set ts=4
set autoindent
set expandtab
set wildmode=longest,list
set number
set incsearch
set list listchars=tab:>-,trail:.,extends:>
set t_Co=256

filetype plugin on
colorscheme zenburn

nnoremap q <Esc>
noremap <F1> <Esc>

let g:posero_default_mappings = 1
iab >>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
iab pos_pa POSERO>> let b:posero_push_all = 1

au BufWritePost *.coffee silent CoffeeMake! | cwindow | redraw!
