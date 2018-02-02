syntax on

set ts=2
set autoindent
set expandtab
set wildmode=longest,list
set number
set incsearch
set list listchars=tab:>-,trail:.,extends:>
set t_Co=256
" set cursorline
" set cursorcolumn
set laststatus=2

filetype plugin on

nnoremap q <Esc>
noremap <F1> <Esc>
" sane uparrow/downarrow with wrapped lines
nmap j gj
nmap k gk
" C-e to go b#
nmap <C-e> :e#<CR>
