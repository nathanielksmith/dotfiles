" This file assumes installation of:
" - FZF
" - Fugitive
" - vim-orgmode
" - vim-plug
" - the fd command

" Environment overrides

#let $FZF_DEFAULT_COMMAND = 'fd --type f'

" Plugin setup

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plug')

set rtp+=~/src/fzf
Plug '~/src/fzf'
Plug 'junegunn/fzf.vim'

call plug#end()

" Settings

let g:Grep_Default_Options="--exclude-dir=vendor"
syntax on
filetype plugin indent on

set shiftwidth=2
set tabstop=2
set textwidth=100
autocmd BufRead,BufNewFile /home/vilmibm/src/tildemush/* setlocal ts=4 sw=4
autocmd BufRead,BufNewFile /home/vilmibm/src/tildetown/* setlocal ts=4 sw=4
autocmd BufRead,BufNewFile /home/vilmibm/src/github/* setlocal ts=2 sw=2
set gfn=Fantasque\ Sans\ Mono\ 12

set shiftround
set autoindent
set expandtab
set wildmode=longest,list
set number
set incsearch
"set list listchars=tab:>-,trail:.,extends:>
set t_Co=256
set background=dark
colorscheme herald

" set cursorline
" set cursorcolumn

" STATUSLINE

" These are unused since what I was doing was too slow for github/github. If I
" want to go without Fugitive though I can switch back.
function! GitBranch()
  " This is too slow in github/github.
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

set laststatus=2 " always show statusline
set statusline=
set statusline+=%#PmenuSel#
"set statusline+=%{StatuslineGit()}
set statusline+=%{fugitive#statusline()}
set statusline+=%#LineNr#
set statusline+=\ %f
set statusline+=%m
set statusline+=%=
set statusline+=%#CursorColumn#
set statusline+=\ %y
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\[%{&fileformat}\]
set statusline+=\ %p%%
set statusline+=\ %l:%c
set statusline+=\

" BINDINGS
"
let mapleader = " "

nnoremap q <Esc>
noremap <F1> <Esc>
" sane uparrow/downarrow with wrapped lines
nmap j gj
nmap k gk

nmap <Leader>f :GFiles<Return>
nmap <Leader><Tab> :b#<Return>
nmap <Leader>/ :Ag<Return>
nmap <Leader>b :Buffers<Return>
nmap <Leader>s :w<Return>
nmap <Leader>w <C-w>
