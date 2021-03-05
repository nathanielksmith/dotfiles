" This file assumes installation of:
" - FZF -- specifically, a clone of junegunn/fzf at ~/src/fzf as well as the fzf binary
" - ag -- silversearch-ag package on debian

" Environment overrides

let $FZF_DEFAULT_COMMAND = 'ag -l --ignore=vendor --ignore=__pycache__ -g ""'

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
Plug 'flazz/vim-colorschemes'
Plug 'vimwiki/vimwiki'
Plug 'tpope/vim-fugitive'
"Plug 'kchmck/vim-coffee-script'
"Plug 'leafgarland/typescript-vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'arcticicestudio/nord-vim'
"Plug '~/src/vim-hy'

call plug#end()

" Settings

syntax on
filetype plugin indent on

set shiftwidth=2
set tabstop=2
set textwidth=100
set relativenumber
set backspace=indent,eol,start
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
"set background=dark
"colorscheme 256_noir
"set background=light
"colorscheme ancient
"colorscheme Atelier_CaveDark
"colorscheme sourcerer
colorscheme nord

hi MatchParen cterm=bold ctermbg=none ctermfg=magenta

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

command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, '--path-to-ignore ~/.ignore --hidden', <bang>0)

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
" preferred uparrow/downarrow with wrapped lines
nmap j gj
nmap k gk

nmap <Leader>f :Files<Return>
nmap <Leader><Tab> :b#<Return>
nmap <Leader>/ :Ag<Return>
nmap <Leader>b :Buffers<Return>
nmap <Leader>s :w<Return>
nmap <Leader>w <C-w>
nmap <Leader>wm <C-w>o
nmap <Leader>. :source%<Return>
nmap <Leader>ed :e~/.vimrc<Return>

" Go stuff
nmap <Leader>gr :!go run %<Return>
nmap <Leader>gt :GoTest<Return>
nmap <Leader>gd :GoDef<Return>
nmap <Leader>gi :GoImports<Return>
nmap <Leader>gb :wa<Return>:GoBuild<Return>
nmap <Leader>gR :wa<Return>:GoBuild<Return>:GoRename<Return>

iab ife if err != nil {<CR>return err<CR>}
iab ifne if err != nil {<CR>return nil, err<CR>}
iab dbg fmt.Printf("DBG %#v\n",
iab ae assert.Equal

nnoremap <leader>d "_d
xnoremap <leader>d "_d
xnoremap <leader>p "_dP

nmap <C-g> :GoDecls<cr>
imap <C-g> <esc>:<C-u>GoDecls<cr>
