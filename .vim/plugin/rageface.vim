" File: rageface.vim
" Description: ragefaces
" Maintainer: nathanielksmith
" License: WTF
"===========================
"

" f7u12.rage

" may not want this complete; want it only to complete against g:ragepath.
command! -nargs=+ -complete=file Rageface call s:Rageface(<f-args>)

function! s:Completion(ArgLead, CmdLine, CursorPos)
endfunction

function! s:SetGlobals() abort " stop executing if anything fails.
    " Globals
    " let g:ragepath = "~/.vim/plugin/ragefaces/"
    let g:ragepath = "/home/nksmith/.vim/plugin/ragefaces/"
endfunction

function! s:LoadFile(file_path)
    " TODO how concat?
    " let contents = readfile("~/.vim/plugin/ragefaces/f7u12.rage")
    "let contents = readfile(a:file_path)
    let contents = readfile(g:ragepath . a:file_path)
    let content_len = len(contents)
    let lineno = line('.')
    for line in contents
        call setline(lineno, line)
        let lineno += 1
    endfor
endfunction

function! s:Rageface(whichface)
    call s:SetGlobals()
    " call s:LoadFile("/home/nksmith/.vim/plugin/ragefaces/f7u12.rage")
    call s:LoadFile(a:whichface)
    "if filereadable(a:whichface)
    "if filereadable("~/.vim/plugin/ragefaces/f7u12.rage")
    "    echo "looking up rageface on fs"
    "    call s:LoadFile(a:whichface)
    "elseif (a:whichface == "version")
    "    echo "version?"
    "    call s:Version()
    "else
    "    echo "F7U12"
    "endif
endfunction

" call s:Echo("")
" :call cat filepath<CR> ?


" syn match NameIt 'regex'
" hi def NameIt Type " where Type is Error, Statement...
" b: for buffer option

