" Vim compiler file
" Language:     Erlang
" Author:       Pawel 'kTT' Salata <rockplayer.pl@gmail.com>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
" Version:      2011/09/10

if exists("current_compiler")
    finish
else
    let current_compiler = "erlang"
endif

if exists(":CompilerSet") != 2
    command -nargs=* CompilerSet setlocal <args>
endif

if !exists("g:erlang_highlight_errors")
    let g:erlang_highlight_errors = 1
endif

let s:erlang_check_file = expand("<sfile>:p:h") . "/erlang_check.erl"
let b:error_list        = {}
let b:is_showing_msg    = 0

function! s:HighlightErlangErrors()
    if match(getline(1), "#!.*escript") != -1
        setlocal makeprg=escript\ -s\ %
    else
        execute "setlocal makeprg=" . s:erlang_check_file . "\\ \%"
    endif
    silent make!
    call s:ClearMatches()
    for error in getqflist()
        let item = {}
        let item["lnum"] = error.lnum
        let item["msg"] = error.text
        let b:error_list[error.lnum] = item
        call matchadd("SpellBad", "\\%" . error.lnum . "l")
    endfor
    if len(getqflist())
        redraw!
    endif
    call s:ShowMsg()
    setlocal makeprg=make
endfunction

function! s:ShowMsg()
    let pos = getpos(".")
    if has_key(b:error_list, pos[1])
        let item = get(b:error_list, pos[1])
        echo item.msg
        let b:is_showing_msg = 1
    else
        if exists("b:is_showing_msg") && b:is_showing_msg == 1
            echo
            let b:is_showing_msg = 0
        endif
    endif
endf

function! s:ClearMatches()
    call clearmatches()
    let b:error_list = {}
    if exists("b:is_showing_msg") && b:is_showing_msg == 1
        echo
        let b:is_showing_msg = 0
    endif
endfunction

CompilerSet makeprg=make
CompilerSet errorformat=%f:%l:\ %tarning:\ %m,%E%f:%l:\ %m

if g:erlang_highlight_errors
    autocmd BufLeave *.erl     call s:ClearMatches()
    autocmd BufEnter *.erl     call s:ClearMatches()
    autocmd BufWritePost *.erl call s:HighlightErlangErrors()
    autocmd CursorHold *.erl   call s:ShowMsg()
    autocmd CursorMoved *.erl  call s:ShowMsg()
endif
