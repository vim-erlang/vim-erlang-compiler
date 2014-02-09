" vim-erlang-compiler file
" Language:     Erlang
" Author:       Pawel 'kTT' Salata <rockplayer.pl@gmail.com>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               James Fish <james@fishcakez.com>
" License:      Vim license
" Version:      2014/02/04

if exists('g:autoloaded_erlang_compiler')
    finish
endif

let g:autoloaded_erlang_compiler = 1

sign define ErlangError   text=>> texthl=Error
sign define ErlangWarning text=>> texthl=Todo

function erlang_compiler#EnableShowErrors()
    augroup erlang_compiler
        autocmd!
        autocmd BufWritePost *.erl call erlang_compiler#Run()
        autocmd CursorHold,CursorMoved *.erl,*.hrl call erlang_compiler#ShowErrorMsg()
    augroup END
endfunction

function erlang_compiler#DisableShowErrors()
    sign unplace *
    augroup erlang_compiler
        autocmd!
    augroup END
endfunction

function erlang_compiler#ShowErrorMsg()
    let buffer = bufnr("%")
    let pos = getpos(".")
    if exists("g:erlang_error_list") && has_key(g:erlang_error_list, buffer)
                \ && has_key(g:erlang_error_list[buffer], pos[1])
        let item = get(get(g:erlang_error_list, buffer), pos[1])
        echo item.text
        let b:is_showing_msg = 1
    else
        if exists("b:is_showing_msg") && b:is_showing_msg
            echo
            let b:is_showing_msg = 0
        endif
    endif
endf

function erlang_compiler#ClearErrors()
    sign unplace *
    let g:erlang_error_list   = {}
    let g:erlang_next_sign_id = 1
    if exists("b:is_showing_msg") && b:is_showing_msg
        echo
    end
    let b:is_showing_msg = 0
endfunction

function erlang_compiler#Run()
    let info = erlang_compiler#GetLocalInfo()
    try
        compiler erlang
        setlocal shellpipe=>
        silent make!
        call erlang_compiler#ShowErrors()
    finally
        call erlang_compiler#SetLocalInfo(info)
    endtry
endfunction


function erlang_compiler#ShowErrors()
    call erlang_compiler#ClearErrors()
    for error in getqflist()
        " QF locations in an unnamed file (i.e. generic warnings) get bufnr=0: Ignore these.
        " NB: getqflist can give us a previously not-loaded buffer nr
        " which may not even appear in :bufs/:ls but is available and all
        " marks are pre-loaded if you navigate to it from the qflist
        if error.bufnr == 0
            continue
        endif
        let item          = {}
        let item["bufnr"] = error.bufnr
        let item["lnum"]  = error.lnum
        let item["text"]  = error.text
        if !has_key(g:erlang_error_list, error.bufnr)
            let g:erlang_error_list[error.bufnr] = {}
        endif
        let g:erlang_error_list[error.bufnr][error.lnum] = item
        let type = error.type == "W" ? "ErlangWarning" : "ErlangError"
        execute "sign place" g:erlang_next_sign_id "line=" . item.lnum "name=" . type "buffer=" . item.bufnr
        let g:erlang_next_sign_id += 1
    endfor
endfunction

function erlang_compiler#GetLocalInfo()
    return [get(b:, 'current_compiler', ''), &l:makeprg, &l:efm, &l:shellpipe]
endfunction

function erlang_compiler#SetLocalInfo(info)
    let [name, &l:makeprg, &l:efm, &l:shellpipe] = a:info
    if empty(name)
        unlet! b:current_compiler
    else
        let b:current_compiler = name
    endif
endfunction
