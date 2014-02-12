" vim-erlang-compiler file
" Language:     Erlang
" Author:       Pawel 'kTT' Salata <rockplayer.pl@gmail.com>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               James Fish <james@fishcakez.com>
" License:      Vim license
" Version:      2014/02/10

if exists('g:autoloaded_erlang_compiler')
    finish
endif

let g:autoloaded_erlang_compiler = 1

sign define ErlangError   text=>> texthl=Error
sign define ErlangWarning text=>> texthl=Todo

function erlang_compiler#EnableShowErrors()
    augroup erlang_compiler
        autocmd!
        autocmd BufWritePost *.erl call erlang_compiler#AutoRun(expand("<abuf>")+0)
        autocmd BufDelete *.erl,*.hrl call erlang_compiler#Unload(expand("<abuf>")+0)
        autocmd CursorHold,CursorMoved *.erl,*.hrl
                    \ call erlang_compiler#EchoLineError(expand("<abuf>")+0, getpos("."))
    augroup END
endfunction

function erlang_compiler#DisableShowErrors()
    call erlang_compiler#errors#Clear()
    augroup erlang_compiler
        autocmd!
    augroup END
endfunction

function erlang_compiler#AutoRun(buffer)
    let info = erlang_compiler#GetLocalInfo()
    try
        compiler erlang
        setlocal shellpipe=>
        execute "silent lmake!" shellescape(bufname(a:buffer), 1)
        call erlang_compiler#errors#SetList(a:buffer, getloclist(0))
    finally
        call erlang_compiler#SetLocalInfo(info)
    endtry
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

function erlang_compiler#Unload(bufnr)
    call erlang_compiler#errors#DelList(a:bufnr)
    call erlang_compiler#errors#DelLineErrors(a:bufnr)
endfunction

function erlang_compiler#EchoLineError(bufnr, pos)
    call erlang_compiler#errors#EchoLineError(a:bufnr, a:pos[1])
endfunction
