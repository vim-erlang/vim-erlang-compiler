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

let s:cpo_save = &cpo
set cpo&vim

let g:autoloaded_erlang_compiler = 1
let s:show_errors = 0

sign define ErlangError   text=>> texthl=Error
sign define ErlangWarning text=>> texthl=Todo

function erlang_compiler#EnableShowErrors()
    augroup erlang_compiler
        autocmd!
        autocmd BufWritePost *.erl,*.escript call erlang_compiler#AutoRun(expand("<abuf>")+0)
        autocmd BufDelete *.erl,*.hrl,*.escript call erlang_compiler#Unload(expand("<abuf>")+0)
        autocmd CursorHold,CursorMoved *.erl,*.hrl,*.escript
                    \ call erlang_compiler#EchoLineError(expand("<abuf>")+0, getpos("."))
    augroup END
    let s:show_errors = 1
endfunction

function erlang_compiler#DisableShowErrors()
    call erlang_compiler#errors#Clear()
    augroup erlang_compiler
        autocmd!
    augroup END
    let s:show_errors = 0
endfunction

function erlang_compiler#ToggleShowErrors()
    if s:show_errors
        call erlang_compiler#DisableShowErrors()
        echo "Showing Erlang errors off."
    else
        call erlang_compiler#EnableShowErrors()
        echo "Showing Erlang errors on."
    endif
endfunction

function erlang_compiler#AutoRun(buffer)
    let info = erlang_compiler#GetLocalInfo()
    try
        compiler erlang
        let &l:makeprg = fnameescape(g:erlang_compiler_check_script) . ' ' .
                       \ erlang_compiler#GetFlymakeOptions()
        if !g:erlang_quickfix_support
            setlocal shellpipe=>
            execute "silent lmake!" shellescape(bufname(a:buffer), 1)
            call erlang_compiler#errors#SetList(a:buffer, getloclist(0))
        else
            setlocal shellpipe=>
            execute "silent make!" shellescape(bufname(a:buffer), 1)
            call erlang_compiler#errors#SetList(a:buffer, getqflist())
        endif
    finally
        call erlang_compiler#SetLocalInfo(info)
    endtry
endfunction

function erlang_compiler#GetFlymakeOptions()
    let abs_filename = expand('%:p')
    let flymake_options = g:erlang_flymake_options
    for rule in g:erlang_flymake_options_rules
        if abs_filename =~# get(rule, 'path_re', '')
            let flymake_options = rule['options']
            break
        endif
    endfor
    return flymake_options
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

let &cpo = s:cpo_save
unlet s:cpo_save
