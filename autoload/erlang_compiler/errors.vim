" vim-erlang-compiler file
" Language:     Erlang
" Author:       Pawel 'kTT' Salata <rockplayer.pl@gmail.com>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               James Fish <james@fishcakez.com>
" License:      Vim license
" Version:      2014/02/10

if exists('g:autoloaded_erlang_compiler_errors')
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

let g:autoloaded_erlang_compiler_errors = 1

let g:erlang_errors = {}
let g:erlang_errors_by_srcnr = {}
let g:erlang_errors_by_lnum = {}
let g:erlang_next_error_id = 2000

function erlang_compiler#errors#Clear()
    for error_id in keys(g:erlang_errors)
        call erlang_compiler#errors#DelError(error_id)
    endfor
endfunction

function erlang_compiler#errors#SetList(srcnr, loclist)
    call erlang_compiler#errors#DelList(a:srcnr)
    for loc_error in a:loclist
        let error = copy(loc_error)
        " QF locations in an unnamed file (i.e. generic warnings) get bufnr=0: Ignore these.
        " NB: getqflist can give us a previously not-loaded buffer nr
        " which may not even appear in :bufs/:ls but is available and all
        " marks are pre-loaded if you navigate to it from the qflist
        if error.bufnr == 0
            continue
        endif
        if error.type != 'W'
            let error.type = 'E'
        endif
        let error.srcnr = a:srcnr
        let error_id = erlang_compiler#errors#GetId()
        call erlang_compiler#errors#AddError(error_id, error)
    endfor
endfunction

function erlang_compiler#errors#DelList(srcnr)
    if !has_key(g:erlang_errors_by_srcnr, a:srcnr)
        let g:erlang_errors_by_srcnr[a:srcnr] = {}
    endif
    for error_id in keys(g:erlang_errors_by_srcnr[a:srcnr])
        call erlang_compiler#errors#DelError(error_id)
    endfor
endfunction

function erlang_compiler#errors#AddError(error_id, error)
    let error = copy(a:error)
    let error.id = copy(a:error_id)
    let g:erlang_errors[a:error_id] = error
    call erlang_compiler#errors#AddSign(a:error_id, error)
    call erlang_compiler#errors#AddSrcError(error.srcnr, a:error_id, error)
    call erlang_compiler#errors#AddLineError(error.bufnr, error.lnum, a:error_id, error)
endfunction

function erlang_compiler#errors#DelError(error_id)
    let error = g:erlang_errors[a:error_id]
    call erlang_compiler#errors#DelLineError(error.bufnr, error.lnum, a:error_id)
    call erlang_compiler#errors#DelSrcError(error.srcnr, a:error_id)
    call erlang_compiler#errors#DelSign(a:error_id)
    call remove(g:erlang_errors, a:error_id)
    call erlang_compiler#errors#DelId(a:error_id)
endfunction

function erlang_compiler#errors#AddSrcError(srcnr, error_id, error)
    if !has_key(g:erlang_errors_by_srcnr, a:srcnr)
        let g:erlang_errors_by_srcnr[a:srcnr] = {}
    endif
    let src_dict = g:erlang_errors_by_srcnr[a:srcnr]
    let src_dict[a:error_id] = copy(a:error)
endfunction

function erlang_compiler#errors#DelSrcError(srcnr, error_id)
    call remove(g:erlang_errors_by_srcnr[a:srcnr], a:error_id)
endfunction

function erlang_compiler#errors#GetId()
    let next_id = copy(g:erlang_next_error_id)
    let found = 0
    while !found
        if !has_key(g:erlang_errors, next_id)
            let found = 1
            let g:erlang_next_error_id = next_id + 1
        else
            let next_id += 1
        endif
    endwhile
    return copy(next_id)
endfunction

function erlang_compiler#errors#DelId(error_id)
    if g:erlang_next_error_id > a:error_id
        let g:erlang_next_error_id = copy(a:error_id)
    endif
endfunction

function erlang_compiler#errors#AddSign(error_id, error)
    let type = a:error.type == "W" ? "ErlangWarning" : "ErlangError"
    if a:error.lnum > 0
        execute "sign place" a:error_id "line=" . a:error.lnum "name=" . type "buffer=" . a:error.bufnr
    endif
endfunction

function erlang_compiler#errors#DelSign(error_id)
    execute "sign unplace" a:error_id
endfunction

function erlang_compiler#errors#AddLineError(bufnr, lnum, error_id, error)
    if !has_key(g:erlang_errors_by_lnum, a:bufnr)
        let g:erlang_errors_by_lnum[a:bufnr] = {}
    endif
    let buf_dict = g:erlang_errors_by_lnum[a:bufnr]
    if !has_key(buf_dict, a:lnum)
        let buf_dict[a:lnum] = {'E':{}, 'W': {}}
    endif
    let line_dict = buf_dict[a:lnum]
    let type_dict = line_dict[a:error.type]
    let type_dict[a:error_id] = copy(a:error)
endfunction

function erlang_compiler#errors#DelLineError(bufnr, lnum, error_id)
    let buf_dict = g:erlang_errors_by_lnum[a:bufnr]
    let line_dict = buf_dict[a:lnum]
    if has_key(line_dict.E, a:error_id)
        call remove(line_dict.E, a:error_id)
    else
        call remove(line_dict.W, a:error_id)
    endif
endfunction

function erlang_compiler#errors#DelLineErrors(bufnr)
    if !has_key(g:erlang_errors_by_lnum, a:bufnr)
        let g:erlang_errors_by_lnum[a:bufnr] = {}
    endif
    let buf_dict = g:erlang_errors_by_lnum[a:bufnr]
    for line_dict in values(buf_dict)
        for error_id in keys(line_dict.E)
            call erlang_compiler#errors#DelError(error_id)
        endfor
        for error_id in keys(line_dict.W)
            call erlang_compiler#errors#DelError(error_id)
        endfor
    endfor
    call remove(g:erlang_errors_by_lnum, a:bufnr)
endfunction

function erlang_compiler#errors#GetLineError(bufnr, pos)
    let buf_dict = get(g:erlang_errors_by_lnum, a:bufnr, {})
    let line_dict = get(buf_dict, a:pos, {})
    let line_errors = get(line_dict, 'E', {})
    if !empty(line_errors)
        return copy(get(values(line_errors), 0))
    else
        let line_warnings = get(line_dict, 'W', {})
        if !empty(line_warnings)
            return copy(get(values(line_warnings), 0))
        else
            return {}
        endif
    endif
endfunction

function erlang_compiler#errors#EchoLineError(bufnr, pos)
    let error = erlang_compiler#errors#GetLineError(a:bufnr, a:pos)
    let text = get(error, 'text', '')
    if !empty(text)
        echo text
        let b:erlang_echo_error = 1
    elseif exists('b:erlang_echo_error') && b:erlang_echo_error
        echo ''
        let b:erlang_echo_error = 0
    endif
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
