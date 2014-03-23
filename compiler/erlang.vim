" vim-erlang-compiler file
" Language:     Erlang
" Author:       Pawel 'kTT' Salata <rockplayer.pl@gmail.com>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               James Fish <james@fishcakez.com>
" License:      Vim license
" Version:      2014/02/04

if exists("current_compiler") || v:version < 703
    finish
else
    let current_compiler = "erlang"
endif

let s:cpo_save = &cpo
set cpo&vim

if exists(":CompilerSet") != 2
    command -nargs=* CompilerSet setlocal <args>
endif

let g:erlang_compiler_check_script = expand("<sfile>:p:h") . "/erlang_check.erl"

" Find the appropriate make options
let s:make_options = g:erlang_make_options
for s:rule in g:erlang_make_options_rules
    if expand('%:p') =~# get(s:rule, 'path_re', '')
        let s:make_options = s:rule['options']
        break
    endif
endfor

execute "CompilerSet makeprg=" .
      \ escape(fnameescape(g:erlang_compiler_check_script) . ' ' .
      \        s:make_options . ' ', ' \') . '%'

CompilerSet errorformat=%f:%l:\ %tarning:\ %m,%f:%l:\ %m,%f:\ %m

let &cpo = s:cpo_save
unlet s:cpo_save
