" vim-erlang-compiler file
" Language:     Erlang
" Author:       Pawel 'kTT' Salata <rockplayer.pl@gmail.com>
" Contributors: Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               James Fish <james@fishcakez.com>
" License:      Vim license
" Version:      2014/02/04

if exists('g:loaded_erlang_compiler')
    finish
endif

let g:loaded_erlang_compiler = 1

if !exists("g:erlang_show_errors") || g:erlang_show_errors
    call erlang_compiler#EnableShowErrors()
endif

if !exists("g:erlang_make_options")
    let g:erlang_make_options = '--outdir .'
endif

if !exists("g:erlang_flymake_options")
    let g:erlang_flymake_options = ''
endif

if !exists("g:erlang_make_options_rules")
    let g:erlang_make_options_rules = []
endif

if !exists("g:erlang_flymake_options_rules")
    let g:erlang_flymake_options_rules = []
endif

if !exists("g:erlang_quickfix_support")
    let g:erlang_quickfix_support = 0
endif

command ErlangDisableShowErrors call erlang_compiler#DisableShowErrors()
command ErlangEnableShowErrors  call erlang_compiler#EnableShowErrors()
command ErlangToggleShowErrors  call erlang_compiler#ToggleShowErrors()
