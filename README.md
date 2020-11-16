# vim-erlang-compiler

`vim-erlang-compiler` is an Erlang **syntax checking and compiler plugin** for
Vim.

# Installation

With [pathogen.vim](https://github.com/tpope/vim-pathogen):

- `cd ~/.vim/bundle` and clone this repository.
- Generate help page:

        :Helptags

Manually:

- Clone this repository.
- Add the following line to your `.vimrc` (replace the path with your own):

        :set runtimepath^=/path/to/vim-erlang-compiler

- Restart Vim (or execute the command above).
- Generate help page:

        :helptags /path/to/vim-erlang-compiler/doc

# Documentation

- On the web: [user documentation][doc].
- Inside Vim: `:help vim-erlang-compiler`.

# Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    vim-erlang README.

*   If you modify `erlang_check.erl`, please update the tests in in the
    vim-erlang repository.

[doc]: https://github.com/vim-erlang/vim-erlang-compiler/blob/master/doc/vim-erlang-compiler.txt
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
