# vim-erlang-compiler

`vim-erlang-compiler` is an Erlang **syntax checking and compiler plugin** for
Vim.

## Installation

With [pathogen.vim](https://github.com/tpope/vim-pathogen):

1.  `cd ~/.vim/bundle` and clone this repository.

2.  Generate help page:

    ```
    :Helptags
    ```

Manually:

1.  Clone this repository.

2.  Add the following line to your `.vimrc` (replace the path with your own):

    ```
    :set runtimepath^=/path/to/vim-erlang-compiler
    ```

3.  Restart Vim (or execute the command above).

4.  Generate help page:

    ```
    :helptags /path/to/vim-erlang-compiler/doc
    ```

## Documentation

* On the web: [user documentation][doc].
* Inside Vim: `:help vim-erlang-compiler`.

## Troubleshooting

### "I get compilation error"

If `vim-erlang-compiler` shows you a compilation error even though there is no
compilation error when using your usual build tool, please follow these steps:

1.  Compile your project with your usual build tool first (e.g., `rebar
    compile`, `rebar3 compile` or `make`), and then try again with
    `vim-erlang-compiler`.

    Reason: `vim-erlang-compiler` needs the `ebin` directories and `app` files
    to be in place, because that is how Erlang itself finds applications when
    compiling the `-include_lib` directives.

2.  Execute the following command from the terminal:

    ```
    $ /path/to/vim-erlang-compiler/compiler/erlang_check.erl \
          --verbose /path/to/myfile.erl
    ```

    Reason:

    -   `vim-erlang-compiler` uses `erlang_check.erl` to compile the Erlang
        source files. The `--verbose` option shows more information, which might
        help you to figure out the problem.

    -   If this produces the same error, we can be sure that the problem is on
        the Erlang side (not the Vim side).

3.  Check the existing open issues on GitHub. Someone may have already reported
    the same problem.

4.  If none of the above helps, open a new issue with the following information:

    -   Which build system do you use? `rebar2`, `rebar3`, Makefile, or nothing?

    -   What is the piece of code that cannot be compiled?

    -   What is the verbose output of the `erlang_check.erl` script? (See
        step 2.)

    -   It helps if you can share the project which fails to compile (or a
        simplified version of it).

## Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    vim-erlang README.

*   If you modify `erlang_check.erl`, please update the tests in in the
    vim-erlang repository.

[doc]: https://github.com/vim-erlang/vim-erlang-compiler/blob/master/doc/vim-erlang-compiler.txt
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
