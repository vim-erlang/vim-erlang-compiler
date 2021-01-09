# vim-erlang-compiler

`vim-erlang-compiler` is an Erlang **syntax checking and compiler plugin** for
Vim.

## Installation

<details>
<summary>Vim's built-in package manager</summary>

This is the recommended installation method if you use at least Vim 8 and you
don't use another package manager.

Information about Vim's built-in package manager: [`:help packages`].

Installation steps:

1.  Clone this repository (you can replace `foo` with the directory name of your
    choice):

    ```sh
    $ git clone https://github.com/vim-erlang/vim-erlang-compiler.git \
          ~/.vim/pack/foo/start/vim-erlang-compiler
    ```

2.  Restart Vim.

3.  Generate help page (replace `foo` with the same directory name as above):

    ```
    :helptags ~/.vim/pack/foo/start/vim-erlang-compiler/doc
    ```
</details>

<details>
<summary>Pathogen</summary>

Information about Pathogen: [Pathogen repository].

Installation steps:

1.  Clone this repository:

    ```
    $ git clone https://github.com/vim-erlang/vim-erlang-compiler.git \
          ~/.vim/bundle/vim-erlang-compiler
    ```

2.  Restart Vim.

3.  Generate help page:

    ```
    :Helptags
    ```
</details>

<details>
<summary>Vundle</summary>

Information about Vundle: [Vundle repository].

Installation steps:

1.  Add `vim-erlang-compiler` to your plugin list in `.vimrc` by inserting
    the line that starts with `Plugin`:

    ```
    call vundle#begin()
      [...]
      Plugin 'vim-erlang/vim-erlang-compiler'
      [...]
    call vundle#end()
    ```

2.  Restart Vim.

3.  Run `:PluginInstall`.
</details>

<details>
  <summary>Vim-Plug</summary>

Information about Vim-Plug: [vim-plug repository].

Installation steps:

1.  Add `vim-erlang-compiler` to your plugin list in `.vimrc` by inserting the
    line that starts with `Plug`:

    ```
    call plug#begin()
      [...]
      Plug 'vim-erlang/vim-erlang-compiler'
      [...]
    call plug#end()
    ```

2.  Restart Vim.

3.  Run `:PlugInstall`.
</details>

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

[`:help packages`]: https://vimhelp.org/repeat.txt.html#packages
[doc]: https://github.com/vim-erlang/vim-erlang-compiler/blob/master/doc/vim-erlang-compiler.txt
[Pathogen repository]: https://github.com/tpope/vim-pathogen
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
[vim-plug repository]: https://github.com/junegunn/vim-plug
[Vundle repository]: https://github.com/VundleVim/Vundle.vim
