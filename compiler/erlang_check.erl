#!/usr/bin/env escript

%%------------------------------------------------------------------------------
%% @doc Iterate over the given files, print their compilation warnings and
%% errors, and exit with an appropriate exit code.
%% @end
%%------------------------------------------------------------------------------
-spec main([string()]) -> no_return().
main([]) ->
    io:format("Usage: see --help.~n"),
    halt(2);
main(Args) ->
    Files = parse_args(Args),

    case get(outdir) of
        undefined ->
            % xref, load and copy is supported only if outdir is also specified
            disable(xref),
            disable(load),
            disable(copy);
        _ ->
            ok
    end,

    case [File || File <- Files, check_file(File) /= ok ] of
        % No Errors (but there could be Warnings!)
        [] ->
            halt(0);
        % At least one Error
        _Errors ->
            halt(1)
    end.

%%------------------------------------------------------------------------------
%% @doc Parse the argument list.
%%
%% Put the options into the process dictionary and return the list of files.
%% @end
%%------------------------------------------------------------------------------
-spec parse_args(string()) -> [FileName :: string()].
parse_args(Args) ->
    lists:reverse(parse_args(Args, [])).

-spec parse_args(string(), [FileName :: string()]) -> [FileName :: string()].
parse_args([], Acc) ->
    Acc;
parse_args([Help|_], _Acc) when Help == "-h";
                                Help == "--help" ->
    print_help(),
    halt(0);
parse_args([Verbose|OtherArgs], Acc) when Verbose == "-v";
                                          Verbose == "--verbose" ->
    put(verbose, true),
    log("Verbose mode on.~n"),
    parse_args(OtherArgs, Acc);
parse_args(["--outdir", OutDir|OtherArgs], Acc) ->
    put(outdir, OutDir),
    parse_args(OtherArgs, Acc);
parse_args(["--outdir"], _Acc) ->
    log_error("Argument needed after '--outdir'.~n"),
    halt(2);
parse_args(["--nooutdir"|OtherArgs], Acc) ->
    erase(outdir),
    parse_args(OtherArgs, Acc);
parse_args(["--xref"|OtherArgs], Acc) ->
    put(xref, true),
    parse_args(OtherArgs, Acc);
parse_args(["--load", LongOrShortNames|_OtherArgs], _Acc)
  when LongOrShortNames =/= "shortnames",
       LongOrShortNames =/= "longnames" ->
    log_error("First argument after '--load' should be shortnames or "
              "longnames.~n"),
    halt(2);
parse_args(["--load", LongOrShortNames, MyNodeName, TargetNodeName|OtherArgs],
           Acc) ->
    put(load, {list_to_atom(LongOrShortNames),
               list_to_atom(MyNodeName),
               list_to_atom(TargetNodeName)}),
    parse_args(OtherArgs, Acc);
parse_args(["--load"|_], _Acc) ->
    log_error("More arguments needed after '--load'.~n"),
    halt(2);
parse_args(["--cookie", Cookie|OtherArgs], Acc) ->
    put(cookie, list_to_atom(Cookie)),
    parse_args(OtherArgs, Acc);
parse_args(["--cookie"], _Acc) ->
    log_error("Argument needed after '--cookie'.~n"),
    halt(2);
parse_args(["--copy", TargetDir|OtherArgs], Acc) ->
    put(copy, TargetDir),
    parse_args(OtherArgs, Acc);
parse_args(["--copy"], _Acc) ->
    log_error("Argument needed after '--copy'.~n"),
    halt(2);
parse_args(["--"|Files], Acc) ->
    Files ++ Acc;
parse_args([[$-|_] = Arg|_], _Acc) ->
    log_error("Unknown option: ~s~n", [Arg]),
    halt(2);
parse_args([File|OtherArgs], Acc) ->
    parse_args(OtherArgs, [File|Acc]).

%%------------------------------------------------------------------------------
%% @doc Print the script's help text and exit.
%% @end
%%------------------------------------------------------------------------------
-spec print_help() -> ok.
print_help() ->
    Text =
"Usage: erlang_check.erl [options] [--] <files>

Description:
  erlang_check.erl performs syntax check on the given files, and optionally
  (with the --outdir option) compiles them.

Options:
  --            Process all remaining parameters as filenames.
  -h, --help    Print help.
  -v, --verbose Verbose output.
  --outdir DIR  Put the compiled beam file into the given directory. It is
                relative to directory containing the file to compile.
  --nooutdir    Don't create beam file (default).
  --xref        Execute xref on the beam file and print undefined functions.
                (Other xref warnings are not printed, because those should be
                also printed by the compiler.) Works only if --outdir is
                specified.
  --load NODE_NAME_TYPE MY_NODE_NAME TARGET_NODE_NAME
                After successful compilation, start a node with MY_NODE_NAME and
                load the module into the target node. NODE_NAME_TYPE must be
                either 'shortnames' or 'longnames'. Works only if --outdir is
                specified.
  --cookie COOKIE
                When --load is used, this option can be used to set the cookie
                to be used towards the TARGET_NODE_NAME.
  --copy DIR    After successful compilation, all beam files with the same
                number (recursively) under DIR will be overwritten with the
                newly generated beam file. Works only with Erlang R16 and above.
",
    io:format(Text).

%%------------------------------------------------------------------------------
%% @doc Log the given entry if we are in verbose mode.
%% @end
%%------------------------------------------------------------------------------
-spec log(io:format()) -> ok.
log(Format) ->
    log(Format, []).

-spec log(io:format(), [term()]) -> ok.
log(Format, Data) ->
    case get(verbose) of
        true ->
            io:format(Format, Data);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Log the given error.
%% @end
%%------------------------------------------------------------------------------
-spec log_error(io:format()) -> ok.
log_error(Format) ->
    io:format(standard_error, Format, []).

-spec log_error(io:format(), [term()]) -> ok.
log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

%%------------------------------------------------------------------------------
%% @doc Disable the given feature and print a warning if it was turned on.
%% @end
%%------------------------------------------------------------------------------
-spec disable(atom()) -> ok.
disable(Arg) ->
    case get(Arg) of
        undefined ->
            ok;
        _ ->
            erase(Arg),
            log_error("Warning: ~p disabled (it requires --outdir).~n", [Arg])
    end.

%%------------------------------------------------------------------------------
%% @doc Try to compile the given file, print the warnings and errors, and return
%% whether there were errors.
%% @end
%%------------------------------------------------------------------------------
-spec check_file(string()) -> ok | error.
check_file(File) ->
    case file_type(File) of
        module ->
            check_module(File);
        escript ->
            check_escript(File);
        {error, Reason} ->
            file_error(File, Reason)
    end.

%%------------------------------------------------------------------------------
%% @doc Return the type of the Erlang source file.
%% @end
%%------------------------------------------------------------------------------
-spec file_type(string()) -> module | escript | {error, term()}.
file_type(File) ->
    case file:open(File, [raw, read]) of
        {ok, Fd} ->
            Result = read_file_type(Fd),
            ok = file:close(Fd),
            Result;
        {error, _Reason}  = Error ->
            Error
    end.

-spec read_file_type(file:io_device()) -> module | escript | {error, term()}.
read_file_type(Fd) ->
    case file:read(Fd, 256) of
        {ok, Beginning} ->
            case re:run(Beginning, "^#!.*escript", [{capture, none}]) of
                nomatch ->
                    module;
                match ->
                    escript
            end;
        {error, _Reason} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Try to compile the given module, print the warnings and errors, and
%% return whether there were errors.
%% @end
%%------------------------------------------------------------------------------
-spec check_module(string()) -> ok | error.
check_module(File) ->
    Dir = filename:dirname(File),
    AbsFile = filename:absname(File),
    AbsDir = filename:absname(Dir),

    Defs = [warn_export_all,
            warn_export_vars,
            warn_shadow_vars,
            warn_obsolete_guard,
            warn_unused_import,
            report,
            % By adding debug_info, we ensure that the output of xref:m will
            % contain the caller MFAs too.
            debug_info,
            {i, AbsDir ++ "/include"},
            {i, AbsDir ++ "/../include"},
            {i, AbsDir ++ "/../../include"},
            {i, AbsDir ++ "/../../../include"}],

    case process_rebar_configs(AbsDir) of
        {ok, RebarOpts} ->
            code:add_patha(absname(AbsDir, "ebin")),
            {CompileOpts0, OutDir} =
                case get(outdir) of
                    undefined ->
                        % strong_validation = we only want validation, don't
                        % generate beam file
                        {[strong_validation], undefined};
                    OutDir0 ->
                        AbsOutDir = filename:join(AbsDir, OutDir0),
                        {[{outdir, AbsOutDir}], AbsOutDir}
                end,
            CompileOpts = CompileOpts0 ++ Defs ++ RebarOpts,
            log("Compiling: compile:file(~p,~n    ~p)~n",
                [AbsFile, CompileOpts]),
            case compile:file(AbsFile, CompileOpts) of
                {ok, ModName} ->
                    post_compilation(OutDir, ModName);
                error ->
                    error
            end;
        error ->
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Perform tasks after successful compilation (xref, etc.)
%% @end
%%------------------------------------------------------------------------------
-spec post_compilation(string() | undefined, atom()) -> ok.
post_compilation(undefined, _ModName) ->
    ok;
post_compilation(AbsOutDir, ModName) ->
    BeamFileRoot = filename:join(AbsOutDir, atom_to_list(ModName)),
    maybe_run_xref(AbsOutDir, BeamFileRoot),
    code:add_patha(AbsOutDir),
    maybe_load(ModName),
    maybe_copy(BeamFileRoot, ModName),
    ok.

%%------------------------------------------------------------------------------
%% @doc Perform a remote call towards the given node.
%% @end
%%------------------------------------------------------------------------------
-spec rpc(node(), module(), atom(), integer()) ->
          {ok, term()} |
          {error, Reason :: {badrpc, term()}}.
rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, _Reason} = Error ->
            {error, Error};
        Other ->
            {ok, Other}
    end.

%%------------------------------------------------------------------------------
%% @doc Run xref on the given module and prints the warnings if the xref option
%% is specified.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_run_xref(string(), string()) -> ok.
maybe_run_xref(AbsOutDir, BeamFileRoot) ->
    case get(xref) of
        true ->
            XRefWarnings = xref:m(BeamFileRoot),

            %% We add this directory to the code path because so that
            %% print_xref_warnings can find the beam file. It would not be good
            %% to add it before, because this directory might be e.g. /var/tmp
            %% so it could contain a bunch of outdates beam files, derailing
            %% xref.
            code:add_patha(AbsOutDir),

            print_xref_warnings(XRefWarnings);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Load the given module if the --load option was specified.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_load(module()) -> ok | error.
maybe_load(ModName) ->
    case get(load) of
        {LongOrShortNames, MyNodeName, TargetNodeName} ->
            Cookie = get(cookie),
            load(LongOrShortNames, MyNodeName, TargetNodeName, Cookie, ModName);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Load the given module into the given node.
%% @end
%%------------------------------------------------------------------------------
-spec load(LongOrShortNames :: (shortnames | longnames),
           MyNodeName :: node(),
           TargetNodeName :: node(),
           Cookie :: atom(),
           ModName :: module()) -> ok | error.
load(LongOrShortNames, MyNodeName, TargetNodeName, Cookie, ModName) ->
    case code:get_object_code(ModName) of
        {ModName, BinaryMod, FileName} ->
            net_kernel:start([MyNodeName, LongOrShortNames]),
            case Cookie of
                undefined ->
                    ok;
                Cookie ->
                    erlang:set_cookie(TargetNodeName, Cookie)
            end,
            load_with_rpc(TargetNodeName, ModName, FileName, BinaryMod);
        error ->
            log_error("Failed to find object code for module ~p.~n", [ModName]),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Load the given binary module into the given node.
%% @end
%%------------------------------------------------------------------------------
-spec load_with_rpc(node(), atom(), string(), binary()) -> ok | error.
load_with_rpc(Node, ModName, FileName, BinaryMod) ->
    case rpc(Node, code, purge, [ModName]) of
        {ok, _} ->
            case rpc(Node, code, load_binary, [ModName, FileName, BinaryMod]) of
                {ok, {module, ModName}} ->
                    log("ModName ~p is reloaded~n", [ModName]),
                    ok;
                {ok, {error, Reason}} ->
                    log_error("Failed to load the module into node ~p: ~p~n",
                              [Node, Reason]),
                    error;
                {error, Reason} ->
                    log_error("RPC towards node ~p failed: ~p~n",
                              [Node, Reason]),
                    error
            end;
        {error, Reason} ->
            log_error("RPC towards node ~p failed: ~p~n", [Node, Reason]),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Copy the given module to module files with the same name if the --copy
%% option was specified.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_copy(string(), module()) -> ok | error.
maybe_copy(BeamFileRoot, ModName) ->
    case get(copy) of
        TargetDir when is_list(TargetDir) ->
            copy(BeamFileRoot, ModName, TargetDir);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Copy the given module to module files with the same name in the target
%% directory.
%% @end
%%------------------------------------------------------------------------------
-spec copy(string(), module(), string()) -> ok.
copy(BeamFileRoot, ModName, TargetDir) ->
    BeamFileBase = atom_to_list(ModName) ++ ".beam",
    SourceBeamFile = BeamFileRoot ++ ".beam",
    TargetBeamFiles = filelib:wildcard(
                        filename:join([TargetDir, "**", BeamFileBase])),
    [ case file:copy(SourceBeamFile, TargetBeamFile) of
          {ok, _} ->
              ok;
          Error ->
              log_error("Error when copying: ~p -> ~p: ~p~n",
                        [SourceBeamFile, TargetBeamFile, Error])
      end || TargetBeamFile <- TargetBeamFiles ],
    ok.

%%------------------------------------------------------------------------------
%% @doc Try to compile the given escript, print the warnings and errors, and
%% return whether there were errors.
%% @end
%%------------------------------------------------------------------------------
-spec check_escript(string()) -> ok | error.
check_escript(File) ->
    case command("escript -s " ++ File) of
        0 ->
            ok;
        _Other ->
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Execute the given OS command.
%%
%% The command's output is printed, and its exit code is returned.
%%
%% Original code from
%% http://erlang.org/pipermail/erlang-questions/2007-February/025210.html
%% @end
%%------------------------------------------------------------------------------
-spec command(string()) -> ExitCode :: integer().
command(Cmd) ->
     Opts = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
     Port = open_port({spawn, Cmd}, Opts),
     command_loop(Port).

command_loop(Port) ->
     receive
         {Port, {data, Data}} ->
             io:format(user, "~s", [Data]),
             command_loop(Port);
         {Port, eof} ->
             port_close(Port),
             receive
                 {Port, {exit_status, Ret}} ->
                     Ret
             end
     end.

%%------------------------------------------------------------------------------
%% @doc Print the given error reason in a Vim-friendly and human-friendly way.
%% @end
%%------------------------------------------------------------------------------
-spec file_error(string(), term()) -> error.
file_error(File, Reason) ->
    Reason2 = file:format_error(Reason),
    io:format(user, "~s: ~s~n", [File, Reason2]),
    error.

%%------------------------------------------------------------------------------
%% @doc Find, read and apply the rebar config files appropriate for the given
%% path.
%%
%% This function traverses the directory tree upward until it finds
%% the root directory. It finds all rebar.config files along the way and applies
%% all of them (e.g. the dependency directory in all of them is added to the
%% code path). It returns the options in the first rebar.config file (e.g. the
%% one that is the closest to the file to be compiled).
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar_configs(AbsDir :: string()) ->
          {ok, Options :: [term()]} | error.
process_rebar_configs(AbsDir) ->
    case process_rebar_configs(AbsDir, no_config_yet) of
        error ->
            error;
        Options ->
            {ok, Options}
    end.

process_rebar_configs(AbsDir, Options0) ->
    ConfigFileName = filename:join(AbsDir, "rebar.config"),

    Options =
        case filelib:is_file(ConfigFileName) of
            true ->
                case file:consult(ConfigFileName) of
                    {ok, ConfigTerms} ->
                        log("rebar.config read: ~s~n", [ConfigFileName]),
                        OptionsHere = process_rebar_config(AbsDir, ConfigTerms),
                        case Options0 of
                            no_config_yet ->
                                OptionsHere;
                            _ ->
                                Options0
                        end;
                    {error, Reason} ->
                        log_error("rebar.config consult unsuccessful:~n"),
                        file_error(ConfigFileName, Reason),
                        error
                end;
            false ->
                Options0
        end,

    case {Options, AbsDir} of
        {error, _} ->
            error;
        {no_config_yet, "/"} ->
            [];
        {_, "/"} ->
            Options;
        {_, _} ->
            process_rebar_configs(filename:dirname(AbsDir), Options)
    end.

%%------------------------------------------------------------------------------
%% @doc Apply a rebar.config file.
%%
%% This function adds the directories in the rebar.config file to the code path
%% and returns and compilation options to be used when compiling the file.
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar_config(Dir :: string(), ConfigTerms :: [term()]) ->
          [Option :: term()].
process_rebar_config(Dir, Terms) ->

    % lib_dirs -> include
    Includes = [ {i, absname(Dir, LibDir)} ||
                 LibDir <- proplists:get_value(lib_dirs, Terms, [])],

    % deps -> code path
    RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    code:add_pathsa(filelib:wildcard(absname(Dir, RebarDepsDir) ++ "/*/ebin")),

    % sub_dirs -> code_path
    [ code:add_pathsa(filelib:wildcard(absname(Dir, SubDir) ++ "/ebin"))
      || SubDir <- proplists:get_value(sub_dirs, Terms, []) ],

    ErlOpts = proplists:get_value(erl_opts, Terms, []) ++
              [{i, absname(Dir, "apps")}|Includes],

    % If "warnings_as_errors" is left in, rebar sometimes prints the
    % following line:
    %
    %     compile: warnings being treated as errors
    %
    % The problem is that Vim interprets this as a line about an actual
    % warning about a file called "compile", so it will jump to the
    % "compile" file.
    %
    % And anyway, it is fine to show warnings as warnings as not errors:
    % the developer know whether their project handles warnings as
    % errors and interpret them accordingly.
    proplists:delete(warnings_as_errors, ErlOpts).

-spec print_xref_warnings({deprecated, [{mfa(), mfa()}]} |
                          {undefined, [{mfa(), mfa()}]} |
                          {unused, [mfa()]}) -> ok.
print_xref_warnings(XRef) ->
    {undefined, UndefFuns} = lists:keyfind(undefined, 1, XRef),
    [begin
         {CallerFile, CallerLine} = find_mfa_source(Caller),
         io:format("~s:~p: Warning: Calling undefined function ~p:~p/~p~n",
                   [CallerFile, CallerLine, M, F, A])
     end || {Caller, {M, F, A}} <- lists:reverse(UndefFuns)],
    ok.

%%------------------------------------------------------------------------------
%% @doc Given a MFA, find the file and LOC where it's defined.
%%
%% Note that xref doesn't work if there is no abstract_code, so we can avoid
%% being too paranoid here.
%%
%% This function was copied from rebar's source code:
%% https://github.com/basho/rebar/blob/117c0f7e698f735acfa73b116f9e38c5c54036dc/src/rebar_xref.erl
%%
%% @end
%%------------------------------------------------------------------------------
-spec find_mfa_source({module(), atom(), integer()}) ->
          {FileName :: string(),
           LineNumber :: integer()}.
find_mfa_source({M, F, A}) ->
    {M, Bin, _} = code:get_object_code(M),
    AbstractCode = beam_lib:chunks(Bin, [abstract_code]),
    {ok, {M, [{abstract_code, {raw_abstract_v1, Code}}]}} = AbstractCode,

    %% Extract the original source filename from the abstract code
    [{attribute, 1, file, {Source, _}} | _] = Code,

    %% Extract the line number for a given function def
    Fn = [E || E <- Code,
               safe_element(1, E) == function,
               safe_element(3, E) == F,
               safe_element(4, E) == A],

    case Fn of
        [{function, Line, F, _, _}] ->
            {Source, Line};
        [] ->
            %% Do not crash if functions are exported, even though they are not
            %% in the source. Parameterized modules add new/1 and instance/1 for
            %% example.
            {Source, 1}
    end.

%%------------------------------------------------------------------------------
%% @doc Extract an element from a tuple, or undefined if N > tuple size.
%%
%% This function was copied from rebar's source code:
%% https://github.com/basho/rebar/blob/117c0f7e698f735acfa73b116f9e38c5c54036dc/src/rebar_xref.erl
%%
%% @end
%%------------------------------------------------------------------------------
-spec safe_element(number(), tuple()) -> term().
safe_element(N, Tuple) ->
    case catch(element(N, Tuple)) of
        {'EXIT', {badarg, _}} ->
            undefined;
        Value ->
            Value
    end.

%%------------------------------------------------------------------------------
%% @doc Return the absolute name of the file which is in the given directory.
%%
%% Example:
%%
%% - cwd = "/home/my"
%% - Dir = "projects/erlang"
%% - Filename = "rebar.config"
%% - Result: "/home/my/projects/erlang/rebar.config"
%% @end
%%------------------------------------------------------------------------------
-spec absname(Dir :: string(), Filename :: string()) -> string().
absname(Dir, Filename) ->
    filename:absname(filename:join(Dir, Filename)).
