#!/usr/bin/env escript

%%------------------------------------------------------------------------------
%% @doc Iterate over the given files, print their compilation warnings and
%% errors, and exit with an appropriate exit code.
%% @end
%%------------------------------------------------------------------------------
-spec main([string()]) -> no_return().
main([]) ->
    io:format("Usage: ~s <files>~n", [escript:script_name()]),
    halt(2);
main(Args) ->
    Files = parse_args(Args),

    % xref is supported only if outdir is also specified
    case {get(outdir), get(xref)} of
        {undefined, true} ->
            erase(xref);
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
parse_args(["--outdir"], _Acc) ->
    log_error("More argument needed after '--outdir'.~n", []),
    halt(1);
parse_args(["--outdir", OutDir|OtherArgs], Acc) ->
    put(outdir, OutDir),
    parse_args(OtherArgs, Acc);
parse_args(["--nooutdir"|OtherArgs], Acc) ->
    erase(outdir),
    parse_args(OtherArgs, Acc);
parse_args(["--xref"|OtherArgs], Acc) ->
    put(xref, true),
    parse_args(OtherArgs, Acc);
parse_args(["--"|Files], Acc) ->
    Files ++ Acc;
parse_args([[$-|_] = Arg|_], _Acc) ->
    log_error("Unknown option: ~s~n", [Arg]),
    halt(1);
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
-spec log_error(io:format(), [term()]) -> ok.
log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

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

    RebarConfigResult =
        case read_rebar_config(AbsDir) of
            {ok, {ConfigAbsDir, ConfigFileName, Terms}} ->
                log("rebar.config read: ~s~n", [ConfigFileName]),
                file:set_cwd(ConfigAbsDir),
                {ok, calc_rebar_opts(Terms)};
            {error, not_found} ->
                log("rebar.config not found.~n"),
                {ok, []};
            {error, {consult_error, ConfigFileName, Reason}} ->
                log("rebar.config consult unsuccessful.~n", []),
                file_error(ConfigFileName, Reason)
        end,

    case RebarConfigResult of
        {ok, RebarOpts} ->
            code:add_patha(filename:absname("ebin")),
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
    maybe_run_xref(AbsOutDir, BeamFileRoot).

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
%% @doc Find and read the rebar config appropriate for the given path.
%%
%% This function traverses the directory tree upward until it finds
%% "rebar.config". Afterwards it reads the terms in that file and returns them.
%% @end
%%------------------------------------------------------------------------------
-spec read_rebar_config(AbsDir :: string()) -> {ok, Result} |
                                               {error, Reason}
          when Result :: {ConfigAbsDir :: string(),
                          ConfigFileName :: string(),
                          ConfigTerms :: [term()]},
               Reason :: not_found |
                         {consult_error,
                          ConfigFileName :: string(),
                          ConsultError},
               ConsultError :: atom() |
                               {Line :: integer(),
                                Mod :: module(),
                                Term :: term()}.
read_rebar_config(AbsDir) ->
    ConfigFileName = filename:join(AbsDir, "rebar.config"),
    case filelib:is_file(ConfigFileName) of
        true ->
            case file:consult(ConfigFileName) of
                {ok, ConfigTerms} ->
                    {ok, {AbsDir, ConfigFileName, ConfigTerms}};
                {error, ConsultReason} ->
                    {error, {consult_error, ConfigFileName, ConsultReason}}
            end;
        false ->
            case AbsDir of
                "/" ->
                    {error, not_found};
                _ ->
                    read_rebar_config(filename:dirname(AbsDir))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate the rebar options.
%%
%% This function assumes that the current directory is the one containing the
%% rebar config file (i.e. the paths in the configuration terms are relative to
%% the current directory).
%% @end
%%------------------------------------------------------------------------------
-spec calc_rebar_opts(ConfigTerms :: [term()]) -> [Option :: term()].
calc_rebar_opts(Terms) ->

    % lib_dirs -> include
    Includes = [ {i, LibDir} ||
                 LibDir <- proplists:get_value(lib_dirs, Terms, [])],

    % deps -> code path
    RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    code:add_pathsa(filelib:wildcard(RebarDepsDir ++ "/*/ebin")),

    % sub_dirs -> code_path
    [ code:add_pathsa(filelib:wildcard(SubDir ++ "/ebin"))
      || SubDir <- proplists:get_value(sub_dirs, Terms, []) ],

    ErlOpts = proplists:get_value(erl_opts, Terms, []) ++ [{i,"apps"}|Includes],

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
