#!/usr/bin/env escript

main([]) ->
    io:format("Usage: ~s <files>~n", [escript:script_name()]),
    halt(2);
main(Files) ->
    CheckFilter = fun(File) ->
                          case check_file(File) of
                              {ok, _} ->
                                  false;
                              error ->
                                  true
                          end
                  end,
    case lists:filtermap(CheckFilter, Files) of
        % No Errors (but there could be Warnings!)
        [] ->
            halt(0);
        % At least one Error
        _Errors ->
            halt(1)
    end.

check_file(File) ->
    case file_type(File) of
        module ->
            check_module(File);
        escript ->
            check_escript(File);
        {error, Reason} ->
            file_error(File, Reason)
    end.

file_type(File) ->
    case file:open(File, [raw, read]) of
        {ok, Fd} ->
            Result = read_file_type(Fd),
            ok = file:close(Fd),
            Result;
        {error, _Reason}  = Error ->
            Error
    end.

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

check_module(File) ->
    Dir = filename:dirname(File),
    AbsFile = filename:absname(File),
    AbsDir = filename:absname(Dir),

    Defs = [strong_validation,
            warn_export_all,
            warn_export_vars,
            warn_shadow_vars,
            warn_obsolete_guard,
            warn_unused_import,
            report,
            {i, AbsDir ++ "/include"},
            {i, AbsDir ++ "/../include"},
            {i, AbsDir ++ "/../../include"},
            {i, AbsDir ++ "/../../../include"}],

    RebarConfigResult =
        case read_rebar_config(AbsDir) of
            {ok, {ConfigAbsDir, _ConfigFileName, Terms}} ->
                file:set_cwd(ConfigAbsDir),
                {ok, calc_rebar_opts(Terms)};
            {error, not_found} ->
                {ok, []};
            {error, {consult_error, ConfigFileName, Reason}} ->
                file_error(ConfigFileName, Reason)
        end,

    case RebarConfigResult of
        {ok, RebarOpts} ->
            code:add_patha(filename:absname("ebin")),
            compile:file(AbsFile, Defs ++ RebarOpts);
        error ->
            error
    end.

check_escript(File) ->
    case os:cmd("escript -s " ++ File) of
        [] ->
            {ok, escript};
        Output ->
            io:format(user, "~s", [Output]),
            check_escript_return(Output)
    end.

check_escript_return(Output) ->
    Lines = string:tokens(Output, "\n"),
    {ok, MP} = re:compile(":\\d+: Warning: "),
    WarnFilter = fun(Line) ->
                          case re:run(Line, MP) of
                              % Not a Warning => Error
                              nomatch ->
                                  false;
                              {match, _Captured} ->
                                  true
                          end
                  end,
    case lists:dropwhile(WarnFilter, Lines) of
        % All Warnings
        [] ->
            {ok, escript};
        % At least one error
        _Errors ->
            error
    end.

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
