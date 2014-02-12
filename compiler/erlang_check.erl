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
            case re:run(Beginning, "^#!.*escript") of
                nomatch ->
                    module;
                {match, _Captured} ->
                    escript
            end;
        {error, _Reason} = Error ->
            Error
    end.

check_module(File) ->
    Dir = filename:dirname(File),
    Defs = [strong_validation,
            warn_export_all,
            warn_export_vars,
            warn_shadow_vars,
            warn_obsolete_guard,
            warn_unused_import,
            report,
            {i, Dir ++ "/include"},
            {i, Dir ++ "/../include"},
            {i, Dir ++ "/../../include"},
            {i, Dir ++ "/../../../include"}],
    case file:consult("rebar.config") of
        {ok, Terms} ->
            RebarLibDirs = proplists:get_value(lib_dirs, Terms, []),
            lists:foreach(
                fun(LibDir) ->
                        code:add_pathsa(filelib:wildcard(LibDir ++ "/*/ebin"))
                end, RebarLibDirs),
            RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
            code:add_pathsa(filelib:wildcard(RebarDepsDir ++ "/*/ebin")),
            RebarOpts = proplists:get_value(erl_opts, Terms, []);
        {error, _} ->
            RebarOpts = []
    end,
    code:add_patha(filename:absname("ebin")),
    compile:file(File, Defs ++ RebarOpts).

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
