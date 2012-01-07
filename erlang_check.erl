#!/usr/bin/env escript

main([Filename]) ->
    Dir  = filename:dirname(Filename),
    Opts = [strong_validation,
            warn_export_all,
            warn_export_vars,
            warn_shadow_vars,
            warn_obsolete_guard,
            warn_unused_import,
            report,
            {i, Dir ++ "/include"},
            {i, Dir ++ "/../include"},
            {d, 'TEST'}, {d, 'DEBUG'}],
    compile:file(Filename, Opts).
