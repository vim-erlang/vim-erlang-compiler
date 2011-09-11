#!/usr/bin/env escript

main([Filename]) ->
    compile:file(Filename, [strong_validation,
                            warn_export_all,
                            warn_export_vars,
                            warn_shadow_vars,
                            warn_obsolete_guard,
                            warn_unused_import,
                            report,
                            {i, "include"}, {i, "../include"},
                            {d, 'TEST'}, {d, 'DEBUG'}]).
