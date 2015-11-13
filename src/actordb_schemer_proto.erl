%% @hidden
-module(actordb_schemer_proto).
-author('Biokoda d.o.o.').

-compile(export_all).

-callback schema_def() -> {ok, list()}.
-callback schema_ver() -> {ok, integer()}.
