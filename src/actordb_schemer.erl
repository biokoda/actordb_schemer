-module(actordb_schemer).
-author('Biokoda d.o.o.').
-include("actordb_schemer.hrl").

-export([setup/1]).
-export([schema/0, schema/1]).
-export([version/0]).

-export([check/1]).
-export([upgrade/1]).

setup(Module) ->
  bkdcore:mkmodule(actordb_schemer_cfg,[{def_module, Module}]),
  ok.

-spec version() -> integer().
%% @doc minimum schema version that has to be present in order for the biocoded to expose full functionality
%%
version() ->
  M = actordb_schemer_cfg:def_module(),
  M:schema_ver().

-spec fetch_schema() -> list().
%% @doc retrieve schema
%%
fetch_schema() ->
  M = actordb_schemer_cfg:def_module(),
  M:schema_def().

schema() ->
  schema(version()).

schema(MaxVersion) ->
  [begin
    #adb_actor{ tables = Tables} = Actor,
    SchemaVerTables = [begin
      #adb_table{fields = Fields} = Table,
      SchemaVerFields = [Field || Field <- Fields, is_present(Field, MaxVersion)],
      Table#adb_table{ fields = SchemaVerFields }
    end || Table <- Tables, is_present(Table, MaxVersion)],
    Actor#adb_actor{ tables = SchemaVerTables }
  end || Actor <- fetch_schema(), is_present(Actor, MaxVersion)].

is_present( #adb_actor{ ver = Ver }, MaxVersion) ->
  (Ver =< MaxVersion);
is_present( #adb_table{ ver = Ver }, MaxVersion) ->
  (Ver =< MaxVersion);
is_present( #adb_field{ ver = Ver }, MaxVersion) ->
  (Ver =< MaxVersion);
is_present(_, _) ->
  false.

-spec check(any()) -> {ok,list()}.
check(AdbConfig) ->
  actordb_schemer_util:check(AdbConfig, schema()).

-spec upgrade(any()) -> any().
upgrade(AdbConfig) ->
  actordb_schemer_util:upgrade(AdbConfig, schema()).
