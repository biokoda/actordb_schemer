-module(actordb_schemer).
-author('Biokoda d.o.o.').
-include("actordb_schemer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([schema_def/0,schema_ver/0]).
-endif.

-export([main/1]).

-export([setup/1,setup/2]).
-export([schema/0, schema/1]).
-export([version/0]).

-export([check/1]).
-export([upgrade/1]).

-export([schemer_cfg/1]).

-spec main(Params :: list()) -> any().
%% @doc executes schemer as a standalone tool
%%
main(Params) ->
  actordb_schemer_cmd:main(Params).

-spec setup(atom()) -> ok.
%%	@doc configures ActorDB schemer with callback module for schema and version provider
%%	module has to implement actordb_schemer_proto behaviour
%%
setup(Module) ->
  setup(Module, []).
setup(Module,Opts) when is_atom(Module) ->
  ok = validate_opts(Opts),
  application:set_env(actordb_schemer, opts, Opts),
  ok = validate_callbacks(Module),
	application:set_env(actordb_schemer, callback_module, Module).

-spec validate_opts(Opts :: list()) -> ok.
%% @doc validates the opts configuration
%%
validate_opts(Opts) ->
  [schemer_cfg(Opt)||Opt <- Opts], % validate all schemer options, throws error on unknown opt
  ok.

-spec validate_callbacks(Module :: atom()) -> ok.
%% @doc validates the callback Module
%%
validate_callbacks(Module) ->
  {exports, SCB} = lists:keyfind(exports, 1, Module:module_info()),
  SchemaDef = lists:keyfind(schema_def, 1, SCB) =/= false,
  SchemaVer = lists:keyfind(schema_ver, 1, SCB) =/= false,
  case lists:member(false,[SchemaDef, SchemaVer]) of
    true -> throw({error, bad_callback_mod_def, Module});
    false -> ok
  end.


-spec schemer_opts() -> list().
%%	@doc lists all options that schemer was setup with
%%
schemer_opts() ->
  case application:get_env(actordb_schemer, opts) of
    {ok, Opts} ->
      Opts;
    _ ->
      []
  end.

-spec schemer_cfg(Config :: any()) -> any().
%%	@doc fetches a schemer config, throws an error if it is an unknown configuration entry
%%  Config = silent -> true | false
%%
schemer_cfg(silent) ->
  lists:member(silent, schemer_opts());
schemer_cfg(Opt) ->
  throw({error, unknown_schemer_opt, Opt}).

callback_module() ->
  case application:get_env(actordb_schemer, callback_module) of
    {ok, Mod} ->
      Mod;
    E ->
      throw({error,{bad_callback_module, E}})
  end.

-spec version() -> integer().
%% @doc minimum schema version that has to be present in order for the biocoded to expose full functionality
%%
version() ->
  Mod = callback_module(),
  Mod:schema_ver().

-spec fetch_schema() -> list().
%% @doc retrieve schema
%%
fetch_schema() ->
  Mod = callback_module(),
  Mod:schema_def().

-spec schema() -> list().
%%	@doc fetches schema from callback module for the current version set in callback module
%%
schema() ->
  schema(version()).

-spec schema(integer()) -> list().
%%	@doc fetches schema from callback module for value MaxVersion
%%
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

-spec is_present(term(),integer()) -> true | false.
%% @doc helper function for filtering out if an entity is present in schema of version MaxVersion
%%
is_present( #adb_actor{ ver = Ver }, MaxVersion) ->
  (Ver =< MaxVersion);
is_present( #adb_table{ ver = Ver }, MaxVersion) ->
  (Ver =< MaxVersion);
is_present( #adb_field{ ver = Ver }, MaxVersion) ->
  (Ver =< MaxVersion);
is_present(_, _) ->
  false.

-spec check(any()) -> {ok,list()}.
%% @doc compares schemes and returns a list of queries that have to be run for the schemas to be aligned
%%
check(AdbConfig) ->
  actordb_schemer_exec:check(AdbConfig, schema()).

-spec upgrade(any()) -> any().
%% @doc executes check/1 and runs upgrade queries on ActorDB to align the schema
%%
upgrade(AdbConfig) ->
  actordb_schemer_exec:upgrade(AdbConfig, schema()).

-ifdef(TEST).
-define('test-schema-1',[
  #adb_actor{ name = <<"test-actor">>, opts = [without_rowid], tables = [
    #adb_table{ name = <<"test-table1">>, fields = [
      #adb_field{ name = <<"id">>, type = <<"INTEGER">>, opts = [primary_key]}
    ]}
  ]}
]).
-define('test-schema-2',[
  #adb_actor{ name = <<"test-actor">>, opts = [without_rowid], tables = [
    #adb_table{ name = <<"test-table1">>, fields = [
      #adb_field{ name = <<"id">>, type = <<"INTEGER">>, opts = [primary_key]},
      #adb_field{ name = <<"new-field">>, type = <<"TEXT">>, ver = 2}
    ]}
  ]},
  #adb_actor{ name = <<"test-actor-2">>, ver = 2, opts = [], tables = [
    #adb_table{ name = <<"test-table-21">>, ver = 2, fields = [
      #adb_field{ name = <<"id">>, type = <<"TEXT">>, ver = 2},
      #adb_field{ name = <<"new-field">>, type = <<"TEXT">>, ver = 2}
    ]}
  ]}
]).
-define('test-schema-1-version',0).

schema_def() ->
  ?'test-schema-1'.

schema_ver() ->
  ?'test-schema-1-version'.

schemer_test() ->
  application:ensure_all_started(actordb_schemer),
  actordb_schemer:setup(?MODULE,[silent]),
  CheckResult1 = actordb_schemer_exec:check({schema,?'test-schema-1'},?'test-schema-1'),
  CheckResult2 = actordb_schemer_exec:check({schema,?'test-schema-1'},?'test-schema-2'),
  [?assertEqual(?MODULE,callback_module()),
  ?assertEqual(true,schemer_cfg(silent)),
  ?assertMatch({ok,[]},CheckResult1),
  ?assertMatch({ok,[_|_]},CheckResult2)].
-endif.
