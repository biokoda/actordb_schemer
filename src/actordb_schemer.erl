-module(actordb_schemer).
-author('Biokoda d.o.o.').
-include("actordb_schemer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([setup/1,setup/2]).
-export([schema/0, schema/1]).
-export([version/0]).

-export([check/1]).
-export([upgrade/1]).

-spec setup(atom()) -> {module,actordb_schemer_cfg}.
%%	@doc configures ActorDB schemer with callback module for schema and version provider
%%	module has to implement actordb_schemer_proto behaviour
%%
setup(Module) ->
  setup(Module, []).
setup(Module,Opts) when is_atom(Module) ->
	L = [{attribute,0,module,actordb_schemer_cfg},
	 {attribute,0,export,[{def_module,0},{silent,0}]},
	 {function,0,def_module,0,[{clause,0,[],[],[{atom,0,Module}]}]},
	 {function,0,silent,0,[{clause,0,[],[],[{atom,0,lists:member(silent, Opts)}]}]}],
	{ok, _, Bin} = compile:forms(L, [verbose, report_errors]),
	code:purge(actordb_schemer_cfg),
	code:load_binary(actordb_schemer_cfg, "actordb_schemer_cfg.erl", Bin).

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

schemer_test() ->
  application:ensure_all_started(actordb_schemer),
  actordb_schemer:setup(?MODULE,[silent]),
  CheckResult1 = actordb_schemer_exec:check({schema,?'test-schema-1'},?'test-schema-1'),
  CheckResult2 = actordb_schemer_exec:check({schema,?'test-schema-1'},?'test-schema-2'),
  [?assertEqual(?MODULE,actordb_schemer_cfg:def_module()),
  ?assertEqual(true,actordb_schemer_cfg:silent()),
  ?assertMatch({ok,[]},CheckResult1),
  ?assertMatch({ok,[_|_]},CheckResult2)].

-endif.
