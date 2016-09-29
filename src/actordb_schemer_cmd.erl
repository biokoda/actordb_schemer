-module(actordb_schemer_cmd).

-define(CMD_VER,"0.1").

-include_lib("actordb_schemer/include/actordb_schemer.hrl").

-export([main/1]).
-export([schema_def/0, schema_ver/0]).

-define(B(X), iolist_to_binary(X)).

-spec main(Params :: list()) -> any().
%%  @doc Entry point to schemer loop.
%%
main(Params) ->
  io:format("ActorDB Schemer, ver. ~s~n",[?CMD_VER]),
  Params1 = parse_params(Params),
  case fetch_param(help, Params1, false) of
    true -> print_help();
    false -> ok
  end,
  case fetch_param(schema, Params1, undefined) of
    undefined ->
      io:format("Error: missing schema filename. Run with --help~n"),
      halt(1);
    Filename when is_list(Filename) ->
      Address = fetch_param(address, Params1, "127.0.0.1"),
      Port = fetch_param(port, Params1, 33306),
      Username = fetch_param(username, Params1, ""),
      Password = fetch_param(password, Params1, ""),
      Action = fetch_param(action, Params1, check),
      execute(Address, Port, Username, Password, Filename, Action)
  end.

-spec fetch_param( Param :: atom(), Params :: list(), Default :: any()) -> Value :: any().
%% @doc returns a value for a param from lists of {Param,Value} tuples
%%  if such tuple is not present Default is returned
fetch_param(Param, Params, Default) ->
  case lists:keyfind(Param, 1, Params) of
    false ->
      case Default of
        {throw, Descr} ->
          throw({error, Descr});
        _ ->
          Default
      end;
    {_,Value} ->
      Value
  end.

-spec parse_params( L :: list() ) -> KeyValParams :: list().
%% @doc parses input command line params and puts them to a {Param, Value} lists
%%      params: help, address, port, schema
parse_params(Params) ->
  parse_params1(Params,[]).

parse_params1([],Parsed) ->
  Parsed;
parse_params1(["--help"|R],Out) ->
  parse_params1(R,[{help,true}|Out]);
parse_params1(["--address",Address|R],Out) ->
  parse_params1(R,[{address,Address}|Out]);
parse_params1(["--port",Port|R],Out) ->
  parse_params1(R,[{port,erlang:list_to_integer(Port)}|Out]);
parse_params1(["--schema",SchemaFile|R],Out) ->
  parse_params1(R,[{schema,SchemaFile}|Out]);
parse_params1(["--username",Username|R],Out) ->
  parse_params1(R,[{username,Username}|Out]);
parse_params1(["--password",Password|R],Out) ->
  parse_params1(R,[{password,Password}|Out]);
parse_params1(["--action","check"|R],Out) ->
  parse_params1(R,[{action,check}|Out]);
parse_params1(["--action","upgrade"|R],Out) ->
  parse_params1(R,[{action,upgrade}|Out]);
parse_params1([Ignored|R],Out) ->
  io:format("[config] ignoring unknown parameter: ~s~n",[Ignored]),
  parse_params1(R, Out).

-spec print_help() -> ok.
%%  @doc prints help to stdout
%%
print_help() ->
  io:format("Options:~n"),
  io:format("--help                       - prints this help~n"),
  io:format("--schema <filename>          - set the schema filename (required)~n"),
  io:format("--address <address>          - hostname to connect to (default: 127.0.0.1)~n"),
  io:format("--port <port>                - port (default: 33306)~n"),
  io:format("--username <username>        - username (default: empty)~n"),
  io:format("--password <password>        - password (default: empty)~n"),
  io:format("--action <check|upgrade>     - action to perform, check = check only, upgrade = perform schema upgrade~n"),
  halt(1),
  ok.

-spec execute(Address :: list(), Port :: integer(), Username :: string(),
  Password :: string(), Filename :: string(), Action :: check | upgrade) -> ok.
%% @doc executes schmea
%%
execute(Addr, Port, Username, Password, Fname, Action) ->
  io:format("~p ~p ~p ~p ~p ~p",[Addr, Port, Username, Password, Fname, Action]),
  application:ensure_all_started(crypto),
  application:ensure_all_started(poolboy),
  application:ensure_all_started(actordb_client),
  PoolInfo = [{size, 1}, {max_overflow, 5}],
  WorkerParams = [{hostname, Addr}, {port, Port}, {username, Username}, {password, Password}],
  R = actordb_client:start([{schemer_connection, PoolInfo, WorkerParams}]),
  io:format("ActorDB Schemer connected to ~s:~p, ~p ~n",[Addr, Port, R]),
  actordb_schemer:setup(actordb_schemer_cmd),
  % io:format("Running applications: ~p~n",[application:which_applications()]),
  ActorConfig = actordb_client:config(schemer_connection, infinity, binary),
  put(actordb_schemer_filename, Fname),
  case Action of
    check ->
      {ok, SQLs} = actordb_schemer:check(ActorConfig),
      io:format("~nSchema check: ~n",[]),
      print_sqls("[-]",SQLs),
      halt(0);
    upgrade ->
      {ok, SQLs1} = actordb_schemer:check(ActorConfig),
      io:format("~n1. Check:~n",[]),
      print_sqls("[1.]", SQLs1),
      case SQLs1 of
        [] -> io:format("Schema is already up-to-date.~n");
        _ ->
          io:format("~n2. Upgrading ...",[]),
          actordb_schemer:upgrade(ActorConfig),
          {ok, SQLs2} = actordb_schemer:check(ActorConfig),
          io:format("~n3. Check:~n",[]),
          print_sqls("[3.]", SQLs2),
          case SQLs2 of
            [] -> io:format("Schema is up-to-date.~n");
            _ -> ok
          end,
          halt(0)
      end
  end,
  ok.

-spec schema_def() -> SchemaDef :: list().
%% @doc callback for schema definition
%%  parses the schema file (erlang syntax, file:consult/1)
%%  check example schema example.adbschema for file layout
%%
schema_def() ->
  io:format("ActorDB Schemer, resolving schema ... ~n"),
  {ok, [Parsed]} = file:consult(get(actordb_schemer_filename)),
  {schema, Schema} = lists:keyfind(schema, 1, Parsed),
  % io:format("Consult: ~p~n",[Schema]),
  resolve_schema(Schema).

-spec schema_ver() -> SchemaVersion :: integer().
%% @doc callback for schema version
%%  parses the schema file (erlang syntax, file:consult/1)
%%  check example schema example.adbschema for file layout
%%
schema_ver() ->
  io:format("ActorDB Schemer, resolving schema version ... ~n"),
  {ok, [Parsed]} = file:consult(get(actordb_schemer_filename)),
  Version = fetch_param(version, Parsed, 0),
  io:format("ActorDB Schemer, schema version: ~p ~n",[Version]),
  Version.

-spec resolve_schema(IntermedSchema :: list()) -> SchemerFormat :: list().
%% @doc transforms 'actor' depth of schema definition to #adb_actor{} records
%%
resolve_schema(Schema) ->
  [begin
    {actor,Actor} = ActorDef,
    Name = fetch_param(name, Actor, {throw,"actor.name, empty"}),
    Opts = fetch_param(opts, Actor, []),
    Ver = fetch_param(ver, Actor, 0),
    Tables = fetch_param(tables, Actor, {throw,"actor.tables, empty"}),
    #adb_actor{
      name = ?B(Name),
      opts = Opts,
      ver = Ver,
      tables = resolve_actor_tables(Tables)
    }
  end || ActorDef <- Schema].

-spec resolve_actor_tables(Tables :: list()) -> SchemerFormat :: list().
%% @doc transforms 'table' depth of schema definition to #adb_table{} records
%%  ; parses tables
resolve_actor_tables(Tables) ->
  [begin
    {table, Table} = TableDef,
    Name = fetch_param(name, Table, {throw,"table.name, empty"}),
    Opts = fetch_param(opts, Table, []),
    Ver = fetch_param(ver, Table, 0),
    Fields = fetch_param(fields, Table, {throw,"table.fields, empty"}),
    #adb_table{
      name = ?B(Name),
      fields = resolve_table_fields(Fields),
      opts = Opts,
      ver = Ver
    }
  end || TableDef <- Tables].

-spec resolve_table_fields(Fields :: list()) -> SchemerFormat :: list().
%% @doc transforms 'field' depth of schema definition to #adb_field{} records
%%
resolve_table_fields(Fields) ->
  [begin
    {field, Field} = FieldDef,
    Name = fetch_param(name, Field, {throw,"field.name, empty"}),
    Type = fetch_param(type, Field, {throw,"field.type, empty"}),
    Opts = fetch_param(opts, Field, []),
    Ver = fetch_param(ver, Field, 0),
    #adb_field{
      name = ?B(Name),
      type = ?B(Type),
      opts = Opts,
      ver = Ver
    }
  end || FieldDef <- Fields].

-spec print_sqls(Tag :: string(), Sqls :: list()) -> ok.
%% @doc Prints SQL statements to stdout prepending them with Tag
%%
print_sqls(_,[]) ->
  io:format("~n"),
  ok;
print_sqls(Tag, [Sql|R]) ->
  io:format("~s : ~s~n",[Tag, Sql]),
  print_sqls(Tag, R).
