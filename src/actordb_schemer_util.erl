-module(actordb_schemer_util).
-include("actordb_schemer.hrl").
-author('Biokoda d.o.o.').

-export([check/2]).
-export([upgrade/2]).

-define(MARK(X), case X of [] -> "\\"; _ -> "|" end).
-define(PARENT_MARK, case get(parent_mark) of undefined -> " "; _ -> "|" end).

check(ADBCfg, ProvidedSchema) ->
  ExpectedSchema = ProvidedSchema,
  CurrentSchema =  current_schema(ADBCfg),
  RequiredChanges = upgrade_actors(ExpectedSchema, CurrentSchema, []),
  UpdateCommands = [begin
    format_queries(Name, Changes, ExpectedSchema)
  end || {actor, Name, Changes} <- RequiredChanges],
  {ok, UpdateCommands}.

upgrade(ADBCfg, ProvidedSchema) ->
	case check(ADBCfg, ProvidedSchema) of
    {ok, []} ->
      io:format("~nNothing to execute. Schema up-to-date.~n"),
      ok;
    {ok, Commands} ->
      BinCommands = iolist_to_binary(Commands),
      io:format("~nExecuting: ~s~n",[BinCommands]),
      actordb_client:exec_schema(ADBCfg, BinCommands);
    R ->
      {error, R}
  end.

current_schema(ADBCfg) ->
	{ok,Actors} = actordb_client:actor_types(ADBCfg),
	[begin
		{ok,Tables} = actordb_client:actor_tables(ADBCfg,Actor),
		TableMeta = [begin
			{ok, Columns} = actordb_client:actor_columns(ADBCfg, Actor, Table),
      Fields = [#adb_field{ name = Fname, type = Ftype } || {Fname,Ftype} <- maps:to_list(Columns)],
			#adb_table{name = Table, fields = Fields}
		end || Table <- Tables],
    #adb_actor{name = Actor, tables = TableMeta}
	end || Actor <- Actors].


upgrade_actors([],_,Changes) ->
  Changes;
upgrade_actors([Actor|L], CurSchema, Changes) when is_record(Actor, adb_actor) ->
  TableChanges = case extract_actor(Actor#adb_actor.name, CurSchema) of
    undefined ->
      io:format("~nACTOR ~s (CREATE)~n",[Actor#adb_actor.name]),
      EmptyActor = #adb_actor{ name = Actor#adb_actor.name, opts = Actor#adb_actor.opts, tables = [] },
      upgrade_tables(Actor#adb_actor.tables, EmptyActor, []);
    CurActorDef ->
      % actor exists, update or create tables
      io:format("~nACTOR ~s (VALIDATE)~n",[Actor#adb_actor.name]),
      upgrade_tables(Actor#adb_actor.tables, CurActorDef, [])
  end,
  case TableChanges of
    [] ->
      upgrade_actors(L, CurSchema, Changes);
    TCL when is_list(TCL) ->
      upgrade_actors(L, CurSchema, [{actor, Actor, TCL}|Changes])
  end.


upgrade_tables([],_,Changes) ->
  Changes;
upgrade_tables([Table|L], CurActorDef, Changes) when is_record(Table, adb_table) ->
  case L of [] -> put(parent_mark,undefined); _ -> put(parent_mark,true) end,
  case extract_table(Table#adb_table.name, CurActorDef#adb_actor.tables) of
    undefined ->
      io:format("|~n|-- creating TABLE ~s~n",[Table#adb_table.name]),
      % @TODO: create table
      Command = {create_table, Table},
      upgrade_tables(L, CurActorDef, [Command|Changes]);
    CurTableDef ->
      io:format("|~n|-- validating TABLE ~s~n",[Table#adb_table.name]),
      FieldChanges = upgrade_fields(Table#adb_table.fields, CurTableDef, []),
      case FieldChanges of
        [] ->
          upgrade_tables(L, CurActorDef, Changes);
        FCL when is_list(FCL) ->
          upgrade_tables(L, CurActorDef, [{alter_table, Table, FCL}|Changes])
      end
  end.

upgrade_fields([],_,Changes) ->
  Changes;
upgrade_fields([Field|L], CurTableDef, Out) when is_record(Field, adb_field)->
  case extract_field(Field#adb_field.name, CurTableDef#adb_table.fields) of
    undefined ->
      io:format("~s\t~s-- altering TABLE, adding COLUMN ~s (~p)~n",[?PARENT_MARK,?MARK(L),Field#adb_field.name,lists:flatten(io_lib:format("~p",[Field]))]),
      Command = {add_column, Field},
      upgrade_fields(L, CurTableDef, [Command|Out]);
    CurFieldDef ->
      io:format("~s\t~s-- COLUMN present: ~s (~p)~n",[?PARENT_MARK,?MARK(L),Field#adb_field.name,lists:flatten(io_lib:format("~p",[Field]))]),
      upgrade_fields(L, CurTableDef, Out)
  end.

extract_actor(_, []) ->
  undefined;
extract_actor(Name, [Actor|R]) when is_record(Actor, adb_actor) ->
  case Actor#adb_actor.name of
    Name ->
      Actor;
    _ ->
      extract_actor(Name, R)
  end.

extract_table(_, []) ->
  undefined;
extract_table(Name, [Table|R]) when is_record(Table, adb_table) ->
  case Table#adb_table.name of
    Name ->
      Table;
    _ ->
      extract_table(Name, R)
  end.

extract_field(_,[]) ->
  undefined;
extract_field(Name,[Field|R]) ->
  case Field#adb_field.name of
    Name ->
      Field;
    _ ->
      extract_field(Name, R)
  end.

format_queries(Actor, Changes, ExpectedSchema) ->
  case format_queries0(Actor, Changes, ExpectedSchema, []) of
    [] ->
      <<>>;
    R ->
      ActorName = Actor#adb_actor.name,
      KV = case lists:member(kv,Actor#adb_actor.opts) of
        true ->
          <<" kv">>;
        false ->
          <<"">>
      end,
      iolist_to_binary([ <<"actor ", ActorName/binary, KV/binary, "; ">> | R])
  end.

format_queries0(_, [], _, Queries) ->
  Queries;
format_queries0(Actor, [{alter_table, Table, Actions}|R], ExpectedSchema, Q) ->
  NQ = [begin
    TableName = Table#adb_table.name,
    case Action of
      {add_column,Field} ->
        ColumnName = Field#adb_field.name,
        ColumnType = Field#adb_field.type,
        Constraints = get_constraints(Field#adb_field.opts),
        <<"ALTER TABLE ", TableName/binary," ADD COLUMN ", ColumnName/binary, " ", ColumnType/binary, Constraints/binary, ";">>
    end
  end || Action <- Actions],
  format_queries0(Actor, R, ExpectedSchema, [NQ|Q]);
format_queries0(Actor, [{create_table, Table}|R], ExpectedSchema, Q) ->
  %lager:info("~p",[Table]),
  TableName = Table#adb_table.name,
  Fields0 = [begin
    FieldName = Field#adb_field.name,
    FieldType = Field#adb_field.type,
    FieldConstraints = get_constraints(Field#adb_field.opts),
    <<FieldName/binary, " ", FieldType/binary, FieldConstraints/binary>>
  end || Field <- Table#adb_table.fields],
  Fields = iolist_to_binary(join(Fields0,", ")),
  TableConstraints = table_constraints(Table#adb_table.opts),
  PostCreateStatement = table_postcreate(Table#adb_table.opts), % WIHTOUT ROWID, AS ...
  NQ = <<"CREATE TABLE ",TableName/binary," (", Fields/binary, TableConstraints/binary,")", PostCreateStatement/binary, ";" >>,
  format_queries0(Actor, R, ExpectedSchema, [NQ | Q]);
format_queries0(Actor, Command, _, _) ->
  throw({update_error, Actor#adb_actor.name, Command}).

table_constraints(undefined) ->
  <<>>;
table_constraints(Opts) ->
  C = [begin
    case Opt of
      % FOREIGN KEY (id) REFERENCES actors (id) ON DELETE CASCADE
      % Example: {foreign_key,[{key,["id"]},{ref_table,"actors"},{ref_id,["id"]},{opts,[on_delete_cascade]}]}
      {foreign_key, FKOpts} ->
        Fk = iolist_to_binary(join(butil:ds_val(key,FKOpts), ",")),
        Ref = iolist_to_binary(butil:ds_val(ref_table,FKOpts)),
        RefId = iolist_to_binary(join(butil:ds_val(ref_id,FKOpts),",")),
        FkCfg = butil:ds_val(opts,FKOpts),
        case lists:member(on_delete_cascade, FkCfg) of
          true ->
            ODC = <<" ON DELETE CASCADE">>;
          false ->
            ODC = <<>>
        end,
        <<"FOREIGN KEY (",Fk/binary,") REFERENCES ", Ref/binary," (",RefId/binary,")", ODC/binary>>;
      {primary_key,KeyList} ->
        Keys = iolist_to_binary(join([ Key || Key <- KeyList],",")),
        <<"PRIMARY KEY (",Keys/binary,")">>;
      _ -> <<>>
    end
  end || Opt <- Opts],
  C0 = lists:filter(fun(Itm) ->
    case Itm of
      <<>> -> false;
      _ -> true
    end
  end,C),
  case C0 of
    [] -> <<>>;
    _ -> iolist_to_binary([<<",">>,join(C0," ")])
  end.


table_postcreate(undefined) ->
  <<>>;
table_postcreate(Opts) ->
  C = [begin
    case Opt of
      without_rowid -> <<"WITHOUT ROWID">>;
      _ -> <<>>
    end
  end || Opt <- Opts],
  C0 = lists:filter(fun(Itm) ->
    case Itm of
      <<>> -> false;
      _ -> true
    end
  end,C),
  case C0 of
    [] -> <<>>;
    _ -> iolist_to_binary([<<" ">>,join(C0," ")])
  end.

get_constraints(undefined) ->
  <<>>;
get_constraints(L) ->
  Bin = [begin
    case F of
      not_null -> <<"NOT NULL">>;
      primary_key -> <<"PRIMARY KEY">>;
      autoincrement -> <<"AUTOINCREMENT">>
    end
  end || F <- L],
  iolist_to_binary([<<" ">>,join(Bin," ")]).


join(L,Itm) ->
  join(L,Itm,[]).
join([],_,Out) ->
  lists:reverse(Out);
join([A],_,Out) ->
  lists:reverse([A|Out]);
join([A|B], Itm, Out) ->
  join(B, Itm, [Itm,A|Out]).
