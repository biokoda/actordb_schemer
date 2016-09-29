-module(actordb_schemer_util).
-include_lib("actordb_schemer/include/actordb_schemer.hrl").
-author('Biokoda d.o.o.').

-export([cnstr_fk/3]).

-spec cnstr_fk(ForeignKeys :: list(), ReferenceTableName :: binary(), ReferenceTableKeys :: list()) -> term().
%%  @doc create a table constraint opts structure for foreign key ... references .. on delete cascade | on update cascade
%%  {foreign_key,[{key,list()},{ref_table,binary()},{ref_id,list()},{opts,[on_delete_cascade]}]}.
%%
cnstr_fk(ForeignKeys, ReferenceTableName, ReferenceTableKeys) ->
  FKs = validate_keys(ForeignKeys),
  RefTable = iolist_to_binary(ReferenceTableName),
  RefTableKeys = validate_keys(ReferenceTableKeys),
  {foreign_key,[{key,FKs},{ref_table,RefTable},{ref_id,RefTableKeys},{opts,[on_delete_cascade]}]}.

%% @private
validate_keys(L) when is_list(L) ->
  validate_keys(L,[]).
validate_keys([],O) ->
  lists:reverse(O);
validate_keys([H|T],O) ->
  validate_keys(T,[iolist_to_binary(H)|O]).
