#!/usr/bin/env escript


main(BinFiles) ->
  Apps = [actordb_client, adbt, goldrush, lager, poolboy, thrift],
  true = code:add_path("ebin"),
  ok = code:add_paths(filelib:wildcard("deps/*/ebin")),
  Files1 = [begin
    FileList = filelib:wildcard("deps/"++atom_to_list(Dir)++"/ebin/*.*") ++ filelib:wildcard("ebin/*.*"),
    [{filename:basename(Nm),element(2,file:read_file(Nm))} || Nm <- FileList]
  end || Dir <- Apps],
  Files = [{filename:basename(Fn),element(2,file:read_file(Fn))} || Fn <- BinFiles]++lists:flatten(Files1),
  case zip:create("mem", Files, [memory]) of
    {ok, {"mem", ZipBin}} ->
      %% Archive was successfully created. Prefix that with header and write to "edis" file
      Script = <<"#!/usr/bin/env escript\n%%! +Bc +K true  -smp enable\n", ZipBin/binary>>,
      case file:write_file("actordb_schemer", Script) of
        ok -> ok;
        {error, WriteError} ->
          io:format("Failed to write actordb_schemer: ~p\n", [WriteError]),
          halt(1)
      end;
    {error, ZipError} ->
      io:format("Failed to construct actordb_schemer archive: ~p\n", [ZipError]),
      halt(1)
  end,
  case os:type() of
    {unix,_} ->
      [] = os:cmd("chmod u+x actordb_schemer"),
      ok;
    _ ->
      ok
  end.
