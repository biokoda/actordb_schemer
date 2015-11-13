ActorDB Schemer

ActorDB schemer enables automatic versioned actor schema upgrades.

**EXPERIMENTAL**

Usage:

1. Include with rebar into your project.

2. Create your schema through a module that matches the behaviour of **actordb_schemer_proto**.

Example A:
```
-module(test_schema).
-include_lib("actordb_schemer.hrl").
-export([schema_ver/0, schema_def/0]).

schema_ver() ->
  0.

schema_def() ->
  [
    #adb_actor{ name = <<"user">>, tables = [
      #adb_table{ name = <<"some_table">>, opts = [without_rowid, {primary_key,[<<"data1">>]}], fields = [
        #adb_field{ name = <<"data1">>, type = <<"TEXT">> },
        #adb_field{ name = <<"data2">>, type = <<"TEXT">> },
        #adb_field{ name = <<"data3">>, type = <<"BOOLEAN">> }
      ]}
  ]
].

```

Example B:
```
-module(test_schema).
-include_lib("actordb_schemer.hrl").
-export([schema_ver/0, schema_def/0]).

schema_ver() ->
  1.  % we updated schema_ver() to 1, this way all actors, tables and columns with this version are included when building the change table

schema_def() ->
  [
    #adb_actor{ name = <<"user">>, tables = [
      #adb_table{ name = <<"some_table">>, opts = [without_rowid, {primary_key,[<<"data1">>]}], fields = [
        #adb_field{ name = <<"data1">>, type = <<"TEXT">> },
        #adb_field{ name = <<"data2">>, type = <<"TEXT">> },
        #adb_field{ name = <<"data3">>, type = <<"BOOLEAN">> },
        #adb_field{ name = <<"new_column">>, type = <<"INTEGER">>, ver = 1 }    % we added this field in schema_ver 1
      ]}
  ]
].

```

Schema changes will appear only for all actors, tables and fields that have version (ver field) lower or equal to the schema_ver() value.


3. Setup the schemer with the module that matches the required behaviour
```
actordb_schemer:setup(test_schema).
```

3. Check required actions that need to be done in order to bring actors to the latest schema
```
actordb_schemer:check(actordb_client:config()).
```

4. Execute actions on the actor
```
actordb_schemer:upgrade(actordb_client:config()).
```

**NOTE** : actordb_client connection needs to have permissions to change schema, else commands for update will fail.

**EXPERIMENTAL**
