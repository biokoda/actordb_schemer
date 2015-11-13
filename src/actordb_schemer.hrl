
-record(adb_actor,{name :: binary(),
                  tables :: list(),
                  opts = [ ] :: list(),
                  ver = 0 :: integer() }).

-record(adb_table,{name :: binary(),
                  fields :: list(),
                  opts :: list(),
                  ver = 0 :: integer()}).

-record(adb_field,{name :: binary(),
                  type :: binary(),
                  opts :: list(),
                  ver = 0:: integer()}).
