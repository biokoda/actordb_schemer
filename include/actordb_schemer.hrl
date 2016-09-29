
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
                  ver = 0 :: integer()}).

-record(adb_index,{name :: binary(),
                   field_names :: list(),
                   present :: boolean(),
                   ver = 0 :: integer()
                   }).

-define(ADBSC_FK(Fk,Rt,Rk), actordb_schemer_util:cnstr_fk(Fk,Rt,Rk)).
