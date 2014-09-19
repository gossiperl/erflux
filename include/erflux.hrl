-ifndef(_erflux_records_included).
-define(_erflux_records_included, yeah).

-record(erflux_config, {
          username = <<"root">> :: binary(),
          password = <<"root">> :: binary(),
          host = <<"localhost">> :: binary(),
          port = 8086 :: integer(),
          protocol = <<"http">> :: binary(),
          timeout = infinity :: integer() }).

-endif.