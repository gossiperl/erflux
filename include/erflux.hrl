-ifndef(_erflux_records_included).
-define(_erflux_records_included, yeah).

-record(erflux_config, {
          username :: binary(),
          password :: binary(),
          host :: binary(),
          port :: integer(),
          protocol :: binary(),
          timeout :: integer() }).

-endif.