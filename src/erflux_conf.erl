-module(erflux_conf).

-export([configure/0]).

-include("erflux.hrl").

%% @doc Configures erflux from application environment settings.
configure() ->

  Config1 = case application:get_env(erflux, username) of
    { ok, Value1 } ->
      #erflux_config{ username = Value1 };
    undefined ->
      #erflux_config{ username = <<"root">> }
  end,

  Config2 = case application:get_env(erflux, password) of
    { ok, Value2 } ->
      Config1#erflux_config{ password = Value2 };
    undefined ->
      Config1#erflux_config{ password = <<"root">> }
  end,

  Config3 = case application:get_env(erflux, port) of
    { ok, Value3 } ->
      Config2#erflux_config{ port = Value3 };
    undefined ->
      Config2#erflux_config{ port = 8086 }
  end,

  Config4 = case application:get_env(erflux, host) of
    { ok, Value4 } ->
      Config3#erflux_config{ host = Value4 };
    undefined ->
      Config3#erflux_config{ host = <<"localhost">> }
  end,

  Config5 = case application:get_env(erflux, ssl) of
    { ok, Value5 } ->
      case Value5 of
        true ->
          Config4#erflux_config{ protocol = <<"https">> };
        _ ->
          Config4#erflux_config{ protocol = <<"http">> }
      end;
    undefined ->
      Config4#erflux_config{ protocol = <<"http">> }
  end,

  case application:get_env(erflux, timeout) of
    { ok, Value6 } ->
      Config5#erflux_config{ timeout = Value6 };
    undefined ->
      Config5#erflux_config{ timeout = infinity }
  end.