-module(erflux_conf).

-export([configure/0]).

-include("erflux.hrl").

%% @doc Configures erflux from application environment settings.
configure() ->
  Username = application:get_env(erflux, username, <<"root">>),
  Password = application:get_env(erflux, password, <<"root">>),
  Port     = application:get_env(erflux, port, 8086),
  Host     = application:get_env(erflux, host, <<"localhost">>),
  Timeout  = application:get_env(erflux, timeout, infinity),

  Protocol = case application:get_env(erflux, ssl) of
      { ok, true } -> <<"https">>;
      _            -> <<"http">>
  end,

  #erflux_config{ username = Username,
                  password = Password,
                  port = Port,
                  host = Host,
                  protocol = Protocol,
                  timeout = Timeout }.
