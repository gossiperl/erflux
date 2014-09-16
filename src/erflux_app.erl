-module(erflux_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  case erflux_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.