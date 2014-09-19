-module(erflux_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([
  add_erflux/1,
  add_erflux/3,
  add_erflux/4,
  add_erflux/5,
  add_erflux/6,
  add_erflux/7,
  remove_erflux/1
]).

-include("erflux.hrl").

-define(ATTEMPT_MODULE_USE, true).
-define(CUSTOM_PID, fasle).

-type child() :: undefined | pid().
-type config() :: #erflux_config{}.
-type statup_result() :: {ok, Child :: child()}
                          | {ok, Child :: child(), Info :: term()}
                          | {error, already_present}
                          | {error, {already_started, Child :: child()}}
                          | {error, term()}.

%% @doc Starts erflux supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Supervisor init.
init([]) ->
  {ok, {{one_for_all, 10, 10}, [] }}.

-spec add_erflux( Name :: atom() ) -> statup_result().
%% @doc Starts instance of erflux with a given name. If name other than erflux_http, pid() versions of erflux_http functions have to be used.
add_erflux(Name) ->
  add_erflux_internal( Name, #erflux_config{} ).

-spec add_erflux( Name :: atom(), Username :: binary(), Password :: binary() ) -> statup_result().
%% @doc Starts instance of erflux with a given name. If name other than erflux_http, pid() versions of erflux_http functions have to be used.
add_erflux(Name, Username, Password) ->
  add_erflux_internal( Name, #erflux_config{
                              username = Username,
                              password = Password } ).

-spec add_erflux( Name :: atom(), Username :: binary(), Password :: binary(), Host :: binary() ) -> statup_result().
%% @doc Starts instance of erflux with a given name. If name other than erflux_http, pid() versions of erflux_http functions have to be used.
add_erflux(Name, Username, Password, Host ) ->
  add_erflux_internal( Name, #erflux_config{
                              username = Username,
                              password = Password,
                              host = Host } ).

-spec add_erflux( Name :: atom(), Username :: binary(), Password :: binary(), Host :: binary(), Port :: binary() ) -> statup_result().
%% @doc Starts instance of erflux with a given name. If name other than erflux_http, pid() versions of erflux_http functions have to be used.
add_erflux(Name, Username, Password, Host, Port) ->
  add_erflux_internal( Name, #erflux_config{
                              username = Username,
                              password = Password,
                              host = Host,
                              port = Port } ).

-spec add_erflux( Name :: atom(), Username :: binary(), Password :: binary(), Host :: binary(), Port :: binary(), Protocol :: binary() ) -> statup_result().
%% @doc Starts instance of erflux with a given name. If name other than erflux_http, pid() versions of erflux_http functions have to be used.
add_erflux(Name, Username, Password, Host, Port, Protocol) ->
  add_erflux_internal( Name, #erflux_config{
                              username = Username,
                              password = Password,
                              host = Host,
                              port = Port,
                              protocol = Protocol } ).

-spec add_erflux( Name :: atom(), Username :: binary(), Password :: binary(), Host :: binary(), Port :: binary(), Protocol :: binary(), Timeout :: integer() ) -> statup_result().
%% @doc Starts instance of erflux with a given name. If name other than erflux_http, pid() versions of erflux_http functions have to be used.
add_erflux(Name, Username, Password, Host, Port, Protocol, Timeout) ->
  add_erflux_internal( Name, #erflux_config{
                              username = Username,
                              password = Password,
                              host = Host,
                              port = Port,
                              protocol = Protocol,
                              timeout = Timeout } ).

-spec add_erflux_internal(Name :: atom(), Configuration :: config()) -> statup_result().
%% @doc Used internally to start erflux instance.
add_erflux_internal(Name, Configuration) ->
  case Name of
    erflux_http ->
      supervisor:start_child(?MODULE, {
        Name,
        { erflux_http, start_link, [ Configuration ]},
        permanent,
        brutal_kill,
        supervisor,
        []
      });
    _ ->
      supervisor:start_child(?MODULE, {
        Name,
        { erflux_http, start_link, [Name, Configuration]},
        permanent,
        brutal_kill,
        supervisor,
        []
      })
  end.

-spec remove_erflux( Name :: atom() ) -> {ok, Child :: child()}
                                         | {ok, Child :: child(), Info :: term()}
                                         | {error, running}
                                         | {error, restarting}
                                         | {error, not_found}
                                         | {error, simple_one_for_one}
                                         | {error, term()}.
%% @doc Removing an instance of erflux.
remove_erflux(Name) ->
  case supervisor:terminate_child(?MODULE, Name) of
    ok ->
      supervisor:delete_child(?MODULE, Name);
    {error, Reason} ->
      {error, Reason}
  end.