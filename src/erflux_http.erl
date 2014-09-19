-module(erflux_http).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([
  get_databases/0,
  create_database/1,
  delete_database/1,
  get_database_users/1,
  create_user/3,
  delete_database_user/2,
  get_database_user/2,
  update_database_user/3,
  get_cluster_admins/0,
  delete_cluster_admin/1,
  create_cluster_admin/2,
  update_cluster_admin/2,
  get_continuous_queries/1,
  delete_continuous_query/2,
  get_cluster_servers/0,
  get_cluster_shard_spaces/0,
  get_cluster_shards/0,
  create_cluster_shard/4,
  delete_cluster_shard/2,
  get_interfaces/0,
  read_point/3,
  q/2,
  write_series/2,
  write_point/3,
  a2b/1 ]).

-include("erflux.hrl").

-type status_code() :: integer().
-type json_parse_error_reason() :: atom().

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

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

  case application:get_env(erflux, ssl) of
    { ok, Value5 } ->
      case Value5 of
        true ->
          Config4#erflux_config{ protocol = <<"https">> };
        _ ->
          Config4#erflux_config{ protocol = <<"http">> }
      end;
    undefined ->
      Config4#erflux_config{ protocol = <<"http">> }
  end.

init([]) ->
  {ok, {http, configure()}}.

%% Databases:

-spec get_databases() -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_databases() ->
  get_( path( <<"db">> ) ).

-spec create_database( DatabaseName :: atom() | binary() ) -> ok | { error, status_code() } | { error, status_code() }.
create_database(DatabaseName) when is_atom(DatabaseName) ->
  create_database( a2b( DatabaseName ) );
create_database(DatabaseName) when is_binary(DatabaseName) ->
  post( path( <<"db">> ), [ { name, DatabaseName } ]).

-spec delete_database( DatabaseName :: atom() | binary() ) -> ok.
delete_database(DatabaseName) when is_atom(DatabaseName) ->
  delete_database( a2b( DatabaseName ) );
delete_database(DatabaseName) when is_binary(DatabaseName) ->
  delete( path( <<"db/", DatabaseName/binary>> ) ).

%% Database users:

-spec get_database_users( DatabaseName :: atom() | binary() ) -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_database_users(DatabaseName) when is_atom(DatabaseName) ->
  get_database_users( a2b( DatabaseName ) );
get_database_users(DatabaseName) when is_binary(DatabaseName) ->
  get_( path( <<"db/", DatabaseName/binary, "/users">> ) ).

-spec create_user( DatabaseName :: atom() | binary(), Username :: atom() | binary(), Password :: atom() | binary() ) -> ok | { error, status_code() }.
create_user(DatabaseName, Username, Password) when is_atom(DatabaseName)
                                                andalso is_atom(Username)
                                                andalso is_atom(Password) ->
  create_user( a2b( DatabaseName ), a2b( Username ), a2b( Password ) );
create_user(DatabaseName, Username, Password) when is_binary(DatabaseName)
                                                andalso is_binary(Username)
                                                andalso is_binary(Password) ->
  post( path( <<"db/", DatabaseName/binary, "/users">> ), [ { name, Username }, { password, Password } ] ).

-spec delete_database_user( DatabaseName :: atom() | binary(), Username :: atom() | binary() ) -> ok.
delete_database_user(DatabaseName, Username) when is_atom(DatabaseName)
                                               andalso is_atom(Username) ->
  delete_database_user( a2b( DatabaseName ), a2b( Username ) );
delete_database_user(DatabaseName, Username) when is_binary(DatabaseName)
                                               andalso is_binary(Username) ->
  delete( path( <<"db/", DatabaseName/binary, "/users/", Username/binary>> ) ).

-spec get_database_user( DatabaseName :: atom() | binary(), Username :: atom() | binary() ) -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_database_user(DatabaseName, Username) when is_atom(DatabaseName)
                                            andalso is_atom(Username) ->
  get_database_user( a2b( DatabaseName ), a2b( Username ) );
get_database_user(DatabaseName, Username) when is_binary(DatabaseName)
                                            andalso is_binary(Username) ->
  get_( path( <<"db/", DatabaseName/binary, "/users/", Username/binary>> ) ).

-spec update_database_user( DatabaseName :: atom() | binary(), Username :: atom() | binary(), Params :: [ { binary(), any() } ] ) -> ok | { error, status_code() }.
update_database_user(DatabaseName, Username, Params) when is_atom(DatabaseName)
                                                       andalso is_atom(Username)
                                                       andalso is_list(Params) ->
  update_database_user( a2b( DatabaseName ), a2b( Username ), Params );
update_database_user(DatabaseName, Username, Params) when is_binary(DatabaseName)
                                                       andalso is_binary(Username)
                                                       andalso is_list(Params) ->
  post( path( <<"db/", DatabaseName/binary, "/users/", Username/binary>> ), Params ).

%% Cluster admins:

-spec get_cluster_admins() -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_cluster_admins() ->
  get_( path( <<"cluster_admins">> ) ).

-spec delete_cluster_admin( Username :: atom() | binary() ) -> ok.
delete_cluster_admin(Username) when is_atom(Username) ->
  delete_cluster_admin( a2b( Username ) );
delete_cluster_admin(Username) when is_binary(Username) ->
  delete( path( <<"cluster_admins/", Username/binary>> ) ).

-spec create_cluster_admin( Username :: atom() | binary(), Password :: atom() | binary() ) -> ok | { error, status_code() }.
create_cluster_admin(Username, Password) when is_atom(Username)
                                           andalso is_atom(Password) ->
  create_cluster_admin( a2b( Username ), a2b( Password ) );
create_cluster_admin(Username, Password) when is_binary(Username)
                                           andalso is_binary(Password) ->
  post( path( <<"cluster_admins">> ), [ { name, Username }, { password, Password } ] ).

-spec update_cluster_admin( Username :: atom() | binary(), Params :: [ { binary(), any() } ] ) -> ok | { error, status_code() }.
update_cluster_admin(Username, Params) when is_atom(Username)
                                         andalso is_list(Params) ->
  update_cluster_admin( a2b( Username ), Params );
update_cluster_admin(Username, Params) when is_binary(Username)
                                         andalso is_list(Params) ->
  post( path( <<"cluster_admins/", Username/binary>> ), Params ).

%% Continous queries:

-spec get_continuous_queries( DatabaseName :: atom() | binary() ) -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_continuous_queries(DatabaseName) when is_binary(DatabaseName) ->
  get_continuous_queries( a2b( DatabaseName ) );
get_continuous_queries(DatabaseName) when is_binary(DatabaseName) ->
  get_( path( <<"db/", DatabaseName/binary, "/continuous_queries">> ) ).

-spec delete_continuous_query( DatabaseName :: atom() | binary(), Id :: atom() | binary() ) -> ok.
delete_continuous_query(DatabaseName, Id) when is_binary(DatabaseName)
                                            andalso is_binary(Id) ->
  delete_continuous_query( a2b( DatabaseName ), a2b( Id ) );
delete_continuous_query(DatabaseName, Id) when is_binary(DatabaseName)
                                            andalso is_binary(Id) ->
  delete( path( <<"db/", DatabaseName/binary, "continuous_queries/", Id/binary>> ) ).

%% Cluster servers and shards

-spec get_cluster_servers() -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_cluster_servers() ->
  get_( path( <<"cluster/servers">> ) ).

-spec get_cluster_shards() -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_cluster_shards() ->
  get_( path( <<"cluster/shards">> ) ).

-spec create_cluster_shard( StartTime :: integer(), EndTime :: integer(), LongTerm :: boolean(), ServerIds :: list() ) -> ok | { error, status_code() }.
create_cluster_shard(StartTime, EndTime, LongTerm, ServerIds) ->
  post( path( <<"cluster/shards">> ),
              [
                { startTime, StartTime },
                { endTime, EndTime },
                { longTerm, LongTerm },
                { shards, [ { serverIds, ServerIds } ] } ] ).

-spec delete_cluster_shard( Id :: binary(), ServerIds :: list() ) -> ok.
delete_cluster_shard(Id, ServerIds) when is_binary(Id)
                                      andalso is_list(ServerIds) ->
  delete( path( <<"cluster/shards/", Id/binary>> ), { serverIds, ServerIds } ).

-spec get_cluster_shard_spaces() -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_cluster_shard_spaces() ->
  get_( path( <<"cluster/shard_spaces">> ) ).

%% Interfaces:

-spec get_interfaces() -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_interfaces() ->
  get_( path( <<"interfaces">> ) ).

%% Reading data:

-spec read_point( DatabaseName :: atom() | binary(), FieldNames :: atom() | binary() | [ atom() | binary() ], SeriesName :: atom() | binary() ) -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
read_point(DatabaseName, FieldNames, SeriesName) when is_atom(DatabaseName)
                                                   andalso is_atom(FieldNames)
                                                   andalso is_atom(SeriesName) ->
  read_point( a2b( DatabaseName ), [ a2b( FieldNames ) ], a2b( SeriesName ) );
read_point(DatabaseName, FieldNames, SeriesName) when is_binary(DatabaseName)
                                                   andalso is_binary(FieldNames)
                                                   andalso is_binary(SeriesName) ->
  read_point( DatabaseName, [ FieldNames ], SeriesName );
read_point(DatabaseName, FieldNames, SeriesName) when is_atom(DatabaseName)
                                                   andalso is_list(FieldNames)
                                                   andalso is_atom(SeriesName) ->
  read_point( a2b( DatabaseName ), FieldNames, a2b( SeriesName ) );
read_point(DatabaseName, FieldNames, SeriesName) when is_binary(DatabaseName)
                                                   andalso is_list(FieldNames)
                                                   andalso is_binary(SeriesName) ->
  QueryStart = lists:foldl(fun(FieldName, Bin) ->
    case Bin of
      <<"SELECT">> ->
        case is_atom( FieldName ) of
          true ->
            FieldNameBin = a2b(FieldName),
            <<Bin/binary, " ", FieldNameBin/binary>>;
          false ->
            <<Bin/binary, " ", FieldName/binary>>
        end;
      _ ->
        case is_atom( FieldName ) of
          true ->
            FieldNameBin = a2b(FieldName),
            <<Bin/binary, " ", FieldNameBin/binary>>;
          false ->
            <<Bin/binary, " ", FieldName/binary>>
        end
    end
  end, <<"SELECT">>, FieldNames),
  q( DatabaseName, <<QueryStart/binary, " FROM ", SeriesName/binary, ";">> ).

-spec q( DatabaseName :: atom() | binary(), Query :: binary() ) -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
q(DatabaseName, Query) when is_atom(DatabaseName)
                         andalso is_binary(Query) ->
  q( a2b( DatabaseName ), Query );
q(DatabaseName, Query) when is_binary(DatabaseName)
                         andalso is_binary(Query) ->
  get_( path( <<"db/", DatabaseName/binary, "/series">> , [ { q, Query } ] ) ).

%% Writing data
-spec write_series( DatabaseName :: atom() | binary(), SeriesData :: list() ) -> ok | { error, status_code() }.
write_series( DatabaseName, SeriesData ) when is_atom(DatabaseName)
                                           andalso is_list(SeriesData) ->
  write_series( a2b( DatabaseName ), SeriesData );
write_series( DatabaseName, SeriesData ) when is_binary(DatabaseName)
                                           andalso is_list(SeriesData) ->
  post( path( <<"db/", DatabaseName/binary, "/series">> ), SeriesData ).

-spec write_point( DatabaseName :: atom() | binary(), SeriesName :: atom() | binary(), Values :: [ { atom() | binary(), any() } ] ) -> ok | { error, status_code() }.
write_point( DatabaseName, SeriesName, Values ) when is_atom(DatabaseName)
                                                  andalso is_atom(SeriesName)
                                                  andalso is_list(Values) ->
  write_point( a2b( DatabaseName ), a2b( SeriesName ), Values );
write_point( DatabaseName, SeriesName, Values ) when is_binary(DatabaseName)
                                                  andalso is_binary(SeriesName)
                                                  andalso is_list(Values) ->
  Datum = lists:foldl(fun({ Column, Point }, Acc) ->
    [ { points, [ ExistingPoints ] }, { name, _ }, { columns, ExistingColumns } ] = Acc,
    ColumnToAppend = case is_atom( Column ) of
      true ->
        a2b( Column );
      false ->
        Column
    end,
    [ { points, [ ExistingPoints ++ [ Point ] ] }, { name, SeriesName }, { columns, ExistingColumns ++ [ ColumnToAppend ] } ]
  end, [ { points, [ [] ] }, { name, SeriesName }, { columns, [] } ], Values),

  post( path( <<"db/", DatabaseName/binary, "/series">> ), [ Datum ] ).

%% Internals:

-spec path( Action :: binary() ) -> binary().
path( Action ) ->
  path( Action, [] ).

-spec path( Component :: binary(), Options :: list() ) -> binary().
path( Action, Options ) ->
  gen_server:call(?MODULE, { path, Action, Options }).

-spec get_( Action :: binary() ) -> list() | { error, json_parse, json_parse_error_reason() } | { error, status_code() }.
get_( Action ) ->
  gen_server:call(?MODULE, { get, Action }).

-spec post( Action :: binary(), Data :: list() ) -> ok | { error, status_code() }.
post( Action, Data ) ->
  gen_server:call(?MODULE, { post, Action, Data }).

-spec delete( Action :: binary() ) -> ok.
delete( Action ) ->
  delete( Action, [] ).

-spec delete( Action :: binary(), Data :: list() ) -> ok.
delete( Action, Data ) ->
  gen_server:cast(?MODULE, { delete, Action, Data }),
  ok.

-spec a2b( Atom :: atom() ) -> binary().
a2b( Atom ) ->
  list_to_binary( atom_to_list( Atom ) ).

handle_call( { path, Action, Options }, From, { http, Config } ) ->
  Username = Config#erflux_config.username,
  Password = Config#erflux_config.password,
  case lists:keyfind( q, 1, Options ) of
    false ->
      gen_server:reply(From, <<Action/binary, "?u=", Username/binary, "&p=", Password/binary>>);
    { q, Value } ->
      EscapedValue = list_to_binary( edoc_lib:escape_uri( binary_to_list( Value ) ) ),
      gen_server:reply(From, <<Action/binary, "?u=", Username/binary, "&p=", Password/binary, "&q=", EscapedValue/binary>>)
  end,
  { noreply, { http, Config } };

handle_call( { get, Path }, From, { http, Config } ) ->
  Protocol = Config#erflux_config.protocol,
  Host = Config#erflux_config.host,
  Port = list_to_binary(integer_to_list( Config#erflux_config.port )),
  Uri = <<Protocol/binary, "://", Host/binary, ":", Port/binary, "/", Path/binary>>,
  {ok, StatusCode, _, Ref} = hackney:request(get, Uri, [], <<>>),
  case trunc(StatusCode/100) of
    2 ->
      {ok, Body} = hackney:body(Ref),
      try
        case jsx:decode( Body ) of
          JsonData ->
            gen_server:reply(From, JsonData)
        end
      catch
        _Error:Reason ->
          gen_server:reply(From, { error, json_parse, Reason })
      end;
    _ ->
      gen_server:reply(From, { error, StatusCode })
  end,
  { noreply, { http, Config } };

handle_call( { post, Path, Data }, From, { http, Config } ) ->
  Protocol = Config#erflux_config.protocol,
  Host = Config#erflux_config.host,
  Port = list_to_binary(integer_to_list( Config#erflux_config.port )),
  Uri = <<Protocol/binary, "://", Host/binary, ":", Port/binary, "/", Path/binary>>,
  {ok, StatusCode, _, Ref} = hackney:request(post, Uri, [], jsx:encode( Data )),
  {ok, _Body} = hackney:body(Ref),
  case trunc(StatusCode/100) of
    2 ->
      gen_server:reply(From, ok);
    _ ->
      gen_server:reply(From, { error, StatusCode } )
  end,
  { noreply, { http, Config } }.

handle_cast( { delete, Path, Data }, { http, Config } ) ->
  Protocol = Config#erflux_config.protocol,
  Host = Config#erflux_config.host,
  Port = list_to_binary(integer_to_list( Config#erflux_config.port )),
  Uri = <<Protocol/binary, "://", Host/binary, ":", Port/binary, "/", Path/binary>>,
  BinaryData = case Data of
    [] ->
      <<"">>;
    _ ->
      jsx:encode( Data )
  end,
  {ok, _, _, _} = hackney:request(delete, Uri, [], BinaryData),
  { noreply, { http, Config } }.

%% gen_server behaviour:

handle_info(_Msg, LoopData) ->
  {noreply, LoopData}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _LoopData) ->
  ok.
