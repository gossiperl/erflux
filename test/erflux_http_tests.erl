-module(erflux_http_tests).
-include_lib("eunit/include/eunit.hrl").

-export([get_timestamp/0]).

-define(DATABASE_NAME, <<"erfluxtest">>).
-define(DATABASE_ADMIN, <<"testadmin">>).
-define(DATABASE_USER, <<"testuser">>).
-define(DATABASE_USER_PASS, <<"testpass">>).
-define(SERIES_NAME, <<"testseries">>).

erflux_http_test_() ->
  {setup, fun start/0, fun stop/1, [
    fun database_process_test/0 ] }.

start() ->
  application:start(crypto),
  application:start(asn1),
  application:start(public_key),
  application:start(ssl),
  application:start(idna),
  application:start(hackney),
  application:start(jsx),
  application:start(erflux).

stop(_State) ->
  noreply.

get_timestamp() ->
  {Mega,Sec,Micro} = os:timestamp(),
  trunc( ((Mega*1000000+Sec)*1000000+Micro) / 1000000 ).

database_process_test() ->
  ?assertEqual(false, lists:member( [{<<"name">>, ?DATABASE_NAME}], erflux_http:get_databases()) ),
  ?assertEqual(ok, erflux_http:create_database(?DATABASE_NAME)),
  ?assertEqual(true, lists:member( [{<<"name">>, ?DATABASE_NAME}], erflux_http:get_databases()) ),
  ?assertEqual(ok, erflux_http:delete_database(?DATABASE_NAME)),
  ?assertEqual(false, lists:member( [{<<"name">>, ?DATABASE_NAME}], erflux_http:get_databases()) ).

user_process_test() ->
  erflux_http:create_database(?DATABASE_NAME),
  ?assertMatch([], erflux_http:get_database_users(?DATABASE_NAME)),
  ?assertEqual(ok, erflux_http:create_user(?DATABASE_NAME, ?DATABASE_USER, ?DATABASE_USER_PASS)),
  ?assertMatch([{<<"name">>, ?DATABASE_USER}, {<<"isAdmin">>, false}], erflux_http:get_database_user(?DATABASE_NAME, ?DATABASE_USER)),
  ?assertEqual(ok, erflux_http:update_database_user(?DATABASE_NAME, ?DATABASE_USER, [{<<"admin">>, true}])),
  ?assertMatch([{<<"name">>, ?DATABASE_USER}, {<<"isAdmin">>, true}], erflux_http:get_database_user(?DATABASE_NAME, ?DATABASE_USER)),
  ?assertEqual(ok, erflux_http:delete_database_user(?DATABASE_NAME, ?DATABASE_USER)),
  ?assertMatch([], erflux_http:get_database_users(?DATABASE_NAME)),
  erflux_http:delete_database(?DATABASE_NAME).

admin_process_test() ->
  ?assertEqual(false, lists:member([{<<"name">>, ?DATABASE_ADMIN}], erflux_http:get_cluster_admins())),
  ?assertEqual(ok, erflux_http:create_cluster_admin(?DATABASE_ADMIN, ?DATABASE_USER_PASS)),
  ?assertEqual(true, lists:member([{<<"name">>, ?DATABASE_ADMIN}], erflux_http:get_cluster_admins())),
  ?assertEqual(ok, erflux_http:delete_cluster_admin(?DATABASE_ADMIN)),
  ?assertEqual(false, lists:member([{<<"name">>, ?DATABASE_ADMIN}], erflux_http:get_cluster_admins())).

cluster_servers_test() ->
  ?assertEqual(true, length( erflux_http:get_cluster_servers() ) > 0).

interfaces_test() ->
  Interfaces = erflux_http:get_interfaces(),
  ?assertEqual(true, is_list( Interfaces ) ),
  ?assertMatch(<<"default">>, hd(Interfaces)).

shards_process_test() ->
  yet_to_be_implemented.

shard_spaces_process_test() ->
  yet_to_be_implemented.

write_read_test() ->
  erflux_http:create_database(?DATABASE_NAME),

  Result1 = erflux_http:write_series(?DATABASE_NAME, [
    [
      { points, [ [ 1, 2, 3 ] ] },
      { name, ?SERIES_NAME },
      { columns, [ <<"A">>, <<"B">>, <<"C">> ] }
    ]
  ]),
  ?assertEqual(ok, Result1),

  Result2 = erflux_http:write_point(?DATABASE_NAME, ?SERIES_NAME, [
    { <<"A">>, 4 },
    { <<"B">>, 5 },
    { <<"C">>, 6 }
  ]),
  ?assertEqual(ok, Result2),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"A">>]},
                 {<<"points">>,
                  [[_,_,4],[_,_,1]]}]],
               erflux_http:read_point(?DATABASE_NAME, [<<"A">>], ?SERIES_NAME)),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"A">>]},
                 {<<"points">>,[[_,_,4]]}]],
               erflux_http:q(?DATABASE_NAME, <<"select A from testseries limit 1">>)),

  erflux_http:delete_database(?DATABASE_NAME).
