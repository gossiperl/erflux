-module(erflux_http_tests).
-include_lib("eunit/include/eunit.hrl").

-include("erflux.hrl").

-export([get_timestamp/0]).

-define(DATABASE_NAME, erfluxtest).
-define(DATABASE_ADMIN, testadmin).
-define(DATABASE_USER, testuser).
-define(DATABASE_USER_PASS, testpass).
-define(SERIES_NAME, testseries).

-define(DATABASE_NAME_ATOM, <<"erfluxtest">>).
-define(DATABASE_ADMIN_ATOM, <<"testadmin">>).
-define(DATABASE_USER_ATOM, <<"testuser">>).

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
  application:start(erflux),
  erflux_sup:add_erflux(erflux_http),
  erflux_http:delete_database(?DATABASE_NAME).

stop(_State) ->
  noreply.

get_timestamp() ->
  {Mega,Sec,Micro} = os:timestamp(),
  trunc( ((Mega*1000000+Sec)*1000000+Micro) / 1000000 ).

database_process_test() ->
  ?assertEqual(false, lists:member( [{<<"name">>, ?DATABASE_NAME_ATOM}], erflux_http:get_databases()) ),
  ?assertEqual(ok, erflux_http:create_database(?DATABASE_NAME)),
  ?assertEqual(true, lists:member( [{<<"name">>, ?DATABASE_NAME_ATOM}], erflux_http:get_databases()) ),
  ?assertEqual(ok, erflux_http:delete_database(?DATABASE_NAME)),
  ?assertEqual(false, lists:member( [{<<"name">>, ?DATABASE_NAME_ATOM}], erflux_http:get_databases()) ).

user_process_test() ->
  erflux_http:create_database(?DATABASE_NAME),
  ?assertMatch([], erflux_http:get_database_users(?DATABASE_NAME)),
  ?assertEqual(ok, erflux_http:create_user(?DATABASE_NAME, ?DATABASE_USER, ?DATABASE_USER_PASS)),
  ?assertMatch([{<<"name">>, ?DATABASE_USER_ATOM}, {<<"isAdmin">>, false}], erflux_http:get_database_user(?DATABASE_NAME, ?DATABASE_USER)),
  ?assertEqual(ok, erflux_http:update_database_user(?DATABASE_NAME, ?DATABASE_USER, [{<<"admin">>, true}])),
  ?assertMatch([{<<"name">>, ?DATABASE_USER_ATOM}, {<<"isAdmin">>, true}], erflux_http:get_database_user(?DATABASE_NAME, ?DATABASE_USER)),
  ?assertEqual(ok, erflux_http:delete_database_user(?DATABASE_NAME, ?DATABASE_USER)),
  ?assertMatch([], erflux_http:get_database_users(?DATABASE_NAME)),
  erflux_http:delete_database(?DATABASE_NAME).

admin_process_test() ->
  ?assertEqual(false, lists:member([{<<"name">>, ?DATABASE_ADMIN_ATOM}], erflux_http:get_cluster_admins())),
  ?assertEqual(ok, erflux_http:create_cluster_admin(?DATABASE_ADMIN, ?DATABASE_USER_PASS)),
  ?assertEqual(true, lists:member([{<<"name">>, ?DATABASE_ADMIN_ATOM}], erflux_http:get_cluster_admins())),
  ?assertEqual(ok, erflux_http:delete_cluster_admin(?DATABASE_ADMIN)),
  ?assertEqual(false, lists:member([{<<"name">>, ?DATABASE_ADMIN_ATOM}], erflux_http:get_cluster_admins())).

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
      { columns, [ a, b, c ] }
    ]
  ]),
  ?assertEqual(ok, Result1),

  Result2 = erflux_http:write_point(?DATABASE_NAME, ?SERIES_NAME, [
    { a, 4 },
    { b, 5 },
    { c, 6 }
  ]),
  ?assertEqual(ok, Result2),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"a">>]},
                 {<<"points">>,
                  [[_,_,4],[_,_,1]]}]],
               erflux_http:read_point(?DATABASE_NAME, [a], ?SERIES_NAME)),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"a">>]},
                 {<<"points">>,[[_,_,4]]}]],
               erflux_http:q(?DATABASE_NAME, <<"select a from testseries limit 1">>)),

  erflux_http:delete_database(?DATABASE_NAME).

write_read_mixed_test() ->
  erflux_http:create_database(?DATABASE_NAME),

  Result1 = erflux_http:write_series(?DATABASE_NAME, [
    [
      { points, [ [ 1, 2, 3 ] ] },
      { name, ?SERIES_NAME },
      { columns, [ a, <<"b">>, c ] }
    ]
  ]),
  ?assertEqual(ok, Result1),

  Result2 = erflux_http:write_point(?DATABASE_NAME, ?SERIES_NAME, [
    { <<"a">>, 4 },
    { b, 5 },
    { c, 6 }
  ]),
  ?assertEqual(ok, Result2),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"a">>]},
                 {<<"points">>,
                  [[_,_,4],[_,_,1]]}]],
               erflux_http:read_point(?DATABASE_NAME, [a], ?SERIES_NAME)),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"a">>]},
                 {<<"points">>,
                  [[_,_,4],[_,_,1]]}]],
               erflux_http:read_point(?DATABASE_NAME, [<<"a">>], ?SERIES_NAME)),

  ?assertMatch([[{<<"name">>,<<"testseries">>},
                 {<<"columns">>,[<<"time">>,<<"sequence_number">>,<<"a">>]},
                 {<<"points">>,[[_,_,4]]}]],
               erflux_http:q(?DATABASE_NAME, <<"select a from testseries limit 1">>)),

  erflux_http:delete_database(?DATABASE_NAME).

simultaneous_connections_test() ->
  {ok, ECPid} =       erflux_sup:add_erflux(erflux_custom, <<"root">>, <<"root">>, <<"192.168.50.115">>),
  ?assertEqual(ok,    erflux_http:create_database(?DATABASE_NAME)),
  ?assertEqual(true,  lists:member( [{<<"name">>, ?DATABASE_NAME_ATOM}], erflux_http:get_databases()) ),
  ?assertEqual(false, lists:member( [{<<"name">>, ?DATABASE_NAME_ATOM}], erflux_http:get_databases( ECPid )) ),
  ?assertEqual(ok,    erflux_http:delete_database(?DATABASE_NAME )),
  erflux_sup:remove_erflux(erflux_custom).

autneticate_user_test() ->
  ?assertEqual(ok,        erflux_http:create_database(?DATABASE_NAME)),
  ?assertEqual(ok,        erflux_http:create_user(?DATABASE_NAME, ?DATABASE_USER, ?DATABASE_USER_PASS)),
  ?assertMatch({ok, 200}, erflux_http:authenticate_database_user(?DATABASE_NAME, ?DATABASE_USER, ?DATABASE_USER_PASS)),
  ?assertEqual(ok,    erflux_http:delete_database(?DATABASE_NAME )).

ping_test() ->
  ?assertMatch([{<<"status">>, <<"ok">>}], erflux_http:ping()).
