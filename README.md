# InfluxDB client for Erlang

InfluxDB client for Erlang.

## Installation

Add dependency:

    {deps, [
      {erflux, ".*",
        {git, "git://github.com/radekg/erflux.git", {tag, "version-1"}}}
    }],

## Install

    ./rebar get-deps

Starting with default settings:

    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(idna),
    application:start(hackney),
    application:start(jsx),
    application:start(erflux),
    erflux_sup:add_erflux(erflux_http),
    erflux_http:get_databases().

Starting with additional configuration:

    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(idna),
    application:start(hackney),
    application:start(jsx),
    application:load(erflux),
    application:set_env(erflux, host, <<"192.168.50.115">>),
    application:set_env(erflux, port, 8086),
    application:set_env(erflux, username, <<"root">>),
    application:set_env(erflux, password, <<"root">>),
    application:set_env(erflux, ssl, false),
    application:set_env(erflux, timeout, infinity),
    application:start(erflux),
    erflux_sup:add_erflux(erflux_http),
    erflux_http:get_databases().

## Configuration

- `host`: IP address or host name of InfluxDB, default: `localhost`
- `port`: InfluxDB API port, default: `8086`
- `username`: username to use for the connection, default: `root`
- `password`: password for the user, default: `root`
- `ssl`: use SSL? boolean, default: false
- `timeout`: gen_server:call timeout, integer, milliseconds, default: `infinity`

## Writing data

    erflux_http:write_series(erfluxtest, [
      [
        { points, [ [ 1, 2, 3 ] ] },
        { name, testseries },
        { columns, [ a, b, c ] }
      ]
    ]).

or

    erflux_http:write_point(erfluxtest, testseries, [
      { a, 1 },
      { b, 2 },
      { c, 3 }
    ]).

## Reading data

Reading many columns:

    erflux_http:read_point(erfluxtest, [a, b], testseries).

or a single column:

    erflux_http:read_point(erfluxtest, a, testseries).

More complex queries like this:

    erflux_http:q(erfluxtest, <<"select A from testseries limit 1">>).

### Atoms? Binaries?

All functions support 2 variants:

- accepting atoms
- and accepting binaries

It's not possible to mix argument types. For example, this will fail:

    erflux_http:create_database_user(database, <<"username">>, password).

`Query` parameter of `erflux_http:q` is always binary.

The exceptions to the *all* rule are columns in `write_series`, `write_point` and `read_series`. All these are valid:

    erflux_http:write_series(erfluxtest, [
      [
        { points, [ [ 1, 2, 3 ] ] },
        { name, testseries },
        { columns, [ a, <<"b">>, c ] }
      ]
    ]).

    erflux_http:write_point(erfluxtest, testseries, [
      { a, 1 },
      { <<"b">>, 2 },
      { c, 3 }
    ]).

    erflux_http:read_point(erfluxtest, [<<"a">>, b], testseries).

## Other operations

Unit tests contain the full doverage of all other available operations. Unit tests can be found in the `test/` directory.

## Running unit tests

    ./rebar get-deps
    ./rebar compile
    ./rebar eunit

`simultaneous_connections_test` test requires Vagrant box to be up. The definition of the box can be found in the `Vagrantfile`.

## Starting additional clients

This can be done in two ways. With a supervisor:

    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(idna),
    application:start(hackney),
    application:start(jsx),
    application:start(erflux),
    %% This will connect to localhost, if no other settings provided:
    erflux_sup:add_erflux(erflux_http),
    erflux_http:get_databases().

    %% Start the additional client, connect to a different host:
    {ok, RemoteHost} = erflux_sup:add_erflux(erflux_custom_host, <<"root">>, <<"root">>, <<"somehost.influxdb">>).

    %% To list databases of the remote host, do the following:
    erflux_http:get_databases(RemoteHost).

    %% To remove the instance:
    erflux_sup:remove_erflux(erflux_custom_host).

Or bypassing the supervisor:

    { ok, Pid } = erflux_http:start_link( erflux_custom, #erflux_config{} ),
    erflux_http:get_databases( Pid ).

## Known issues

- `create_cluster_shard/4` and `create_cluster_shard/5` not necessarily implemented correctly
- `delete_cluster_shard/2` and `delete_cluster_shard/3` not necessarily implemented correctly
- no unit tests covering above functions

## Author

Radoslaw Gruchalski <radek@gruchalski.com>
https://github.com/radekg

## License

The MIT License (MIT)

Copyright (c) 2014 Radoslaw Gruchalski <radek@gruchalski.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.